"""
FDS simulation runner and job management.
"""

import subprocess
from pathlib import Path
from typing import TYPE_CHECKING

from .exceptions import FDSExecutionError, FDSTimeoutError
from .monitor import ProgressInfo, ProgressMonitor, parse_out_file_for_errors
from .process import (
    build_fds_command,
    find_fds_executable,
    get_environment_for_execution,
    validate_fds_executable,
)

if TYPE_CHECKING:
    from ..analysis.results import Results


class Job:
    """
    Represents a running or completed FDS simulation job.

    Parameters
    ----------
    process : subprocess.Popen
        The running FDS process
    fds_file : Path
        Path to the FDS input file
    output_dir : Path
        Directory where outputs are written
    monitor : ProgressMonitor, optional
        Progress monitor instance
    chid : str
        Case identifier

    Examples
    --------
    >>> job = runner.run(fds_file="test.fds", wait=False)
    >>> while job.is_running():
    ...     print(f"Progress: {job.progress}%")
    ...     time.sleep(5)
    >>> results = job.get_results()
    """

    def __init__(
        self,
        process: "subprocess.Popen[bytes]",
        fds_file: Path,
        output_dir: Path,
        monitor: ProgressMonitor | None,
        chid: str,
    ):
        self._process = process
        self.fds_file = fds_file
        self.output_dir = output_dir
        self._monitor = monitor
        self.chid = chid
        self._stdout: str | None = None
        self._stderr: str | None = None
        self._exit_code: int | None = None

    def is_running(self) -> bool:
        """
        Check if the job is still running.

        Returns
        -------
        bool
            True if job is running, False otherwise
        """
        if self._process.poll() is None:
            return True

        # Process finished, capture output
        if self._stdout is None:
            stdout, stderr = self._process.communicate()
            self._stdout = stdout.decode() if stdout else ""
            self._stderr = stderr.decode() if stderr else ""
            self._exit_code = self._process.returncode

        return False

    @property
    def progress(self) -> float:
        """
        Get current progress percentage.

        Returns
        -------
        float
            Progress percentage (0-100)
        """
        if self._monitor is None:
            return 0.0

        progress_info = self._monitor.get_progress()
        if progress_info is None:
            return 0.0

        return progress_info.percent_complete

    @property
    def progress_info(self) -> ProgressInfo | None:
        """
        Get detailed progress information.

        Returns
        -------
        ProgressInfo, optional
            Detailed progress info, or None if not available
        """
        if self._monitor is None:
            return None
        return self._monitor.get_progress()

    @property
    def estimated_time_remaining(self) -> float | None:
        """
        Get estimated time remaining in seconds.

        Returns
        -------
        float, optional
            Estimated seconds remaining, or None if not available
        """
        progress_info = self.progress_info
        if progress_info is None:
            return None
        return progress_info.eta_seconds

    @property
    def exit_code(self) -> int | None:
        """
        Get process exit code.

        Returns
        -------
        int, optional
            Exit code if process finished, None otherwise
        """
        if self._exit_code is None:
            self._process.poll()
            self._exit_code = self._process.returncode
        return self._exit_code

    def wait(self, timeout: float | None = None) -> "Results":
        """
        Wait for job to complete and return results.

        Parameters
        ----------
        timeout : float, optional
            Maximum time to wait in seconds

        Returns
        -------
        Results
            Simulation results object

        Raises
        ------
        FDSTimeoutError
            If timeout is exceeded
        FDSExecutionError
            If FDS execution failed
        """
        try:
            stdout, stderr = self._process.communicate(timeout=timeout)
            self._stdout = stdout.decode() if stdout else ""
            self._stderr = stderr.decode() if stderr else ""
            self._exit_code = self._process.returncode
        except subprocess.TimeoutExpired as e:
            self.kill()
            raise FDSTimeoutError(
                f"FDS execution exceeded timeout of {timeout} seconds",
                fds_file=str(self.fds_file),
            ) from e
        finally:
            if self._monitor is not None:
                self._monitor.stop()

        # Check for errors
        if self._exit_code != 0:
            # Parse .out file for error messages
            out_file = self.output_dir / f"{self.chid}.out"
            errors = parse_out_file_for_errors(out_file)
            error_msg = "\n".join(errors) if errors else "Unknown error"

            raise FDSExecutionError(
                f"FDS execution failed: {error_msg}",
                exit_code=self._exit_code,
                stdout=self._stdout,
                stderr=self._stderr,
                fds_file=str(self.fds_file),
            )

        # Import here to avoid circular import
        from ..analysis.results import Results

        return Results(chid=self.chid, output_dir=self.output_dir)

    def kill(self) -> None:
        """Kill the running job."""
        if self.is_running():
            self._process.kill()
            if self._monitor is not None:
                self._monitor.stop()

    def get_results(self) -> "Results":
        """
        Get results (waits for completion if still running).

        Returns
        -------
        Results
            Simulation results object
        """
        return self.wait()


class FDSRunner:
    """
    Execute FDS simulations locally.

    Parameters
    ----------
    fds_executable : Path, optional
        Path to FDS executable (auto-detected if not provided)

    Examples
    --------
    >>> runner = FDSRunner()
    >>> results = runner.run("test.fds", n_threads=4)

    >>> # Non-blocking execution
    >>> job = runner.run("test.fds", wait=False, monitor=True)
    >>> while job.is_running():
    ...     print(f"Progress: {job.progress}%")
    ...     time.sleep(5)
    >>> results = job.get_results()
    """

    def __init__(self, fds_executable: Path | None = None):
        if fds_executable is None:
            self.fds_executable = find_fds_executable()
        else:
            self.fds_executable = Path(fds_executable)

        # Validate FDS executable
        is_valid, version = validate_fds_executable(self.fds_executable)
        if not is_valid:
            raise ValueError(f"Invalid FDS executable: {version}")

        self.fds_version = version

    def run(
        self,
        fds_file: str | Path,
        n_threads: int = 1,
        n_mpi: int = 1,
        mpiexec_path: str = "mpiexec",
        output_dir: Path | None = None,
        monitor: bool = True,
        wait: bool = True,
        timeout: float | None = None,
    ) -> "Results | Job":
        """
        Run an FDS simulation.

        Parameters
        ----------
        fds_file : str or Path
            Path to FDS input file
        n_threads : int
            Number of OpenMP threads (default: 1)
        n_mpi : int
            Number of MPI processes (default: 1)
        mpiexec_path : str
            Path to mpiexec command (default: 'mpiexec')
        output_dir : Path, optional
            Output directory (default: same as fds_file)
        monitor : bool
            Enable progress monitoring (default: True)
        wait : bool
            Wait for completion (default: True)
        timeout : float, optional
            Timeout in seconds (only used if wait=True)

        Returns
        -------
        Results or Job
            Results object if wait=True, Job object if wait=False

        Raises
        ------
        FDSExecutionError
            If FDS execution fails
        FDSTimeoutError
            If execution exceeds timeout

        Examples
        --------
        >>> runner = FDSRunner()
        >>> results = runner.run("test.fds", n_threads=4)

        >>> # Non-blocking
        >>> job = runner.run("test.fds", wait=False)
        >>> results = job.wait()
        """
        fds_file = Path(fds_file)
        if not fds_file.exists():
            raise FileNotFoundError(f"FDS file not found: {fds_file}")

        # Determine output directory
        if output_dir is None:
            output_dir = fds_file.parent

        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Extract CHID from file
        chid = fds_file.stem

        # Build command
        cmd = build_fds_command(
            fds_file=fds_file,
            fds_executable=self.fds_executable,
            n_threads=n_threads,
            n_mpi=n_mpi,
            mpiexec_path=mpiexec_path,
        )

        # Get environment
        env = get_environment_for_execution(n_threads=n_threads)

        # Start process
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=str(output_dir),
            env=env,
        )

        # Set up monitoring
        progress_monitor = None
        if monitor:
            out_file = output_dir / f"{chid}.out"
            progress_monitor = ProgressMonitor(out_file)
            progress_monitor.start()

        # Create job object
        job = Job(
            process=process,
            fds_file=fds_file,
            output_dir=output_dir,
            monitor=progress_monitor,
            chid=chid,
        )

        # Wait for completion or return job
        if wait:
            return job.wait(timeout=timeout)
        return job
