"""
FDS simulation runner and job management.
"""

import subprocess
from pathlib import Path
from typing import TYPE_CHECKING, Any

from pyfds.exceptions import ExecutionError, FDSTimeoutError
from pyfds.execution.monitor import ProgressInfo, ProgressMonitor, parse_out_file_for_errors
from pyfds.execution.platform import PlatformExecutor
from pyfds.execution.process import get_environment_for_execution, validate_fds_executable
from pyfds.utils import get_logger
from pyfds.validation import ExecutionValidator


def _extract_chid(fds_file: Path) -> str:
    """Extract CHID from FDS input file.

    Parses the &HEAD namelist to find CHID. Falls back to filename
    if parsing fails.

    Parameters
    ----------
    fds_file : Path
        Path to FDS input file.

    Returns
    -------
    str
        The CHID value.
    """
    import re

    try:
        content = fds_file.read_text(encoding="utf-8", errors="replace")

        # Look for CHID in &HEAD namelist
        # Handles both single and double quotes
        pattern = r"&HEAD\s+.*?CHID\s*=\s*['\"]([^'\"]+)['\"]"
        match = re.search(pattern, content, re.DOTALL | re.IGNORECASE)

        if match:
            return match.group(1)
    except Exception:
        pass  # Fall back to filename

    return fds_file.stem


if TYPE_CHECKING:
    from pyfds.analysis.results import Results
    from pyfds.config import RunConfig

logger = get_logger(__name__)


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
        ExecutionError
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

            raise ExecutionError(
                f"FDS execution failed: {error_msg}",
                exit_code=self._exit_code,
                stdout=self._stdout,
                stderr=self._stderr,
                fds_file=str(self.fds_file),
            )

        # Import here to avoid circular import
        from pyfds.analysis.results import Results

        return Results(chid=self.chid, output_dir=self.output_dir)

    def kill(self) -> None:
        """Kill the running job."""
        if self.is_running():
            self._process.kill()
            if self._monitor is not None:
                self._monitor.stop()

    def request_stop(self) -> None:
        """
        Request graceful shutdown by creating CHID.stop file.

        From FDS User Guide §3.4:
        "To stop a calculation before its scheduled time, create a file in
        the same directory as the output files called CHID.stop. The existence
        of this file stops the program gracefully, causing it to dump out the
        latest flow variables for viewing in Smokeview."

        This method creates the stop file and logs the action. FDS will check
        for the file and stop gracefully after completing the current timestep.

        Examples
        --------
        >>> job = sim.run(wait=False)
        >>> # ... simulation running ...
        >>> job.request_stop()  # Ask FDS to stop gracefully
        >>> job.wait()  # Wait for graceful shutdown
        """
        stop_file = self.output_dir / f"{self.chid}.stop"
        stop_file.touch()
        logger.info(f"Created stop file: {stop_file}")
        logger.info(
            "FDS will stop gracefully after current timestep completes. "
            "Use wait() or get_results() to wait for shutdown."
        )

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

    def __init__(
        self,
        fds_executable: Path | None = None,
        validate_parallel: bool = True,
    ):
        """
        Initialize FDS runner.

        Parameters
        ----------
        fds_executable : Path, optional
            Path to FDS executable (auto-detected if not provided)
        validate_parallel : bool
            Enable parallel configuration validation (default: True)
        """
        # Initialize platform executor
        self.platform_executor = PlatformExecutor(fds_executable)
        self.fds_executable = self.platform_executor.fds_command

        logger.debug(f"FDS executable: {self.fds_executable}")
        logger.debug(
            f"Platform: {self.platform_executor.platform}, "
            f"Wrapper: {self.platform_executor.uses_wrapper}"
        )

        # Validate FDS executable
        is_valid, version = validate_fds_executable(self.fds_executable)
        if not is_valid:
            logger.error(f"Invalid FDS executable: {self.fds_executable}")
            raise ValueError(f"Invalid FDS executable: {version}")

        self.fds_version = version
        logger.info(f"Initialized FDS runner with version: {version}")

        # Initialize validators
        self.execution_validator = ExecutionValidator()
        self.validate_parallel = validate_parallel

    def run(
        self,
        fds_file: str | Path,
        config: "RunConfig | None" = None,
        **kwargs: Any,
    ) -> "Results | Job":
        """
        Run an FDS simulation.

        Parameters
        ----------
        fds_file : str or Path
            Path to FDS input file
        config : RunConfig, optional
            Execution configuration (default: RunConfig())
        **kwargs
            Additional keyword arguments passed to RunConfig()

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

        >>> # With validation
        >>> results = runner.run("test.fds", simulation=sim, n_mpi=4)
        """
        # Import here to avoid circular import
        from pyfds.config import RunConfig

        # Create config from kwargs if not provided
        if config is None:
            # Filter out non-RunConfig parameters
            runconfig_kwargs = {k: v for k, v in kwargs.items() if k not in ("simulation",)}
            config = RunConfig(**runconfig_kwargs)
        elif kwargs:
            raise ValueError("Cannot specify both 'config' and keyword arguments")

        fds_file = Path(fds_file)
        if not fds_file.exists():
            logger.error(f"FDS file not found: {fds_file}")
            raise FileNotFoundError(f"FDS file not found: {fds_file}")

        # Determine output directory
        output_dir = fds_file.parent if config.output_dir is None else Path(config.output_dir)

        output_dir.mkdir(parents=True, exist_ok=True)

        # Extract CHID from file content (falls back to filename)
        chid = _extract_chid(fds_file)

        logger.info(f"Starting FDS simulation: {chid}")
        logger.debug(f"  Input file: {fds_file}")
        logger.debug(f"  Output directory: {output_dir}")
        logger.debug(f"  Threads: {config.n_threads}, MPI processes: {config.n_mpi}")

        # Build command using platform executor
        cmd = self.platform_executor.build_command(
            fds_file=fds_file,
            n_mpi=config.n_mpi,
            n_threads=config.n_threads,
            mpiexec_path=config.mpiexec_path,
        )
        logger.debug(f"  Command: {' '.join(str(c) for c in cmd)}")

        # Get environment
        env = get_environment_for_execution(n_threads=config.n_threads)

        # Start process
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=str(output_dir),
            env=env,
        )
        logger.info(f"FDS process started with PID: {process.pid}")

        # Set up monitoring
        progress_monitor = None
        if config.monitor:
            out_file = output_dir / f"{chid}.out"
            progress_monitor = ProgressMonitor(out_file)
            progress_monitor.start()
            logger.debug("Progress monitoring enabled")

        # Create job object
        job = Job(
            process=process,
            fds_file=fds_file,
            output_dir=output_dir,
            monitor=progress_monitor,
            chid=chid,
        )

        # Wait for completion or return job
        if config.wait:
            logger.debug("Waiting for simulation to complete...")
            result = job.wait(timeout=config.timeout)
            logger.info(f"Simulation completed successfully: {chid}")
            return result

        logger.info("Simulation running in background")
        return job
