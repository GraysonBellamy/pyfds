"""
Platform-specific FDS execution handling.

This module handles platform-specific differences in FDS execution,
particularly the Windows fds_local wrapper.
"""

import platform
import shutil
from pathlib import Path

from ..exceptions import FDSNotFoundError
from ..utils import get_logger

logger = get_logger(__name__)


class PlatformExecutor:
    """
    Handle platform-specific FDS execution details.

    From FDS User Guide §3.2:
    - Windows uses fds_local wrapper with -p (MPI) and -o (OpenMP) flags
    - macOS/Linux use mpiexec directly

    Examples
    --------
    >>> executor = PlatformExecutor()
    >>> cmd = executor.build_command(
    ...     fds_file=Path("test.fds"),
    ...     n_mpi=4,
    ...     n_threads=2
    ... )
    """

    def __init__(self, fds_executable: Path | None = None):
        """
        Initialize platform executor.

        Parameters
        ----------
        fds_executable : Path, optional
            Explicit FDS executable path. If None, auto-detect.
        """
        self.platform = platform.system()
        logger.debug(f"Platform detected: {self.platform}")

        if fds_executable is not None:
            self.fds_command = fds_executable
            self.uses_wrapper = self._is_fds_local_wrapper(fds_executable)
        else:
            self.fds_command, self.uses_wrapper = self._detect_fds_command()

        logger.info(
            f"FDS command: {self.fds_command}, "
            f"wrapper: {self.uses_wrapper}, "
            f"platform: {self.platform}"
        )

    def _is_fds_local_wrapper(self, path: Path) -> bool:
        """
        Check if a path is the fds_local wrapper.

        Parameters
        ----------
        path : Path
            Path to check

        Returns
        -------
        bool
            True if it's fds_local wrapper
        """
        name = path.name.lower()
        return "fds_local" in name or "fds_local.bat" in name

    def _detect_fds_command(self) -> tuple[Path, bool]:
        """
        Detect appropriate FDS command for platform.

        From FDS User Guide §3.2.1:
        "Open up the special FDS command prompt, CMDfds...By opening this
        special command prompt, a script is run automatically ensuring that
        the FDS commands and libraries are all consistent."

        Returns
        -------
        tuple[Path, bool]
            (fds_command_path, uses_wrapper)

        Raises
        ------
        FDSNotFoundError
            If no FDS executable found
        """
        # On Windows, prefer fds_local wrapper
        if self.platform == "Windows":
            logger.debug("Searching for Windows fds_local wrapper")

            # Look for fds_local.bat or fds_local
            fds_local = shutil.which("fds_local")
            if fds_local:
                logger.info(f"Found fds_local wrapper at: {fds_local}")
                return Path(fds_local), True

            fds_local_bat = shutil.which("fds_local.bat")
            if fds_local_bat:
                logger.info(f"Found fds_local.bat wrapper at: {fds_local_bat}")
                return Path(fds_local_bat), True

            logger.warning(
                "fds_local wrapper not found on Windows. "
                "This may cause library path issues. "
                "Consider running from FDS CMDfds prompt."
            )

        # Fall back to standard fds executable
        logger.debug("Searching for standard fds executable")
        fds_exe = self._find_standard_fds()

        return fds_exe, False

    def _find_standard_fds(self) -> Path:
        """
        Find standard FDS executable.

        Returns
        -------
        Path
            Path to FDS executable

        Raises
        ------
        FDSNotFoundError
            If FDS executable not found
        """
        # Check system PATH
        fds_in_path = shutil.which("fds")
        if fds_in_path:
            logger.debug(f"Found fds in PATH: {fds_in_path}")
            return Path(fds_in_path)

        # Check common installation locations
        if self.platform == "Windows":
            common_locations = [
                Path("C:/Program Files/FDS/FDS6/bin/fds.exe"),
                Path("C:/Program Files (x86)/FDS/FDS6/bin/fds.exe"),
            ]
        elif self.platform == "Darwin":  # macOS
            common_locations = [
                Path("/usr/local/bin/fds"),
                Path.home() / "bin" / "fds",
                Path("/Applications/FDS/FDS6/bin/fds"),
            ]
        else:  # Linux
            common_locations = [
                Path("/usr/local/bin/fds"),
                Path("/usr/bin/fds"),
                Path.home() / "bin" / "fds",
                Path("/opt/fds/bin/fds"),
            ]

        for location in common_locations:
            if location.exists() and location.is_file():
                logger.debug(f"Found fds at: {location}")
                return location

        # Not found
        raise FDSNotFoundError(
            "FDS executable not found. Please install FDS or set the "
            "FDS_EXECUTABLE environment variable.\n"
            "Download FDS from: https://pages.nist.gov/fds-smv/"
        )

    def build_command(
        self,
        fds_file: Path,
        n_mpi: int = 1,
        n_threads: int = 1,
        mpiexec_path: str = "mpiexec",
    ) -> list[str]:
        """
        Build platform-appropriate FDS command.

        Parameters
        ----------
        fds_file : Path
            Path to FDS input file
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads per process
        mpiexec_path : str
            Path to mpiexec command (ignored if using Windows wrapper)

        Returns
        -------
        list[str]
            Command as list of arguments

        Examples
        --------
        >>> # Windows with fds_local:
        >>> cmd = executor.build_command(Path("test.fds"), n_mpi=4, n_threads=2)
        >>> # Returns: ['fds_local', '-p', '4', '-o', '2', 'test.fds']

        >>> # Linux/macOS:
        >>> cmd = executor.build_command(Path("test.fds"), n_mpi=4)
        >>> # Returns: ['mpiexec', '-n', '4', 'fds', 'test.fds']
        """
        if self.uses_wrapper and self.platform == "Windows":
            # Windows fds_local syntax
            # From FDS User Guide §3.2.1:
            # "fds_local -p 4 -o 2 job_name.fds"
            return self._build_windows_wrapper_command(fds_file, n_mpi, n_threads)

        # Standard mpiexec syntax
        # From FDS User Guide §3.2.3, §3.2.4
        return self._build_standard_mpi_command(fds_file, n_mpi, n_threads, mpiexec_path)

    def _build_windows_wrapper_command(
        self, fds_file: Path, n_mpi: int, n_threads: int
    ) -> list[str]:
        """
        Build Windows fds_local wrapper command.

        From FDS User Guide §3.2.1:
        The -p parameter indicates the number of MPI processes, and the -o
        indicates the number of OpenMP threads.

        Parameters
        ----------
        fds_file : Path
            FDS input file
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads

        Returns
        -------
        list[str]
            Command list
        """
        cmd = [str(self.fds_command)]

        # Add MPI processes flag
        if n_mpi > 1:
            cmd.extend(["-p", str(n_mpi)])

        # Add OpenMP threads flag
        if n_threads > 1:
            cmd.extend(["-o", str(n_threads)])

        # Input file
        cmd.append(str(fds_file))

        logger.debug(f"Built Windows wrapper command: {' '.join(cmd)}")
        return cmd

    def _build_standard_mpi_command(
        self,
        fds_file: Path,
        n_mpi: int,
        n_threads: int,  # noqa: ARG002 - handled via env var
        mpiexec_path: str,
    ) -> list[str]:
        """
        Build standard mpiexec command (Linux/macOS).

        From FDS User Guide §3.2.3, §3.2.4

        Parameters
        ----------
        fds_file : Path
            FDS input file
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads (set via OMP_NUM_THREADS env var)
        mpiexec_path : str
            Path to mpiexec

        Returns
        -------
        list[str]
            Command list
        """
        cmd = []

        # MPI execution
        if n_mpi > 1:
            cmd.extend([mpiexec_path, "-n", str(n_mpi)])

        # FDS executable
        cmd.append(str(self.fds_command))

        # Note: OpenMP threads controlled via OMP_NUM_THREADS environment
        # variable, not command-line flags

        # Input file
        cmd.append(str(fds_file))

        logger.debug(f"Built standard MPI command: {' '.join(cmd)}")
        return cmd

    def get_info(self) -> dict[str, str | bool]:
        """
        Get platform executor information.

        Returns
        -------
        dict
            Information about detected configuration

        Examples
        --------
        >>> executor = PlatformExecutor()
        >>> info = executor.get_info()
        >>> info['platform']
        'Linux'
        >>> info['uses_wrapper']
        False
        """
        return {
            "platform": self.platform,
            "fds_command": str(self.fds_command),
            "uses_wrapper": self.uses_wrapper,
            "wrapper_type": ("fds_local" if self.uses_wrapper else "none"),
        }
