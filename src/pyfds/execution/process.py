"""
Process management utilities for FDS execution.
"""

import os
import shutil
import subprocess
from pathlib import Path

from .exceptions import FDSNotFoundError


def find_fds_executable() -> Path:
    """
    Find the FDS executable.

    Searches in the following order:
    1. Environment variable FDS_EXECUTABLE
    2. System PATH for 'fds' command
    3. Common installation locations

    Returns
    -------
    Path
        Path to FDS executable

    Raises
    ------
    FDSNotFoundError
        If FDS executable cannot be found

    Examples
    --------
    >>> fds_path = find_fds_executable()
    >>> print(f"Found FDS at: {fds_path}")
    """
    # Check environment variable
    env_path = os.environ.get("FDS_EXECUTABLE")
    if env_path:
        fds_path = Path(env_path)
        if fds_path.exists() and fds_path.is_file():
            return fds_path

    # Check system PATH
    fds_in_path = shutil.which("fds")
    if fds_in_path:
        return Path(fds_in_path)

    # Check common installation locations
    common_locations = [
        Path("/usr/local/bin/fds"),
        Path("/usr/bin/fds"),
        Path.home() / "bin" / "fds",
        Path("C:/Program Files/FDS/FDS6/bin/fds.exe"),  # Windows
        Path("C:/Program Files (x86)/FDS/FDS6/bin/fds.exe"),  # Windows
    ]

    for location in common_locations:
        if location.exists() and location.is_file():
            return location

    # Not found
    raise FDSNotFoundError(
        "FDS executable not found. Please install FDS or set the FDS_EXECUTABLE "
        "environment variable to point to the FDS executable.\n"
        "Download FDS from: https://pages.nist.gov/fds-smv/"
    )


def validate_fds_executable(fds_path: Path) -> tuple[bool, str]:
    """
    Validate that the FDS executable works.

    Parameters
    ----------
    fds_path : Path
        Path to FDS executable

    Returns
    -------
    Tuple[bool, str]
        (is_valid, version_string)

    Examples
    --------
    >>> fds_path = find_fds_executable()
    >>> is_valid, version = validate_fds_executable(fds_path)
    >>> print(f"FDS version: {version}")
    """
    try:
        result = subprocess.run(
            [str(fds_path)],
            capture_output=True,
            text=True,
            timeout=5,
            check=False,
        )

        # FDS prints version info to stdout or stderr when run without args
        output = result.stdout + result.stderr

        # Look for version/revision string
        for line in output.split("\n"):
            # FDS prints "Revision" line with version info
            if "Revision" in line and "FDS" in line:
                return True, line.strip()
            # Also check for older FDS versions that might use "Version"
            if "FDS" in line and ("Version" in line or "version" in line):
                return True, line.strip()

        # If we see "Fire Dynamics Simulator" we know it's FDS
        if "Fire Dynamics Simulator" in output:
            # Extract revision line if available
            for line in output.split("\n"):
                if "Revision" in line:
                    return True, line.strip()
            return True, "FDS (version info not parsed)"

        return False, "Could not determine FDS version"

    except subprocess.TimeoutExpired:
        return False, "FDS executable timed out"
    except Exception as e:
        return False, f"Error running FDS: {e}"


def build_fds_command(
    fds_file: Path,
    fds_executable: Path,
    n_threads: int = 1,  # noqa: ARG001 - kept for API consistency, used via environment
    n_mpi: int = 1,
    mpiexec_path: str = "mpiexec",
) -> list[str]:
    """
    Build the FDS command to execute.

    Parameters
    ----------
    fds_file : Path
        Path to FDS input file
    fds_executable : Path
        Path to FDS executable
    n_threads : int
        Number of OpenMP threads (controlled via OMP_NUM_THREADS environment variable)
    n_mpi : int
        Number of MPI processes
    mpiexec_path : str
        Path to mpiexec command

    Returns
    -------
    List[str]
        Command as list of arguments

    Examples
    --------
    >>> cmd = build_fds_command(
    ...     Path("test.fds"),
    ...     Path("/usr/local/bin/fds"),
    ...     n_threads=4
    ... )
    >>> print(" ".join(cmd))
    """
    cmd = []

    # MPI execution
    if n_mpi > 1:
        cmd.extend([mpiexec_path, "-n", str(n_mpi)])

    # FDS executable
    cmd.append(str(fds_executable))

    # Note: OpenMP threads are controlled via OMP_NUM_THREADS environment variable,
    # not command-line flags. See get_environment_for_execution().

    # Input file
    cmd.append(str(fds_file))

    return cmd


def get_environment_for_execution(n_threads: int = 1) -> dict[str, str]:
    """
    Get environment variables for FDS execution.

    Parameters
    ----------
    n_threads : int
        Number of OpenMP threads

    Returns
    -------
    Dict[str, str]
        Environment variables

    Examples
    --------
    >>> env = get_environment_for_execution(n_threads=4)
    >>> env['OMP_NUM_THREADS']
    '4'
    """
    env = os.environ.copy()

    # Set OpenMP thread count
    env["OMP_NUM_THREADS"] = str(n_threads)

    return env
