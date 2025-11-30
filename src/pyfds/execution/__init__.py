"""
FDS simulation execution module.
"""

from pathlib import Path
from typing import TYPE_CHECKING, Any

from pyfds.validation import ParallelValidator

from ..exceptions import ExecutionError, FDSNotFoundError, FDSTimeoutError
from .monitor import ProgressInfo, ProgressMonitor
from .platform import PlatformExecutor
from .process import find_fds_executable, validate_fds_executable
from .runner import FDSRunner, Job

if TYPE_CHECKING:
    from ..analysis.results import Results
    from ..config import RunConfig


def run_fds(
    fds_file: str | Path, config: "RunConfig | None" = None, **kwargs: Any
) -> "Results | Job":
    """
    Run an FDS simulation.

    This is a convenience function that creates an FDSRunner and executes the simulation.

    Parameters
    ----------
    fds_file : str or Path
        Path to FDS input file
    config : RunConfig, optional
        Execution configuration (default: RunConfig())
        **kwargs
            Additional keyword arguments passed to RunConfig()    Returns
    -------
    Results or Job
        Results object if wait=True, Job object if wait=False

    Examples
    --------
    >>> results = run_fds("test.fds", n_threads=4)

    >>> # Non-blocking
    >>> job = run_fds("test.fds", wait=False)
    >>> results = job.wait()
    """
    runner = FDSRunner()
    return runner.run(fds_file, config=config, **kwargs)


__all__ = [
    "ExecutionError",
    "FDSNotFoundError",
    "FDSRunner",
    "FDSTimeoutError",
    "Job",
    "ParallelValidator",
    "PlatformExecutor",
    "ProgressInfo",
    "ProgressMonitor",
    "find_fds_executable",
    "run_fds",
    "validate_fds_executable",
]
