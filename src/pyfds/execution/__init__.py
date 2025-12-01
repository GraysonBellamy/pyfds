"""
FDS simulation execution module.
"""

from pathlib import Path
from typing import TYPE_CHECKING, Any

from pyfds.exceptions import ExecutionError, FDSNotFoundError, FDSTimeoutError
from pyfds.execution.monitor import ProgressInfo, ProgressMonitor
from pyfds.execution.platform import PlatformExecutor
from pyfds.execution.process import find_fds_executable, validate_fds_executable
from pyfds.execution.runner import FDSRunner, Job
from pyfds.validation import ExecutionValidator

if TYPE_CHECKING:
    from pyfds.analysis.results import Results
    from pyfds.config import RunConfig


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
    "ExecutionValidator",
    "FDSNotFoundError",
    "FDSRunner",
    "FDSTimeoutError",
    "Job",
    "PlatformExecutor",
    "ProgressInfo",
    "ProgressMonitor",
    "find_fds_executable",
    "run_fds",
    "validate_fds_executable",
]
