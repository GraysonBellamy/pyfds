"""
PyFDS - Python interface to NIST Fire Dynamics Simulator (FDS)

A comprehensive Python library for creating, executing, and analyzing
FDS fire simulations programmatically.
"""

from .analysis import Results
from .core import (
    BasicValidationStrategy,
    ComprehensiveValidationStrategy,
    Device,
    Head,
    Mesh,
    Obstruction,
    Simulation,
    Surface,
    Time,
    ValidationError,
    ValidationStrategy,
    ValidationWarning,
    Validator,
)
from .execution import (
    FDSExecutionError,
    FDSNotFoundError,
    FDSRunner,
    FDSTimeoutError,
    Job,
    ProgressInfo,
    ProgressMonitor,
)
from .io import CSVParser

__version__ = "0.1.0"

__all__ = [
    # Analysis and I/O
    "BasicValidationStrategy",
    "CSVParser",
    "ComprehensiveValidationStrategy",
    "Device",
    # Execution classes
    "FDSExecutionError",
    "FDSNotFoundError",
    "FDSRunner",
    "FDSTimeoutError",
    "Head",
    "Job",
    "Mesh",
    "Obstruction",
    "ProgressInfo",
    "ProgressMonitor",
    "Results",
    "Simulation",
    "Surface",
    "Time",
    "ValidationError",
    "ValidationStrategy",
    "ValidationWarning",
    "Validator",
]


def main() -> None:
    """CLI entry point."""
    print("PyFDS - Python interface to FDS")
    print(f"Version: {__version__}")
    print("Use 'import pyfds' to get started.")
