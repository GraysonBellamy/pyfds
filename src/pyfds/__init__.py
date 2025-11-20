"""
PyFDS - Python interface to NIST Fire Dynamics Simulator (FDS)

A comprehensive Python library for creating, executing, and analyzing
FDS fire simulations programmatically.
"""

from .core import (
    Device,
    Head,
    Mesh,
    Obstruction,
    Simulation,
    Surface,
    Time,
    ValidationError,
    ValidationWarning,
    Validator,
)

__version__ = "0.1.0"

__all__ = [
    "Device",
    "Head",
    "Mesh",
    "Obstruction",
    "Simulation",
    "Surface",
    "Time",
    "ValidationError",
    "ValidationWarning",
    "Validator",
]


def main() -> None:
    """CLI entry point."""
    print("PyFDS - Python interface to FDS")
    print(f"Version: {__version__}")
    print("Use 'import pyfds' to get started.")
