"""
Core module for PyFDS.

This module contains the fundamental classes for building FDS simulations,
including namelist definitions, the main Simulation class, and validation.
"""

from pyfds.core.namelist import (
    Device,
    Head,
    Mesh,
    NamelistBase,
    Obstruction,
    Surface,
    Time,
)
from pyfds.core.simulation import Simulation
from pyfds.core.validator import ValidationError, ValidationWarning, Validator, validate_fds_file

__all__ = [
    "Device",
    "Head",
    "Mesh",
    "NamelistBase",
    "Obstruction",
    "Simulation",
    "Surface",
    "Time",
    "ValidationError",
    "ValidationWarning",
    "Validator",
    "validate_fds_file",
]
