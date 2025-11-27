"""
Core module for PyFDS.

This module contains the fundamental classes for building FDS simulations,
including namelist definitions, the main Simulation class, and validation.
"""

from pyfds.core.namelists import (
    ControlFunction,
    Ctrl,
    Device,
    Head,
    Init,
    Material,
    Mesh,
    Misc,
    NamelistBase,
    Obstruction,
    Prop,
    Ramp,
    Reaction,
    Surface,
    Time,
    TurbulenceModel,
    Vent,
    VentShape,
    VentType,
)
from pyfds.core.simulation import Simulation
from pyfds.core.validator import (
    BasicValidationStrategy,
    ComprehensiveValidationStrategy,
    ValidationError,
    ValidationStrategy,
    ValidationWarning,
    Validator,
    validate_fds_file,
)

__all__ = [
    "BasicValidationStrategy",
    "ComprehensiveValidationStrategy",
    "ControlFunction",
    "Ctrl",
    "Device",
    "Head",
    "Init",
    "Material",
    "Mesh",
    "Misc",
    "NamelistBase",
    "Obstruction",
    "Prop",
    "Ramp",
    "Reaction",
    "Simulation",
    "Surface",
    "Time",
    "TurbulenceModel",
    "ValidationError",
    "ValidationStrategy",
    "ValidationWarning",
    "Validator",
    "Vent",
    "VentShape",
    "VentType",
    "validate_fds_file",
]
