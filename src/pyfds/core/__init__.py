"""
Core module for PyFDS.

This module contains the fundamental classes for building FDS simulations,
including namelist definitions, the main Simulation class, and validation.
"""

# Validation system (re-exported from validation module)
# Enumerations (Severity comes from validation module above)
from pyfds.core.enums import (
    BackingCondition,
    BuiltinSpecies,
    BuiltinSurface,
    ControlFunction,
    HeatTransferModel,
    SolidGeometry,
    SprayPattern,
    TurbulenceModel,
    VentShape,
    VentType,
)

# Geometry value objects
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D

# Data models (non-namelist)
from pyfds.core.models import PyrolysisProduct, PyrolysisReaction

# Namelist classes
from pyfds.core.namelists import (
    Combustion,
    Control,
    Device,
    Geometry,
    Head,
    Hole,
    Initialization,
    Material,
    Mesh,
    Misc,
    Move,
    Multiplier,
    NamelistBase,
    NamelistFactory,
    Obstruction,
    Particle,
    Property,
    Ramp,
    Reaction,
    Species,
    Surface,
    Time,
    Vent,
)

# Registry system
from pyfds.core.registry import Registry, SimulationRegistry
from pyfds.core.registry_view import RegistryView

# Simulation class
from pyfds.core.simulation import Simulation
from pyfds.validation import Issue, Severity, Validator

__all__ = [
    "BackingCondition",
    "Bounds3D",
    "BuiltinSpecies",
    "BuiltinSurface",
    "Combustion",
    "Control",
    "ControlFunction",
    "Device",
    "Geometry",
    "Grid3D",
    "Head",
    "HeatTransferModel",
    "Hole",
    "Initialization",
    "Issue",
    "Material",
    "Mesh",
    "Misc",
    "Move",
    "Multiplier",
    "NamelistBase",
    "NamelistFactory",
    "Obstruction",
    "Particle",
    "Point3D",
    "Property",
    "PyrolysisProduct",
    "PyrolysisReaction",
    "Ramp",
    "Reaction",
    "Registry",
    "RegistryView",
    "Severity",
    "Simulation",
    "SimulationRegistry",
    "SolidGeometry",
    "Species",
    "SprayPattern",
    "Surface",
    "Time",
    "TurbulenceModel",
    "Validator",
    "Vent",
    "VentShape",
    "VentType",
]
