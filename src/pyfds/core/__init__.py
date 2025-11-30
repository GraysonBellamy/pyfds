"""
Core module for PyFDS.

This module contains the fundamental classes for building FDS simulations,
including namelist definitions, the main Simulation class, and validation.
"""

# Validation system (re-exported from validation module)
from pyfds.validation import Issue, Severity, Validator

# Enumerations (Severity comes from validation module above)
from .enums import (
    BackingCondition,
    ControlFunction,
    HeatTransferModel,
    SolidGeometry,
    SprayPattern,
    TurbulenceModel,
    VentShape,
    VentType,
)

# Geometry value objects
from .geometry import Bounds3D, Grid3D, Point3D

# Namelist classes
from .namelists import (
    Combustion,
    Ctrl,
    Device,
    Geom,
    Head,
    Hole,
    Init,
    Material,
    Mesh,
    Misc,
    Move,
    Mult,
    NamelistBase,
    NamelistFactory,
    Obstruction,
    Part,
    Prop,
    Ramp,
    Reaction,
    Species,
    Surface,
    Time,
    Vent,
)

# Registry system
from .registry import Registry, SimulationRegistry
from .registry_view import RegistryView

# Simulation class
from .simulation import Simulation

__all__ = [
    "BackingCondition",
    "Bounds3D",
    "Combustion",
    "ControlFunction",
    "Ctrl",
    "Device",
    "Geom",
    "Grid3D",
    "Head",
    "HeatTransferModel",
    "Hole",
    "Init",
    "Issue",
    "Material",
    "Mesh",
    "Misc",
    "Move",
    "Mult",
    "NamelistBase",
    "NamelistFactory",
    "Obstruction",
    "Part",
    "Point3D",
    "Prop",
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
