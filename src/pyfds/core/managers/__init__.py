"""
Manager classes for organizing simulation components.

This module provides specialized manager classes that handle different aspects
of FDS simulations, following the Single Responsibility Principle.
"""

from .combustion import CombustionManager
from .control import ControlManager
from .geometry import GeometryManager
from .instrumentation import InstrumentationManager
from .material import MaterialManager
from .output import OutputManager
from .physics import PhysicsManager
from .ramp import RampManager
from .species import SpeciesManager

__all__ = [
    "CombustionManager",
    "ControlManager",
    "GeometryManager",
    "InstrumentationManager",
    "MaterialManager",
    "OutputManager",
    "PhysicsManager",
    "RampManager",
    "SpeciesManager",
]
