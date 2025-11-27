"""
Manager classes for organizing simulation components.

This module provides specialized manager classes that handle different aspects
of FDS simulations, following the Single Responsibility Principle.
"""

from .control import ControlManager
from .geometry import GeometryManager
from .instrumentation import InstrumentationManager
from .material import MaterialManager
from .output import OutputManager
from .physics import PhysicsManager
from .ramp import RampManager

__all__ = [
    "ControlManager",
    "GeometryManager",
    "InstrumentationManager",
    "MaterialManager",
    "OutputManager",
    "PhysicsManager",
    "RampManager",
]
