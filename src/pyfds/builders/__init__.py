"""
Builder classes for creating FDS namelists with fluent API.

This module provides builder pattern implementations for complex FDS namelists,
making it easier to construct simulations with readable, chainable methods.

Examples
--------
>>> from pyfds.builders import RampBuilder, MaterialBuilder, ReactionBuilder
>>>
>>> # Create a fire growth ramp
>>> fire_ramp = RampBuilder('HRR_GROWTH') \
...     .t_squared('FAST', peak_hrr=2500, t_peak=300) \
...     .build()
>>>
>>> # Create a material with temperature-dependent properties
>>> steel = MaterialBuilder('STEEL') \
...     .density(7850) \
...     .thermal_conductivity(45.8) \
...     .specific_heat(0.46) \
...     .emissivity(0.7) \
...     .build()
>>>
>>> # Create a combustion reaction
>>> reac = ReactionBuilder() \
...     .fuel('PROPANE') \
...     .soot_yield(0.015) \
...     .build()
"""

from .base import Builder
from .control import ControlBuilder
from .devc import DevcBuilder
from .geom import GeomBuilder
from .hole import HoleBuilder
from .material import MaterialBuilder
from .mesh import MeshBuilder
from .move import MoveBuilder
from .mult import MultBuilder
from .part import PartBuilder
from .prop import PropBuilder
from .ramp import RampBuilder
from .reaction import ReactionBuilder
from .surf import SurfBuilder
from .vent import VentBuilder

__all__ = [
    "Builder",
    "ControlBuilder",
    "DevcBuilder",
    "GeomBuilder",
    "HoleBuilder",
    "MaterialBuilder",
    "MeshBuilder",
    "MoveBuilder",
    "MultBuilder",
    "PartBuilder",
    "PropBuilder",
    "RampBuilder",
    "ReactionBuilder",
    "SurfBuilder",
    "VentBuilder",
]
