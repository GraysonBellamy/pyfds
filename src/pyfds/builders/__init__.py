"""
Builder classes for creating FDS namelists with fluent API.

This module provides builder pattern implementations for complex FDS namelists,
making it easier to construct simulations with readable, chainable methods.

Examples
--------
>>> from pyfds.builders import RampBuilder, MaterialBuilder
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
"""

from .control import ControlBuilder
from .geom import GeomBuilder
from .hole import HoleBuilder
from .material import MaterialBuilder
from .mesh import MeshBuilder
from .move import MoveBuilder
from .mult import MultBuilder
from .obstruction import ObstructionBuilder
from .part import PartBuilder
from .prop import PropBuilder
from .ramp import RampBuilder
from .reaction import ReactionBuilder
from .surface import SurfaceBuilder
from .vent import VentBuilder

__all__ = [
    "ControlBuilder",
    "GeomBuilder",
    "HoleBuilder",
    "MaterialBuilder",
    "MeshBuilder",
    "MoveBuilder",
    "MultBuilder",
    "ObstructionBuilder",
    "PartBuilder",
    "PropBuilder",
    "RampBuilder",
    "ReactionBuilder",
    "SurfaceBuilder",
    "VentBuilder",
]
