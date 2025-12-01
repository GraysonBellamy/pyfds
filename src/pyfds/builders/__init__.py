"""
Builders for creating FDS namelists with fluent APIs.

This module provides builders for complex namelists that benefit from
domain-specific construction logic. For simple namelists, use direct
Pydantic construction from `pyfds.core.namelists`.

Builders (for complex domain logic)
-----------------------------------
- ControlBuilder: Control logic with any(), all(), time_delay()
- GeomBuilder: Complex geometry with sphere(), cylinder(), terrain()
- MaterialBuilder: Materials with pyrolysis, liquid fuels, ramps
- PartBuilder: Particles with as_water_droplet(), as_aerosol()
- RampBuilder: Time functions with t_squared(), exponential()
- ReactionBuilder: Combustion with fuel database, stoichiometry
- SurfaceBuilder: Surfaces with burning(), flow(), layer()

Factory Libraries (for predefined objects)
------------------------------------------
Import from `pyfds.builders.libraries`:
- CommonMaterials: concrete(), steel(), wood(), etc.
- CommonProps: sprinkler(), smoke_detector(), heat_detector()
- CommonHoles: door(), window()
- CommonRamps: Predefined ramp patterns

Direct Construction (for simple namelists)
------------------------------------------
For simple namelists without complex domain logic, import directly:

    from pyfds.core.namelists import Mesh, Move, Multiplier, Obstruction, Vent, Hole, Property

Examples
--------
>>> from pyfds.builders import RampBuilder, ReactionBuilder, SurfaceBuilder
>>> from pyfds.builders.libraries import CommonMaterials, CommonProps
>>> from pyfds.core.namelists import Mesh, Obstruction

>>> # Complex: use builders
>>> ramp = RampBuilder("FIRE").t_squared("FAST", peak_hrr=1000).build()
>>> reaction = ReactionBuilder().fuel("PROPANE").build()

>>> # Predefined: use factories
>>> steel = CommonMaterials.steel()
>>> sprinkler = CommonProps.quick_response_sprinkler()

>>> # Simple: use direct construction
>>> mesh = Mesh(ijk=(100, 100, 50), xb=(0, 10, 0, 10, 0, 5))
>>> obst = Obstruction(xb=(2, 3, 2, 3, 0, 1), surf_id="FIRE")
"""

from pyfds.builders.control import ControlBuilder
from pyfds.builders.geom import GeomBuilder
from pyfds.builders.material import MaterialBuilder
from pyfds.builders.part import PartBuilder
from pyfds.builders.ramp import RampBuilder
from pyfds.builders.reaction import ReactionBuilder
from pyfds.builders.surface import SurfaceBuilder

__all__ = [
    "ControlBuilder",
    "GeomBuilder",
    "MaterialBuilder",
    "PartBuilder",
    "RampBuilder",
    "ReactionBuilder",
    "SurfaceBuilder",
]
