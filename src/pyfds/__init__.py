"""
PyFDS - Python interface to NIST Fire Dynamics Simulator (FDS)

A comprehensive Python library for creating, executing, and analyzing
FDS fire simulations programmatically.

Example
-------
>>> from pyfds import Simulation, Mesh, Surface, Obstruction
>>>
>>> sim = Simulation(chid="simple_fire")
>>> sim.add(Mesh(id="mesh1", xb=(0, 10, 0, 10, 0, 3), ijk=(100, 100, 30)))
>>> sim.add(Surface(id="burner", hrrpua=1000))
>>> sim.add(Obstruction(id="fire", xb=(4, 6, 4, 6, 0, 0.5), surf_id="burner"))
>>> sim.write("simple_fire.fds")
"""

import sys

__version__ = "0.1.0"

# Builders (high-value only - for complex domain logic)
from pyfds.builders import (
    ControlBuilder,
    GeomBuilder,
    MaterialBuilder,
    PartBuilder,
    RampBuilder,
    ReactionBuilder,
    SurfaceBuilder,
)

# Configuration
from pyfds.config import RunConfig

# Geometry value objects
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D

# Namelist classes
from pyfds.core.namelists import (
    Combustion,
    Control,
    Device,
    FdsField,
    Head,
    Hole,
    Initialization,
    Material,
    Mesh,
    Misc,
    Multiplier,
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

# Core simulation
from pyfds.core.simulation import Simulation

# Exceptions
from pyfds.exceptions import (
    DuplicateIdError,
    ExecutionError,
    FDSNotFoundError,
    PyFDSError,
    UnknownIdError,
    ValidationError,
)

# Execution functions
from pyfds.execution import run_fds

# IO functions
from pyfds.io import parse_fds

__all__ = [
    "Bounds3D",
    "Combustion",
    "Control",
    "ControlBuilder",
    "Device",
    "DuplicateIdError",
    "ExecutionError",
    "FDSNotFoundError",
    "FdsField",
    "GeomBuilder",
    "Grid3D",
    "Head",
    "Hole",
    "Initialization",
    "Material",
    "MaterialBuilder",
    "Mesh",
    "Misc",
    "Multiplier",
    "Obstruction",
    "PartBuilder",
    "Particle",
    "Point3D",
    "Property",
    "PyFDSError",
    "Ramp",
    "RampBuilder",
    "Reaction",
    "ReactionBuilder",
    "RunConfig",
    "Simulation",
    "Species",
    "Surface",
    "SurfaceBuilder",
    "Time",
    "UnknownIdError",
    "ValidationError",
    "Vent",
    "__version__",
    "parse_fds",
    "run_fds",
]


def main() -> None:
    """CLI entry point."""
    from pyfds.cli import main as cli_main

    sys.exit(cli_main())
