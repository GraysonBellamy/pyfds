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

# Builders
from .builders import (
    HoleBuilder,
    MaterialBuilder,
    MeshBuilder,
    ObstructionBuilder,
    PropBuilder,
    RampBuilder,
    SurfaceBuilder,
    VentBuilder,
)

# Configuration
from .config import RunConfig

# Geometry value objects
from .core.geometry import Bounds3D, Grid3D, Point3D

# Namelist classes
from .core.namelists import (
    Combustion,
    Ctrl,
    Device,
    FdsField,
    Head,
    Hole,
    Init,
    Material,
    Mesh,
    Misc,
    Mult,
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

# Core simulation
from .core.simulation import Simulation

# Exceptions
from .exceptions import (
    DuplicateIdError,
    ExecutionError,
    FDSNotFoundError,
    PyFDSError,
    UnknownIdError,
    ValidationError,
)

# Execution functions
from .execution import run_fds

# IO functions
from .io import parse_fds

__all__ = [
    "Bounds3D",
    "Combustion",
    "Ctrl",
    "Device",
    "DuplicateIdError",
    "ExecutionError",
    "FDSNotFoundError",
    "FdsField",
    "Grid3D",
    "Head",
    "Hole",
    "HoleBuilder",
    "Init",
    "Material",
    "MaterialBuilder",
    "Mesh",
    "MeshBuilder",
    "Misc",
    "Mult",
    "Obstruction",
    "ObstructionBuilder",
    "Part",
    "Point3D",
    "Prop",
    "PropBuilder",
    "PyFDSError",
    "Ramp",
    "RampBuilder",
    "Reaction",
    "RunConfig",
    "Simulation",
    "Species",
    "Surface",
    "SurfaceBuilder",
    "Time",
    "UnknownIdError",
    "ValidationError",
    "Vent",
    "VentBuilder",
    "__version__",
    "parse_fds",
    "run_fds",
]


def main() -> None:
    """CLI entry point."""
    from .cli import main as cli_main

    sys.exit(cli_main())
