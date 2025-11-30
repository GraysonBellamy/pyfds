#!/usr/bin/env python3
"""
Multi-Layer Wall Example

Demonstrates creating complex wall assemblies with multiple
material layers and proper thermal boundary conditions.
"""

from pathlib import Path

from pyfds import Simulation, Surface
from pyfds.builders import MaterialBuilder
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.time import Time
from pyfds.core.namelists.vent import Vent


def main():
    """Create multi-layer wall simulation."""

    sim = Simulation(chid="multi_layer", title="Multi-Layer Wall Assembly")

    # Time and mesh setup
    sim.add(Time(t_end=3600.0))  # 1 hour simulation
    sim.add(
        Mesh(ijk=Grid3D.of(50, 20, 20), xb=Bounds3D.of(0.0, 5.0, 0.0, 2.0, 0.0, 2.0))
    )  # 5m x 2m x 2m domain

    # Define materials
    gypsum = (
        MaterialBuilder("GYPSUM")
        .density(930)
        .thermal_conductivity(0.48)
        .specific_heat(1.09)
        .build()
    )

    insulation = (
        MaterialBuilder("FIBERGLASS")
        .density(12)
        .thermal_conductivity(0.04)
        .specific_heat(0.84)
        .build()
    )

    stud = (
        MaterialBuilder("WOOD_STUD")
        .density(500)
        .thermal_conductivity(0.13)
        .specific_heat(2.5)
        .build()
    )

    # Create multi-layer wall surface
    wall = Surface(
        id="EXTERIOR_WALL",
        layers=[
            {"matl_id": "GYPSUM", "thickness": 0.013},  # 1/2" drywall
            {"matl_id": "FIBERGLASS", "thickness": 0.089},  # 3.5" insulation
            {"matl_id": "GYPSUM", "thickness": 0.013},  # 1/2" drywall
        ],
        backing="EXPOSED",
    )

    # Simple multi-layer wall (no multi-component layers for now)
    interior_wall = Surface(
        id="INTERIOR_WALL",
        layers=[
            {"matl_id": "GYPSUM", "thickness": 0.013},
            {"matl_id": "WOOD_STUD", "thickness": 0.089},
            {"matl_id": "GYPSUM", "thickness": 0.013},
        ],
    )

    # Add to simulation
    for matl in [gypsum, insulation, stud]:
        sim.add(matl)

    sim.add(wall)
    sim.add(interior_wall)

    # Wall obstructions
    # Exterior wall at x=0
    sim.add(Obstruction(xb=Bounds3D.of(0.0, 0.0, 0.0, 2.0, 0.0, 2.0), surf_id="EXTERIOR_WALL"))

    # Interior wall at x=2.5
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 2.5, 0.0, 2.0, 0.0, 2.0), surf_id="INTERIOR_WALL"))

    # Ventilation vents
    sim.add(Vent(xb=Bounds3D.of(0.0, 5.0, 0.0, 0.0, 0.0, 2.0), surf_id="OPEN"))  # Left boundary
    sim.add(Vent(xb=Bounds3D.of(5.0, 5.0, 0.0, 2.0, 0.0, 2.0), surf_id="OPEN"))  # Right boundary
    sim.add(Vent(xb=Bounds3D.of(0.0, 5.0, 2.0, 2.0, 0.0, 2.0), surf_id="OPEN"))  # Back boundary
    sim.add(Vent(xb=Bounds3D.of(0.0, 5.0, 0.0, 2.0, 2.0, 2.0), surf_id="OPEN"))  # Top boundary
    sim.add(
        Vent(xb=Bounds3D.of(0.0, 5.0, 0.0, 2.0, 0.0, 0.0), surf_id="OPEN")
    )  # Bottom boundary (exposed to fire)
    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)
    sim.write(output_dir / "multi_layer_wall.fds")

    print("Multi-layer wall example written successfully")


if __name__ == "__main__":
    main()
