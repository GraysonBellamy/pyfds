#!/usr/bin/env python3
"""
Multi-Layer Wall Example

Demonstrates creating complex wall assemblies with multiple
material layers and proper thermal boundary conditions.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders import MaterialBuilder, SurfBuilder


def main():
    """Create multi-layer wall simulation."""

    sim = Simulation(chid="multi_layer", title="Multi-Layer Wall Assembly")

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
    wall = (
        SurfBuilder("EXTERIOR_WALL")
        .with_multi_layer_material(
            layers=[
                {"matl_id": "GYPSUM", "thickness": 0.013},  # 1/2" drywall
                {"matl_id": "FIBERGLASS", "thickness": 0.089},  # 3.5" insulation
                {"matl_id": "GYPSUM", "thickness": 0.013},  # 1/2" drywall
            ],
            backing="EXPOSED",
        )
        .build()
    )

    # Simple multi-layer wall (no multi-component layers for now)
    interior_wall = (
        SurfBuilder("INTERIOR_WALL")
        .with_multi_layer_material(
            layers=[
                {"matl_id": "GYPSUM", "thickness": 0.013},
                {"matl_id": "WOOD_STUD", "thickness": 0.089},
                {"matl_id": "GYPSUM", "thickness": 0.013},
            ]
        )
        .build()
    )

    # Add to simulation
    for matl in [gypsum, insulation, stud]:
        sim.add_material(matl)

    sim.add_surface(wall)
    sim.add_surface(interior_wall)

    # ... rest of simulation setup

    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)
    sim.write(output_dir / "multi_layer_wall.fds")

    print("Multi-layer wall example written successfully")


if __name__ == "__main__":
    main()
