#!/usr/bin/env python3
"""
3D Heat Transfer in Sphere Example
==================================

Demonstrates 3D heat conduction in solid objects using HT3D feature.
Models heat transfer within a solid sphere with internal heat generation.

FDS Reference: Heat_Transfer/ht3d_sphere_24.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/ht3d_sphere_24.fds

Key Namelists: OBST (HT3D), SURF

Usage
-----
    python ht3d_sphere.py

Output
------
    fds/heat_transfer/ht3d_sphere.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import (
    Material,
    Mesh,
    Misc,
    Multiplier,
    Obstruction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create 3D heat transfer simulation in a sphere."""

    # Create simulation
    sim = Simulation(chid="ht3d_sphere", title="3D Heat Transfer in Solid Sphere")

    # Time
    sim.add(Time(t_end=180.0, dt=2.0))

    # Mesh - domain around sphere
    sim.add(Mesh(ijk=Grid3D.of(24, 24, 24), xb=Bounds3D.of(-0.12, 0.12, -0.12, 0.12, -0.12, 0.12)))

    # Solid phase only
    sim.add(Misc(solid_phase_only=True, radiation=False))

    # Material for the sphere
    sim.add(
        Material(
            id="STUFF",
            conductivity=1.0,  # W/m·K
            specific_heat=1.0,  # kJ/kg·K
            density=1000.0,  # kg/m³
        )
    )

    # Surface for HT3D sphere with internal heat source
    sim.add(
        Surface(
            id="SPHERE",
            ht3d=True,  # Enable 3D heat transfer
            tmp_front=20.0,  # Surface boundary temperature
            color="ORANGE",
        )
    )

    # Create sphere using MULT for cube array with SHAPE='SPHERE'
    # This creates a sphere of radius 0.1m at the origin
    sim.add(
        Multiplier(id="cube_array", dx=0.01, dy=0.01, dz=0.01, i_upper=21, j_upper=21, k_upper=21)
    )

    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.110, -0.100, -0.110, -0.100, -0.110, -0.100),
            mult_id="cube_array",
            shape="SPHERE",
            radius=0.1,
            surf_id="SPHERE",
            internal_heat_source=200.0,  # W/m³ volumetric heat source
            cell_size=0.01,
            matl_id="STUFF",
        )
    )

    # Open boundaries
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMIN", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Note: Profile output would be added with PROF namelist
    # Temperature along radius

    # Write output
    output_path = write_example(sim, "heat_transfer")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
