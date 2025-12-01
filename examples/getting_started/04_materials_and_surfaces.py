#!/usr/bin/env python3
"""
Materials and Surfaces Example
==============================

This example demonstrates how to define custom materials (MATL) and
surfaces (SURF) for solid boundaries. This is essential for modeling
heat transfer through walls and other solid objects.

FDS Reference: Heat_Transfer/heat_conduction_a.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/heat_conduction_a.fds

Key Namelists: MATL, SURF (THICKNESS)

Usage
-----
    python 04_materials_and_surfaces.py

Output
------
    fds/getting_started/materials_and_surfaces.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Material,
    Mesh,
    Misc,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with custom materials and surfaces."""

    # Create simulation
    sim = Simulation(chid="materials_and_surfaces", title="Heat Transfer Through Solid Slab")

    # Time: Long enough to see heat conduction
    sim.add(Time(t_end=2000.0))

    # Mesh: Small domain for solid phase calculation
    sim.add(Mesh(ijk=Grid3D.of(3, 3, 3), xb=Bounds3D.of(-0.15, 0.15, -0.15, 0.15, 0.0, 0.3)))

    # Solid phase only calculation
    sim.add(Misc(solid_phase_only=True))

    # =========================================================================
    # MATERIAL DEFINITION
    # =========================================================================
    # Based on Case A from heat_conduction_a.fds
    # A simple slab material with constant properties
    sim.add(
        Material(
            id="SLAB_A",
            conductivity=0.1,  # W/m·K
            specific_heat=1.0,  # kJ/kg·K
            density=100.0,  # kg/m³
            emissivity=0.0,  # No radiation
        )
    )

    # =========================================================================
    # SURFACE DEFINITION WITH MATERIAL
    # =========================================================================
    # Surface using the material with thermal boundary conditions
    sim.add(
        Surface(
            id="SLAB",
            matl_id="SLAB_A",
            thickness=0.1,  # 10 cm thick slab
            backing="INSULATED",  # Insulated back surface
            tmp_gas_front=120.0,  # Gas temperature at front
            heat_transfer_coefficient=100.0,  # h = 100 W/m²·K (Bi = 100)
            stretch_factor=1.0,
        )
    )

    # =========================================================================
    # GEOMETRY
    # =========================================================================
    # Vent to apply the surface (represents the slab face)
    sim.add(Vent(xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0), surf_id="SLAB"))

    # Open boundaries
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # =========================================================================
    # DEVICES - Temperature through the slab thickness
    # =========================================================================
    # Temperature at different depths through the slab
    depths = [
        (0.0, "Front"),
        (0.02, "2cm"),
        (0.04, "4cm"),
        (0.06, "6cm"),
        (0.08, "8cm"),
        (0.10, "Back"),
    ]

    for depth, label in depths:
        if depth == 0.0:
            # Front surface temperature
            sim.add(
                Device(id=label, quantity="WALL TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.0), ior=3)
            )
        elif depth == 0.10:
            # Back surface temperature
            sim.add(
                Device(
                    id=label, quantity="BACK WALL TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.0), ior=3
                )
            )
        else:
            # Internal temperature
            sim.add(
                Device(
                    id=label,
                    quantity="INSIDE WALL TEMPERATURE",
                    xyz=Point3D.of(0.0, 0.0, 0.0),
                    ior=3,
                    depth=depth,
                )
            )

    # Write output
    output_path = write_example(sim, "getting_started")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
