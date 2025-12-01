#!/usr/bin/env python3
"""
1D Heat Conduction Example
==========================

Demonstrates 1D heat conduction through a solid slab with
specified thermal boundary conditions.

FDS Reference: Heat_Transfer/heat_conduction_a.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/heat_conduction_a.fds

Key Namelists: MATL, SURF (THICKNESS)

Usage
-----
    python conduction_1d.py

Output
------
    fds/heat_transfer/conduction_1d.fds
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
    """Create 1D heat conduction simulation."""

    # Create simulation
    sim = Simulation(chid="conduction_1d", title="1D Heat Conduction Through Solid Slab")

    # Time: Long enough for heat to penetrate slab
    sim.add(Time(t_end=2000.0, dt=0.5))

    # Small mesh - solid phase only
    sim.add(Mesh(ijk=Grid3D.of(3, 3, 3), xb=Bounds3D.of(-0.15, 0.15, -0.15, 0.15, 0.0, 0.3)))

    # Solid phase only calculation
    sim.add(Misc(solid_phase_only=True))

    # Material: Simple slab with constant properties
    # Case A: Bi = 100 (h*L/k = 100*0.1/0.1 = 100)
    sim.add(
        Material(
            id="SLAB_A",
            conductivity=0.1,  # k = 0.1 W/m·K
            specific_heat=1.0,  # cp = 1.0 kJ/kg·K
            density=100.0,  # rho = 100 kg/m³
            emissivity=0.0,  # No radiation
        )
    )

    # Surface with material and thermal boundary conditions
    sim.add(
        Surface(
            id="SLAB",
            matl_id="SLAB_A",
            thickness=0.1,  # L = 0.1 m
            backing="INSULATED",  # Adiabatic back surface
            tmp_gas_front=120.0,  # T_inf = 120°C
            heat_transfer_coefficient=100.0,  # h = 100 W/m²·K
            stretch_factor=1.0,
        )
    )

    # Apply surface as a vent at z=0
    sim.add(Vent(xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0), surf_id="SLAB"))

    # Open boundaries
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Temperature devices through the slab thickness
    # Initial temperature is 20°C throughout

    # Front surface (exposed to hot gas)
    sim.add(Device(id="Front", quantity="WALL TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.0), ior=3))

    # Interior points
    for depth_cm in [2, 4, 6, 8]:
        depth_m = depth_cm / 100.0
        sim.add(
            Device(
                id=f"{depth_cm}cm",
                quantity="INSIDE WALL TEMPERATURE",
                xyz=Point3D.of(0.0, 0.0, 0.0),
                ior=3,
                depth=depth_m,
            )
        )

    # Back surface (insulated)
    sim.add(
        Device(id="Back", quantity="BACK WALL TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.0), ior=3)
    )

    # Write output
    output_path = write_example(sim, "heat_transfer")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
