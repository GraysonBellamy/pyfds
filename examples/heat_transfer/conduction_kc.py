#!/usr/bin/env python3
"""
Temperature-Dependent Conductivity Example
==========================================

Demonstrates materials with temperature-dependent thermal conductivity
using CONDUCTIVITY_RAMP.

FDS Reference: Heat_Transfer/heat_conduction_kc.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/heat_conduction_kc.fds

Key Namelists: MATL (CONDUCTIVITY_RAMP)

Usage
-----
    python conduction_kc.py

Output
------
    fds/heat_transfer/conduction_kc.fds
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
    Ramp,
    Surface,
    Time,
    Vent,
)


def main():
    """Create simulation with temperature-dependent conductivity."""

    # Create simulation
    sim = Simulation(chid="conduction_kc", title="Heat Conduction with Temperature-Dependent k")

    # Time
    sim.add(Time(t_end=3600.0, dt=1.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(3, 3, 3), xb=Bounds3D.of(-0.15, 0.15, -0.15, 0.15, 0.0, 0.3)))

    # Solid phase only
    sim.add(Misc(solid_phase_only=True))

    # Temperature-dependent conductivity ramp
    # Steel-like behavior: conductivity decreases with temperature
    sim.add(
        Ramp(
            id="K_RAMP",
            points=[
                (20.0, 54.0),  # k at 20°C
                (200.0, 48.0),  # k at 200°C
                (400.0, 40.0),  # k at 400°C
                (600.0, 32.0),  # k at 600°C
                (800.0, 27.0),  # k at 800°C
            ],
        )
    )

    # Material with temperature-dependent conductivity
    sim.add(
        Material(
            id="STEEL_VAR_K",
            conductivity_ramp="K_RAMP",
            specific_heat=0.46,  # kJ/kg·K
            density=7850.0,  # kg/m³
            emissivity=0.9,
        )
    )

    # Surface with the material
    sim.add(
        Surface(
            id="STEEL_SLAB",
            matl_id="STEEL_VAR_K",
            thickness=0.05,  # 5 cm thick
            backing="INSULATED",
            tmp_gas_front=800.0,  # Expose to 800°C
            heat_transfer_coefficient=25.0,
        )
    )

    # Apply surface
    sim.add(Vent(xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0), surf_id="STEEL_SLAB"))

    # Open boundaries
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Temperature devices
    sim.add(Device(id="T_Front", quantity="WALL TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.0), ior=3))

    sim.add(
        Device(
            id="T_Mid",
            quantity="INSIDE WALL TEMPERATURE",
            xyz=Point3D.of(0.0, 0.0, 0.0),
            ior=3,
            depth=0.025,
        )
    )

    sim.add(
        Device(id="T_Back", quantity="BACK WALL TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.0), ior=3)
    )

    # Write output
    output_path = write_example(sim, "heat_transfer")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
