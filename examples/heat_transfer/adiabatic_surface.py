#!/usr/bin/env python3
"""
Adiabatic Surface Temperature Example
=====================================

Demonstrates measurement of Adiabatic Surface Temperature (AST),
which is the theoretical temperature a perfectly insulated surface
would reach under given heat flux conditions.

FDS Reference: Radiation/adiabatic_surface_temperature.fds
https://github.com/firemodels/fds/blob/master/Verification/Radiation/adiabatic_surface_temperature.fds

Key Namelists: DEVC (AST)

Usage
-----
    python adiabatic_surface.py

Output
------
    fds/heat_transfer/adiabatic_surface.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Mesh,
    Obstruction,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create adiabatic surface temperature simulation."""

    # Create simulation
    sim = Simulation(chid="adiabatic_surface", title="Adiabatic Surface Temperature Measurement")

    # Time
    sim.add(Time(t_end=120.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(30, 30, 30), xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0)))

    # Reaction
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))

    # Fire surface
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="RED"))

    # Adiabatic target surface
    sim.add(Surface(id="ADIABATIC_TARGET", adiabatic=True, color="BLUE"))

    # Fire source
    sim.add(
        Obstruction(
            xb=Bounds3D.of(1.25, 1.75, 1.25, 1.75, 0.0, 0.1),
            surf_ids=("FIRE", "INERT", "INERT"),  # top=FIRE, sides=INERT, bottom=INERT
        )
    )

    # Target wall for AST measurement
    sim.add(Obstruction(xb=Bounds3D.of(0.0, 0.1, 0.0, 3.0, 0.0, 2.5), surf_id="ADIABATIC_TARGET"))

    # Open boundaries
    for mb in ["XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # AST device - measures what temperature an adiabatic surface would reach
    sim.add(
        Device(
            id="AST_1",
            quantity="ADIABATIC SURFACE TEMPERATURE",
            xyz=Point3D.of(0.1, 1.5, 1.0),
            ior=1,  # Facing +X (toward fire)
        )
    )

    # Compare with actual wall temperature
    sim.add(Device(id="T_Wall", quantity="WALL TEMPERATURE", xyz=Point3D.of(0.1, 1.5, 1.0), ior=1))

    # Gas temperature near wall
    sim.add(Device(id="T_Gas", quantity="TEMPERATURE", xyz=Point3D.of(0.2, 1.5, 1.0)))

    # Heat flux components
    sim.add(
        Device(id="HF_Rad", quantity="RADIATIVE HEAT FLUX", xyz=Point3D.of(0.1, 1.5, 1.0), ior=1)
    )

    sim.add(
        Device(id="HF_Conv", quantity="CONVECTIVE HEAT FLUX", xyz=Point3D.of(0.1, 1.5, 1.0), ior=1)
    )

    # Write output
    output_path = write_example(sim, "heat_transfer")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
