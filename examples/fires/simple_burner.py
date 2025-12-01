#!/usr/bin/env python3
"""
Simple Burner Example
=====================

A constant heat release rate burner - the most basic fire source in FDS.
Uses HRRPUA (Heat Release Rate Per Unit Area) on a surface.

FDS Reference: Fires/simple_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/simple_test.fds

Key Namelists: SURF (HRRPUA)

Usage
-----
    python simple_burner.py

Output
------
    fds/fires/simple_burner.fds
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
    """Create a simple burner simulation."""

    # Create simulation
    sim = Simulation(chid="simple_burner", title="Simple Constant HRR Burner")

    # Time: 60 seconds
    sim.add(Time(t_end=60.0))

    # Mesh: Room with 10cm cells
    # 3.6m x 2.4m x 2.4m (based on simple_test.fds)
    sim.add(Mesh(ijk=Grid3D.of(36, 24, 24), xb=Bounds3D.of(0.0, 3.6, 0.0, 2.4, 0.0, 2.4)))

    # Reaction: Propane combustion
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))

    # Burner surface with constant HRRPUA
    # 1000 kW/m² on a 0.4m x 0.4m = 0.16 m² burner
    # Total HRR = 1000 * 0.16 = 160 kW
    sim.add(Surface(id="BURNER", hrrpua=1000.0, color="RED"))

    # Burner obstruction - fire on top surface only
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.0, 0.4, 1.0, 1.4, 0.0, 0.2),
            surf_ids=("BURNER", "INERT", "INERT"),
        )
    )

    # Open vent for exhaust
    sim.add(Vent(xb=Bounds3D.of(3.6, 3.6, 0.8, 1.6, 0.0, 2.0), surf_id="OPEN"))

    # Temperature devices
    sim.add(Device(id="TEMP_PLUME", quantity="TEMPERATURE", xyz=Point3D.of(0.2, 1.2, 1.5)))

    sim.add(Device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=Point3D.of(0.2, 1.2, 2.35)))

    # Write output
    output_path = write_example(sim, "fires")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
