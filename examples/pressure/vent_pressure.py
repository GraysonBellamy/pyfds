#!/usr/bin/env python3
"""
Vent Pressure Boundary Condition Example
=========================================

Demonstrates pressure boundary conditions on vents
for modeling pressure differentials across openings.

Based on FDS Verification: vent_pressure tests.

Key Namelists: VENT (DYNAMIC_PRESSURE), SURF (OPEN)

Usage
-----
    python vent_pressure.py

Output
------
    fds/pressure/vent_pressure.fds
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
    Misc,
    Obstruction,
    Time,
    Vent,
)


def main():
    """Create vent pressure boundary condition example."""
    sim = Simulation(
        chid="vent_pressure",
        title="Pressure-Driven Flow Through Opening",
    )

    # Time
    sim.add(Time(t_end=30.0))

    # Misc
    sim.add(Misc(stratification=False, radiation=False))

    # Two connected rooms
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 20, 20),
            xb=Bounds3D.of(0.0, 8.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    # Wall dividing two rooms with opening
    sim.add(
        Obstruction(
            id="WALL_BOTTOM",
            xb=Bounds3D.of(3.9, 4.1, 0.0, 4.0, 0.0, 1.0),
            color="GRAY",
        )
    )
    sim.add(
        Obstruction(
            id="WALL_TOP",
            xb=Bounds3D.of(3.9, 4.1, 0.0, 4.0, 3.0, 4.0),
            color="GRAY",
        )
    )
    sim.add(
        Obstruction(
            id="WALL_LEFT",
            xb=Bounds3D.of(3.9, 4.1, 0.0, 1.0, 1.0, 3.0),
            color="GRAY",
        )
    )
    sim.add(
        Obstruction(
            id="WALL_RIGHT",
            xb=Bounds3D.of(3.9, 4.1, 3.0, 4.0, 1.0, 3.0),
            color="GRAY",
        )
    )

    # Left room boundary - higher pressure (25 Pa)
    sim.add(
        Vent(
            id="LEFT_PRESSURE",
            mb="XMIN",
            surf_id="OPEN",
            dynamic_pressure=25.0,
        )
    )

    # Right room boundary - lower pressure (0 Pa, ambient)
    sim.add(
        Vent(
            id="RIGHT_PRESSURE",
            mb="XMAX",
            surf_id="OPEN",
            dynamic_pressure=0.0,
        )
    )

    # --- Measurements ---

    # Pressure in left room
    sim.add(
        Device(
            id="P_LEFT",
            quantity="PRESSURE",
            xyz=Point3D(1.5, 2.0, 2.0),
        )
    )

    # Pressure in right room
    sim.add(
        Device(
            id="P_RIGHT",
            quantity="PRESSURE",
            xyz=Point3D(6.5, 2.0, 2.0),
        )
    )

    # Pressure at opening
    sim.add(
        Device(
            id="P_OPENING",
            quantity="PRESSURE",
            xyz=Point3D(4.0, 2.0, 2.0),
        )
    )

    # Velocity through opening
    sim.add(
        Device(
            id="U_OPENING",
            quantity="U-VELOCITY",
            xyz=Point3D(4.0, 2.0, 2.0),
        )
    )

    # Volume flow through opening
    sim.add(
        Device(
            id="FLOW_OPENING",
            quantity="VOLUME FLOW",
            xb=Bounds3D.of(3.9, 4.1, 1.0, 3.0, 1.0, 3.0),
        )
    )

    # Velocities in each room
    sim.add(
        Device(
            id="U_LEFT",
            quantity="U-VELOCITY",
            xyz=Point3D(2.0, 2.0, 2.0),
        )
    )

    sim.add(
        Device(
            id="U_RIGHT",
            quantity="U-VELOCITY",
            xyz=Point3D(6.0, 2.0, 2.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "pressure")
