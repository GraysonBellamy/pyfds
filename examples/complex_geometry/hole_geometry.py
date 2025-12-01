#!/usr/bin/env python3
"""
Hole Geometry Example
=====================

Demonstrates the HOLE namelist for creating openings
in obstructions, such as doors, windows, and vents.

Based on FDS Verification: hole tests.

Key Namelists: HOLE, OBST

Usage
-----
    python hole_geometry.py

Output
------
    fds/complex_geometry/hole_geometry.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Hole,
    Mesh,
    Misc,
    Obstruction,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create hole geometry example."""
    sim = Simulation(
        chid="hole_geometry",
        title="Holes in Obstructions",
    )

    # Time
    sim.add(Time(t_end=60.0))

    # Misc
    sim.add(Misc(radiation=True))

    # Two-room domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 20, 20),
            xb=Bounds3D.of(0.0, 8.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    # Combustion
    sim.add(Reaction(fuel="PROPANE"))

    # Wall surface
    wall_surf = Surface(id="WALL", color="GRAY 70")
    sim.add(wall_surf)

    # Dividing wall between rooms
    sim.add(
        Obstruction(
            id="DIVIDING_WALL",
            xb=Bounds3D.of(3.9, 4.1, 0.0, 4.0, 0.0, 4.0),
            surf_id="WALL",
        )
    )

    # Door hole in dividing wall
    sim.add(
        Hole(
            id="DOOR",
            xb=Bounds3D.of(3.8, 4.2, 1.5, 2.5, 0.0, 2.2),
        )
    )

    # Window hole in dividing wall (upper portion)
    sim.add(
        Hole(
            id="WINDOW",
            xb=Bounds3D.of(3.8, 4.2, 0.5, 1.2, 2.5, 3.2),
        )
    )

    # Fire in left room
    fire_surf = Surface(id="FIRE", hrrpua=400.0, color="ORANGE")
    sim.add(fire_surf)
    sim.add(Vent(xb=Bounds3D.of(1.0, 2.0, 1.5, 2.5, 0.0, 0.0), surf_id="FIRE"))

    # Open ceiling vent in right room
    sim.add(Vent(xb=Bounds3D.of(5.0, 7.0, 1.0, 3.0, 4.0, 4.0), surf_id="OPEN"))

    # --- Measurements ---

    # Temperature in fire room
    sim.add(
        Device(
            id="T_FIRE_ROOM_LOW",
            quantity="TEMPERATURE",
            xyz=Point3D(2.0, 2.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="T_FIRE_ROOM_HIGH",
            quantity="TEMPERATURE",
            xyz=Point3D(2.0, 2.0, 3.5),
        )
    )

    # Temperature in adjacent room
    sim.add(
        Device(
            id="T_ADJ_ROOM_LOW",
            quantity="TEMPERATURE",
            xyz=Point3D(6.0, 2.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="T_ADJ_ROOM_HIGH",
            quantity="TEMPERATURE",
            xyz=Point3D(6.0, 2.0, 3.5),
        )
    )

    # Velocity through door
    sim.add(
        Device(
            id="U_DOOR_LOW",
            quantity="U-VELOCITY",
            xyz=Point3D(4.0, 2.0, 0.5),
        )
    )

    sim.add(
        Device(
            id="U_DOOR_HIGH",
            quantity="U-VELOCITY",
            xyz=Point3D(4.0, 2.0, 1.8),
        )
    )

    # Velocity through window
    sim.add(
        Device(
            id="U_WINDOW",
            quantity="U-VELOCITY",
            xyz=Point3D(4.0, 0.85, 2.85),
        )
    )

    # Volume flow through door
    sim.add(
        Device(
            id="FLOW_DOOR",
            quantity="VOLUME FLOW",
            xb=Bounds3D.of(3.9, 4.1, 1.5, 2.5, 0.0, 2.2),
        )
    )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 8.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "complex_geometry")
