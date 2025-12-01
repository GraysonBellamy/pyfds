#!/usr/bin/env python3
"""
Room Pressurization Example
============================

Demonstrates room pressurization from supply air
with pressure relief through open boundaries.

Based on FDS Verification: pressure_rise tests.

Key Namelists: SURF (VEL, VOLUME_FLOW), VENT (DYNAMIC_PRESSURE)

Usage
-----
    python room_pressurization.py

Output
------
    fds/pressure/room_pressurization.fds
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
    Ramp,
    Surface,
    Time,
    Vent,
)


def main():
    """Create room pressurization example."""
    sim = Simulation(
        chid="room_pressurization",
        title="Room Pressurization from Supply Air",
    )

    # Time
    sim.add(Time(t_end=120.0))

    # Misc
    sim.add(Misc(stratification=False, radiation=False))

    # Room domain - enclosed room with one opening
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 20, 20),
            xb=Bounds3D.of(0.0, 6.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    # Ramp for supply air - gradual startup
    supply_ramp = Ramp(
        id="SUPPLY_RAMP",
        points=[
            (0.0, 0.0),
            (10.0, 1.0),  # Ramp up over 10 seconds
            (100.0, 1.0),  # Steady operation
            (110.0, 0.0),  # Shutdown
            (120.0, 0.0),
        ],
    )
    sim.add(supply_ramp)

    # Supply air inlet - blowing into room
    supply_surf = Surface(
        id="SUPPLY",
        vel=-2.0,  # 2 m/s into room (negative = into domain)
        ramp_v="SUPPLY_RAMP",
        color="BLUE",
    )
    sim.add(supply_surf)

    # Supply diffuser on ceiling
    sim.add(
        Vent(
            id="SUPPLY_DIFFUSER",
            xb=Bounds3D.of(2.5, 3.5, 1.5, 2.5, 4.0, 4.0),
            surf_id="SUPPLY",
        )
    )

    # Pressure relief opening - door at floor level
    # Partially open door (1m x 2m)
    sim.add(
        Vent(
            id="DOOR",
            xb=Bounds3D.of(0.0, 0.0, 1.5, 2.5, 0.0, 2.0),
            surf_id="OPEN",
        )
    )

    # --- Measurements ---

    # Room pressure at various heights
    for height, label in [(1.0, "LOW"), (2.0, "MID"), (3.0, "HIGH")]:
        sim.add(
            Device(
                id=f"P_{label}",
                quantity="PRESSURE",
                xyz=Point3D(3.0, 2.0, height),
            )
        )

    # Pressure at door
    sim.add(
        Device(
            id="P_DOOR",
            quantity="PRESSURE",
            xyz=Point3D(0.5, 2.0, 1.0),
        )
    )

    # Velocity at supply diffuser
    sim.add(
        Device(
            id="V_SUPPLY",
            quantity="W-VELOCITY",
            xyz=Point3D(3.0, 2.0, 3.8),
        )
    )

    # Velocity at door (outflow)
    sim.add(
        Device(
            id="U_DOOR",
            quantity="U-VELOCITY",
            xyz=Point3D(0.2, 2.0, 1.0),
        )
    )

    # Volume flow at door
    sim.add(
        Device(
            id="FLOW_DOOR",
            quantity="VOLUME FLOW",
            xb=Bounds3D.of(0.0, 0.0, 1.5, 2.5, 0.0, 2.0),
        )
    )

    # Supply volume flow
    sim.add(
        Device(
            id="FLOW_SUPPLY",
            quantity="VOLUME FLOW",
            xb=Bounds3D.of(2.5, 3.5, 1.5, 2.5, 4.0, 4.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "pressure")
