#!/usr/bin/env python3
"""
Device Activation Example
=========================

Demonstrates device setpoint triggering using DEVC SETPOINT
and TRIP_DIRECTION parameters for automated control.

FDS Reference: Controls/device_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Controls/device_test.fds

Key Namelists: DEVC (SETPOINT, TRIP_DIRECTION), OBST (DEVC_ID)

Usage
-----
    python device_activation.py

Output
------
    fds/controls/device_activation.fds
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
    """Create a device activation simulation with temperature setpoints."""

    # Create simulation
    sim = Simulation(
        chid="device_activation",
        title="Device Setpoint Activation Test",
    )

    # Time parameters - run until setpoint reached
    sim.add(Time(t_end=30.0))

    # Single mesh for simple geometry
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Simple propane combustion
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))

    # Fire surface
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="ORANGE"))

    # Fire vent on floor
    sim.add(
        Vent(
            xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0.0, 0.0),
            surf_id="FIRE",
        )
    )

    # Open top boundary for venting
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # Obstruction that disappears when temperature setpoint is reached
    # DEVC_ID links to device - obstruction removed when device activates
    sim.add(
        Obstruction(
            id="TEMP_CONTROLLED_BLOCK",
            xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 1.6, 1.8),
            color="CYAN",
            devc_id="TEMP_SENSOR",
        )
    )

    # Temperature sensor with setpoint
    # TRIP_DIRECTION=1 means triggers when value goes ABOVE setpoint
    sim.add(
        Device(
            id="TEMP_SENSOR",
            xyz=Point3D(1.0, 1.0, 1.5),
            quantity="TEMPERATURE",
            setpoint=100.0,  # Trigger at 100°C
            trip_direction=1,  # Trigger when temp rises above
        )
    )

    # Time-based device (triggers at specific time)
    sim.add(
        Device(
            id="TIMER_5S",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
            setpoint=5.0,  # Trigger at 5 seconds
        )
    )

    # Time-based device with later trigger
    sim.add(
        Device(
            id="TIMER_15S",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
            setpoint=15.0,  # Trigger at 15 seconds
        )
    )

    # Obstruction tied to time-based device
    sim.add(
        Obstruction(
            id="TIME_CONTROLLED_BLOCK",
            xb=Bounds3D.of(0.2, 0.4, 0.2, 0.4, 0.0, 0.2),
            color="GREEN",
            devc_id="TIMER_5S",
        )
    )

    # Monitoring devices for output
    sim.add(
        Device(
            id="PLUME_TEMP",
            xyz=Point3D(1.0, 1.0, 0.5),
            quantity="TEMPERATURE",
        )
    )

    sim.add(
        Device(
            id="CEILING_TEMP",
            xyz=Point3D(1.0, 1.0, 1.9),
            quantity="TEMPERATURE",
        )
    )

    sim.add(
        Device(
            id="HRR",
            xyz=Point3D(1.0, 1.0, 1.0),
            quantity="HRR",
        )
    )

    # Write output
    output_path = write_example(sim, "controls")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
