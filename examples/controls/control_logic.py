#!/usr/bin/env python3
"""
Control Logic Example
=====================

Demonstrates boolean control functions using CTRL namelist with
FUNCTION_TYPE for complex control logic.

FDS Reference: Controls/control_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Controls/control_test.fds

Key Namelists: CTRL (FUNCTION_TYPE, INPUT_ID, DELAY), DEVC (SETPOINT)

Usage
-----
    python control_logic.py

Output
------
    fds/controls/control_logic.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.enums import ControlFunction
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Control,
    Device,
    Mesh,
    Obstruction,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation demonstrating various control logic functions."""

    # Create simulation
    sim = Simulation(
        chid="control_logic",
        title="Control Function Testing",
    )

    # Time parameters
    sim.add(Time(t_end=20.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(-1.0, 1.0, -1.0, 1.0, 0.0, 2.0),
        )
    )

    # Open boundary for smoke venting
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))

    # Combustion
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))

    # Fire surface - controlled by delay control
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="ORANGE"))

    # Fire vent that activates after delay
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.2, 0.2, -0.2, 0.2, 0.0, 0.0),
            surf_id="FIRE",
            ctrl_id="DELAY_CTRL",  # Controlled by CTRL
        )
    )

    # ==========================================================================
    # Timer devices (inputs for control logic)
    # ==========================================================================

    # Timer 1: triggers at 3 seconds
    sim.add(
        Device(
            id="TIMER_3S",
            xyz=Point3D(0.0, 0.0, 0.1),
            quantity="TIME",
            setpoint=3.0,
        )
    )

    # Timer 2: triggers at 8 seconds
    sim.add(
        Device(
            id="TIMER_8S",
            xyz=Point3D(0.0, 0.0, 0.1),
            quantity="TIME",
            setpoint=8.0,
        )
    )

    # Temperature sensor with setpoint
    sim.add(
        Device(
            id="TEMP_50C",
            xyz=Point3D(0.0, 0.0, 1.5),
            quantity="TEMPERATURE",
            setpoint=50.0,
            trip_direction=1,
        )
    )

    # ==========================================================================
    # Control logic examples
    # ==========================================================================

    # TIME_DELAY: Fire starts 2 seconds after TIMER_3S triggers (at t=5s)
    sim.add(
        Control(
            id="DELAY_CTRL",
            function_type=ControlFunction.TIME_DELAY,
            input_id="TIMER_3S",
            delay=2.0,
        )
    )

    # ALL: Both conditions must be true
    # Obstruction disappears when TIMER_8S AND TEMP_50C both trigger
    sim.add(
        Control(
            id="ALL_CTRL",
            function_type=ControlFunction.ALL,
            input_id=["TIMER_8S", "TEMP_50C"],
            initial_state=True,  # Start with obstruction visible
        )
    )

    # Obstruction controlled by ALL logic
    sim.add(
        Obstruction(
            id="ALL_BLOCK",
            xb=Bounds3D.of(-0.8, -0.5, -0.8, -0.5, 1.5, 1.8),
            color="CYAN",
            ctrl_id="ALL_CTRL",
        )
    )

    # ANY: Either condition triggers
    # Additional timer devices for ANY logic
    sim.add(
        Device(
            id="TIMER_6S",
            xyz=Point3D(0.0, 0.0, 0.1),
            quantity="TIME",
            setpoint=6.0,
        )
    )

    sim.add(
        Device(
            id="TIMER_10S",
            xyz=Point3D(0.0, 0.0, 0.1),
            quantity="TIME",
            setpoint=10.0,
        )
    )

    sim.add(
        Control(
            id="ANY_CTRL",
            function_type=ControlFunction.ANY,
            input_id=["TIMER_6S", "TIMER_10S"],
        )
    )

    # Obstruction controlled by ANY logic
    sim.add(
        Obstruction(
            id="ANY_BLOCK",
            xb=Bounds3D.of(0.5, 0.8, 0.5, 0.8, 0.0, 0.3),
            color="GREEN",
            ctrl_id="ANY_CTRL",
        )
    )

    # ==========================================================================
    # Monitoring devices
    # ==========================================================================

    sim.add(
        Device(
            id="CEILING_TEMP",
            xyz=Point3D(0.0, 0.0, 1.9),
            quantity="TEMPERATURE",
        )
    )

    sim.add(
        Device(
            id="HRR_MONITOR",
            xyz=Point3D(0.0, 0.0, 0.5),
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
