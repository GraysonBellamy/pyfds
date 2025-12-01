#!/usr/bin/env python3
"""
Vent Activation Example
=======================

Demonstrates automated vent control using VENT CTRL_ID and DEVC_ID
parameters to activate/deactivate vents based on control logic.

FDS Reference: Controls/activate_vents.fds
https://github.com/firemodels/fds/blob/master/Verification/Controls/activate_vents.fds

Key Namelists: VENT (CTRL_ID, DEVC_ID), CTRL (FUNCTION_TYPE), SURF (VEL)

Usage
-----
    python vent_activation.py

Output
------
    fds/controls/vent_activation.fds
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
    Particle,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with multiple controlled vents."""

    # Create simulation
    sim = Simulation(
        chid="vent_activation",
        title="Test of VENT Activation/Deactivation",
    )

    # Time parameters
    sim.add(Time(t_end=20.0, dt=0.05))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(21, 10, 10),
            xb=Bounds3D.of(0.0, 2.1, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # Open boundaries for flow
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # ==========================================================================
    # Tracer particles for flow visualization
    # ==========================================================================

    for i, color in enumerate(["PURPLE", "RED", "GREEN", "CYAN"], start=1):
        sim.add(
            Particle(
                id=f"TRACER_{i}",
                massless=True,
                color=color,
            )
        )

    # ==========================================================================
    # Blower surfaces with different colors and tracers
    # ==========================================================================

    colors = ["PURPLE", "RED", "GREEN", "CYAN"]
    for i, color in enumerate(colors, start=1):
        sim.add(
            Surface(
                id=f"BLOW_{i}",
                vel=-0.2,  # Blowing velocity into domain
                color=color,
                part_id=f"TRACER_{i}",
            )
        )

    # ==========================================================================
    # Timer devices for triggering
    # ==========================================================================

    # Immediate timer (for clock reference)
    sim.add(
        Device(
            id="CLOCK",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
        )
    )

    # Timer at 3 seconds
    sim.add(
        Device(
            id="TIMER_3S",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
            setpoint=3.0,
        )
    )

    # Timer at 5 seconds
    sim.add(
        Device(
            id="TIMER_5S",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
            setpoint=5.0,
        )
    )

    # Timer at 8 seconds
    sim.add(
        Device(
            id="TIMER_8S",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
            setpoint=8.0,
        )
    )

    # Timer at 10 seconds
    sim.add(
        Device(
            id="TIMER_10S",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
            setpoint=10.0,
        )
    )

    # ==========================================================================
    # Control logic for vents
    # ==========================================================================

    # Control 1: Time delay after TIMER_3S (activates at 6s)
    sim.add(
        Control(
            id="CTRL_DELAY",
            function_type=ControlFunction.TIME_DELAY,
            input_id="TIMER_3S",
            delay=3.0,
        )
    )

    # Control 2: ALL - both must trigger
    sim.add(
        Control(
            id="CTRL_ALL",
            function_type=ControlFunction.ALL,
            input_id=["TIMER_5S", "TIMER_8S"],
        )
    )

    # ==========================================================================
    # Controlled vents
    # ==========================================================================

    # Vent 1: Activated directly by device at 3 seconds
    sim.add(
        Vent(
            id="VENT_DIRECT",
            xb=Bounds3D.of(0.1, 0.2, 0.4, 0.6, 0.0, 0.0),
            surf_id="BLOW_1",
            devc_id="TIMER_3S",  # Direct device control
        )
    )

    # Vent 2: Controlled by delay logic (3s + 3s delay = 6s)
    sim.add(
        Vent(
            id="VENT_DELAYED",
            xb=Bounds3D.of(0.6, 0.7, 0.4, 0.6, 0.0, 0.0),
            surf_id="BLOW_2",
            ctrl_id="CTRL_DELAY",  # Control logic
        )
    )

    # Vent 3: Controlled by ALL logic (needs both 5s AND 8s)
    sim.add(
        Vent(
            id="VENT_ALL",
            xb=Bounds3D.of(1.1, 1.2, 0.4, 0.6, 0.0, 0.0),
            surf_id="BLOW_3",
            ctrl_id="CTRL_ALL",
        )
    )

    # Vent 4: Direct timer control at 10s
    sim.add(
        Vent(
            id="VENT_LATE",
            xb=Bounds3D.of(1.6, 1.7, 0.4, 0.6, 0.0, 0.0),
            surf_id="BLOW_4",
            devc_id="TIMER_10S",
        )
    )

    # ==========================================================================
    # Monitoring devices
    # ==========================================================================

    sim.add(
        Device(
            id="VEL_1",
            xyz=Point3D(0.15, 0.5, 0.1),
            quantity="VELOCITY",
        )
    )

    sim.add(
        Device(
            id="VEL_2",
            xyz=Point3D(0.65, 0.5, 0.1),
            quantity="VELOCITY",
        )
    )

    sim.add(
        Device(
            id="VEL_3",
            xyz=Point3D(1.15, 0.5, 0.1),
            quantity="VELOCITY",
        )
    )

    sim.add(
        Device(
            id="VEL_4",
            xyz=Point3D(1.65, 0.5, 0.1),
            quantity="VELOCITY",
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
