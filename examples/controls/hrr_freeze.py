#!/usr/bin/env python3
"""
HRR Freeze Example
==================

Demonstrates freezing the heat release rate at a setpoint using
DEVC NO_UPDATE_DEVC_ID to halt RAMP progression.

FDS Reference: Controls/hrr_freeze.fds
https://github.com/firemodels/fds/blob/master/Verification/Controls/hrr_freeze.fds

Key Namelists: RAMP (DEVC_ID), DEVC (NO_UPDATE_DEVC_ID, SETPOINT), SURF (RAMP_Q)

Usage
-----
    python hrr_freeze.py

Output
------
    fds/controls/hrr_freeze.fds
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
    Ramp,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation where HRR freezes at a temperature setpoint."""

    # Create simulation
    sim = Simulation(
        chid="hrr_freeze",
        title="Test of HRR Freeze at Setpoint",
    )

    # Time parameters - fire ramps up over 50s but should freeze earlier
    sim.add(Time(t_end=30.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # Open boundaries all around except floor
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Propane combustion
    sim.add(Reaction(fuel="PROPANE"))

    # ==========================================================================
    # HRR Ramp - linear growth that can be frozen
    # ==========================================================================

    # The RAMP is linked to a DEVC that tracks time.
    # When the freeze device triggers, the time-tracking DEVC stops updating,
    # freezing the RAMP (and thus the HRR) at its current value.

    # Fire ramp: grows linearly from 0% at t=0 to 100% at t=50s
    # Using DEVC_ID on RAMP links it to the "freeze time" device
    sim.add(
        Ramp(
            id="FIRE_RAMP",
            points=[(0.0, 0.0), (50.0, 1.0)],  # Linear growth over 50s
            devc_id="FREEZE_TIME",  # Linked to freeze device
        )
    )

    # Fire surface with RAMP_Q for time-varying HRR
    sim.add(
        Surface(
            id="FIRE",
            hrrpua=1000.0,  # Peak HRR per unit area
            ramp_q="FIRE_RAMP",
            color="ORANGE",
        )
    )

    # Fire vent
    sim.add(
        Vent(
            xb=Bounds3D.of(0.3, 0.7, 0.3, 0.7, 0.0, 0.0),
            surf_id="FIRE",
        )
    )

    # ==========================================================================
    # Freeze control devices
    # ==========================================================================

    # Temperature sensor that triggers at setpoint
    # INITIAL_STATE=False means it starts in "not triggered" state
    sim.add(
        Device(
            id="TEMP_TRIGGER",
            xyz=Point3D(0.5, 0.5, 0.8),
            quantity="TEMPERATURE",
            setpoint=200.0,  # Trigger when temp reaches 200°C
            initial_state=False,
        )
    )

    # Time-tracking device that freezes when TEMP_TRIGGER activates
    # NO_UPDATE_DEVC_ID stops this device from updating after trigger
    sim.add(
        Device(
            id="FREEZE_TIME",
            xyz=Point3D(0.5, 0.5, 0.8),
            quantity="TIME",
            no_update_devc_id="TEMP_TRIGGER",
        )
    )

    # ==========================================================================
    # Monitoring devices
    # ==========================================================================

    sim.add(
        Device(
            id="CEILING_TEMP",
            xyz=Point3D(0.5, 0.5, 0.95),
            quantity="TEMPERATURE",
        )
    )

    sim.add(
        Device(
            id="FIRE_TEMP",
            xyz=Point3D(0.5, 0.5, 0.2),
            quantity="TEMPERATURE",
        )
    )

    sim.add(
        Device(
            id="HRR",
            xyz=Point3D(0.5, 0.5, 0.5),
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
