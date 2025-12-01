#!/usr/bin/env python3
"""
HVAC Leakage Example
====================

Demonstrates pressure-driven leakage between compartments
using the ZONE and LEAK_PATH parameters.

FDS Reference: HVAC/leak_test.fds
https://github.com/firemodels/fds/blob/master/Verification/HVAC/leak_test.fds

Key Namelists: ZONE (LEAK_AREA), SURF (LEAK_PATH)

Usage
-----
    python leakage.py

Output
------
    fds/hvac/leakage.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Hvac,
    Mesh,
    Obstruction,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create an HVAC leakage simulation."""

    # Create simulation
    sim = Simulation(chid="leakage", title="HVAC Leakage Test")

    # Time parameters
    sim.add(Time(t_end=30.0))

    # Single mesh domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 10, 10),
            xb=Bounds3D.of(-1.0, 1.0, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # Background species
    sim.add(
        Species(
            id="BACKGROUND",
            mw=28.0,
            background=True,
        )
    )

    # Partition wall between left and right compartments
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.1, 0.1, 0.0, 1.0, 0.1, 1.0),
        )
    )

    # Floor obstruction
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.1, 0.1, 0.0, 1.0, 0.0, 0.1),
        )
    )

    # HVAC surface
    sim.add(Surface(id="HVAC"))

    # Leak surface connecting zones
    sim.add(
        Surface(
            id="LEAK",
            leak_path=(1, 2),  # Leak path from zone 1 to zone 2
        )
    )

    # Fan inlet/outlet vents through partition
    sim.add(
        Vent(
            id="V_FAN_IN",
            xb=Bounds3D.of(-0.1, -0.1, 0.4, 0.6, 0.7, 0.9),
            surf_id="HVAC",
            color="PINK",
        )
    )

    sim.add(
        Vent(
            id="V_FAN_OUT",
            xb=Bounds3D.of(0.1, 0.1, 0.4, 0.6, 0.7, 0.9),
            surf_id="HVAC",
            color="RED",
        )
    )

    # Leak vents at floor level
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.2, -0.1, 0.0, 1.0, 0.0, 0.0),
            surf_id="LEAK",
            color="GREEN",
        )
    )

    sim.add(
        Vent(
            xb=Bounds3D.of(0.1, 0.2, 0.0, 1.0, 0.0, 0.0),
            surf_id="LEAK",
            color="ORANGE",
        )
    )

    # HVAC Nodes
    sim.add(
        Hvac(
            id="N_FAN_IN",
            type_id="NODE",
            vent_id="V_FAN_IN",
            duct_id=["DUCT_FAN"],
        )
    )

    sim.add(
        Hvac(
            id="N_FAN_OUT",
            type_id="NODE",
            vent_id="V_FAN_OUT",
            duct_id=["DUCT_FAN"],
        )
    )

    # Fan component with ramp-up
    sim.add(
        Hvac(
            id="FAN_1",
            type_id="FAN",
            max_flow=0.06,
            max_pressure=300.0,
            tau_fan=1.0,  # 1 second ramp-up time constant
        )
    )

    # Fan duct
    sim.add(
        Hvac(
            id="DUCT_FAN",
            type_id="DUCT",
            node_id=("N_FAN_IN", "N_FAN_OUT"),
            length=0.1,
            area=0.1,
            fan_id="FAN_1",
            loss=[0.0, 0.0],
        )
    )

    # Pressure measurement devices
    sim.add(
        Device(
            id="P_LEFT",
            xyz=Point3D.of(-0.5, 0.5, 0.5),
            quantity="PRESSURE",
        )
    )

    sim.add(
        Device(
            id="P_RIGHT",
            xyz=Point3D.of(0.5, 0.5, 0.5),
            quantity="PRESSURE",
        )
    )

    # Volume flow through duct
    sim.add(
        Device(
            id="Q_FAN",
            quantity="DUCT VOLUME FLOW",
            duct_id="DUCT_FAN",
        )
    )

    # Write output
    output_path = write_example(sim, "hvac")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
