#!/usr/bin/env python3
"""
HVAC Fan System Example
=======================

Demonstrates a fan-driven HVAC system connecting two sealed
compartments with a fan curve specification.

FDS Reference: HVAC/fan_test.fds
https://github.com/firemodels/fds/blob/master/Verification/HVAC/fan_test.fds

Key Namelists: HVAC (TYPE_ID='FAN', MAX_FLOW, MAX_PRESSURE)

Usage
-----
    python fan_system.py

Output
------
    fds/hvac/fan_system.fds
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
    Surface,
    Time,
    Vent,
)


def main():
    """Create an HVAC fan system simulation."""

    # Create simulation
    sim = Simulation(chid="fan_system", title="HVAC Fan System Test")

    # Time parameters
    sim.add(Time(t_end=60.0))

    # Two-mesh domain (left and right compartments)
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 10, 10),
            xb=Bounds3D.of(-3.0, 0.0, -1.0, 1.0, 0.0, 2.0),
        )
    )
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 10, 10),
            xb=Bounds3D.of(0.0, 3.0, -1.0, 1.0, 0.0, 2.0),
        )
    )

    # Partition wall between compartments
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.15, 0.15, -1.0, 1.0, 0.0, 2.0),
        )
    )

    # Define HVAC surface type
    sim.add(Surface(id="HVAC"))

    # HVAC vents through the partition wall
    # Lower vent pair (fan-driven left-to-right)
    sim.add(
        Vent(
            id="V_FAN_IN",
            xb=Bounds3D.of(-0.15, -0.15, -0.2, 0.2, 0.4, 0.6),
            surf_id="HVAC",
            color="RED",
            ior=-1,
        )
    )
    sim.add(
        Vent(
            id="V_FAN_OUT",
            xb=Bounds3D.of(0.15, 0.15, -0.2, 0.2, 0.4, 0.6),
            surf_id="HVAC",
            color="GREEN",
            ior=1,
        )
    )

    # Upper vent pair (passive flow with loss)
    sim.add(
        Vent(
            id="V_PASSIVE_IN",
            xb=Bounds3D.of(-0.15, -0.15, -0.2, 0.2, 1.4, 1.6),
            surf_id="HVAC",
            color="BLUE",
            ior=-1,
        )
    )
    sim.add(
        Vent(
            id="V_PASSIVE_OUT",
            xb=Bounds3D.of(0.15, 0.15, -0.2, 0.2, 1.4, 1.6),
            surf_id="HVAC",
            color="YELLOW",
            ior=1,
        )
    )

    # HVAC Nodes for fan duct
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

    # HVAC Nodes for passive duct
    sim.add(
        Hvac(
            id="N_PASSIVE_IN",
            type_id="NODE",
            vent_id="V_PASSIVE_IN",
            duct_id=["DUCT_PASSIVE"],
        )
    )
    sim.add(
        Hvac(
            id="N_PASSIVE_OUT",
            type_id="NODE",
            vent_id="V_PASSIVE_OUT",
            duct_id=["DUCT_PASSIVE"],
        )
    )

    # Fan component with characteristic curve
    sim.add(
        Hvac(
            id="FAN_1",
            type_id="FAN",
            max_flow=0.16,  # Maximum volume flow (m³/s)
            max_pressure=10.0,  # Maximum stall pressure (Pa)
        )
    )

    # Fan-driven duct
    sim.add(
        Hvac(
            id="DUCT_FAN",
            type_id="DUCT",
            node_id=("N_FAN_IN", "N_FAN_OUT"),
            area=0.04,
            length=1.0,
            fan_id="FAN_1",
            reverse=True,  # Reverse fan direction
            loss=[0.0, 0.0],
        )
    )

    # Passive duct with loss
    sim.add(
        Hvac(
            id="DUCT_PASSIVE",
            type_id="DUCT",
            node_id=("N_PASSIVE_IN", "N_PASSIVE_OUT"),
            area=0.04,
            length=1.0,
            loss=[10.0, 10.0],  # Loss coefficient
        )
    )

    # Pressure measurement devices in each compartment
    sim.add(
        Device(
            id="P_LEFT",
            xyz=Point3D.of(-1.5, 0.0, 0.5),
            quantity="PRESSURE",
        )
    )
    sim.add(
        Device(
            id="P_RIGHT",
            xyz=Point3D.of(1.5, 0.0, 0.5),
            quantity="PRESSURE",
        )
    )

    # Volume flow measurements in ducts
    sim.add(
        Device(
            id="Q_FAN",
            quantity="DUCT VOLUME FLOW",
            duct_id="DUCT_FAN",
        )
    )
    sim.add(
        Device(
            id="Q_PASSIVE",
            quantity="DUCT VOLUME FLOW",
            duct_id="DUCT_PASSIVE",
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
