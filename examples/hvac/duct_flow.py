#!/usr/bin/env python3
"""
HVAC Duct Flow Example
======================

Demonstrates a simple HVAC duct system with nodes, ducts, and
specified volume flow rate.

FDS Reference: HVAC/HVAC_flow_loss.fds
https://github.com/firemodels/fds/blob/master/Verification/HVAC/HVAC_flow_loss.fds

Key Namelists: HVAC (TYPE_ID='DUCT', 'NODE'), VENT (SURF_ID='HVAC')

Usage
-----
    python duct_flow.py

Output
------
    fds/hvac/duct_flow.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
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
    """Create a simple HVAC duct flow simulation."""

    # Create simulation
    sim = Simulation(chid="duct_flow", title="HVAC Duct Flow Test")

    # Time parameters
    sim.add(Time(t_end=10.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # Solid obstruction with HVAC openings
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.45, 0.55),
        )
    )

    # Define HVAC surface type
    sim.add(Surface(id="HVAC"))

    # Open boundaries all around
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMIN", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # HVAC vents (supply and exhaust)
    # Inlet vent (above obstruction)
    sim.add(
        Vent(
            id="V_INLET",
            xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0.55, 0.55),
            surf_id="HVAC",
            color="GREEN",
        )
    )

    # Outlet vents (below obstruction)
    sim.add(
        Vent(
            id="V_OUTLET_A",
            xb=Bounds3D.of(0.1, 0.3, 0.4, 0.6, 0.45, 0.45),
            surf_id="HVAC",
            color="RED",
        )
    )

    sim.add(
        Vent(
            id="V_OUTLET_B",
            xb=Bounds3D.of(0.6, 1.0, 0.4, 0.6, 0.45, 0.45),
            surf_id="HVAC",
            color="ORANGE",
        )
    )

    # HVAC Nodes - connect vents to internal ductwork
    sim.add(
        Hvac(
            id="N_INLET",
            type_id="NODE",
            vent_id="V_INLET",
            duct_id=["DUCT_MAIN"],
        )
    )

    sim.add(
        Hvac(
            id="N_OUTLET_A",
            type_id="NODE",
            vent_id="V_OUTLET_A",
            duct_id=["DUCT_A"],
        )
    )

    sim.add(
        Hvac(
            id="N_OUTLET_B",
            type_id="NODE",
            vent_id="V_OUTLET_B",
            duct_id=["DUCT_B"],
        )
    )

    # Internal tee junction node
    sim.add(
        Hvac(
            id="TEE",
            type_id="NODE",
            xyz=(0.5, 0.5, 0.0),  # Internal node location
            duct_id=["DUCT_MAIN", "DUCT_A", "DUCT_B"],
        )
    )

    # HVAC Ducts - connect nodes with specified flow
    # Main supply duct with fixed volume flow
    sim.add(
        Hvac(
            id="DUCT_MAIN",
            type_id="DUCT",
            node_id=("N_INLET", "TEE"),
            area=0.1,
            length=1.0,
            volume_flow=0.3,  # 0.3 m³/s fixed flow
        )
    )

    # Branch duct A with higher loss
    sim.add(
        Hvac(
            id="DUCT_A",
            type_id="DUCT",
            node_id=("TEE", "N_OUTLET_A"),
            area=0.1,
            length=1.0,
            loss=[16.0, 16.0],  # High loss coefficient
        )
    )

    # Branch duct B with lower loss
    sim.add(
        Hvac(
            id="DUCT_B",
            type_id="DUCT",
            node_id=("TEE", "N_OUTLET_B"),
            area=0.1,
            length=1.0,
            loss=[4.0, 4.0],  # Lower loss coefficient
        )
    )

    # Velocity measurements in ducts
    sim.add(
        Device(
            id="VEL_MAIN",
            quantity="DUCT VELOCITY",
            duct_id="DUCT_MAIN",
        )
    )

    sim.add(
        Device(
            id="VEL_A",
            quantity="DUCT VELOCITY",
            duct_id="DUCT_A",
        )
    )

    sim.add(
        Device(
            id="VEL_B",
            quantity="DUCT VELOCITY",
            duct_id="DUCT_B",
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
