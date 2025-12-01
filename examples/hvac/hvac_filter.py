#!/usr/bin/env python3
"""
HVAC Filter Example
===================

Demonstrates an HVAC filter system with particle removal
and filter loading.

FDS Reference: HVAC/HVAC_filter.fds
https://github.com/firemodels/fds/blob/master/Verification/HVAC/HVAC_filter.fds

Key Namelists: HVAC (TYPE_ID='FILTER', EFFICIENCY, CLEAN_LOSS, LOADING_MULTIPLIER)

Usage
-----
    python hvac_filter.py

Output
------
    fds/hvac/hvac_filter.fds
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
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create an HVAC filter simulation."""

    # Create simulation
    sim = Simulation(chid="hvac_filter", title="HVAC Filter Test")

    # Time parameters
    sim.add(Time(t_end=500.0))

    # Single mesh domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # Species definitions
    # Background gas
    sim.add(
        Species(
            id="BACKGROUND",
            mw=28.0,
            background=True,
            specific_heat=1.0,
        )
    )

    # Particulate species to be filtered
    sim.add(
        Species(
            id="PARTICULATE",
            mw=28.0,
            mass_fraction_0=0.001,  # Initial concentration
            specific_heat=1.0,
        )
    )

    # HVAC surface type
    sim.add(Surface(id="HVAC"))

    # Surface with no heat transfer
    sim.add(
        Surface(
            id="NO_HTC",
            heat_transfer_coefficient=0.0,
            default=True,
        )
    )

    # Inlet and outlet vents
    sim.add(
        Vent(
            id="V_INLET",
            xb=Bounds3D.of(0.0, 0.0, 0.0, 0.1, 0.5, 0.6),
            surf_id="HVAC",
            color="RED",
        )
    )

    sim.add(
        Vent(
            id="V_OUTLET",
            xb=Bounds3D.of(1.0, 1.0, 0.9, 1.0, 0.5, 0.6),
            surf_id="HVAC",
            color="BLUE",
        )
    )

    # HVAC Nodes
    sim.add(
        Hvac(
            id="N_INLET",
            type_id="NODE",
            vent_id="V_INLET",
            duct_id=["DUCT_IN"],
        )
    )

    sim.add(
        Hvac(
            id="N_OUTLET",
            type_id="NODE",
            vent_id="V_OUTLET",
            duct_id=["DUCT_OUT"],
        )
    )

    # Filter node - internal node with filter
    sim.add(
        Hvac(
            id="N_FILTER",
            type_id="NODE",
            xyz=(0.5, 0.5, 0.55),
            duct_id=["DUCT_IN", "DUCT_OUT"],
            filter_id="FILTER_1",
        )
    )

    # Fan component
    sim.add(
        Hvac(
            id="FAN_1",
            type_id="FAN",
            max_flow=0.2,
            max_pressure=20.0,
        )
    )

    # Filter component
    sim.add(
        Hvac(
            id="FILTER_1",
            type_id="FILTER",
            clean_loss=1.0,  # Clean filter loss coefficient
            spec_id=["PARTICULATE"],
            efficiency=[1.0],  # 100% efficiency for particulate
            loading_multiplier=[1.0],  # Loading multiplier
        )
    )

    # Inlet duct with fan
    sim.add(
        Hvac(
            id="DUCT_IN",
            type_id="DUCT",
            node_id=("N_INLET", "N_FILTER"),
            area=0.01,
            fan_id="FAN_1",
            loss=[0.0, 0.0],
        )
    )

    # Outlet duct
    sim.add(
        Hvac(
            id="DUCT_OUT",
            type_id="DUCT",
            node_id=("N_FILTER", "N_OUTLET"),
            area=0.01,
            loss=[0.0, 0.0],
        )
    )

    # Measurement devices
    # Filter loading
    sim.add(
        Device(
            id="FILTER_LOAD",
            quantity="FILTER LOADING",
            node_id="N_FILTER",
            spec_id="PARTICULATE",
        )
    )

    # Filter loss coefficient
    sim.add(
        Device(
            id="FILTER_LOSS",
            quantity="FILTER LOSS",
            node_id="N_FILTER",
        )
    )

    # Duct velocity
    sim.add(
        Device(
            id="DUCT_VEL",
            quantity="DUCT VELOCITY",
            duct_id="DUCT_IN",
        )
    )

    # Node pressures
    sim.add(
        Device(
            id="P_INLET",
            quantity="NODE PRESSURE",
            node_id="N_INLET",
        )
    )

    sim.add(
        Device(
            id="P_FILTER",
            quantity="NODE PRESSURE",
            node_id="N_FILTER",
        )
    )

    sim.add(
        Device(
            id="P_OUTLET",
            quantity="NODE PRESSURE",
            node_id="N_OUTLET",
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
