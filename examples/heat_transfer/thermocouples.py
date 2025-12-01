#!/usr/bin/env python3
"""
Thermocouple Response Time Example
==================================

Demonstrates thermocouple modeling with response time (time constant).
Shows how thermocouples lag behind actual gas temperature changes.

FDS Reference: Heat_Transfer/thermocouple_time_constant.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/thermocouple_time_constant.fds

Key Namelists: DEVC (PROP), PROP (TIME_CONSTANT)

Usage
-----
    python thermocouples.py

Output
------
    fds/heat_transfer/thermocouples.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Initialization,
    Mesh,
    Property,
    Surface,
    Time,
    Vent,
)


def main():
    """Create thermocouple response time simulation."""

    # Create simulation
    sim = Simulation(chid="thermocouples", title="Thermocouple Time Constant Verification")

    # Time
    sim.add(Time(t_end=20.0))

    # Mesh - wind tunnel style
    sim.add(Mesh(ijk=Grid3D.of(30, 15, 15), xb=Bounds3D.of(-2.0, 2.0, -1.0, 1.0, -1.0, 1.0)))

    # Initial gas temperature
    sim.add(Initialization(xb=Bounds3D.of(-2.0, 2.0, -1.0, 1.0, -1.0, 1.0), temperature=30.0))

    # Inlet surface with hot gas at velocity
    sim.add(
        Surface(
            id="INLET",
            free_slip=True,
            vel=-20.0,  # 20 m/s inlet velocity
            tau_v=0.0,  # Immediate velocity
            tmp_front=30.0,  # Hot gas at 30°C
            tau_t=0.0,  # Immediate temperature
        )
    )

    # Adiabatic walls with free slip
    sim.add(
        Surface(
            id="default_wall",
            free_slip=True,
            default=True,
            adiabatic=True,
            heat_transfer_coefficient=0.0,
            color="GRAY",
        )
    )

    # Inlet and outlet
    sim.add(Vent(mb="XMIN", surf_id="INLET"))
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))

    # Thermocouple properties with different time constants
    # Time constant tau determines response: T_tc = T_gas - (T_gas - T_tc0) * exp(-t/tau)

    sim.add(
        Property(
            id="TC-0.5",
            time_constant=0.5,  # Fast response
        )
    )

    sim.add(
        Property(
            id="TC-3.0",
            time_constant=3.0,  # Medium response
        )
    )

    sim.add(
        Property(
            id="TC-8.0",
            time_constant=8.0,  # Slow response
            heat_transfer_coefficient=50.0,  # Alternative specification
        )
    )

    # Reference gas temperature
    sim.add(Device(id="T_Gas", quantity="TEMPERATURE", xyz=Point3D.of(-0.02, 0.0, 0.0)))

    # Thermocouples at different locations
    sim.add(
        Device(
            id="T_0.5s",
            quantity="THERMOCOUPLE",
            prop_id="TC-0.5",
            xyz=Point3D.of(-0.2, -0.5, 0.0),
            time_averaged=False,
        )
    )

    sim.add(
        Device(
            id="T_3.0s",
            quantity="THERMOCOUPLE",
            prop_id="TC-3.0",
            xyz=Point3D.of(-0.2, 0.0, 0.0),
            time_averaged=False,
        )
    )

    sim.add(
        Device(
            id="T_8.0s",
            quantity="THERMOCOUPLE",
            prop_id="TC-8.0",
            xyz=Point3D.of(-0.2, 0.5, 0.0),
            time_averaged=False,
        )
    )

    # Write output
    output_path = write_example(sim, "heat_transfer")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
