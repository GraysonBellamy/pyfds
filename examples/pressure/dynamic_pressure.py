#!/usr/bin/env python3
"""
Dynamic Pressure Boundary Condition Example
============================================

Demonstrates the use of dynamic pressure on OPEN boundaries
to model wind effects and external pressure variations.

Based on FDS Verification: pressure_boundary tests.

Key Namelists: VENT (DYNAMIC_PRESSURE, PRESSURE_RAMP)

Usage
-----
    python dynamic_pressure.py

Output
------
    fds/pressure/dynamic_pressure.fds
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
    Time,
    Vent,
)


def main():
    """Create dynamic pressure boundary condition example."""
    sim = Simulation(
        chid="dynamic_pressure",
        title="Dynamic Pressure Boundary Conditions",
    )

    # Time
    sim.add(Time(t_end=60.0))

    # Misc - disable stratification for cleaner pressure testing
    sim.add(Misc(stratification=False, radiation=False))

    # Domain - simple box
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 20, 20),
            xb=Bounds3D.of(0.0, 6.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    # Ramp for time-varying pressure (simulating wind gusts)
    pressure_ramp = Ramp(
        id="WIND_RAMP",
        points=[
            (0.0, 0.0),
            (5.0, 1.0),  # Ramp up
            (25.0, 1.0),  # Steady
            (30.0, 0.5),  # Decrease
            (45.0, 0.5),  # Steady
            (50.0, 0.0),  # Calm
            (60.0, 0.0),
        ],
    )
    sim.add(pressure_ramp)

    # High pressure on inlet (XMIN) - 50 Pa above ambient
    sim.add(
        Vent(
            id="INLET",
            mb="XMIN",
            surf_id="OPEN",
            dynamic_pressure=50.0,
            pressure_ramp="WIND_RAMP",
        )
    )

    # Low pressure on outlet (XMAX) - ambient pressure
    sim.add(
        Vent(
            id="OUTLET",
            mb="XMAX",
            surf_id="OPEN",
        )
    )

    # Open top
    sim.add(
        Vent(
            id="TOP",
            mb="ZMAX",
            surf_id="OPEN",
        )
    )

    # --- Velocity Measurements ---

    # Velocity at inlet
    sim.add(
        Device(
            id="U_INLET",
            quantity="U-VELOCITY",
            xyz=Point3D(0.5, 2.0, 2.0),
        )
    )

    # Velocity in middle
    sim.add(
        Device(
            id="U_MID",
            quantity="U-VELOCITY",
            xyz=Point3D(3.0, 2.0, 2.0),
        )
    )

    # Velocity at outlet
    sim.add(
        Device(
            id="U_OUTLET",
            quantity="U-VELOCITY",
            xyz=Point3D(5.5, 2.0, 2.0),
        )
    )

    # Pressure measurements
    sim.add(
        Device(
            id="P_INLET",
            quantity="PRESSURE",
            xyz=Point3D(0.5, 2.0, 2.0),
        )
    )

    sim.add(
        Device(
            id="P_MID",
            quantity="PRESSURE",
            xyz=Point3D(3.0, 2.0, 2.0),
        )
    )

    sim.add(
        Device(
            id="P_OUTLET",
            quantity="PRESSURE",
            xyz=Point3D(5.5, 2.0, 2.0),
        )
    )

    # Volume flow rates
    sim.add(
        Device(
            id="FLOW_INLET",
            quantity="VOLUME FLOW",
            xb=Bounds3D.of(0.0, 0.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    sim.add(
        Device(
            id="FLOW_OUTLET",
            quantity="VOLUME FLOW",
            xb=Bounds3D.of(6.0, 6.0, 0.0, 4.0, 0.0, 4.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "pressure")
