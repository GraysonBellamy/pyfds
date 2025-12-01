#!/usr/bin/env python3
"""
Ambient Pressure Example
========================

Demonstrates setting ambient pressure conditions
and their effects on flow behavior.

Based on FDS Verification: pressure tests.

Key Namelists: MISC (P_INF)

Usage
-----
    python pressure_bc.py

Output
------
    fds/pressure/pressure_bc.fds
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
    Surface,
    Time,
    Vent,
)


def main():
    """Create ambient pressure boundary condition example."""
    sim = Simulation(
        chid="pressure_bc",
        title="Ambient Pressure Effects",
    )

    # Time
    sim.add(Time(t_end=60.0))

    # Misc - different ambient pressure (high altitude simulation)
    # P_INF = 80000 Pa (approximately 2000m elevation)
    sim.add(
        Misc(
            p_inf=80000.0,  # Reduced ambient pressure
            tmpa=15.0,  # 15°C ambient temperature
            stratification=False,
            radiation=False,
        )
    )

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 20, 20),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Velocity inlet - fixed velocity
    inlet_surf = Surface(
        id="INLET",
        vel=-1.0,  # 1 m/s into domain
        color="BLUE",
    )
    sim.add(inlet_surf)

    # Inlet vent
    sim.add(
        Vent(
            id="INLET_VENT",
            xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 0.5, 1.5),
            surf_id="INLET",
        )
    )

    # Open outlet
    sim.add(
        Vent(
            id="OUTLET",
            mb="XMAX",
            surf_id="OPEN",
        )
    )

    # --- Measurements ---

    # Pressure at various locations
    sim.add(
        Device(
            id="P_INLET",
            quantity="PRESSURE",
            xyz=Point3D(0.3, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="P_MID",
            quantity="PRESSURE",
            xyz=Point3D(1.5, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="P_OUTLET",
            quantity="PRESSURE",
            xyz=Point3D(2.7, 1.0, 1.0),
        )
    )

    # Velocity measurements
    sim.add(
        Device(
            id="U_INLET",
            quantity="U-VELOCITY",
            xyz=Point3D(0.3, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="U_MID",
            quantity="U-VELOCITY",
            xyz=Point3D(1.5, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="U_OUTLET",
            quantity="U-VELOCITY",
            xyz=Point3D(2.7, 1.0, 1.0),
        )
    )

    # Density (affected by pressure)
    sim.add(
        Device(
            id="RHO_MID",
            quantity="DENSITY",
            xyz=Point3D(1.5, 1.0, 1.0),
        )
    )

    # Temperature
    sim.add(
        Device(
            id="T_MID",
            quantity="TEMPERATURE",
            xyz=Point3D(1.5, 1.0, 1.0),
        )
    )

    # Mass flow rate
    sim.add(
        Device(
            id="MDOT_INLET",
            quantity="MASS FLOW",
            xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 0.5, 1.5),
        )
    )

    sim.add(
        Device(
            id="MDOT_OUTLET",
            quantity="MASS FLOW",
            xb=Bounds3D.of(3.0, 3.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "pressure")
