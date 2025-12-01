#!/usr/bin/env python3
"""
Velocity Boundary Condition Example
====================================

Demonstrates specified velocity inlet using SURF VEL parameter
for creating controlled flow conditions.

FDS Reference: Flowfields/velocity_bc_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Flowfields/velocity_bc_test.fds

Key Namelists: SURF (VEL, VEL_T, PROFILE, RAMP_V)

Usage
-----
    python velocity_bc.py

Output
------
    fds/flowfields/velocity_bc.fds
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
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with various velocity boundary conditions."""

    # Create simulation
    sim = Simulation(
        chid="velocity_bc",
        title="Velocity Boundary Condition Test",
    )

    # Time parameters
    sim.add(Time(t_end=30.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 20, 20),
            xb=Bounds3D.of(0.0, 4.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Miscellaneous
    sim.add(
        Misc(
            stratification=False,  # Isothermal-like behavior
        )
    )

    # ==========================================================================
    # Velocity Ramps
    # ==========================================================================

    # Ramp-up velocity profile
    sim.add(
        Ramp(
            id="VEL_RAMP",
            points=[
                (0.0, 0.0),  # Zero at start
                (5.0, 1.0),  # Full velocity at 5s
                (25.0, 1.0),  # Maintain
                (30.0, 0.0),  # Ramp down
            ],
        )
    )

    # Pulsing velocity
    sim.add(
        Ramp(
            id="VEL_PULSE",
            points=[
                (0.0, 0.0),
                (2.0, 1.0),
                (4.0, 0.0),
                (6.0, 1.0),
                (8.0, 0.0),
                (10.0, 1.0),
                (12.0, 0.0),
            ],
        )
    )

    # ==========================================================================
    # Inlet Surfaces with Different Velocity Types
    # ==========================================================================

    # Constant velocity inlet (bottom)
    sim.add(
        Surface(
            id="INLET_CONST",
            vel=-1.0,  # Negative = into domain, 1 m/s
            color="BLUE",
        )
    )

    # Ramped velocity inlet (middle)
    sim.add(
        Surface(
            id="INLET_RAMP",
            vel=-2.0,  # Peak velocity 2 m/s
            ramp_v="VEL_RAMP",
            color="GREEN",
        )
    )

    # Pulsing velocity inlet (top)
    sim.add(
        Surface(
            id="INLET_PULSE",
            vel=-1.5,  # Peak velocity 1.5 m/s
            ramp_v="VEL_PULSE",
            color="RED",
        )
    )

    # ==========================================================================
    # Vents
    # ==========================================================================

    # Constant inlet (lower third)
    sim.add(
        Vent(
            id="IN_CONST",
            xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 0.1, 0.6),
            surf_id="INLET_CONST",
        )
    )

    # Ramped inlet (middle third)
    sim.add(
        Vent(
            id="IN_RAMP",
            xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 0.7, 1.2),
            surf_id="INLET_RAMP",
        )
    )

    # Pulsing inlet (upper third)
    sim.add(
        Vent(
            id="IN_PULSE",
            xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 1.3, 1.8),
            surf_id="INLET_PULSE",
        )
    )

    # Outlet (open boundary)
    sim.add(
        Vent(
            mb="XMAX",
            surf_id="OPEN",
        )
    )

    # ==========================================================================
    # Monitoring Devices
    # ==========================================================================

    # Velocity near each inlet
    sim.add(
        Device(
            id="U_CONST",
            xyz=Point3D(0.1, 1.0, 0.35),
            quantity="U-VELOCITY",
        )
    )

    sim.add(
        Device(
            id="U_RAMP",
            xyz=Point3D(0.1, 1.0, 0.95),
            quantity="U-VELOCITY",
        )
    )

    sim.add(
        Device(
            id="U_PULSE",
            xyz=Point3D(0.1, 1.0, 1.55),
            quantity="U-VELOCITY",
        )
    )

    # Velocity downstream
    sim.add(
        Device(
            id="U_MID_CONST",
            xyz=Point3D(2.0, 1.0, 0.35),
            quantity="U-VELOCITY",
        )
    )

    sim.add(
        Device(
            id="U_MID_RAMP",
            xyz=Point3D(2.0, 1.0, 0.95),
            quantity="U-VELOCITY",
        )
    )

    sim.add(
        Device(
            id="U_MID_PULSE",
            xyz=Point3D(2.0, 1.0, 1.55),
            quantity="U-VELOCITY",
        )
    )

    # Mass flow through domain
    sim.add(
        Device(
            id="MASS_FLOW_IN",
            xb=Bounds3D.of(0.5, 0.5, 0.0, 2.0, 0.0, 2.0),
            quantity="MASS FLOW +",
            ior=1,
        )
    )

    sim.add(
        Device(
            id="MASS_FLOW_OUT",
            xb=Bounds3D.of(3.5, 3.5, 0.0, 2.0, 0.0, 2.0),
            quantity="MASS FLOW +",
            ior=1,
        )
    )

    # Write output
    output_path = write_example(sim, "flowfields")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
