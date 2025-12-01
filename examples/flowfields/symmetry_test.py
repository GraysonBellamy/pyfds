#!/usr/bin/env python3
"""
Symmetry Test Example
=====================

Demonstrates symmetric flow configuration using MIRROR boundary
conditions and zero-gravity settings for testing flow solver symmetry.

FDS Reference: Flowfields/symmetry_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Flowfields/symmetry_test.fds

Key Namelists: MISC (GVEC, NOISE), SURF (VEL), VENT (SURF_ID='MIRROR')

Usage
-----
    python symmetry_test.py

Output
------
    fds/flowfields/symmetry_test.fds
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
    Obstruction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a symmetric flow test configuration."""

    # Create simulation
    sim = Simulation(
        chid="symmetry_test",
        title="Test Symmetry of Flow Solver",
    )

    # Time parameters
    sim.add(Time(t_end=50.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # ==========================================================================
    # Miscellaneous - Zero gravity, no noise for clean symmetry
    # ==========================================================================

    sim.add(
        Misc(
            stratification=False,
            noise=False,  # No velocity perturbations
            gvec=(0.0, 0.0, 0.0),  # Zero gravity
            radiation=False,  # No radiation
        )
    )

    # ==========================================================================
    # Surfaces
    # ==========================================================================

    # Blower surface with fixed velocity
    sim.add(
        Surface(
            id="BLOW",
            vel=-0.05,  # Blowing into domain
            color="RED",
            h_fixed=0.0,  # No heat transfer
        )
    )

    # Wall surface with no heat transfer
    sim.add(
        Surface(
            id="WALL",
            h_fixed=0.0,
            default=True,
        )
    )

    # ==========================================================================
    # Central obstructions (symmetric arrangement)
    # ==========================================================================

    # Central block
    sim.add(
        Obstruction(
            id="CENTER",
            xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0.4, 0.6),
            color="BLUE",
        )
    )

    # Corner blocks (8 corners of inner cube)
    corners = [
        (0.3, 0.4, 0.3, 0.4, 0.3, 0.4),
        (0.6, 0.7, 0.3, 0.4, 0.3, 0.4),
        (0.6, 0.7, 0.6, 0.7, 0.3, 0.4),
        (0.3, 0.4, 0.6, 0.7, 0.3, 0.4),
        (0.3, 0.4, 0.3, 0.4, 0.6, 0.7),
        (0.6, 0.7, 0.3, 0.4, 0.6, 0.7),
        (0.6, 0.7, 0.6, 0.7, 0.6, 0.7),
        (0.3, 0.4, 0.6, 0.7, 0.6, 0.7),
    ]

    for i, corner in enumerate(corners, 1):
        sim.add(
            Obstruction(
                id=f"CORNER_{i}",
                xb=Bounds3D.of(*corner),
                color="BLUE",
            )
        )

    # ==========================================================================
    # Blower vents (symmetric on all 6 faces)
    # ==========================================================================

    # X-direction faces
    sim.add(
        Vent(
            id="BLOW_XMIN",
            xb=Bounds3D.of(0.0, 0.0, 0.4, 0.6, 0.4, 0.6),
            surf_id="BLOW",
        )
    )
    sim.add(
        Vent(
            id="BLOW_XMAX",
            xb=Bounds3D.of(1.0, 1.0, 0.4, 0.6, 0.4, 0.6),
            surf_id="BLOW",
        )
    )

    # Y-direction faces
    sim.add(
        Vent(
            id="BLOW_YMIN",
            xb=Bounds3D.of(0.4, 0.6, 0.0, 0.0, 0.4, 0.6),
            surf_id="BLOW",
        )
    )
    sim.add(
        Vent(
            id="BLOW_YMAX",
            xb=Bounds3D.of(0.4, 0.6, 1.0, 1.0, 0.4, 0.6),
            surf_id="BLOW",
        )
    )

    # Z-direction faces
    sim.add(
        Vent(
            id="BLOW_ZMIN",
            xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0.0, 0.0),
            surf_id="BLOW",
        )
    )
    sim.add(
        Vent(
            id="BLOW_ZMAX",
            xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 1.0, 1.0),
            surf_id="BLOW",
        )
    )

    # ==========================================================================
    # Monitoring Devices (symmetric pairs)
    # ==========================================================================

    # U-velocity at symmetric points
    sim.add(
        Device(
            id="U_1",
            xyz=Point3D(0.25, 0.25, 0.25),
            quantity="U-VELOCITY",
        )
    )
    sim.add(
        Device(
            id="U_2",
            xyz=Point3D(0.65, 0.75, 0.75),
            quantity="U-VELOCITY",
        )
    )

    # V-velocity at symmetric points
    sim.add(
        Device(
            id="V_1",
            xyz=Point3D(0.25, 0.25, 0.25),
            quantity="V-VELOCITY",
        )
    )
    sim.add(
        Device(
            id="V_2",
            xyz=Point3D(0.75, 0.65, 0.75),
            quantity="V-VELOCITY",
        )
    )

    # W-velocity at symmetric points
    sim.add(
        Device(
            id="W_1",
            xyz=Point3D(0.25, 0.25, 0.25),
            quantity="W-VELOCITY",
        )
    )
    sim.add(
        Device(
            id="W_2",
            xyz=Point3D(0.75, 0.75, 0.65),
            quantity="W-VELOCITY",
        )
    )

    # Pressure at center
    sim.add(
        Device(
            id="P_CENTER",
            xyz=Point3D(0.5, 0.5, 0.5),
            quantity="PRESSURE",
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
