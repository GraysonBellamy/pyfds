#!/usr/bin/env python3
"""
Mesh Alignment Example
======================

Demonstrates proper mesh alignment for multi-mesh
configurations with different resolutions.

Based on FDS Verification: mesh_align tests.

Key Namelists: MESH (multiple with alignment)

Usage
-----
    python mesh_alignment.py

Output
------
    fds/multi_mesh/mesh_alignment.fds
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
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create mesh alignment example."""
    sim = Simulation(
        chid="mesh_alignment",
        title="Mesh Alignment with Different Resolutions",
    )

    # Time
    sim.add(Time(t_end=30.0))

    # Misc
    sim.add(Misc(radiation=True))

    # Fine mesh around fire (0.05m cells)
    # Domain: 2m x 2m x 2m centered at origin
    sim.add(
        Mesh(
            id="FINE",
            ijk=Grid3D.of(40, 40, 40),  # 0.05m cells
            xb=Bounds3D.of(1.0, 3.0, 1.0, 3.0, 0.0, 2.0),
        )
    )

    # Coarse mesh extending domain (0.1m cells)
    # Left extension
    sim.add(
        Mesh(
            id="COARSE_LEFT",
            ijk=Grid3D.of(10, 20, 20),  # 0.1m cells
            xb=Bounds3D.of(0.0, 1.0, 1.0, 3.0, 0.0, 2.0),
        )
    )

    # Right extension
    sim.add(
        Mesh(
            id="COARSE_RIGHT",
            ijk=Grid3D.of(10, 20, 20),  # 0.1m cells
            xb=Bounds3D.of(3.0, 4.0, 1.0, 3.0, 0.0, 2.0),
        )
    )

    # Front extension
    sim.add(
        Mesh(
            id="COARSE_FRONT",
            ijk=Grid3D.of(40, 10, 20),  # Match fine mesh X resolution
            xb=Bounds3D.of(0.0, 4.0, 0.0, 1.0, 0.0, 2.0),
        )
    )

    # Back extension
    sim.add(
        Mesh(
            id="COARSE_BACK",
            ijk=Grid3D.of(40, 10, 20),  # Match fine mesh X resolution
            xb=Bounds3D.of(0.0, 4.0, 3.0, 4.0, 0.0, 2.0),
        )
    )

    # Combustion
    sim.add(Reaction(fuel="METHANE"))

    # Fire in fine mesh region
    fire_surf = Surface(id="FIRE", hrrpua=500.0, color="ORANGE")
    sim.add(fire_surf)
    sim.add(Vent(xb=Bounds3D.of(1.8, 2.2, 1.8, 2.2, 0.0, 0.0), surf_id="FIRE"))

    # Open top boundary
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Measurements ---

    # Temperature in fine mesh
    sim.add(
        Device(
            id="T_FINE_CENTER",
            quantity="TEMPERATURE",
            xyz=Point3D(2.0, 2.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="T_FINE_EDGE",
            quantity="TEMPERATURE",
            xyz=Point3D(1.1, 2.0, 1.0),
        )
    )

    # Temperature in coarse mesh
    sim.add(
        Device(
            id="T_COARSE_LEFT",
            quantity="TEMPERATURE",
            xyz=Point3D(0.5, 2.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="T_COARSE_RIGHT",
            quantity="TEMPERATURE",
            xyz=Point3D(3.5, 2.0, 1.0),
        )
    )

    # At mesh boundaries
    sim.add(
        Device(
            id="T_BOUNDARY_L",
            quantity="TEMPERATURE",
            xyz=Point3D(1.0, 2.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="T_BOUNDARY_R",
            quantity="TEMPERATURE",
            xyz=Point3D(3.0, 2.0, 1.0),
        )
    )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 4.0, 0.0, 4.0, 0.0, 2.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "multi_mesh")
