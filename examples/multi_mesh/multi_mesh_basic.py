#!/usr/bin/env python3
"""
Multi-Mesh Basic Example
========================

Demonstrates basic multi-mesh configuration for
domain decomposition and parallel execution.

Based on FDS Verification: multi_mesh tests.

Key Namelists: MESH (multiple)

Usage
-----
    python multi_mesh_basic.py

Output
------
    fds/multi_mesh/multi_mesh_basic.fds
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
    """Create multi-mesh basic example."""
    sim = Simulation(
        chid="multi_mesh_basic",
        title="Basic Multi-Mesh Configuration",
    )

    # Time
    sim.add(Time(t_end=30.0))

    # Misc
    sim.add(Misc(radiation=True))

    # Multiple meshes - 2x2 array for parallel execution
    # Each mesh covers a quadrant of the domain
    # Meshes must align at boundaries for proper communication

    # Mesh 1 - lower left quadrant
    sim.add(
        Mesh(
            id="MESH_1",
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Mesh 2 - lower right quadrant
    sim.add(
        Mesh(
            id="MESH_2",
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(2.0, 4.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Mesh 3 - upper left quadrant
    sim.add(
        Mesh(
            id="MESH_3",
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 2.0, 2.0, 4.0, 0.0, 2.0),
        )
    )

    # Mesh 4 - upper right quadrant
    sim.add(
        Mesh(
            id="MESH_4",
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(2.0, 4.0, 2.0, 4.0, 0.0, 2.0),
        )
    )

    # Combustion
    sim.add(Reaction(fuel="PROPANE"))

    # Fire in center (spans multiple meshes)
    fire_surf = Surface(id="FIRE", hrrpua=500.0, color="ORANGE")
    sim.add(fire_surf)
    sim.add(Vent(xb=Bounds3D.of(1.5, 2.5, 1.5, 2.5, 0.0, 0.0), surf_id="FIRE"))

    # Open top boundary
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Measurements ---
    # Devices in each mesh

    # Mesh 1
    sim.add(
        Device(
            id="T_MESH1",
            quantity="TEMPERATURE",
            xyz=Point3D(1.0, 1.0, 1.0),
        )
    )

    # Mesh 2
    sim.add(
        Device(
            id="T_MESH2",
            quantity="TEMPERATURE",
            xyz=Point3D(3.0, 1.0, 1.0),
        )
    )

    # Mesh 3
    sim.add(
        Device(
            id="T_MESH3",
            quantity="TEMPERATURE",
            xyz=Point3D(1.0, 3.0, 1.0),
        )
    )

    # Mesh 4
    sim.add(
        Device(
            id="T_MESH4",
            quantity="TEMPERATURE",
            xyz=Point3D(3.0, 3.0, 1.0),
        )
    )

    # Center (at mesh boundary)
    sim.add(
        Device(
            id="T_CENTER",
            quantity="TEMPERATURE",
            xyz=Point3D(2.0, 2.0, 1.0),
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
