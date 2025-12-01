#!/usr/bin/env python3
"""
Particle Drag Example
=====================

Demonstrates different drag coefficients for Lagrangian particles in FDS.
Uses static particles with user-specified drag coefficients to verify
the drag force calculation.

FDS Reference: Sprinklers_and_Sprays/sphere_drag_1.fds, terminal_velocity_dt_1_0.fds
https://github.com/firemodels/fds/blob/master/Verification/Sprinklers_and_Sprays/sphere_drag_1.fds

Key Namelists: PART (DRAG_COEFFICIENT), SURF (GEOMETRY=SPHERICAL)

Usage
-----
    python particle_drag.py

Output
------
    fds/sprinklers/particle_drag.fds
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
    Material,
    Mesh,
    Particle,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a particle drag verification simulation."""

    # Create simulation
    sim = Simulation(chid="particle_drag", title="Particle Drag Verification")

    # Time parameters
    sim.add(Time(t_end=10.0))

    # Single mesh with inlet flow
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 20, 20),
            xb=Bounds3D.of(0.0, 2.0, 0.0, 1.0, 0.0, 1.0),
        )
    )

    # Material for solid particles
    sim.add(
        Material(
            id="PARTICLE",
            density=1000.0,
            conductivity=1.0,
            specific_heat=1.0,
        )
    )

    # Spherical particle surface
    sim.add(
        Surface(
            id="PARTICLE_SURF",
            thickness=0.005,  # 5 mm radius
            geometry="SPHERICAL",
            matl_id=["PARTICLE"],
        )
    )

    # Inlet surface with velocity
    sim.add(Surface(id="INLET", vel=-2.0))

    # Free-slip surface for side walls
    sim.add(Surface(id="SLIP", free_slip=True))

    # Multiple particle classes with different drag coefficients
    drag_values = [5.0, 10.0, 20.0, 50.0]
    z_positions = [0.0, 0.25, 0.5, 0.75]  # Different z-layers

    for cd in drag_values:
        # Particle class with user-specified drag coefficient
        sim.add(
            Particle(
                id=f"DRAG_{int(cd)}",
                surf_id="PARTICLE_SURF",
                drag_coefficient=[cd],  # User-specified drag coefficient
                static=True,  # Particles don't move
            )
        )

    # Initialize particles at different positions
    for cd, z_pos in zip(drag_values, z_positions, strict=True):
        sim.add(
            Initialization(
                part_id=f"DRAG_{int(cd)}",
                n_particles_per_cell=10,
                cell_centered=True,
                xb=Bounds3D.of(1.0, 1.02, 0.0, 1.0, z_pos, z_pos + 0.2),
            )
        )

    # Boundary conditions
    sim.add(Vent(mb="XMIN", surf_id="INLET"))  # Inlet with velocity
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))  # Outlet
    sim.add(Vent(mb="YMIN", surf_id="SLIP"))  # Free-slip sides
    sim.add(Vent(mb="YMAX", surf_id="SLIP"))
    sim.add(Vent(mb="ZMIN", surf_id="SLIP"))
    sim.add(Vent(mb="ZMAX", surf_id="SLIP"))

    # Pressure measurement devices upstream of particles
    for cd, z_pos in zip(drag_values, z_positions, strict=True):
        sim.add(
            Device(
                id=f"P_CD{int(cd)}",
                xyz=Point3D.of(0.5, 0.5, z_pos + 0.1),
                quantity="PRESSURE",
            )
        )

    # Write output
    output_path = write_example(sim, "sprinklers")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
