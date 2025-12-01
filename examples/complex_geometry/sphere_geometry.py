#!/usr/bin/env python3
"""
Sphere Geometry Example
=======================

Demonstrates the GEOM namelist for creating spherical geometry
using the built-in sphere primitive.

Based on FDS Verification: geom_sphere tests.

Key Namelists: GEOM (SPHERE_ORIGIN, SPHERE_RADIUS, N_LEVELS)

Usage
-----
    python sphere_geometry.py

Output
------
    fds/complex_geometry/sphere_geometry.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Geometry,
    Material,
    Mesh,
    Misc,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create sphere geometry example."""
    sim = Simulation(
        chid="sphere_geometry",
        title="Spherical Geometry with GEOM",
    )

    # Time
    sim.add(Time(t_end=30.0))

    # Misc
    sim.add(Misc(radiation=True))

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 30, 30),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
        )
    )

    # Material for sphere
    steel_matl = Material(
        id="STEEL",
        density=7850.0,
        conductivity=45.0,
        specific_heat=0.46,
        emissivity=0.9,
    )
    sim.add(steel_matl)

    # Surface for sphere
    steel_surf = Surface(
        id="STEEL_SURF",
        matl_id="STEEL",
        thickness=0.01,
        color="GRAY 50",
    )
    sim.add(steel_surf)

    # Sphere geometry using GEOM
    sphere = Geometry(
        id="BALL",
        surf_id="STEEL_SURF",
        sphere_origin=(1.5, 1.5, 1.5),  # Center of sphere
        sphere_radius=0.5,  # Radius in meters
        n_levels=3,  # Subdivision level (higher = smoother)
    )
    sim.add(sphere)

    # Combustion for heat source
    sim.add(Reaction(fuel="METHANE"))

    # Fire below sphere
    fire_surf = Surface(id="BURNER", hrrpua=300.0, color="ORANGE")
    sim.add(fire_surf)
    sim.add(Vent(xb=Bounds3D.of(1.0, 2.0, 1.0, 2.0, 0.0, 0.0), surf_id="BURNER"))

    # Open top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Measurements ---

    # Temperature around sphere
    sim.add(
        Device(
            id="T_BELOW",
            quantity="TEMPERATURE",
            xyz=Point3D(1.5, 1.5, 0.8),
        )
    )

    sim.add(
        Device(
            id="T_ABOVE",
            quantity="TEMPERATURE",
            xyz=Point3D(1.5, 1.5, 2.2),
        )
    )

    sim.add(
        Device(
            id="T_SIDE",
            quantity="TEMPERATURE",
            xyz=Point3D(2.2, 1.5, 1.5),
        )
    )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "complex_geometry")
