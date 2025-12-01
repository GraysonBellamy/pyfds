#!/usr/bin/env python3
"""
Cylinder Geometry Example
=========================

Demonstrates the GEOM namelist for creating cylindrical geometry
using the built-in cylinder primitive.

Based on FDS Verification: geom_cylinder tests.

Key Namelists: GEOM (CYLINDER_ORIGIN, CYLINDER_AXIS, CYLINDER_RADIUS, CYLINDER_LENGTH)

Usage
-----
    python cylinder_geometry.py

Output
------
    fds/complex_geometry/cylinder_geometry.fds
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
    Surface,
    Time,
    Vent,
)


def main():
    """Create cylinder geometry example."""
    sim = Simulation(
        chid="cylinder_geometry",
        title="Cylindrical Geometry with GEOM",
    )

    # Time
    sim.add(Time(t_end=30.0))

    # Misc
    sim.add(Misc(radiation=False, stratification=False))

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 20, 20),
            xb=Bounds3D.of(0.0, 4.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Material for cylinder
    pipe_matl = Material(
        id="PIPE",
        density=2700.0,  # Aluminum
        conductivity=200.0,
        specific_heat=0.9,
    )
    sim.add(pipe_matl)

    # Surface for cylinder
    pipe_surf = Surface(
        id="PIPE_SURF",
        matl_id="PIPE",
        thickness=0.005,
        color="SILVER",
    )
    sim.add(pipe_surf)

    # Horizontal cylinder (pipe) using GEOM
    cylinder = Geometry(
        id="PIPE_1",
        surf_id="PIPE_SURF",
        cylinder_origin=(0.5, 1.0, 1.0),  # One end of cylinder
        cylinder_axis=(1.0, 0.0, 0.0),  # Axis direction (X-direction)
        cylinder_radius=0.2,  # Radius in meters
        cylinder_length=3.0,  # Length in meters
        n_levels=2,  # Subdivision level
    )
    sim.add(cylinder)

    # Velocity inlet on left
    inlet_surf = Surface(id="INLET", vel=-1.0, color="BLUE")
    sim.add(inlet_surf)
    sim.add(Vent(xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 0.5, 1.5), surf_id="INLET"))

    # Open boundaries
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Measurements ---

    # Velocity around cylinder
    sim.add(
        Device(
            id="U_UPSTREAM",
            quantity="U-VELOCITY",
            xyz=Point3D(0.3, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="U_ABOVE",
            quantity="U-VELOCITY",
            xyz=Point3D(2.0, 1.0, 1.4),
        )
    )

    sim.add(
        Device(
            id="U_DOWNSTREAM",
            quantity="U-VELOCITY",
            xyz=Point3D(3.7, 1.0, 1.0),
        )
    )

    # Pressure
    sim.add(
        Device(
            id="P_UPSTREAM",
            quantity="PRESSURE",
            xyz=Point3D(0.3, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="P_DOWNSTREAM",
            quantity="PRESSURE",
            xyz=Point3D(3.7, 1.0, 1.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "complex_geometry")
