#!/usr/bin/env python3
"""
Circular Burner Example
=======================

A circular vent fire source using RADIUS parameter.
Demonstrates non-rectangular fire geometry.

FDS Reference: Fires/circular_burner.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/circular_burner.fds

Key Namelists: VENT (RADIUS, XYZ)

Usage
-----
    python circular_burner.py

Output
------
    fds/fires/circular_burner.fds
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
    Multiplier,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a circular burner simulation."""

    # Create simulation
    sim = Simulation(chid="circular_burner", title="Circular Burner Mass Flow Test")

    # Time
    sim.add(Time(t_end=20.0))

    # Multi-mesh setup using MULT (2x2x2 = 8 meshes)
    sim.add(Multiplier(id="mesh_array", dx=1.0, dy=1.0, dz=1.0, i_upper=1, j_upper=1, k_upper=1))

    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(-1.0, 0.0, -1.0, 0.0, 0.0, 1.0),
            mult_id="mesh_array",
        )
    )

    # Reaction
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.015))

    # Burner surface with mass flux
    # Using MASS_FLUX instead of HRRPUA for this example
    # mass_flux and spec_id are parallel arrays
    sim.add(
        Surface(
            id="BURNER",
            mass_flux=[0.02],  # kg/m²/s
            spec_id=["PROPANE"],  # Species for the mass flux
            tau_mf=[0.01],  # Time constant for ramp-up
            color="BLUE",
        )
    )

    # Circular vent at z=0 plane
    # Center at origin, radius 0.5m, spreading from center
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.6, 0.6, -0.6, 0.6, 0.0, 0.0),
            xyz=Point3D.of(0.0, 0.0, 0.0),
            radius=0.5,
            spread_rate=0.05,  # Fire grows outward
            surf_id="BURNER",
        )
    )

    # Open boundaries
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Temperature device above burner
    sim.add(Device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=Point3D.of(0.0, 0.0, 0.5)))

    # Write output
    output_path = write_example(sim, "fires")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
