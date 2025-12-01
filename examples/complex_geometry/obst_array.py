#!/usr/bin/env python3
"""
Obstruction Array Example
=========================

Demonstrates creating arrays of obstructions
for complex configurations like furniture, storage, etc.

Based on FDS Verification: obst_array tests.

Key Namelists: OBST (multiple)

Usage
-----
    python obst_array.py

Output
------
    fds/complex_geometry/obst_array.fds
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
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create obstruction array example."""
    sim = Simulation(
        chid="obst_array",
        title="Array of Obstructions",
    )

    # Time
    sim.add(Time(t_end=60.0))

    # Misc
    sim.add(Misc(radiation=True))

    # Domain - warehouse-like space
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 30, 20),
            xb=Bounds3D.of(0.0, 8.0, 0.0, 6.0, 0.0, 4.0),
        )
    )

    # Combustion
    sim.add(Reaction(fuel="PROPANE"))

    # Surface for storage racks
    rack_surf = Surface(id="RACK", color="BROWN")
    sim.add(rack_surf)

    # Create array of storage racks (3x2 array)
    rack_width = 1.0
    rack_depth = 0.4
    rack_height = 3.0
    x_spacing = 2.0
    y_spacing = 2.5

    for i in range(3):  # 3 columns
        for j in range(2):  # 2 rows
            x_start = 1.0 + i * x_spacing
            y_start = 1.0 + j * y_spacing

            sim.add(
                Obstruction(
                    id=f"RACK_{i}_{j}",
                    xb=Bounds3D.of(
                        x_start,
                        x_start + rack_width,
                        y_start,
                        y_start + rack_depth,
                        0.0,
                        rack_height,
                    ),
                    surf_id="RACK",
                )
            )

    # Fire in corner
    fire_surf = Surface(id="FIRE", hrrpua=500.0, color="ORANGE")
    sim.add(fire_surf)
    sim.add(Vent(xb=Bounds3D.of(0.2, 0.8, 0.2, 0.8, 0.0, 0.0), surf_id="FIRE"))

    # Open boundary at top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Measurements ---

    # Temperature at aisle intersections
    for i in range(4):  # Between racks
        for j in range(2):
            x_pos = 0.5 + i * x_spacing
            y_pos = 1.0 + j * y_spacing + rack_depth / 2

            sim.add(
                Device(
                    id=f"T_AISLE_{i}_{j}",
                    quantity="TEMPERATURE",
                    xyz=Point3D(x_pos, y_pos, 2.0),
                )
            )

    # Ceiling temperatures
    for x in [2.0, 4.0, 6.0]:
        for y in [2.0, 4.0]:
            sim.add(
                Device(
                    id=f"T_CEIL_{int(x)}_{int(y)}",
                    quantity="TEMPERATURE",
                    xyz=Point3D(x, y, 3.8),
                )
            )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 8.0, 0.0, 6.0, 0.0, 4.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "complex_geometry")
