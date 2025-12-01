#!/usr/bin/env python3
"""
MULT Obstruction Array Example
==============================

Demonstrates using the MULT namelist to create
arrays of obstructions efficiently.

Based on FDS Verification: mult tests.

Key Namelists: MULT, OBST (MULT_ID)

Usage
-----
    python mult_obstruction.py

Output
------
    fds/multi_mesh/mult_obstruction.fds
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
    Multiplier,
    Obstruction,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create MULT obstruction array example."""
    sim = Simulation(
        chid="mult_obstruction",
        title="MULT Namelist for Obstruction Arrays",
    )

    # Time
    sim.add(Time(t_end=60.0))

    # Misc
    sim.add(Misc(radiation=True))

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(50, 40, 20),
            xb=Bounds3D.of(0.0, 10.0, 0.0, 8.0, 0.0, 4.0),
        )
    )

    # Combustion
    sim.add(Reaction(fuel="PROPANE"))

    # --- MULT for creating a 4x3 array of columns ---
    # Creates columns at positions (1,1), (3,1), (5,1), (7,1),
    #                              (1,3), (3,3), (5,3), (7,3),
    #                              (1,5), (3,5), (5,5), (7,5)
    sim.add(
        Multiplier(
            id="COLUMN_ARRAY",
            dx=2.0,  # 2m spacing in X
            dy=2.0,  # 2m spacing in Y
            i_lower=0,
            i_upper=3,  # 4 columns in X
            j_lower=0,
            j_upper=2,  # 3 columns in Y
        )
    )

    # Column surface
    column_surf = Surface(id="COLUMN_SURF", color="GRAY 50")
    sim.add(column_surf)

    # Single column definition - MULT replicates it
    sim.add(
        Obstruction(
            id="COLUMN",
            xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 0.0, 3.0),
            surf_id="COLUMN_SURF",
            mult_id="COLUMN_ARRAY",  # Reference to MULT
        )
    )

    # --- MULT for ceiling beams ---
    sim.add(
        Multiplier(
            id="BEAM_ARRAY",
            dy=2.0,  # 2m spacing in Y
            j_lower=0,
            j_upper=3,  # 4 beams
        )
    )

    # Beam surface
    beam_surf = Surface(id="BEAM_SURF", color="BROWN")
    sim.add(beam_surf)

    # Single beam - replicated by MULT
    sim.add(
        Obstruction(
            id="BEAM",
            xb=Bounds3D.of(0.5, 9.5, 0.8, 1.2, 3.5, 3.8),
            surf_id="BEAM_SURF",
            mult_id="BEAM_ARRAY",
        )
    )

    # Fire source
    fire_surf = Surface(id="FIRE", hrrpua=500.0, color="ORANGE")
    sim.add(fire_surf)
    sim.add(Vent(xb=Bounds3D.of(4.0, 6.0, 3.5, 4.5, 0.0, 0.0), surf_id="FIRE"))

    # Open boundary
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Measurements ---

    # Temperature at various column locations
    sim.add(
        Device(
            id="T_COL_1_1",
            quantity="TEMPERATURE",
            xyz=Point3D(1.0, 1.0, 1.5),
        )
    )

    sim.add(
        Device(
            id="T_COL_3_3",
            quantity="TEMPERATURE",
            xyz=Point3D(5.0, 5.0, 1.5),
        )
    )

    # Ceiling temperature
    sim.add(
        Device(
            id="T_CEILING",
            quantity="TEMPERATURE",
            xyz=Point3D(5.0, 4.0, 3.9),
        )
    )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 10.0, 0.0, 8.0, 0.0, 4.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "multi_mesh")
