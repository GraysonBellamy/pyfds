#!/usr/bin/env python3
"""
Emissivity Effects Example
==========================

Demonstrates how surface emissivity affects radiative heat
transfer between surfaces. Uses different emissivity values
to show impact on heating rates.

Based on FDS Verification: emissivity and radiation tests.

Key Namelists: MATL (EMISSIVITY), SURF (EMISSIVITY)

Usage
-----
    python emissivity_effects.py

Output
------
    fds/radiation/emissivity_effects.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Material,
    Mesh,
    Misc,
    Obstruction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create emissivity effects example."""
    sim = Simulation(
        chid="emissivity_effects",
        title="Effect of Surface Emissivity",
    )

    # Time and misc
    sim.add(Time(t_end=120.0))
    sim.add(Misc(radiation=True, stratification=False))

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 20, 20),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Hot surface as radiation source
    hot_surf = Surface(
        id="HOT_PLATE",
        tmp_front=500.0,  # 500°C hot plate
        emissivity=0.95,  # Near blackbody
        color="RED",
    )
    sim.add(hot_surf)

    # Materials with different emissivity
    mat_high = Material(
        id="HIGH_E",
        density=2000.0,
        conductivity=1.0,
        specific_heat=1.0,
        emissivity=0.95,  # High emissivity (near blackbody)
    )
    sim.add(mat_high)

    mat_low = Material(
        id="LOW_E",
        density=2000.0,
        conductivity=1.0,
        specific_heat=1.0,
        emissivity=0.1,  # Low emissivity (reflective)
    )
    sim.add(mat_low)

    # Surfaces using the materials
    surf_high = Surface(
        id="HIGH_EMISSIVITY",
        matl_id="HIGH_E",
        thickness=0.01,
        color="GRAY 80",
    )
    sim.add(surf_high)

    surf_low = Surface(
        id="LOW_EMISSIVITY",
        matl_id="LOW_E",
        thickness=0.01,
        color="SILVER",
    )
    sim.add(surf_low)

    # Hot plate on left side
    sim.add(Vent(xb=Bounds3D.of(0.0, 0.0, 0.5, 1.5, 0.5, 1.5), surf_id="HOT_PLATE"))

    # Target plates at same distance from hot plate
    # High emissivity plate - will heat up faster
    sim.add(
        Obstruction(
            id="HIGH_E_PLATE",
            xb=Bounds3D.of(2.5, 2.55, 0.5, 1.0, 0.5, 1.5),
            surf_ids=("HIGH_EMISSIVITY", "HIGH_EMISSIVITY", "HIGH_EMISSIVITY"),
        )
    )

    # Low emissivity plate - will heat up slower (reflects radiation)
    sim.add(
        Obstruction(
            id="LOW_E_PLATE",
            xb=Bounds3D.of(2.5, 2.55, 1.0, 1.5, 0.5, 1.5),
            surf_ids=("LOW_EMISSIVITY", "LOW_EMISSIVITY", "LOW_EMISSIVITY"),
        )
    )

    # --- Measurements ---

    # Surface temperatures
    sim.add(
        Device(
            id="T_HIGH_E",
            quantity="WALL TEMPERATURE",
            xyz=Point3D(2.5, 0.75, 1.0),
            ior=-1,
        )
    )

    sim.add(
        Device(
            id="T_LOW_E",
            quantity="WALL TEMPERATURE",
            xyz=Point3D(2.5, 1.25, 1.0),
            ior=-1,
        )
    )

    # Gas temperatures for comparison
    sim.add(
        Device(
            id="GAS_T_1",
            quantity="TEMPERATURE",
            xyz=Point3D(1.5, 1.0, 1.0),
        )
    )

    sim.add(
        Device(
            id="GAS_T_2",
            quantity="TEMPERATURE",
            xyz=Point3D(2.0, 1.0, 1.0),
        )
    )

    # Hot plate temperature
    sim.add(
        Device(
            id="T_HOT",
            quantity="WALL TEMPERATURE",
            xyz=Point3D(0.0, 1.0, 1.0),
            ior=1,
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "radiation")
