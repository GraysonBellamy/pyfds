#!/usr/bin/env python3
"""
Radiative Fraction Example
==========================

Demonstrates how the radiative fraction in combustion
affects heat flux to surroundings. Different fuels have
different characteristic radiative fractions.

Based on FDS Verification: radiation and combustion tests.

Key Namelists: REAC (RADIATIVE_FRACTION, SOOT_YIELD)

Usage
-----
    python radiative_fraction.py

Output
------
    fds/radiation/radiative_fraction.fds
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
    Ramp,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create radiative fraction example."""
    sim = Simulation(
        chid="radiative_fraction",
        title="Effect of Radiative Fraction",
    )

    # Time and misc
    sim.add(Time(t_end=60.0))
    sim.add(Misc(radiation=True))

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 30, 25),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 2.5),
        )
    )

    # Ramp for time-varying radiative fraction
    # Simulates smoke buildup increasing radiation
    chi_r_ramp = Ramp(
        id="CHI_R_RAMP",
        points=[
            (0.0, 0.2),  # Clean burning initially
            (20.0, 0.2),
            (30.0, 0.35),  # Transition to smokier
            (60.0, 0.35),
        ],
    )
    sim.add(chi_r_ramp)

    # Fuel with custom radiative fraction
    # Typical values: methane ~0.15-0.20, propane ~0.25-0.30,
    # sooty fuels (polystyrene) ~0.35-0.45
    sim.add(
        Reaction(
            id="SOOTY_FUEL",
            fuel="PROPANE",  # Base fuel chemistry
            radiative_fraction=0.35,  # Higher radiation (sooty)
            soot_yield=0.05,  # 5% soot
        )
    )

    # Fire surface with moderate HRR
    fire_surf = Surface(id="FIRE", hrrpua=400.0, color="ORANGE")
    sim.add(fire_surf)

    # Fire on floor
    sim.add(Vent(xb=Bounds3D.of(1.2, 1.8, 1.2, 1.8, 0.0, 0.0), surf_id="FIRE"))

    # Open top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Heat Flux Measurements ---

    # Radial array at different distances
    for dist, label in [(0.5, "NEAR"), (1.0, "MID"), (1.5, "FAR")]:
        # Radiative flux
        sim.add(
            Device(
                id=f"RAD_HF_{label}",
                quantity="RADIATIVE HEAT FLUX",
                xyz=Point3D(1.5 + dist, 1.5, 1.0),
                ior=-1,
            )
        )

        # Total gauge heat flux
        sim.add(
            Device(
                id=f"GAUGE_HF_{label}",
                quantity="GAUGE HEAT FLUX",
                xyz=Point3D(1.5 + dist, 1.5, 1.0),
                ior=-1,
            )
        )

        # Gas temperature
        sim.add(
            Device(
                id=f"GAS_T_{label}",
                quantity="TEMPERATURE",
                xyz=Point3D(1.5 + dist, 1.5, 1.0),
            )
        )

    # Vertical array above fire
    for height in [0.5, 1.0, 1.5, 2.0]:
        sim.add(
            Device(
                id=f"PLUME_T_{int(height * 10):02d}",
                quantity="TEMPERATURE",
                xyz=Point3D(1.5, 1.5, height),
            )
        )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 2.5),
        )
    )

    # Radiative fraction output (if supported)
    sim.add(
        Device(
            id="Q_RAD",
            quantity="RADIATIVE HEAT FLUX GAS",
            xyz=Point3D(1.5, 1.5, 0.5),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "radiation")
