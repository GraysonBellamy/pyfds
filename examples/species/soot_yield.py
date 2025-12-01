#!/usr/bin/env python3
"""
Soot Yield and Deposition Example
=================================

Demonstrates soot production from combustion and soot deposition
on surfaces using gravitational and thermophoretic mechanisms.

FDS Reference: Aerosols/propane_flame_deposition.fds
https://github.com/firemodels/fds/blob/master/Verification/Aerosols/propane_flame_deposition.fds

Key Namelists: REAC (SOOT_YIELD), SPEC (AEROSOL), MISC (deposition)

Usage
-----
    python soot_yield.py

Output
------
    fds/species/soot_yield.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import (
    Combustion,
    Mesh,
    Misc,
    Ramp,
    Reaction,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with soot production and deposition."""

    # Create simulation
    sim = Simulation(chid="soot_yield", title="Propane Flame with Soot Deposition")

    # Time
    sim.add(Time(t_end=60.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 40), xb=Bounds3D.of(-1.0, 1.0, -1.0, 1.0, 0.0, 4.0)))

    # Enable deposition mechanisms in MISC
    sim.add(
        Misc(
            deposition=True,
            gravitational_deposition=True,
            thermophoretic_deposition=True,
            gravitational_settling=True,
        )
    )

    # Combustion settings
    sim.add(Combustion(suppression=False))

    # Define SOOT as an aerosol species
    # Note: When SOOT_YIELD is used, FDS automatically creates SOOT species
    # but we can define it explicitly to set aerosol properties
    sim.add(
        Species(
            id="SOOT",
            aerosol=True,
            mean_diameter=1.0e-6,  # 1 micron
            density_solid=1800.0,  # kg/m³ - typical soot density
        )
    )

    # Propane reaction with soot yield
    # SOOT_YIELD represents mass of soot per mass of fuel consumed
    sim.add(
        Reaction(
            fuel="PROPANE",
            soot_yield=0.03,  # 3% soot yield (high for visibility)
            co_yield=0.01,  # 1% CO yield
        )
    )

    # Fire ramp: ramp up, steady, ramp down
    sim.add(
        Ramp(
            id="FIRE",
            points=[(0.0, 0.0), (10.0, 1.0), (40.0, 1.0), (50.0, 0.0), (60.0, 0.0)],
        )
    )

    # Burner surface - 100 kW total
    sim.add(Surface(id="BURNER", hrrpua=1000.0, ramp_q="FIRE"))

    # Burner vent (0.1 m x 0.1 m)
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Open boundaries on sides and top
    sim.add(Vent(xb=Bounds3D.of(-1.0, -1.0, -1.0, 1.0, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(1.0, 1.0, -1.0, 1.0, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-1.0, 1.0, -1.0, -1.0, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-1.0, 1.0, 1.0, 1.0, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-1.0, 1.0, -1.0, 1.0, 4.0, 4.0), surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "species")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
