#!/usr/bin/env python3
"""
Heat of Combustion Example
==========================

Demonstrates custom heat of combustion in reactions.
Shows how to specify fuel properties for accurate energy release.

FDS Reference: Fires/HoC_Ideal.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/HoC_Ideal.fds

Key Namelists: REAC (HEAT_OF_COMBUSTION)

Usage
-----
    python heat_of_combustion.py

Output
------
    fds/fires/heat_of_combustion.fds
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
    """Create a simulation demonstrating heat of combustion."""

    # Create simulation
    sim = Simulation(chid="heat_of_combustion", title="Custom Heat of Combustion Test")

    # Time
    sim.add(Time(t_end=60.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 40), xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 2.0)))

    # Misc settings for ideal behavior
    sim.add(
        Misc(
            suppression=False,  # No suppression
            dns=True,  # DNS mode for ideal combustion
        )
    )

    # Reaction with custom heat of combustion
    # Methane: CH4 + 2O2 -> CO2 + 2H2O
    # Standard heat of combustion: ~50,000 kJ/kg
    # We can customize this value
    sim.add(
        Reaction(
            fuel="METHANE",
            heat_of_combustion=50000.0,  # kJ/kg
            soot_yield=0.0,  # Ideal combustion - no soot
            co_yield=0.0,  # Ideal combustion - no CO
            ideal=True,
        )
    )

    # Simple burner
    sim.add(
        Surface(
            id="BURNER",
            hrrpua=500.0,  # kW/m²
            color="ORANGE",
        )
    )

    # Burner obstruction
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.3, 0.7, 0.3, 0.7, 0.0, 0.1),
            surf_ids=("BURNER", "INERT", "INERT"),
        )
    )

    # Open top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # Temperature devices
    for z in [0.5, 1.0, 1.5]:
        sim.add(
            Device(id=f"TEMP_{int(z * 100)}cm", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, z))
        )

    # HRR device for verification
    sim.add(
        Device(
            id="HRR_CHECK",
            quantity="HRRPUV",
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 2.0),
            spatial_statistic="VOLUME INTEGRAL",
        )
    )

    # Write output
    output_path = write_example(sim, "fires")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
