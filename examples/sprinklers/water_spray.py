#!/usr/bin/env python3
"""
Water Spray Evaporation Example
===============================

Demonstrates water droplet evaporation in a heated environment.
This tests mass and energy conservation with the Lagrangian particle model.

FDS Reference: Sprinklers_and_Sprays/water_evaporation_1.fds
https://github.com/firemodels/fds/blob/master/Verification/Sprinklers_and_Sprays/water_evaporation_1.fds

Key Namelists: PART (STATIC, DIAMETER), INIT (PART_ID, MASS_PER_VOLUME)

Usage
-----
    python water_spray.py

Output
------
    fds/sprinklers/water_spray.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import (
    Initialization,
    Mesh,
    Misc,
    Particle,
    Species,
    Surface,
    Time,
)


def main():
    """Create a simulation testing water droplet evaporation."""

    # Create simulation
    sim = Simulation(chid="water_spray", title="Water Evaporation Test")

    # Time
    sim.add(Time(t_end=10.0, dt=0.01))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)))

    # Hot environment with no gravity (droplets float)
    sim.add(
        Misc(
            tmpa=200.0,  # 200°C ambient temperature
            gvec=(0.0, 0.0, 0.0),  # No gravity
            stratification=False,
            humidity=0.0,
            y_co2_infty=0.0,
        )
    )

    # Water vapor species
    sim.add(Species(id="WATER VAPOR"))

    # Static water droplets (don't move)
    sim.add(
        Particle(
            id="WATER_DROPS",
            spec_id="WATER VAPOR",
            static=True,  # Particles don't move
            diameter=0.0002,  # 200 micron droplets
            initial_temperature=20.0,  # 20°C initial temp
            monodisperse=True,  # All same size
            sampling_factor=5,
        )
    )

    # Adiabatic wall surface
    sim.add(
        Surface(
            id="WALL",
            adiabatic=True,
            color="SILVER",
            default=True,
        )
    )

    # Initialize droplets throughout domain
    sim.add(
        Initialization(
            part_id="WATER_DROPS",
            mass_per_volume=0.01,  # kg/m³
            n_particles_per_cell=10,
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
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
