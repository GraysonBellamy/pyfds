#!/usr/bin/env python3
"""
Predefined Fuel Example
=======================

Demonstrates using a predefined fuel (methane) with simple chemistry.
FDS automatically handles the stoichiometry and species tracking.

FDS Reference: Species/methane_flame_simple.fds
https://github.com/firemodels/fds/blob/master/Verification/Species/methane_flame_simple.fds

Key Namelists: REAC (FUEL)

Usage
-----
    python predefined_fuel.py

Output
------
    fds/species/predefined_fuel.fds
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
    Obstruction,
    Ramp,
    Reaction,
    Surface,
    Time,
)


def main():
    """Create a simulation using predefined methane fuel."""

    # Create simulation
    sim = Simulation(chid="predefined_fuel", title="Methane Flame with Simple Chemistry")

    # Time
    sim.add(Time(t_end=10.0, dt=0.01))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 20), xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 2.0)))

    # Combustion - disable suppression for verification
    sim.add(Combustion(suppression=False))

    # Reaction using predefined fuel - FDS knows the stoichiometry
    # CO_YIELD specifies carbon monoxide production
    sim.add(Reaction(fuel="METHANE", co_yield=0.1))

    # Fire ramp: full power for 5 seconds, then off
    sim.add(Ramp(id="HRR", points=[(0.0, 1.0), (5.0, 1.0), (5.01, 0.0), (10.0, 0.0)]))

    # Burner surface with 625 kW/m² (100 kW total for 0.4x0.4 m burner)
    sim.add(Surface(id="BURNER", hrrpua=625.0, ramp_q="HRR"))

    # Burner obstruction
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.3, 0.7, 0.3, 0.7, 0.0, 0.1),
            surf_ids=("BURNER", "INERT", "INERT"),
        )
    )

    # Write output
    output_path = write_example(sim, "species")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
