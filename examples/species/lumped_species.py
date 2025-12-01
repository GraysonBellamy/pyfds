#!/usr/bin/env python3
"""
Lumped Species Example
======================

Demonstrates using lumped species to reduce computational cost by
grouping multiple primitive species into single tracked species.

FDS Reference: Species/methane_flame_lumped.fds
https://github.com/firemodels/fds/blob/master/Verification/Species/methane_flame_lumped.fds

Key Namelists: SPEC (MASS_FRACTION, SPEC_ID, LUMPED_COMPONENT_ONLY)

Usage
-----
    python lumped_species.py

Output
------
    fds/species/lumped_species.fds
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
    Ramp,
    Reaction,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with lumped air and products species."""

    # Create simulation
    sim = Simulation(chid="lumped_species", title="Methane Flame with Lumped Species")

    # Time
    sim.add(Time(t_end=10.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 20), xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 0.0, 2.0)))

    # Combustion settings
    sim.add(Combustion(suppression=False))

    # Define primitive species as lumped components
    # These won't be tracked individually, only as part of lumped species
    sim.add(Species(id="NITROGEN", lumped_component_only=True))
    sim.add(Species(id="OXYGEN", lumped_component_only=True))
    sim.add(Species(id="WATER VAPOR", lumped_component_only=True))
    sim.add(Species(id="CARBON DIOXIDE", lumped_component_only=True))

    # Lumped AIR species (mass fractions: 76.7% N2, 23.3% O2)
    sim.add(
        Species(
            id="AIR",
            spec_id=["NITROGEN", "OXYGEN"],
            mass_fraction=[0.767, 0.233],
            background=True,  # This is the ambient gas
        )
    )

    # Lumped PRODUCTS species (combustion products)
    # Stoichiometrically determined from CH4 + 2*O2 -> CO2 + 2*H2O
    # with N2 from air included
    sim.add(
        Species(
            id="PRODUCTS",
            spec_id=["NITROGEN", "CARBON DIOXIDE", "WATER VAPOR"],
            mass_fraction=[0.726, 0.158, 0.116],
        )
    )

    # Reaction: methane + air -> products
    # Using stoichiometric coefficients for lumped species
    sim.add(
        Reaction(
            fuel="METHANE",
            spec_id_nu=["METHANE", "AIR", "PRODUCTS"],
            nu=[-1.0, -17.4, 18.4],  # mass-based stoichiometry
            heat_of_combustion=50000.0,  # kJ/kg
        )
    )

    # Fire ramp
    sim.add(Ramp(id="FIRE_RAMP", points=[(0.0, 0.0), (2.0, 1.0), (10.0, 1.0)]))

    # Burner surface - 100 kW total
    sim.add(Surface(id="BURNER", hrrpua=1000.0, ramp_q="FIRE_RAMP"))

    # Burner vent at floor (0.1 m x 0.1 m = 0.01 m²)
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Open boundaries
    sim.add(Vent(xb=Bounds3D.of(-0.5, -0.5, -0.5, 0.5, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(0.5, 0.5, -0.5, 0.5, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, -0.5, -0.5, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, 0.5, 0.5, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 2.0, 2.0), surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "species")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
