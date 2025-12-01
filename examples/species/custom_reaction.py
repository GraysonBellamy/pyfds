#!/usr/bin/env python3
"""
Custom Reaction Example
=======================

Demonstrates defining custom multi-step combustion reactions with
user-specified species and stoichiometric coefficients.

FDS Reference: Species/propane_flame_2reac.fds
https://github.com/firemodels/fds/blob/master/Verification/Species/propane_flame_2reac.fds

Key Namelists: REAC (SPEC_ID_NU, NU), SPEC (LUMPED_COMPONENT_ONLY)

Usage
-----
    python custom_reaction.py

Output
------
    fds/species/custom_reaction.fds
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
    """Create a simulation with custom two-step propane combustion."""

    # Create simulation
    sim = Simulation(chid="custom_reaction", title="Propane Flame with 2-Step Reaction")

    # Time
    sim.add(Time(t_end=30.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 40), xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 0.0, 4.0)))

    # Combustion settings
    sim.add(Combustion(suppression=False))

    # Define primitive species (LUMPED_COMPONENT_ONLY means they exist
    # only as components of lumped species, not as separate tracked species)
    sim.add(Species(id="NITROGEN", lumped_component_only=True))
    sim.add(Species(id="OXYGEN", lumped_component_only=True))
    sim.add(Species(id="CARBON MONOXIDE", lumped_component_only=True))
    sim.add(Species(id="WATER VAPOR", lumped_component_only=True))
    sim.add(Species(id="CARBON DIOXIDE", lumped_component_only=True))
    sim.add(Species(id="SOOT", lumped_component_only=True))

    # First reaction: propane + oxygen -> CO + water + soot
    # C3H8 + 2.5 O2 -> 3 CO + 4 H2O + 0.02 SOOT
    sim.add(
        Reaction(
            id="r1",
            fuel="PROPANE",
            spec_id_nu=["PROPANE", "OXYGEN", "CARBON MONOXIDE", "WATER VAPOR", "SOOT"],
            nu=[-1, -2.5, 3, 4, 0.02],
            heat_of_combustion=26000.0,  # kJ/kg
        )
    )

    # Second reaction: CO + oxygen -> CO2
    # CO + 0.5 O2 -> CO2
    sim.add(
        Reaction(
            id="r2",
            fuel="CARBON MONOXIDE",
            spec_id_nu=["CARBON MONOXIDE", "OXYGEN", "CARBON DIOXIDE"],
            nu=[-1, -0.5, 1],
            heat_of_combustion=10100.0,  # kJ/kg
        )
    )

    # Fire ramp: quick ramp-up then steady
    sim.add(Ramp(id="FIRE_RAMP", points=[(0.0, 0.0), (1.0, 1.0), (30.0, 1.0)]))

    # Burner surface
    sim.add(Surface(id="BURNER", hrrpua=1000.0, ramp_q="FIRE_RAMP"))

    # Burner vent at floor
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.1, 0.1, -0.1, 0.1, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Open boundaries on sides and top
    sim.add(Vent(xb=Bounds3D.of(-0.5, -0.5, -0.5, 0.5, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(0.5, 0.5, -0.5, 0.5, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, -0.5, -0.5, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, 0.5, 0.5, 0.0, 4.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 4.0, 4.0), surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "species")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
