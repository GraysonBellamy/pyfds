#!/usr/bin/env python3
"""
Multi-Step Solid Reaction Example
=================================

Demonstrates a two-step solid material decomposition where:
Material A → Material B → Material C

This models consecutive pyrolysis reactions like wood decomposition
where virgin material becomes a char intermediate before final residue.

FDS Reference: Pyrolysis/two_step_solid_reaction.fds
https://github.com/firemodels/fds/blob/master/Verification/Pyrolysis/two_step_solid_reaction.fds

Key Namelists: MATL (A, E, NU_MATL, MATL_ID chain)

Usage
-----
    python multi_step_reaction.py

Output
------
    fds/pyrolysis/multi_step_reaction.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.models import PyrolysisProduct, PyrolysisReaction
from pyfds.core.namelists import (
    Material,
    Mesh,
    Misc,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with two-step solid decomposition."""

    # Create simulation
    sim = Simulation(chid="multi_step_reaction", title="Two-Step Solid Reaction Test")

    # Time
    sim.add(Time(t_end=50.0, dt=0.01))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(3, 3, 4), xb=Bounds3D.of(-0.15, 0.15, -0.15, 0.15, 0.0, 0.4)))

    # Solid phase only - reaction rate test
    sim.add(Misc(solid_phase_only=True))

    # Material A - initial material that decomposes to B
    # Using Arrhenius kinetics with A=0.389 and E=0 (constant rate)
    sim.add(
        Material(
            id="MAT_A",
            density=1.0,
            conductivity=0.1,
            specific_heat=1.0,
            reactions=[
                PyrolysisReaction(
                    a=0.389,  # Pre-exponential factor [1/s]
                    e=0.0,  # Zero activation energy = temperature-independent
                    heat_of_reaction=0.0,
                    products=[
                        PyrolysisProduct(matl_id="MAT_B", nu_matl=1.0),
                    ],
                )
            ],
        )
    )

    # Material B - intermediate that decomposes to C
    sim.add(
        Material(
            id="MAT_B",
            density=1.0,
            conductivity=0.1,
            specific_heat=1.0,
            reactions=[
                PyrolysisReaction(
                    a=0.262,  # Different rate constant
                    e=0.0,  # Zero activation energy = temperature-independent
                    heat_of_reaction=0.0,
                    products=[
                        PyrolysisProduct(matl_id="MAT_C", nu_matl=1.0),
                    ],
                )
            ],
        )
    )

    # Material C - final residue (inert)
    sim.add(
        Material(
            id="MAT_C",
            density=1.0,
            conductivity=0.1,
            specific_heat=1.0,
        )
    )

    # Test surface starting with Material A
    sim.add(
        Surface(
            id="TEST",
            matl_id=["MAT_A"],
            thickness=[0.001],
            stretch_factor=[1.0],
        )
    )

    # Test sample vent
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0),
            surf_id="TEST",
            color="GRAY",
        )
    )

    # Write output
    output_path = write_example(sim, "pyrolysis")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
