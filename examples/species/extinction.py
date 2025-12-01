#!/usr/bin/env python3
"""
Extinction Model Example
========================

Demonstrates using FDS extinction models to simulate flame extinction
behavior under conditions like oxygen depletion or low temperatures.

FDS Reference: Species/extinction_1.fds
https://github.com/firemodels/fds/blob/master/Verification/Species/extinction_1.fds

Key Namelists: COMB (EXTINCTION_MODEL, FIXED_MIX_TIME), INIT

Usage
-----
    python extinction.py

Output
------
    fds/species/extinction.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.enums import ExtinctionModel
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import (
    Combustion,
    Initialization,
    Mesh,
    Ramp,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation demonstrating extinction model behavior."""

    # Create simulation
    sim = Simulation(chid="extinction", title="Extinction Model Demonstration")

    # Time
    sim.add(Time(t_end=60.0))

    # Mesh - three regions for different temperatures
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0)))

    # Combustion with EXTINCTION 1 model and fixed mixing time
    # EXTINCTION 1 uses critical flame temperature criterion
    sim.add(
        Combustion(
            extinction_model=ExtinctionModel.EXTINCTION_1,
            fixed_mix_time=0.0025,  # Fixed mixing time for consistent behavior
        )
    )

    # Simple propane reaction
    sim.add(Reaction(fuel="PROPANE"))

    # Initialize different temperature regions
    # Region 1: Cold gas (will likely cause extinction)
    sim.add(
        Initialization(
            xb=Bounds3D.of(0.0, 0.5, 0.0, 2.0, 0.0, 2.0),
            temperature=10.0,  # 10°C - cold
        )
    )

    # Region 2: Normal ambient temperature
    sim.add(
        Initialization(
            xb=Bounds3D.of(0.5, 1.5, 0.0, 2.0, 0.0, 2.0),
            temperature=20.0,  # 20°C - ambient
        )
    )

    # Region 3: Pre-heated gas (easier to sustain combustion)
    sim.add(
        Initialization(
            xb=Bounds3D.of(1.5, 2.0, 0.0, 2.0, 0.0, 2.0),
            temperature=100.0,  # 100°C - warm
        )
    )

    # Fire ramp - steady fire
    sim.add(Ramp(id="FIRE", points=[(0.0, 1.0), (60.0, 1.0)]))

    # Burner surfaces for each region
    sim.add(Surface(id="BURNER", hrrpua=500.0, ramp_q="FIRE"))

    # Burner vents in each temperature region
    # Cold region burner
    sim.add(
        Vent(
            xb=Bounds3D.of(0.1, 0.4, 0.9, 1.1, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Ambient region burner
    sim.add(
        Vent(
            xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Warm region burner
    sim.add(
        Vent(
            xb=Bounds3D.of(1.6, 1.9, 0.9, 1.1, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Open boundaries on sides and top
    sim.add(Vent(xb=Bounds3D.of(0.0, 0.0, 0.0, 2.0, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(2.0, 2.0, 0.0, 2.0, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(0.0, 2.0, 0.0, 0.0, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(0.0, 2.0, 2.0, 2.0, 0.0, 2.0), surf_id="OPEN"))
    sim.add(Vent(xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 2.0, 2.0), surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "species")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
