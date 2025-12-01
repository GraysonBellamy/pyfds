#!/usr/bin/env python3
"""
Simple Fire Example
===================

This example builds on the minimal simulation by adding a fire source.
It demonstrates the essential namelists for a basic fire:
- SURF: Surface with heat release rate (HRRPUA)
- OBST: Obstruction to place the fire
- REAC: Combustion reaction

FDS Reference: Fires/simple_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/simple_test.fds

Key Namelists: SURF, OBST, REAC

Usage
-----
    python 02_simple_fire.py

Output
------
    fds/getting_started/simple_fire.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Mesh, Obstruction, Reaction, Surface, Time


def main():
    """Create a simple fire simulation."""

    # Create simulation
    sim = Simulation(chid="simple_fire", title="Simple Fire Demonstration")

    # Time: 60 second simulation
    sim.add(Time(t_end=60.0))

    # Mesh: 3.6m x 2.4m x 2.4m room with 10cm cells
    # Based on simple_test.fds from FDS Verification
    sim.add(Mesh(ijk=Grid3D.of(36, 24, 24), xb=Bounds3D.of(0.0, 3.6, 0.0, 2.4, 0.0, 2.4)))

    # Reaction: Propane combustion with small soot yield
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))

    # Surface: Fire surface with 1000 kW/m² heat release rate
    sim.add(
        Surface(
            id="BURNER",
            hrrpua=1000.0,  # kW/m²
            color="RED",
        )
    )

    # Obstruction: Fire source (0.4m x 0.4m burner, 0.2m high)
    # Fire is on top surface (top, sides, bottom)
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.0, 0.4, 1.0, 1.4, 0.0, 0.2),
            surf_ids=("BURNER", "INERT", "INERT"),
        )
    )

    # Write output
    output_path = write_example(sim, "getting_started")
    print(f"Created: {output_path}")

    # Show generated content
    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
