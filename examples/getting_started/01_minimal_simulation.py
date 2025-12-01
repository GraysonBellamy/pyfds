#!/usr/bin/env python3
"""
Minimal Simulation Example
==========================

This is the absolute minimum working FDS simulation using PyFDS.
It demonstrates the three essential namelists required for any FDS run:
- HEAD: Simulation identification
- TIME: Duration control
- MESH: Computational domain

FDS Reference: N/A (tutorial example)
Key Namelists: HEAD, TIME, MESH

Usage
-----
    python 01_minimal_simulation.py

Output
------
    fds/getting_started/minimal_simulation.fds
"""

import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Mesh, Time


def main():
    """Create a minimal FDS simulation."""

    # Create simulation with just a CHID (required)
    # This automatically creates the HEAD namelist
    sim = Simulation(chid="minimal_simulation", title="Absolute Minimum FDS Simulation")

    # Add time control - 10 seconds is enough for a minimal test
    sim.add(Time(t_end=10.0))

    # Add computational domain
    # A 1m x 1m x 1m cube with 10cm cells (10x10x10 grid)
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

    # Write to output
    output_path = write_example(sim, "getting_started")
    print(f"Created: {output_path}")

    # Show the generated FDS content
    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
