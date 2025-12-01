#!/usr/bin/env python3
"""
Statistics Output Example
=========================

Demonstrates device statistical output configuration including
averaging, max/min tracking, and time integration.

FDS Reference: N/A (general PyFDS output patterns)

Key Namelists: DEVC (STATISTICS, TIME_AVERAGED, TEMPORAL_STATISTIC)

Usage
-----
    python statistics_output.py

Output
------
    fds/output/statistics_output.fds
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
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation demonstrating statistical output options."""

    # Create simulation
    sim = Simulation(
        chid="statistics_output",
        title="Statistics Output Configuration",
    )

    # Time parameters
    sim.add(Time(t_end=120.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Simple fire
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="ORANGE"))

    sim.add(
        Vent(
            xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0.0, 0.0),
            surf_id="FIRE",
        )
    )

    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # ==========================================================================
    # Spatial Statistics (within a volume)
    # ==========================================================================

    # Volume-mean temperature in upper layer
    sim.add(
        Device(
            id="MEAN_TEMP_UPPER",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 1.5, 2.0),
            quantity="TEMPERATURE",
            statistics="VOLUME MEAN",
        )
    )

    # Maximum temperature in domain
    sim.add(
        Device(
            id="MAX_TEMP",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
            quantity="TEMPERATURE",
            statistics="MAX",
        )
    )

    # Minimum temperature (cold spots)
    sim.add(
        Device(
            id="MIN_TEMP",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
            quantity="TEMPERATURE",
            statistics="MIN",
        )
    )

    # Mean velocity magnitude in plume region
    sim.add(
        Device(
            id="MEAN_VEL_PLUME",
            xb=Bounds3D.of(0.5, 1.5, 0.5, 1.5, 0.5, 2.0),
            quantity="VELOCITY",
            statistics="VOLUME MEAN",
        )
    )

    # Mass flow through horizontal plane
    sim.add(
        Device(
            id="MASS_FLOW_UP",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 1.5, 1.5),
            quantity="MASS FLOW +",
            ior=3,  # +Z direction
        )
    )

    sim.add(
        Device(
            id="MASS_FLOW_DOWN",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 1.5, 1.5),
            quantity="MASS FLOW -",
            ior=3,
        )
    )

    # ==========================================================================
    # Time-averaged Measurements
    # ==========================================================================

    # Instantaneous temperature at ceiling
    sim.add(
        Device(
            id="TEMP_CEILING_INST",
            xyz=Point3D(1.0, 1.0, 1.9),
            quantity="TEMPERATURE",
        )
    )

    # Time-averaged temperature at same location
    sim.add(
        Device(
            id="TEMP_CEILING_AVG",
            xyz=Point3D(1.0, 1.0, 1.9),
            quantity="TEMPERATURE",
            time_averaged=True,
        )
    )

    # Time-averaged HRR
    sim.add(
        Device(
            id="HRR_TOTAL",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
            quantity="HRR",
        )
    )

    sim.add(
        Device(
            id="HRR_AVG",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
            quantity="HRR",
            time_averaged=True,
        )
    )

    # ==========================================================================
    # Integrated Quantities
    # ==========================================================================

    # Layer interface height
    sim.add(
        Device(
            id="LAYER_HEIGHT",
            xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 0.0, 2.0),
            quantity="LAYER HEIGHT",
        )
    )

    # Upper layer temperature
    sim.add(
        Device(
            id="UPPER_LAYER_TEMP",
            xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 0.0, 2.0),
            quantity="UPPER TEMPERATURE",
        )
    )

    # Lower layer temperature
    sim.add(
        Device(
            id="LOWER_LAYER_TEMP",
            xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 0.0, 2.0),
            quantity="LOWER TEMPERATURE",
        )
    )

    # ==========================================================================
    # Path/Line Measurements
    # ==========================================================================

    # Path-integrated visibility (beam obscuration)
    sim.add(
        Device(
            id="BEAM_OBSCURATION",
            xb=Bounds3D.of(0.1, 1.9, 1.0, 1.0, 1.5, 1.5),
            quantity="PATH OBSCURATION",
        )
    )

    # Write output
    output_path = write_example(sim, "output")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
