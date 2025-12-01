#!/usr/bin/env python3
"""
Time-Varying Fire Example
=========================

This example demonstrates how to use RAMP to create time-varying
heat release rates. RAMPs are essential for modeling realistic fire
growth and decay patterns.

FDS Reference: Fires/spray_burner.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/spray_burner.fds

Key Namelists: RAMP

Usage
-----
    python 05_time_varying_fire.py

Output
------
    fds/getting_started/time_varying_fire.fds
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
    Obstruction,
    Ramp,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a fire simulation with time-varying heat release rate."""

    # Create simulation
    sim = Simulation(chid="time_varying_fire", title="Fire with RAMP-Controlled HRR")

    # Time: 300 seconds to see full fire evolution
    sim.add(Time(t_end=300.0))

    # Mesh: Room domain
    sim.add(Mesh(ijk=Grid3D.of(30, 30, 30), xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0)))

    # Reaction
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01, co_yield=0.005))

    # =========================================================================
    # RAMP - Time-varying heat release rate
    # =========================================================================
    # T-squared fire growth with steady-state and decay
    # Points: (time, fraction of peak HRR)
    sim.add(
        Ramp(
            id="FIRE_RAMP",
            points=[
                (0.0, 0.0),  # Start at zero
                (30.0, 0.01),  # 1% at 30s (slow start)
                (60.0, 0.04),  # 4% at 60s
                (90.0, 0.09),  # 9% at 90s
                (120.0, 0.16),  # 16% at 120s
                (150.0, 0.25),  # 25% at 150s
                (180.0, 0.64),  # 64% at 180s
                (210.0, 1.0),  # Peak at 210s
                (240.0, 1.0),  # Maintain peak
                (270.0, 0.5),  # Begin decay
                (300.0, 0.1),  # Low intensity
            ],
        )
    )

    # Alternative: Using t-squared formula
    # Fast fire: t² = t²/(150²) = t²/22500, reaches 1 at t=150s
    # sim.add(Ramp(
    #     id="TSQUARED_FAST",
    #     points=[(t, (t/150.0)**2) for t in range(0, 160, 10)]
    # ))

    # =========================================================================
    # SURFACE WITH RAMP
    # =========================================================================
    # Fire surface with RAMP-controlled HRR
    sim.add(
        Surface(
            id="FIRE",
            hrrpua=1000.0,  # Peak: 1000 kW/m²
            ramp_q="FIRE_RAMP",  # Apply the ramp
            color="ORANGE",
        )
    )

    # =========================================================================
    # GEOMETRY
    # =========================================================================
    # Fire source (1m x 1m burner)
    sim.add(
        Obstruction(
            xb=Bounds3D.of(1.0, 2.0, 1.0, 2.0, 0.0, 0.1),
            surf_ids=("FIRE", "INERT", "INERT"),
        )
    )

    # Open top for exhaust
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # =========================================================================
    # DEVICES
    # =========================================================================
    # Temperature above fire
    sim.add(Device(id="TEMP_PLUME", quantity="TEMPERATURE", xyz=Point3D.of(1.5, 1.5, 2.5)))

    # Temperature at ceiling
    sim.add(Device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=Point3D.of(1.5, 1.5, 2.95)))

    # Heat flux at floor
    sim.add(
        Device(id="HF_FLOOR", quantity="RADIATIVE HEAT FLUX", xyz=Point3D.of(2.5, 2.5, 0.0), ior=3)
    )

    # Write output
    output_path = write_example(sim, "getting_started")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
