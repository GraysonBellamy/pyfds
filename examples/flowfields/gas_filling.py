#!/usr/bin/env python3
"""
Gas Filling Example
===================

Demonstrates mass injection into a domain using SURF with MASS_FLUX
to simulate gas leaks or controlled gas releases.

FDS Reference: Flowfields/gas_filling.fds
https://github.com/firemodels/fds/blob/master/Verification/Flowfields/gas_filling.fds

Key Namelists: SURF (MASS_FLUX, SPEC_ID), SPEC, RAMP

Usage
-----
    python gas_filling.py

Output
------
    fds/flowfields/gas_filling.fds
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
    Misc,
    Ramp,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a gas filling simulation with controlled mass release."""

    # Create simulation
    sim = Simulation(
        chid="gas_filling",
        title="Fill Room with Hydrogen Gas",
    )

    # Time parameters - 5 minute simulation
    sim.add(Time(t_end=300.0))

    # Single mesh for room
    sim.add(
        Mesh(
            ijk=Grid3D.of(32, 32, 15),
            xb=Bounds3D.of(-3.2, 3.2, -3.2, 3.2, 0.0, 3.0),
        )
    )

    # Miscellaneous settings
    sim.add(
        Misc(
            stratification=False,  # No buoyancy-driven stratification
        )
    )

    # ==========================================================================
    # Species Definition
    # ==========================================================================

    # Hydrogen gas species
    sim.add(Species(id="HYDROGEN"))

    # ==========================================================================
    # Mass Flux Ramp
    # ==========================================================================

    # Leak starts at t=1s, runs until t=180s (3 minutes)
    sim.add(
        Ramp(
            id="LEAK_RAMP",
            points=[
                (0.0, 0.0),  # Off at start
                (1.0, 1.0),  # Full on at 1s
                (180.0, 1.0),  # Still on at 3min
                (181.0, 0.0),  # Off at 3min + 1s
            ],
        )
    )

    # ==========================================================================
    # Leak Surface with Mass Flux
    # ==========================================================================

    # Leak surface: 0.01667 kg/(m²·s) hydrogen
    # Total area = 1 m², time = 180s → 3 kg total
    sim.add(
        Surface(
            id="LEAK",
            spec_id="HYDROGEN",
            mass_flux_total=0.01667,  # kg/(m²·s)
            ramp_mf="LEAK_RAMP",
            color="RED",
        )
    )

    # ==========================================================================
    # Vents
    # ==========================================================================

    # Leak vent on floor (1 m² area)
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 0.0, 0.0),
            surf_id="LEAK",
        )
    )

    # ==========================================================================
    # Monitoring Devices
    # ==========================================================================

    # Hydrogen mass fraction at various heights
    for z in [0.5, 1.5, 2.5]:
        sim.add(
            Device(
                id=f"H2_Z{int(z * 10):02d}",
                xyz=Point3D(0.0, 0.0, z),
                quantity="MASS FRACTION",
                spec_id="HYDROGEN",
            )
        )

    # Total hydrogen mass in domain
    sim.add(
        Device(
            id="H2_MASS",
            xb=Bounds3D.of(-3.2, 3.2, -3.2, 3.2, 0.0, 3.0),
            quantity="MASS",
            spec_id="HYDROGEN",
        )
    )

    # Velocity near leak
    sim.add(
        Device(
            id="VEL_LEAK",
            xyz=Point3D(0.0, 0.0, 0.1),
            quantity="W-VELOCITY",
        )
    )

    # Write output
    output_path = write_example(sim, "flowfields")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
