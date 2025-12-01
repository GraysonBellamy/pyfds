#!/usr/bin/env python3
"""
Single Material Pyrolysis Example
=================================

Demonstrates a basic material with pyrolysis reaction using the
simplified kinetics model (REFERENCE_TEMPERATURE and REFERENCE_RATE).
This is similar to TGA (thermogravimetric analysis) testing.

FDS Reference: Pyrolysis/pyrolysis_1.fds
https://github.com/firemodels/fds/blob/master/Verification/Pyrolysis/pyrolysis_1.fds

Key Namelists: MATL (REFERENCE_TEMPERATURE, REFERENCE_RATE, NU_SPEC, SPEC_ID)

Usage
-----
    python single_material.py

Output
------
    fds/pyrolysis/single_material.fds
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
    Ramp,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with single material pyrolysis."""

    # Create simulation
    sim = Simulation(chid="single_material", title="TGA-style Pyrolysis Example")

    # Time - long duration for slow heating rate
    sim.add(Time(t_end=4800.0, dt=0.1))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(3, 1, 4), xb=Bounds3D.of(-2.0, 2.0, -0.5, 0.5, 0.0, 1.0)))

    # Solid phase only - no gas phase reactions
    sim.add(Misc(solid_phase_only=True))

    # Temperature ramp for TGA test: 5 K/min heating rate
    # Over 9600 seconds, temperature rises from ambient to ~820°C
    sim.add(Ramp(id="T_RAMP", points=[(0.0, 0.0), (9600.0, 1.0)]))

    # Define the gas species produced
    sim.add(Species(id="METHANE"))

    # Material with simplified pyrolysis kinetics
    # Using REFERENCE_TEMPERATURE and REFERENCE_RATE (Method 2)
    sim.add(
        Material(
            id="STUFF",
            density=500.0,
            conductivity=0.20,
            specific_heat=1.0,
            emissivity=1.0,
            reactions=[
                PyrolysisReaction(
                    reference_temperature=300.0,  # Peak reaction at 300°C
                    reference_rate=0.002,  # Normalized mass loss rate
                    heating_rate=5.0,  # TGA heating rate [K/min]
                    heat_of_reaction=1000.0,  # kJ/kg
                    products=[
                        PyrolysisProduct(spec_id="METHANE", nu_spec=1.0),
                    ],
                )
            ],
        )
    )

    # Sample surface - small thickness for quick thermal response
    sim.add(
        Surface(
            id="SAMPLE",
            matl_id=["STUFF"],
            thickness=[0.00001],  # 10 micron sample
            backing="INSULATED",
            color="RED",
            tmp_gas_front=820.0,  # Final temperature
            ramp_tmp_gas_front="T_RAMP",
            heat_transfer_coefficient=1000.0,  # High HTC for fast heating
        )
    )

    # Sample vent
    sim.add(
        Vent(
            xb=Bounds3D.of(-1.0, 1.0, -0.5, 0.5, 0.0, 0.0),
            surf_id="SAMPLE",
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
