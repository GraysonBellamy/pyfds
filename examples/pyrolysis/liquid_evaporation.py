#!/usr/bin/env python3
"""
Liquid Evaporation Example
==========================

Demonstrates liquid pool evaporation using the FDS boiling temperature
model. This simulates liquids like methanol, ethanol, or other fuels
evaporating under radiant heat exposure.

FDS Reference: Pyrolysis/methanol_evaporation.fds
https://github.com/firemodels/fds/blob/master/Verification/Pyrolysis/methanol_evaporation.fds

Key Namelists: MATL (BOILING_TEMPERATURE, NU_SPEC, SPEC_ID)

Usage
-----
    python liquid_evaporation.py

Output
------
    fds/pyrolysis/liquid_evaporation.fds
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
    Obstruction,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with liquid pool evaporation."""

    # Create simulation
    sim = Simulation(chid="liquid_evaporation", title="Methanol Pool Evaporation")

    # Time
    sim.add(Time(t_end=360.0))

    # Single mesh covering the domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(24, 24, 12),
            xb=Bounds3D.of(-0.6, 0.6, -0.6, 0.6, 0.0, 0.6),
        )
    )

    # Define methanol species
    sim.add(Species(id="METHANOL"))

    # Methanol liquid material with boiling temperature
    sim.add(
        Material(
            id="METHANOL LIQUID",
            density=796.0,
            conductivity=100.0,  # High conductivity for well-mixed liquid
            specific_heat=2.48,
            emissivity=1.0,
            absorption_coefficient=1500.0,
            boiling_temperature=64.65,  # Methanol boiling point [°C]
            reactions=[
                PyrolysisReaction(
                    heat_of_reaction=1099.0,  # Heat of vaporization [kJ/kg]
                    products=[
                        PyrolysisProduct(spec_id="METHANOL", nu_spec=1.0),
                    ],
                )
            ],
        )
    )

    # Steel material for pan walls
    sim.add(
        Material(
            id="STEEL",
            density=7850.0,
            conductivity=45.8,
            specific_heat=0.46,
        )
    )

    # Methanol pool surface
    sim.add(
        Surface(
            id="METHANOL POOL",
            matl_id=["METHANOL LIQUID"],
            thickness=[0.05],  # 5 cm pool depth
            backing="INSULATED",
            emissivity=1.0,
            color="YELLOW",
            external_flux=20.0,  # External radiant flux [kW/m²]
        )
    )

    # Steel sheet surface for pan walls
    sim.add(
        Surface(
            id="STEEL SHEET",
            matl_id=["STEEL"],
            thickness=[0.003],  # 3 mm steel
            backing="EXPOSED",
            color="BLACK",
        )
    )

    # Open boundaries
    sim.add(Vent(mb="XMIN", surf_id="OPEN"))
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))
    sim.add(Vent(mb="YMIN", surf_id="OPEN"))
    sim.add(Vent(mb="YMAX", surf_id="OPEN"))
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # Pool and pan obstructions
    # Pool surface on top
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.50, 0.50, -0.50, 0.50, 0.00, 0.05),
            surf_ids=("METHANOL POOL", "STEEL SHEET", "STEEL SHEET"),
        )
    )

    # Pan walls
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.50, -0.50, -0.50, 0.50, 0.00, 0.10),
            surf_id="STEEL SHEET",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.50, 0.50, -0.50, 0.50, 0.00, 0.10),
            surf_id="STEEL SHEET",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.50, 0.50, -0.50, -0.50, 0.00, 0.10),
            surf_id="STEEL SHEET",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(-0.50, 0.50, 0.50, 0.50, 0.00, 0.10),
            surf_id="STEEL SHEET",
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
