#!/usr/bin/env python3
"""
Pool Fire (Water Evaporation) Example
=====================================

A liquid pool demonstrating evaporating liquid using the liquid fuel model.
Uses material properties for liquid phase behavior with boiling temperature.

FDS Reference: Pyrolysis/water_pool.fds
https://github.com/firemodels/fds/blob/master/Verification/Pyrolysis/water_pool.fds

Key Namelists: MATL (liquid with BOILING_TEMPERATURE), SURF, SPEC

Usage
-----
    python pool_fire.py

Output
------
    fds/fires/pool_fire.fds

Note
----
This example uses water evaporation from the FDS verification suite.
For combustible liquid fuels, add a REAC namelist with appropriate fuel.
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
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a water pool evaporation simulation (from FDS verification)."""

    # Create simulation
    sim = Simulation(chid="water_pool", title="Water Pool Evaporation")

    # Time - long simulation for steady-state evaporation
    sim.add(Time(t_end=3600.0, dt=0.4))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(50, 10, 10), xb=Bounds3D.of(0.0, 5.0, 0.0, 1.0, 0.0, 1.0)))

    # Misc - ambient temperature and humidity
    sim.add(Misc(tmpa=32.0, humidity=0.0))

    # Species - water vapor with initial mass fraction
    sim.add(Species(id="WATER VAPOR", mass_fraction_0=0.008374))

    # Water material with liquid evaporation model
    # Uses PyrolysisReaction to specify the evaporated species
    sim.add(
        Material(
            id="WATER",
            density=1000.0,
            conductivity=0.6,
            specific_heat=4.18,
            absorption_coefficient=140.0,
            boiling_temperature=100.0,
            adjust_h=False,
            reactions=[
                PyrolysisReaction(
                    heat_of_reaction=2260.0,  # kJ/kg for water evaporation
                    products=[
                        PyrolysisProduct(spec_id="WATER VAPOR", nu_spec=1.0),
                    ],
                )
            ],
        )
    )

    # Pool surface
    sim.add(
        Surface(
            id="POOL",
            matl_id="WATER",
            thickness=0.05,  # 5 cm pool depth
            tmp_inner=20.0,  # Initial temperature
            tmp_gas_back=20.0,
            backing="VOID",
            color="BLUE",
        )
    )

    # Inlet flow (blowing air)
    sim.add(Surface(id="BLOW", vel=-0.15))

    # Boundary vents
    sim.add(Vent(mb="XMIN", surf_id="BLOW"))
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))

    # Pool vent
    sim.add(Vent(xb=Bounds3D.of(1.0, 2.2, 0.0, 1.0, 0.0, 0.0), surf_id="POOL"))

    # Write output
    output_path = write_example(sim, "fires")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
