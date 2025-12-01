#!/usr/bin/env python3
"""
Burn Away Example
=================

Demonstrates material consumption during burning using the BURN_AWAY feature.
The obstruction is consumed as fuel evaporates.

FDS Reference: Fires/box_burn_away1.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/box_burn_away1.fds

Key Namelists: SURF (BURN_AWAY)

Usage
-----
    python burn_away.py

Output
------
    fds/fires/burn_away.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import (
    Device,
    Material,
    Mesh,
    Multiplier,
    Obstruction,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a burn away simulation."""

    # Create simulation
    sim = Simulation(chid="burn_away", title="BURN_AWAY Feature Test")

    # Time
    sim.add(Time(t_end=30.0, dt=0.01))

    # Multi-mesh for better resolution
    sim.add(Multiplier(id="mesh", dx=1.0, dy=1.0, i_upper=1, j_upper=1))

    sim.add(
        Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(-0.3, 0.7, -0.4, 0.6, 0.0, 1.0),
            mult_id="mesh",
        )
    )

    # Species for fuel gas
    sim.add(Species(id="METHANE"))

    # Foam material that decomposes to methane
    # Box mass: 0.4³ m³ x 20 kg/m³ = 1.28 kg
    sim.add(
        Material(
            id="FOAM",
            heat_of_reaction=800.0,
            conductivity=0.2,
            specific_heat=1.0,
            density=20.0,
            nu_spec=1.0,
            spec_id="METHANE",
            reference_temperature=200.0,
        )
    )

    # Surface with BURN_AWAY enabled
    sim.add(Surface(id="FOAM SLAB", color="TOMATO", burn_away=True, variable_thickness=True))

    # Foam box obstruction
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0.30, 0.70, 0.30, 0.70, 0.30, 0.70),
            surf_id="FOAM SLAB",
            bulk_density=20.0,
            matl_id="FOAM",
        )
    )

    # Hot surfaces to provide heat for decomposition
    sim.add(Surface(id="HOT", tmp_front=1100.0, color="RED"))

    # Hot walls on all sides
    sim.add(Vent(xb=Bounds3D.of(-0.3, -0.3, -0.4, 1.6, 0.0, 1.0), surf_id="HOT"))
    sim.add(Vent(xb=Bounds3D.of(1.7, 1.7, -0.4, 1.6, 0.0, 1.0), surf_id="HOT"))
    sim.add(Vent(xb=Bounds3D.of(-0.3, 1.7, -0.4, -0.4, 0.0, 1.0), surf_id="HOT"))
    sim.add(Vent(xb=Bounds3D.of(-0.3, 1.7, 1.6, 1.6, 0.0, 1.0), surf_id="HOT"))
    sim.add(Vent(xb=Bounds3D.of(-0.3, 1.7, -0.4, 1.6, 0.0, 0.0), surf_id="HOT"))
    sim.add(Vent(xb=Bounds3D.of(-0.3, 1.7, -0.4, 1.6, 1.0, 1.0), surf_id="HOT"))

    # Mass tracking device
    sim.add(
        Device(
            id="Mass_fuel",
            quantity="DENSITY",
            spec_id="METHANE",
            xb=Bounds3D.of(-0.3, 1.7, -0.4, 1.6, 0.0, 1.0),
            spatial_statistic="VOLUME INTEGRAL",
        )
    )

    # Write output
    output_path = write_example(sim, "fires")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
