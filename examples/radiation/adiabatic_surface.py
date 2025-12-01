#!/usr/bin/env python3
"""
Adiabatic Surface Temperature Example
======================================

Demonstrates the adiabatic surface technique for measuring
thermal radiation intensity - the ADIABATIC surface produces
a temperature that balances convective and radiative heat transfer.

Based on FDS Verification: adiabatic_surface_temperature tests.

Key Namelists: SURF (ADIABATIC), DEVC (WALL TEMPERATURE)

Usage
-----
    python adiabatic_surface.py

Output
------
    fds/radiation/adiabatic_surface.fds
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
    Obstruction,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create adiabatic surface temperature example."""
    sim = Simulation(
        chid="adiabatic_surface",
        title="Adiabatic Surface Temperature",
    )

    # Time and misc
    sim.add(Time(t_end=60.0))
    sim.add(Misc(radiation=True))

    # Simple mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(20, 20, 20),
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    # Heat source - simple fire
    sim.add(Reaction(fuel="METHANE"))

    fire_surf = Surface(id="BURNER", hrrpua=500.0, color="ORANGE")
    sim.add(fire_surf)

    # Burner on floor
    sim.add(Vent(xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0.0, 0.0), surf_id="BURNER"))

    # Open top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # Adiabatic surface - temperature sensor
    # This surface has zero net heat flux, so temperature
    # represents balance between radiation and convection
    adiabatic_surf = Surface(
        id="ADIABATIC_GAUGE",
        adiabatic=True,
        color="RED",
    )
    sim.add(adiabatic_surf)

    # Regular wall for comparison
    regular_surf = Surface(
        id="REGULAR_WALL",
        default=True,
        color="GRAY 60",
    )
    sim.add(regular_surf)

    # Target plate with adiabatic surface - acts as radiation sensor
    sim.add(
        Obstruction(
            id="ADIABATIC_PLATE",
            xb=Bounds3D.of(0.3, 0.4, 0.9, 1.1, 0.5, 0.55),
            surf_ids=("ADIABATIC_GAUGE", "ADIABATIC_GAUGE", "ADIABATIC_GAUGE"),
            color="RED",
        )
    )

    # Reference plate with regular surface
    sim.add(
        Obstruction(
            id="REGULAR_PLATE",
            xb=Bounds3D.of(1.6, 1.7, 0.9, 1.1, 0.5, 0.55),
            color="GRAY 60",
        )
    )

    # --- Measurements ---

    # Temperature on adiabatic surface
    sim.add(
        Device(
            id="AST_FRONT",
            quantity="WALL TEMPERATURE",
            xyz=Point3D(0.3, 1.0, 0.525),
            ior=1,  # Face normal in +X direction
        )
    )

    # Temperature on regular surface for comparison
    sim.add(
        Device(
            id="WALL_T_REGULAR",
            quantity="WALL TEMPERATURE",
            xyz=Point3D(1.7, 1.0, 0.525),
            ior=-1,  # Face normal in -X direction
        )
    )

    # Gas temperature at plate locations
    sim.add(
        Device(
            id="GAS_T_ADIABATIC",
            quantity="TEMPERATURE",
            xyz=Point3D(0.2, 1.0, 0.525),
        )
    )

    sim.add(
        Device(
            id="GAS_T_REGULAR",
            quantity="TEMPERATURE",
            xyz=Point3D(1.8, 1.0, 0.525),
        )
    )

    # HRR for verification
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "radiation")
