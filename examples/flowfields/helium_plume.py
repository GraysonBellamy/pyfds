#!/usr/bin/env python3
"""
Helium Plume Example
====================

Demonstrates buoyant gas release using helium to create
a density-driven plume.

FDS Reference: Flowfields/helium_2d_isothermal.fds
https://github.com/firemodels/fds/blob/master/Verification/Flowfields/helium_2d_isothermal.fds

Key Namelists: SPEC (density), SURF (MASS_FLUX, SPEC_ID)

Usage
-----
    python helium_plume.py

Output
------
    fds/flowfields/helium_plume.fds
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
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a buoyant helium plume simulation."""

    # Create simulation
    sim = Simulation(
        chid="helium_plume",
        title="Buoyant Helium Plume",
    )

    # Time parameters
    sim.add(Time(t_end=30.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(40, 40, 60),
            xb=Bounds3D.of(-1.0, 1.0, -1.0, 1.0, 0.0, 3.0),
        )
    )

    # Miscellaneous - isothermal mode
    sim.add(
        Misc(
            isothermal=True,  # No thermal effects, just density-driven
        )
    )

    # ==========================================================================
    # Species
    # ==========================================================================

    # Helium - lighter than air (density ~0.164 kg/m³ at STP)
    sim.add(
        Species(
            id="HELIUM",
            mw=4.0,  # Molecular weight (g/mol)
            background=False,
        )
    )

    # ==========================================================================
    # Helium Source
    # ==========================================================================

    # Surface that injects helium
    sim.add(
        Surface(
            id="HELIUM_SOURCE",
            spec_id="HELIUM",
            mass_flux_total=0.05,  # kg/(m²·s)
            color="CYAN",
        )
    )

    # ==========================================================================
    # Vents
    # ==========================================================================

    # Helium inlet on floor (circular approximation)
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.1, 0.1, -0.1, 0.1, 0.0, 0.0),
            surf_id="HELIUM_SOURCE",
        )
    )

    # Open top for outflow
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # Open sides
    sim.add(Vent(mb="XMIN", surf_id="OPEN"))
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))
    sim.add(Vent(mb="YMIN", surf_id="OPEN"))
    sim.add(Vent(mb="YMAX", surf_id="OPEN"))

    # ==========================================================================
    # Monitoring Devices
    # ==========================================================================

    # Helium mass fraction along centerline
    for z in [0.25, 0.5, 1.0, 1.5, 2.0, 2.5]:
        sim.add(
            Device(
                id=f"HE_Z{int(z * 10):02d}",
                xyz=Point3D(0.0, 0.0, z),
                quantity="MASS FRACTION",
                spec_id="HELIUM",
            )
        )

    # Vertical velocity (plume rise)
    for z in [0.5, 1.0, 1.5, 2.0]:
        sim.add(
            Device(
                id=f"W_Z{int(z * 10):02d}",
                xyz=Point3D(0.0, 0.0, z),
                quantity="W-VELOCITY",
            )
        )

    # Radial profile at z=1.5m
    for x in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5]:
        sim.add(
            Device(
                id=f"HE_R{int(x * 10):02d}",
                xyz=Point3D(x, 0.0, 1.5),
                quantity="MASS FRACTION",
                spec_id="HELIUM",
            )
        )

    # Density at centerline (shows buoyancy)
    sim.add(
        Device(
            id="DENSITY_CENTER",
            xyz=Point3D(0.0, 0.0, 1.0),
            quantity="DENSITY",
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
