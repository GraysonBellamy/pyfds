#!/usr/bin/env python3
"""
Convective Cooling Example
==========================

Demonstrates convective heat transfer boundary conditions
using fixed heat transfer coefficient (H_FIXED).

FDS Reference: Heat_Transfer/convective_cooling.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/convective_cooling.fds

Key Namelists: SURF (H_FIXED)

Usage
-----
    python convective_cooling.py

Output
------
    fds/heat_transfer/convective_cooling.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Initialization,
    Material,
    Mesh,
    Surface,
    Time,
    Vent,
)


def main():
    """Create convective cooling simulation."""

    # Create simulation
    sim = Simulation(chid="convective_cooling", title="Convective Cooling of Hot Object")

    # Time
    sim.add(Time(t_end=600.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)))

    # Initial gas temperature
    sim.add(
        Initialization(
            xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
            temperature=25.0,  # Ambient at 25°C
        )
    )

    # Material for the cooling object
    sim.add(
        Material(
            id="ALUMINUM",
            conductivity=205.0,  # High thermal conductivity
            specific_heat=0.9,  # kJ/kg·K
            density=2700.0,  # kg/m³
            emissivity=0.1,  # Low emissivity (polished)
        )
    )

    # Surface with specified heat transfer coefficient
    # h_fixed overrides the natural/forced convection correlations
    sim.add(
        Surface(
            id="COOLING_SURF",
            matl_id="ALUMINUM",
            thickness=0.02,  # 2 cm thick
            backing="INSULATED",
            tmp_inner=200.0,  # Start hot at 200°C
            h_fixed=25.0,  # Fixed h = 25 W/m²·K
            color="SILVER",
        )
    )

    # Insulated surface (adiabatic)
    sim.add(Surface(id="INSULATED", adiabatic=True, color="GRAY"))

    # Cooling object - only top surface exposed
    sim.add(Vent(xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0.3, 0.3), surf_id="COOLING_SURF"))

    # Open boundaries for natural convection
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Temperature devices
    sim.add(
        Device(id="T_Surface", quantity="WALL TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.3), ior=3)
    )

    sim.add(Device(id="T_Gas", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.5)))

    sim.add(
        Device(id="HF_Conv", quantity="CONVECTIVE HEAT FLUX", xyz=Point3D.of(0.5, 0.5, 0.3), ior=3)
    )

    # Write output
    output_path = write_example(sim, "heat_transfer")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
