#!/usr/bin/env python3
"""
Spray Burner Example
====================

A liquid fuel spray burner demonstrating particle-based fire sources.
Uses PART (particles), PROP (nozzle properties), and DEVC for spray control.

FDS Reference: Fires/spray_burner.fds
https://github.com/firemodels/fds/blob/master/Verification/Fires/spray_burner.fds

Key Namelists: SURF, PART, PROP, SPEC

Usage
-----
    python spray_burner.py

Output
------
    fds/fires/spray_burner.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Material,
    Mesh,
    Misc,
    Obstruction,
    Particle,
    Property,
    Ramp,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a spray burner simulation."""

    # Create simulation
    sim = Simulation(chid="spray_burner", title="2 MW Heptane Spray Burner")

    # Time: 90 seconds
    sim.add(Time(t_end=90.0))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(20, 30, 40), xb=Bounds3D.of(3.0, 5.0, -1.5, 1.5, 0.0, 4.0)))

    # Misc settings
    sim.add(Misc(porous_floor=False, y_co2_infty=0.0, humidity=0.0))

    # Reaction: N-Heptane combustion
    sim.add(Reaction(fuel="N-HEPTANE", co_yield=0.0, soot_yield=0.015))

    # Steel material with temperature-dependent properties
    sim.add(Ramp(id="c_steel", points=[(20, 0.45), (377, 0.60), (677, 0.85)]))
    sim.add(Ramp(id="k_steel", points=[(20, 48.0), (677, 30.0)]))

    sim.add(
        Material(
            id="STEEL", specific_heat_ramp="c_steel", conductivity_ramp="k_steel", density=7850.0
        )
    )

    # Gypsum material
    sim.add(Material(id="GYPSUM", conductivity=0.16, specific_heat=0.9, density=790.0))

    # Surfaces
    sim.add(Surface(id="STEEL SHEET", color="BLACK", matl_id="STEEL", thickness=0.003))

    sim.add(
        Surface(id="GYPSUM BOARD", default=True, color="WHEAT", matl_id="GYPSUM", thickness=0.0254)
    )

    # Particle definition for heptane droplets
    sim.add(
        Particle(
            id="heptane_droplets",
            spec_id="N-HEPTANE",
            quantities=["PARTICLE DIAMETER", "PARTICLE TEMPERATURE", "PARTICLE AGE"],
            diameter=500.0,  # microns
            heat_of_combustion=44500.0,  # kJ/kg
            sampling_factor=10,
        )
    )

    # Fuel flow ramp
    sim.add(Ramp(id="fuel", points=[(0.0, 0.0), (20.0, 1.0), (40.0, 1.0), (60.0, 0.0)]))

    # Nozzle property
    sim.add(
        Property(
            id="nozzle",
            part_id="heptane_droplets",
            flow_rate=1.97,  # L/min
            flow_ramp="fuel",
            particle_velocity=10.0,
            spray_angle=(0.0, 45.0),
            smokeview_id="nozzle",
        )
    )

    # Spray devices (nozzles)
    sim.add(
        Device(
            id="nozzle_1",
            xyz=Point3D.of(4.0, -0.3, 0.5),
            prop_id="nozzle",
            quantity="TIME",
            setpoint=0.0,
        )
    )

    sim.add(
        Device(
            id="nozzle_2",
            xyz=Point3D.of(4.0, 0.3, 0.5),
            prop_id="nozzle",
            quantity="TIME",
            setpoint=0.0,
        )
    )

    # Burner pan geometry
    sim.add(Obstruction(xb=Bounds3D.of(3.5, 4.5, -1.0, 1.0, 0.0, 0.0), surf_id="STEEL SHEET"))

    # Pan walls
    sim.add(Obstruction(xb=Bounds3D.of(3.5, 4.5, -1.0, -1.0, 0.0, 0.1), surf_id="STEEL SHEET"))
    sim.add(Obstruction(xb=Bounds3D.of(3.5, 4.5, 1.0, 1.0, 0.0, 0.1), surf_id="STEEL SHEET"))
    sim.add(Obstruction(xb=Bounds3D.of(3.5, 3.5, -1.0, 1.0, 0.0, 0.1), surf_id="STEEL SHEET"))
    sim.add(Obstruction(xb=Bounds3D.of(4.5, 4.5, -1.0, 1.0, 0.0, 0.1), surf_id="STEEL SHEET"))

    # Open boundaries
    for mb in ["XMIN", "XMAX", "YMIN", "YMAX", "ZMAX"]:
        sim.add(Vent(mb=mb, surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "fires")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
