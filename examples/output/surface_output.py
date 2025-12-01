#!/usr/bin/env python3
"""
Surface Output Example
======================

Demonstrates surface-based output measurements including wall
temperatures, heat fluxes, and surface quantities.

FDS Reference: Heat_Transfer/heat_conduction_a.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/heat_conduction_a.fds

Key Namelists: DEVC (wall quantities, IOR), SURF (output params)

Usage
-----
    python surface_output.py

Output
------
    fds/output/surface_output.fds
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
    Obstruction,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation demonstrating surface output measurements."""

    # Create simulation
    sim = Simulation(
        chid="surface_output",
        title="Surface Output Measurements",
    )

    # Time parameters
    sim.add(Time(t_end=300.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 30, 30),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
        )
    )

    # ==========================================================================
    # Materials
    # ==========================================================================

    # Steel for target
    sim.add(
        Material(
            id="STEEL",
            conductivity=45.0,
            specific_heat=0.46,
            density=7850.0,
            emissivity=0.9,
        )
    )

    # Concrete for walls
    sim.add(
        Material(
            id="CONCRETE",
            conductivity=1.6,
            specific_heat=0.9,
            density=2300.0,
            emissivity=0.9,
        )
    )

    # Insulation
    sim.add(
        Material(
            id="INSULATION",
            conductivity=0.04,
            specific_heat=0.8,
            density=100.0,
        )
    )

    # ==========================================================================
    # Surfaces
    # ==========================================================================

    # Steel surface (thin target)
    sim.add(
        Surface(
            id="STEEL_PLATE",
            matl_id="STEEL",
            thickness=0.01,  # 10mm steel
            color="GRAY 40",
            backing="EXPOSED",  # Both sides exposed
        )
    )

    # Concrete wall
    sim.add(
        Surface(
            id="CONCRETE_WALL",
            matl_id="CONCRETE",
            thickness=0.2,  # 200mm concrete
            color="GRAY 60",
        )
    )

    # Insulated wall (layered)
    sim.add(
        Surface(
            id="INSULATED_WALL",
            matl_id=["CONCRETE", "INSULATION"],
            thickness=[0.1, 0.05],  # 100mm concrete + 50mm insulation
            color="GRAY 80",
        )
    )

    # Fire surface
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="ORANGE"))

    # ==========================================================================
    # Geometry
    # ==========================================================================

    # Back wall (concrete)
    sim.add(
        Obstruction(
            id="BACK_WALL",
            xb=Bounds3D.of(0.0, 0.1, 0.0, 3.0, 0.0, 3.0),
            surf_id="CONCRETE_WALL",
        )
    )

    # Insulated wall (side)
    sim.add(
        Obstruction(
            id="SIDE_WALL",
            xb=Bounds3D.of(0.0, 3.0, 2.9, 3.0, 0.0, 3.0),
            surf_id="INSULATED_WALL",
        )
    )

    # Steel target plate
    sim.add(
        Obstruction(
            id="TARGET_PLATE",
            xb=Bounds3D.of(2.0, 2.0, 1.0, 2.0, 0.5, 1.5),
            surf_id="STEEL_PLATE",
        )
    )

    # Floor column for fire elevation
    sim.add(
        Obstruction(
            id="BURNER_BASE",
            xb=Bounds3D.of(0.9, 1.5, 0.9, 1.5, 0.0, 0.3),
            surf_id="INERT",
        )
    )

    # Fire vent
    sim.add(
        Vent(
            xb=Bounds3D.of(1.0, 1.4, 1.0, 1.4, 0.3, 0.3),
            surf_id="FIRE",
        )
    )

    # Open top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # ==========================================================================
    # Surface Temperature Measurements
    # ==========================================================================

    # Back wall surface temperature at multiple heights
    for z in [0.5, 1.0, 1.5, 2.0, 2.5]:
        sim.add(
            Device(
                id=f"WALL_T_Z{int(z * 10):02d}",
                xyz=Point3D(0.1, 1.5, z),
                quantity="WALL TEMPERATURE",
                ior=-1,  # -X face (front of wall)
            )
        )

    # Back wall temperature (back face for comparison)
    sim.add(
        Device(
            id="WALL_T_BACK",
            xyz=Point3D(0.0, 1.5, 1.0),
            quantity="BACK WALL TEMPERATURE",
            ior=-1,
        )
    )

    # Target plate surface temperatures (front and back)
    sim.add(
        Device(
            id="TARGET_T_FRONT",
            xyz=Point3D(2.0, 1.5, 1.0),
            quantity="WALL TEMPERATURE",
            ior=-1,  # Facing fire
        )
    )

    sim.add(
        Device(
            id="TARGET_T_BACK",
            xyz=Point3D(2.0, 1.5, 1.0),
            quantity="BACK WALL TEMPERATURE",
            ior=-1,
        )
    )

    # ==========================================================================
    # Heat Flux Measurements
    # ==========================================================================

    # Incident heat flux on back wall
    sim.add(
        Device(
            id="WALL_IHF",
            xyz=Point3D(0.1, 1.5, 1.0),
            quantity="INCIDENT HEAT FLUX",
            ior=-1,
        )
    )

    # Net heat flux (absorbed)
    sim.add(
        Device(
            id="WALL_NHF",
            xyz=Point3D(0.1, 1.5, 1.0),
            quantity="NET HEAT FLUX",
            ior=-1,
        )
    )

    # Convective heat flux
    sim.add(
        Device(
            id="WALL_CONV_HF",
            xyz=Point3D(0.1, 1.5, 1.0),
            quantity="CONVECTIVE HEAT FLUX",
            ior=-1,
        )
    )

    # Radiative heat flux
    sim.add(
        Device(
            id="WALL_RAD_HF",
            xyz=Point3D(0.1, 1.5, 1.0),
            quantity="RADIATIVE HEAT FLUX",
            ior=-1,
        )
    )

    # Heat flux to target
    sim.add(
        Device(
            id="TARGET_IHF",
            xyz=Point3D(2.0, 1.5, 1.0),
            quantity="INCIDENT HEAT FLUX",
            ior=-1,
        )
    )

    sim.add(
        Device(
            id="TARGET_NHF",
            xyz=Point3D(2.0, 1.5, 1.0),
            quantity="NET HEAT FLUX",
            ior=-1,
        )
    )

    # ==========================================================================
    # Gauge Heat Flux (virtual sensors)
    # ==========================================================================

    # Gauge heat flux at target location (radiometer-type)
    sim.add(
        Device(
            id="GAUGE_HF",
            xyz=Point3D(2.0, 1.5, 1.0),
            quantity="GAUGE HEAT FLUX",
            ior=-1,
        )
    )

    # ==========================================================================
    # Reference gas measurements near surfaces
    # ==========================================================================

    # Gas temperature near wall
    sim.add(
        Device(
            id="GAS_T_NEAR_WALL",
            xyz=Point3D(0.2, 1.5, 1.0),
            quantity="TEMPERATURE",
        )
    )

    # Gas temperature near target
    sim.add(
        Device(
            id="GAS_T_NEAR_TARGET",
            xyz=Point3D(1.9, 1.5, 1.0),
            quantity="TEMPERATURE",
        )
    )

    # HRR
    sim.add(
        Device(
            id="HRR",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
            quantity="HRR",
        )
    )

    # Write output
    output_path = write_example(sim, "output")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
