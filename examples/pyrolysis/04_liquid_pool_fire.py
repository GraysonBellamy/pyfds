#!/usr/bin/env python3
"""

from pathlib import Path
Liquid Pool Fire Example

Demonstrates liquid fuel evaporation and combustion.
This example shows how to model pool fires with liquid fuels
that evaporate and burn according to their physical properties.
"""

from pathlib import Path

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Material, Mesh, Obstruction, Reaction, Surface, Time, Vent
from pyfds.core.simulation import Simulation


def main():
    """Run the liquid pool fire simulation."""

    # Create simulation
    sim = Simulation(chid="liquid_pool_fire", title="Liquid Pool Fire Example")

    # Time parameters
    sim.time_params = Time(t_end=300.0)  # 5 minutes

    # Mesh
    mesh = Mesh(id="MESH", ijk=Grid3D.of(30, 30, 20), xb=Bounds3D.of(0.0, 1.5, 0.0, 1.5, 0.0, 1.0))
    sim.add(mesh)

    # Liquid methanol fuel
    methanol = Material(
        id="METHANOL_LIQUID",
        density=792.0,  # Liquid density (kg/m³)
        conductivity=0.2,  # Thermal conductivity (W/(m·K))
        specific_heat=2.51,  # Specific heat (kJ/(kg·K))
        emissivity=1.0,  # Emissivity
        nu_spec=1.0,  # Species yield
        spec_id="METHANOL",  # Vapor species ID
        heat_of_reaction=1100.0,  # Heat of vaporization (kJ/kg)
        boiling_temperature=64.7,  # Boiling temperature (°C)
    )

    # Pool surface (liquid layer)
    methanol_pool = Surface(
        id="METHANOL_POOL",
        matl_id="METHANOL_LIQUID",
        thickness=0.05,  # Liquid depth (m)
        tmp_front=25.0,  # Initial temperature (°C)
    )

    # Pool obstruction
    pool = Obstruction(
        id="POOL",
        xb=Bounds3D.of(0.5, 1.0, 0.5, 1.0, 0.0, 0.05),  # 50cm x 50cm x 5cm deep
        surf_id="METHANOL_POOL",
    )

    # Ignition source
    ignition = Surface(
        id="IGNITER",
        hrrpua=500.0,  # Heat release rate (kW/m²)
        ignition_temperature=100.0,  # Ignition temperature (°C)
    )

    # Small ignition patch
    igniter = Obstruction(
        id="IGNITER_PATCH",
        xb=Bounds3D.of(0.7, 0.8, 0.7, 0.8, 0.0, 0.01),  # Small 10cm x 10cm patch
        surf_id="IGNITER",
    )

    # Combustion reaction for methanol vapor
    methanol_combustion = Reaction(
        fuel="METHANOL",
        soot_yield=0.015,  # Soot yield
        co_yield=0.001,  # CO yield
        radiative_fraction=0.35,  # Radiative fraction
        hoc_complete=22600.0,  # Complete heat of combustion (kJ/kg)
    )

    # Open vents for ventilation
    vent_north = Vent(id="VENT_NORTH", xb=Bounds3D.of(0.0, 1.5, 1.5, 1.5, 0.0, 1.0), surf_id="OPEN")

    vent_south = Vent(id="VENT_SOUTH", xb=Bounds3D.of(0.0, 1.5, 0.0, 0.0, 0.0, 1.0), surf_id="OPEN")

    vent_east = Vent(id="VENT_EAST", xb=Bounds3D.of(1.5, 1.5, 0.0, 1.5, 0.0, 1.0), surf_id="OPEN")

    vent_west = Vent(id="VENT_WEST", xb=Bounds3D.of(0.0, 0.0, 0.0, 1.5, 0.0, 1.0), surf_id="OPEN")

    # Add components to simulation
    sim.add(methanol)
    sim.add(methanol_pool)
    sim.add(ignition)
    sim.add(pool)
    sim.add(igniter)
    sim.add(vent_north)
    sim.add(vent_south)
    sim.add(vent_east)
    sim.add(vent_west)
    sim.add(methanol_combustion)

    # Create output directory
    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write input file
    output_file = sim.write(output_dir / "liquid_pool_fire.fds")

    print(f"Liquid pool fire example input file written: {output_file}")
    print(f"Run with: fds {output_file}")
    print("")
    print("This example demonstrates:")
    print("- Liquid fuel evaporation modeling")
    print("- Pool fire burning behavior")
    print("- Heat of vaporization effects")
    print("- Ignition and flame spread")


if __name__ == "__main__":
    main()
