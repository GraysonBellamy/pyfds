#!/usr/bin/env python3
"""
Liquid Pool Fire Example

Demonstrates liquid fuel evaporation and combustion.
This example shows how to model pool fires with liquid fuels
that evaporate and burn according to their physical properties.
"""

from pyfds.core.namelists import Material, Mesh, Obstacle, Reaction, Surface, Time, Vent
from pyfds.core.simulation import Simulation


def main():
    """Run the liquid pool fire simulation."""

    # Create simulation
    sim = Simulation(chid="liquid_pool_fire", title="Liquid Pool Fire Example")

    # Time parameters
    sim.time_params = Time(t_end=300.0)  # 5 minutes

    # Mesh
    mesh = Mesh(id="MESH", ijk=(30, 30, 20), xb=(0.0, 1.5, 0.0, 1.5, 0.0, 1.0))
    sim.geometry.add_mesh(mesh)

    # Liquid methanol fuel
    methanol = Material(
        id="METHANOL_LIQUID",
        density=792.0,  # Liquid density (kg/m³)
        conductivity=0.2,  # Thermal conductivity (W/(m·K))
        specific_heat=2.51,  # Specific heat (kJ/(kg·K))
        # Liquid fuel parameters
        boiling_temperature=64.7,  # Boiling point (°C)
        spec_id="METHANOL",  # Vapor species
        heat_of_reaction=837.0,  # Heat of combustion (kJ/kg)
        absorption_coefficient=140.0,  # Absorption coefficient (1/m)
        heat_of_vaporization=1100.0,  # Heat of vaporization (kJ/kg)
        mw=32.0,  # Molecular weight (g/mol)
    )

    # Pool surface (liquid layer)
    methanol_pool = Surface(
        id="METHANOL_POOL",
        matl_id="METHANOL_LIQUID",
        thickness=0.05,  # Liquid depth (m)
        tmp_front=25.0,  # Initial temperature (°C)
    )

    # Pool obstruction
    pool = Obstacle(
        id="POOL",
        xb=[0.5, 1.0, 0.5, 1.0, 0.0, 0.05],  # 50cm x 50cm x 5cm deep
        surf_id="METHANOL_POOL",
    )

    # Ignition source
    ignition = Surface(
        id="IGNITER",
        hrrpua=500.0,  # Heat release rate (kW/m²)
        ignition_temperature=100.0,  # Ignition temperature (°C)
    )

    # Small ignition patch
    igniter = Obstacle(
        id="IGNITER_PATCH",
        xb=[0.7, 0.8, 0.7, 0.8, 0.0, 0.01],  # Small 10cm x 10cm patch
        surf_id="IGNITER",
    )

    # Combustion reaction for methanol vapor
    methanol_combustion = Reaction(
        fuel="METHANOL",
        soot_yield=0.015,  # Soot yield
        co_yield=0.001,  # CO yield
        radiative_fraction=0.35,  # Radiative fraction
        heat_of_combustion_complete=22600.0,  # Complete heat of combustion (kJ/kg)
    )

    # Open vents for ventilation
    vent_north = Vent(id="VENT_NORTH", xb=[0.0, 1.5, 1.5, 1.5, 0.0, 1.0], surf_id="OPEN")

    vent_south = Vent(id="VENT_SOUTH", xb=[0.0, 1.5, 0.0, 0.0, 0.0, 1.0], surf_id="OPEN")

    vent_east = Vent(id="VENT_EAST", xb=[1.5, 1.5, 0.0, 1.5, 0.0, 1.0], surf_id="OPEN")

    vent_west = Vent(id="VENT_WEST", xb=[0.0, 0.0, 0.0, 1.5, 0.0, 1.0], surf_id="OPEN")

    # Add components to simulation
    sim.material_mgr.add_material(methanol)
    sim.material_mgr.add_surface(methanol_pool)
    sim.material_mgr.add_surface(ignition)
    sim.geometry.add_obstacle(pool)
    sim.geometry.add_obstacle(igniter)
    sim.geometry.add_vent(vent_north)
    sim.geometry.add_vent(vent_south)
    sim.geometry.add_vent(vent_east)
    sim.geometry.add_vent(vent_west)
    sim.physics.add_reaction(methanol_combustion)

    # Write input file
    sim.write_input_file()

    print("Liquid pool fire example input file written: liquid_pool_fire.fds")
    print("Run with: fds liquid_pool_fire.fds")
    print("")
    print("This example demonstrates:")
    print("- Liquid fuel evaporation modeling")
    print("- Pool fire burning behavior")
    print("- Heat of vaporization effects")
    print("- Ignition and flame spread")


if __name__ == "__main__":
    main()
