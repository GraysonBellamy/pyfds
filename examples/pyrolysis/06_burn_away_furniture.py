#!/usr/bin/env python3
"""
Burn-Away Furniture Example

Demonstrates combustible furniture with BULK_DENSITY.
This example shows how to model furniture that burns away
during a fire, exposing interior materials and changing geometry.
"""

from pyfds.core.namelists import Material, Mesh, Obstacle, Reaction, Surface, Time, Vent
from pyfds.core.simulation import Simulation


def main():
    """Run the burn-away furniture simulation."""

    # Create simulation
    sim = Simulation(chid="burn_away_furniture", title="Burn-Away Furniture Example")

    # Time parameters
    sim.time_params = Time(t_end=900.0)  # 15 minutes

    # Mesh
    mesh = Mesh(id="MESH", ijk=(40, 40, 20), xb=(0.0, 4.0, 0.0, 4.0, 0.0, 2.0))
    sim.geometry.add_mesh(mesh)

    # Foam material (flexible polyurethane foam)
    foam = Material(
        id="FOAM",
        density=30.0,  # kg/m³ (very low density)
        conductivity=0.04,  # W/(m·K)
        specific_heat=1.5,  # kJ/(kg·K)
        n_reactions=1,
        a=[1e8],  # Pre-exponential factor (1/s)
        e=[80000],  # Activation energy (kJ/kmol)
        heat_of_reaction=[1000],  # Heat of pyrolysis (kJ/kg)
        spec_id=["FOAM_GAS"],  # Gaseous products
        nu_spec=[0.85],  # Gas yield
        matl_id=["CHAR"],  # Solid residue
        nu_matl=[0.15],  # Residue yield
    )

    # Char residue
    char = Material(id="CHAR", density=50.0, conductivity=0.08, specific_heat=1.2)

    # Burn-away surface
    foam_surface = Surface(
        id="FOAM_SURFACE",
        matl_id="FOAM",
        thickness=0.05,  # 5cm thick foam
        burn_away=True,  # Enable burn-away
        tmp_front=25.0,
    )

    # Sofa (burn-away furniture)
    sofa = Obstacle(
        id="SOFA",
        xb=[1.0, 3.0, 1.0, 2.5, 0.0, 0.8],  # Sofa dimensions
        surf_id="FOAM_SURFACE",
        bulk_density=25.0,  # Effective density (kg/m³)
    )

    # Coffee table (wooden)
    wood = Material(
        id="WOOD",
        density=600.0,
        conductivity=0.15,
        specific_heat=2.4,
        n_reactions=1,
        a=[5e9],
        e=[95000],
        heat_of_reaction=[1600],
        spec_id=["WOOD_GAS"],
        nu_spec=[0.7],
        matl_id=["WOOD_CHAR"],
        nu_matl=[0.3],
    )

    wood_char = Material(id="WOOD_CHAR", density=300.0, conductivity=0.12, specific_heat=1.5)

    table_surface = Surface(id="TABLE_SURFACE", matl_id="WOOD", thickness=0.03, burn_away=True)

    table = Obstacle(
        id="TABLE",
        xb=[1.5, 2.5, 0.5, 1.5, 0.0, 0.45],  # Table dimensions
        surf_id="TABLE_SURFACE",
        bulk_density=500.0,
    )

    # Combustion reactions
    foam_combustion = Reaction(
        fuel="FOAM_GAS",
        soot_yield=0.08,  # High soot yield
        co_yield=0.02,
        radiative_fraction=0.25,
        heat_of_combustion_complete=25000.0,
    )

    wood_combustion = Reaction(
        fuel="WOOD_GAS",
        soot_yield=0.015,
        co_yield=0.005,
        radiative_fraction=0.3,
        heat_of_combustion_complete=18000.0,
    )

    # Ventilation
    vent = Vent(id="CEILING_VENT", xb=[0.0, 4.0, 0.0, 4.0, 2.0, 2.0], surf_id="OPEN")

    # Add components to simulation
    sim.material_mgr.add_material(foam)
    sim.material_mgr.add_material(char)
    sim.material_mgr.add_material(wood)
    sim.material_mgr.add_material(wood_char)
    sim.material_mgr.add_surface(foam_surface)
    sim.material_mgr.add_surface(table_surface)
    sim.geometry.add_obstacle(sofa)
    sim.geometry.add_obstacle(table)
    sim.geometry.add_vent(vent)
    sim.physics.add_reaction(foam_combustion)
    sim.physics.add_reaction(wood_combustion)

    # Write input file
    sim.write_input_file()

    print("Burn-away furniture example input file written: burn_away_furniture.fds")
    print("Run with: fds burn_away_furniture.fds")
    print("")
    print("This example demonstrates:")
    print("- Burn-away of combustible furniture")
    print("- BULK_DENSITY for interior mass")
    print("- Different burning rates for materials")
    print("- Geometry changes during fire")
    print("- Smoke production from furnishings")


if __name__ == "__main__":
    main()
