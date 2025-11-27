#!/usr/bin/env python3
"""
Cone Calorimeter Simulation

Demonstrates bench-scale test setup using SOLID_PHASE_ONLY.
This example shows how to model cone calorimeter experiments
for material flammability testing.
"""

from pyfds.core.namelists import Material, Mesh, Obstacle, Reaction, Surface, Time
from pyfds.core.simulation import Simulation


def main():
    """Run the cone calorimeter simulation."""

    # Create simulation
    sim = Simulation(chid="cone_calorimeter", title="Cone Calorimeter Simulation")

    # Time parameters
    sim.time_params = Time(t_end=600.0)  # 10 minutes

    # Mesh - small domain for bench-scale test
    mesh = Mesh(
        id="MESH",
        ijk=(20, 20, 15),
        xb=(0.0, 0.1, 0.0, 0.1, 0.0, 0.075),  # 10cm x 10cm x 7.5cm
    )
    sim.geometry.add_mesh(mesh)

    # Test sample material
    sample = Material(
        id="SAMPLE",
        density=600.0,  # kg/m³
        conductivity=0.18,  # W/(m·K)
        specific_heat=2.3,  # kJ/(kg·K)
        n_reactions=1,
        a=[2e9],  # Pre-exponential factor (1/s)
        e=[90000],  # Activation energy (kJ/kmol)
        heat_of_reaction=[1500],  # Heat of pyrolysis (kJ/kg)
        spec_id=["FUEL_GAS"],  # Gaseous products
        nu_spec=[0.8],  # Gas yield
        matl_id=["CHAR"],  # Solid residue
        nu_matl=[0.2],  # Residue yield
    )

    # Residue material
    char = Material(id="CHAR", density=200.0, conductivity=0.15, specific_heat=1.2)

    # Cone calorimeter surface
    cone_surface = Surface(
        id="CONE_CALORIMETER",
        matl_id="SAMPLE",
        thickness=0.006,  # 6mm thick sample
        tmp_front=25.0,  # Initial temperature
        heat_flux=50000.0,  # 50 kW/m² irradiance
        solid_phase_only=True,  # Bench-scale mode
    )

    # Sample holder
    sample_holder = Obstacle(
        id="SAMPLE",
        xb=[0.03, 0.07, 0.03, 0.07, 0.0, 0.006],  # 4cm x 4cm x 6mm
        surf_id="CONE_CALORIMETER",
    )

    # Combustion reaction
    combustion = Reaction(
        fuel="FUEL_GAS",
        soot_yield=0.02,
        co_yield=0.01,
        radiative_fraction=0.25,
        heat_of_combustion_complete=18000.0,  # kJ/kg
    )

    # Add components to simulation
    sim.material_mgr.add_material(sample)
    sim.material_mgr.add_material(char)
    sim.material_mgr.add_surface(cone_surface)
    sim.geometry.add_obstacle(sample_holder)
    sim.physics.add_reaction(combustion)

    # Write input file
    sim.write_input_file()

    print("Cone calorimeter example input file written: cone_calorimeter.fds")
    print("Run with: fds cone_calorimeter.fds")
    print("")
    print("This example demonstrates:")
    print("- Bench-scale material testing")
    print("- Constant heat flux exposure")
    print("- SOLID_PHASE_ONLY mode")
    print("- Small-scale pyrolysis kinetics")
    print("")
    print("Typical cone calorimeter measurements:")
    print("- Heat release rate")
    print("- Mass loss rate")
    print("- Time to ignition")
    print("- Smoke production")


if __name__ == "__main__":
    main()
