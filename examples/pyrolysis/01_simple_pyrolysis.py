#!/usr/bin/env python3
"""
Simple Pyrolysis Example

Demonstrates a single-reaction pyrolysis material.
This example shows the basic setup for a material that decomposes
in a single pyrolysis reaction producing gaseous and solid products.
"""

from pathlib import Path

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Material, Mesh, Reaction, Species, Time
from pyfds.core.simulation import Simulation


def main():
    """Run the simple pyrolysis simulation."""

    # Create simulation
    sim = Simulation(chid="simple_pyrolysis", title="Simple Pyrolysis Example")

    # Time parameters
    sim.time_params = Time(t_end=300.0)  # 5 minutes

    # Mesh
    mesh = Mesh(
        id="MESH",
        ijk=Grid3D(20, 20, 10),
        xb=Bounds3D(0.0, 1.0, 0.0, 1.0, 0.0, 0.5),
    )
    sim.geometry.add_mesh(mesh)

    # Define pyrolysis gas species
    # Simple hydrocarbon approximation for wood pyrolysis gases
    wood_gas = Species(
        id="WOOD_GAS",
        formula="CH1.5O0.5",  # Approximate composition for wood pyrolysis products
    )
    sim.add_species(wood_gas)

    # Pyrolysis material - wood
    wood = Material(
        id="WOOD",
        density=500.0,  # kg/m³
        conductivity=0.13,  # W/(m·K)
        specific_heat=2.5,  # kJ/(kg·K)
        n_reactions=1,
        a=[1e10],  # Pre-exponential factor (1/s)
        e=[100000],  # Activation energy (kJ/kmol)
        heat_of_reaction=[1800],  # Heat of pyrolysis (kJ/kg)
        spec_id=["WOOD_GAS"],  # Gaseous products
        nu_spec=[0.75],  # Gas yield
        matl_id=["CHAR"],  # Solid residue
        nu_matl=[0.25],  # Residue yield
    )

    # Residue material
    char = Material(id="CHAR", density=150.0, conductivity=0.1, specific_heat=1.0)

    # Combustion reaction for pyrolysis gases
    combustion = Reaction(fuel="WOOD_GAS", soot_yield=0.02, co_yield=0.01, radiative_fraction=0.25)

    # Add components to simulation
    sim.material_mgr.add_material(wood)
    sim.material_mgr.add_material(char)
    sim.physics.add_reaction(combustion)

    # Create output directory
    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write input file
    output_file = sim.write(output_dir / "simple_pyrolysis.fds")

    print(f"Simple pyrolysis example input file written: {output_file}")
    print(f"Run with: fds {output_file}")


if __name__ == "__main__":
    main()
