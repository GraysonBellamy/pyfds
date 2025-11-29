#!/usr/bin/env python3
"""

from pathlib import Path
Charring Material Example

Demonstrates wood pyrolysis with char formation.
This example shows how to model materials that form a char residue
during pyrolysis, which affects heat transfer and burning behavior.
"""

from pathlib import Path

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Material, Mesh, Reaction, Species, Surface, Time
from pyfds.core.simulation import Simulation


def main():
    """Run the charring material simulation."""

    # Create simulation
    sim = Simulation(chid="charring_material", title="Charring Material Example")

    # Time parameters
    sim.time_params = Time(t_end=600.0)  # 10 minutes

    # Mesh
    mesh = Mesh(
        id="MESH",
        ijk=Grid3D(30, 30, 15),
        xb=Bounds3D(0.0, 1.5, 0.0, 1.5, 0.0, 0.75),
    )
    sim.geometry.add_mesh(mesh)

    # Define pyrolysis gas species
    # Simple hydrocarbon approximation for wood pyrolysis gases
    wood_gas = Species(
        id="WOOD_GAS",
        formula="CH1.5O0.5",  # Approximate composition for wood pyrolysis products
    )
    sim.add_species(wood_gas)

    # Char residue (non-pyrolyzing)
    char = Material(
        id="CHAR",
        density=150.0,  # kg/m³
        conductivity=0.1,  # W/(m·K) - lower than wood
        specific_heat=1.0,  # kJ/(kg·K)
        emissivity=0.95,  # High emissivity
    )

    # Wood with charring pyrolysis
    wood = Material(
        id="WOOD",
        density=500.0,  # kg/m³
        conductivity=0.13,  # W/(m·K)
        specific_heat=2.5,  # kJ/(kg·K)
        emissivity=0.9,
        n_reactions=1,
        a=[1e10],  # Pre-exponential factor (1/s)
        e=[100000],  # Activation energy (kJ/kmol)
        heat_of_reaction=[1800],  # Heat of pyrolysis (kJ/kg)
        spec_id=["WOOD_GAS"],  # Gaseous products
        nu_spec=[0.75],  # Gas yield
        matl_id=["CHAR"],  # Solid residue
        nu_matl=[0.25],  # Residue yield
    )

    # Wood surface
    wood_surface = Surface(
        id="WOOD_SURFACE",
        matl_id="WOOD",
        thickness=0.02,  # 2cm thick
        tmp_front=25.0,  # Initial temperature
    )

    # Combustion reaction for pyrolysis gases
    combustion = Reaction(
        fuel="WOOD_GAS",
        soot_yield=0.015,
        co_yield=0.005,
        radiative_fraction=0.3,
        heat_of_combustion_complete=18000.0,  # kJ/kg
    )

    # Add components to simulation
    sim.material_mgr.add_material(char)
    sim.material_mgr.add_material(wood)
    sim.material_mgr.add_surface(wood_surface)
    sim.physics.add_reaction(combustion)

    # Create output directory
    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write input file
    output_file = sim.write(output_dir / "charring_material.fds")

    print(f"Charring material example input file written: {output_file}")
    print(f"Run with: fds {output_file}")
    print("")
    print("This example demonstrates:")
    print("- Char formation during pyrolysis")
    print("- Heat transfer through char layer")
    print("- Effect of char on burning rate")


if __name__ == "__main__":
    main()
