#!/usr/bin/env python3
"""

from pathlib import Path
Multi-Layer Composite Example

Demonstrates a wall with multiple material layers.
This example shows how to model composite structures with
different materials that pyrolyze at different rates.
"""

from pathlib import Path

from pyfds.core.namelists import Material, Mesh, Obstacle, Reaction, Surface, Time
from pyfds.core.simulation import Simulation


def main():
    """Run the multi-layer composite simulation."""

    # Create simulation
    sim = Simulation(chid="multi_layer_composite", title="Multi-Layer Composite Example")

    # Time parameters
    sim.time_params = Time(t_end=1200.0)  # 20 minutes

    # Mesh
    mesh = Mesh(id="MESH", ijk=(40, 20, 20), xb=(0.0, 2.0, 0.0, 1.0, 0.0, 1.0))
    sim.geometry.add_mesh(mesh)

    # Materials
    gypsum = Material(
        id="GYPSUM",
        density=800.0,
        conductivity=0.16,
        specific_heat=0.84,
        n_reactions=1,
        a=[2e10],
        e=[100000],
        heat_of_reaction=[500],
        spec_id=["GYPSUM_GAS"],
        nu_spec=[0.2],
        matl_id=["CALCINED_GYPSUM"],
        nu_matl=[0.8],
    )

    wood = Material(
        id="WOOD",
        density=500.0,
        conductivity=0.13,
        specific_heat=2.5,
        n_reactions=1,
        a=[1e10],
        e=[100000],
        heat_of_reaction=[1800],
        spec_id=["WOOD_GAS"],
        nu_spec=[0.75],
        matl_id=["CHAR"],
        nu_matl=[0.25],
    )

    insulation = Material(
        id="INSULATION",
        density=50.0,
        conductivity=0.04,
        specific_heat=1.2,
        n_reactions=1,
        a=[5e8],
        e=[80000],
        heat_of_reaction=[1000],
        spec_id=["INSULATION_GAS"],
        nu_spec=[0.9],
        matl_id=["CHAR"],
        nu_matl=[0.1],
    )

    # Residue materials
    calcined_gypsum = Material(
        id="CALCINED_GYPSUM", density=600.0, conductivity=0.2, specific_heat=0.8
    )

    char = Material(id="CHAR", density=150.0, conductivity=0.1, specific_heat=1.0)

    # Multi-layer surface
    composite_wall = Surface(
        id="COMPOSITE_WALL",
        matl_id=["GYPSUM", "WOOD", "INSULATION", "GYPSUM"],
        thickness=[0.013, 0.019, 0.089, 0.013],  # Standard wall construction
        tmp_front=25.0,
    )

    # Wall obstruction
    wall = Obstacle(
        id="WALL",
        xb=[0.0, 0.134, 0.0, 1.0, 0.0, 1.0],  # Wall thickness = sum of layers
        surf_id="COMPOSITE_WALL",
    )

    # Combustion reactions
    gypsum_combustion = Reaction(
        fuel="GYPSUM_GAS", soot_yield=0.01, co_yield=0.005, radiative_fraction=0.2
    )

    wood_combustion = Reaction(
        fuel="WOOD_GAS",
        soot_yield=0.015,
        co_yield=0.01,
        radiative_fraction=0.3,
        heat_of_combustion_complete=18000.0,
    )

    insulation_combustion = Reaction(
        fuel="INSULATION_GAS", soot_yield=0.05, co_yield=0.02, radiative_fraction=0.25
    )

    # Add components to simulation
    sim.material_mgr.add_material(gypsum)
    sim.material_mgr.add_material(wood)
    sim.material_mgr.add_material(insulation)
    sim.material_mgr.add_material(calcined_gypsum)
    sim.material_mgr.add_material(char)
    sim.material_mgr.add_surface(composite_wall)
    sim.geometry.add_obstacle(wall)
    sim.physics.add_reaction(gypsum_combustion)
    sim.physics.add_reaction(wood_combustion)
    sim.physics.add_reaction(insulation_combustion)

    # Create output directory
    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write input file
    output_file = sim.write(output_dir / "multi_layer_composite.fds")

    print(f"Multi-layer composite example input file written: {output_file}")
    print(f"Run with: fds {output_file}")
    print("")
    print("This example demonstrates:")
    print("- Multi-layer material construction")
    print("- Different pyrolysis rates for each layer")
    print("- Heat transfer through composite structures")
    print("- Sequential layer exposure")


if __name__ == "__main__":
    main()
