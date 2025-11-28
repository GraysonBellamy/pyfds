#!/usr/bin/env python3
"""
Structured Pyrolysis Example

Demonstrates the structured pyrolysis API with complex
multi-step material decomposition reactions.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders import MaterialBuilder, SurfBuilder
from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction


def main():
    """Create structured pyrolysis simulation."""

    sim = Simulation(chid="structured_pyrolysis", title="Structured Pyrolysis Example")

    # Create material with structured pyrolysis reactions
    wood = (
        MaterialBuilder("WOOD")
        .density(500)
        .thermal_conductivity(0.13)
        .specific_heat(2.5)
        .add_reaction(
            PyrolysisReaction(
                a=1.0e10,  # pre-exponential factor (1/s)
                e=1.5e5,  # activation energy (J/mol)
                heat_of_reaction=500,  # kJ/kg
                reference_temperature=None,
                pyrolysis_range=None,
                heating_rate=5.0,
                n_s=1.0,
                n_t=0.0,
                n_o2=0.0,
                gas_diffusion_depth=None,
                max_reaction_rate=None,
                products=[
                    PyrolysisProduct(
                        spec_id="CELLULOSE_GAS",
                        nu_spec=0.8,
                        heat_of_combustion=None,
                        matl_id=None,
                        nu_matl=None,
                        part_id=None,
                        nu_part=None,
                    ),
                    PyrolysisProduct(
                        spec_id=None,
                        nu_spec=None,
                        heat_of_combustion=None,
                        matl_id="CHAR",
                        nu_matl=0.2,
                        part_id=None,
                        nu_part=None,
                    ),
                ],
            )
        )
        .add_reaction(
            PyrolysisReaction(
                a=3.0e9,
                e=1.2e5,
                heat_of_reaction=600,
                reference_temperature=None,
                pyrolysis_range=None,
                heating_rate=5.0,
                n_s=1.0,
                n_t=0.0,
                n_o2=0.0,
                gas_diffusion_depth=None,
                max_reaction_rate=None,
                products=[
                    PyrolysisProduct(
                        spec_id="HEMI_GAS",
                        nu_spec=0.75,
                        heat_of_combustion=None,
                        matl_id=None,
                        nu_matl=None,
                        part_id=None,
                        nu_part=None,
                    ),
                    PyrolysisProduct(
                        spec_id=None,
                        nu_spec=None,
                        heat_of_combustion=None,
                        matl_id="CHAR",
                        nu_matl=0.25,
                        part_id=None,
                        nu_part=None,
                    ),
                ],
            )
        )
        .add_reaction(
            PyrolysisReaction(
                a=5.0e8,
                e=1.8e5,
                heat_of_reaction=400,
                reference_temperature=None,
                pyrolysis_range=None,
                heating_rate=5.0,
                n_s=1.0,
                n_t=0.0,
                n_o2=0.0,
                gas_diffusion_depth=None,
                max_reaction_rate=None,
                products=[
                    PyrolysisProduct(
                        spec_id="LIGNIN_GAS",
                        nu_spec=0.6,
                        heat_of_combustion=None,
                        matl_id=None,
                        nu_matl=None,
                        part_id=None,
                        nu_part=None,
                    ),
                    PyrolysisProduct(
                        spec_id=None,
                        nu_spec=None,
                        heat_of_combustion=None,
                        matl_id="CHAR",
                        nu_matl=0.4,
                        part_id=None,
                        nu_part=None,
                    ),
                ],
            )
        )
        .build()
    )

    # Create char material
    char = (
        MaterialBuilder("CHAR").density(300).thermal_conductivity(0.05).specific_heat(1.2).build()
    )

    # Create surface with pyrolysis
    wood_surface = (
        SurfBuilder("WOOD_SURFACE")
        .with_material("WOOD", thickness=0.012)
        .with_backing("EXPOSED")
        .build()
    )

    # Add to simulation
    sim.add_material(wood)
    sim.add_material(char)
    sim.add_surface(wood_surface)

    # ... rest of simulation setup

    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)
    sim.write(output_dir / "structured_pyrolysis.fds")

    print("Structured pyrolysis example written successfully")


if __name__ == "__main__":
    main()
