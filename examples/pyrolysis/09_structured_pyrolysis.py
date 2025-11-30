#!/usr/bin/env python3
"""
Structured Pyrolysis Example

Demonstrates the structured pyrolysis API with complex
multi-step material decomposition reactions.
"""

from pathlib import Path

from pyfds import Simulation, Surface
from pyfds.builders import MaterialBuilder
from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction


def main():
    """Create structured pyrolysis simulation."""

    sim = Simulation(chid="structured_pyrolysis", title="Structured Pyrolysis Example")

    # Create material with structured pyrolysis reactions
    # This demonstrates three-reaction wood decomposition model
    wood = (
        MaterialBuilder("WOOD")
        .density(500)
        .thermal_conductivity(0.13)
        .specific_heat(2.5)
        # Reaction 1: Cellulose decomposition (Arrhenius kinetics)
        .add_reaction(
            PyrolysisReaction(
                a=1.0e10,  # pre-exponential factor (1/s)
                e=1.5e5,  # activation energy (J/mol)
                heat_of_reaction=500,  # kJ/kg
                products=[
                    PyrolysisProduct(spec_id="CELLULOSE_GAS", nu_spec=0.8),
                    PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
                ],
            )
        )
        # Reaction 2: Hemicellulose decomposition (simplified with REFERENCE_TEMPERATURE)
        .add_reaction(
            PyrolysisReaction(
                heat_of_reaction=600,
                reference_temperature=280,  # °C - peak reaction temperature
                pyrolysis_range=60,  # °C - width of reaction
                products=[
                    PyrolysisProduct(spec_id="HEMI_GAS", nu_spec=0.75),
                    PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
                ],
            )
        )
        # Reaction 3: Lignin decomposition (simplified with REFERENCE_RATE)
        .add_reaction(
            PyrolysisReaction(
                heat_of_reaction=400,
                reference_temperature=350,  # °C
                reference_rate=0.001,  # 1/s - normalized mass loss rate
                heating_rate=5.0,  # K/min - TGA heating rate
                products=[
                    PyrolysisProduct(spec_id="LIGNIN_GAS", nu_spec=0.6),
                    PyrolysisProduct(matl_id="CHAR", nu_matl=0.4),
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
    wood_surface = Surface(id="WOOD_SURFACE", matl_id="WOOD", thickness=0.012, backing="EXPOSED")

    # Add to simulation
    sim.add(wood)
    sim.add(char)
    sim.add(wood_surface)

    # ... rest of simulation setup

    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)
    sim.write(output_dir / "structured_pyrolysis.fds")

    print("Structured pyrolysis example written successfully")


if __name__ == "__main__":
    main()
