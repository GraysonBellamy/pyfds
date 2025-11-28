#!/usr/bin/env python3
"""
01_predefined_species.py

Example demonstrating the use of predefined species from PyFDS library.

This example shows:
- Using predefined species from the species library
- Creating standard air composition with humidity
- Basic combustion setup with predefined fuel
- Species concentration monitoring

Run with: python examples/species/01_predefined_species.py
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders.libraries import (
    create_standard_air,
    get_species_info,
    list_predefined_species,
)
from pyfds.core.namelists import Species


def main():
    """Demonstrate predefined species usage."""

    print("PyFDS Chemical Species Example 01: Predefined Species")
    print("=" * 55)

    # Show available predefined species
    print("\n1. Available Predefined Species:")
    species_list = list_predefined_species()
    print(f"   Total species available: {len(species_list)}")

    # Show some common species
    common_species = [
        "METHANE",
        "PROPANE",
        "OXYGEN",
        "NITROGEN",
        "CARBON_DIOXIDE",
        "WATER_VAPOR",
        "HYDROGEN",
    ]
    print("   Common species:")
    for species in common_species:
        info = get_species_info(species)
        print(f"   - {species}: {info['formula']} (MW: {info['mw']:.1f})")

    # Create simulation
    print("\n2. Creating Simulation with Predefined Species:")
    sim = Simulation(chid="predefined_species_example")

    # Set up basic simulation parameters
    sim.time(t_end=30.0)
    sim.mesh(ijk=(20, 20, 20), xb=(0, 2, 0, 2, 0, 2))

    # Create standard air with 40% humidity
    print("   - Creating standard air (40% humidity)")
    air_composition = create_standard_air(humidity=40.0)
    sim.add_species(Species(**air_composition))

    # Add propane fuel
    print("   - Adding propane fuel")
    propane_info = get_species_info("PROPANE")
    print(f"     Propane: {propane_info['formula']}, MW: {propane_info['mw']:.1f} g/mol")

    sim.species(id="PROPANE", mass_fraction_0=0.0)

    # Set up combustion reaction
    print("   - Setting up propane combustion")
    sim.reaction(
        fuel="PROPANE",
        heat_of_combustion=46300,  # kJ/kg (typical value for propane)
        soot_yield=0.01,  # 1% soot yield
        co_yield=0.005,  # 0.5% CO yield
    )

    # Add fire source
    print("   - Adding fire source")
    sim.vent(
        id="BURNER",
        xb=(0.8, 1.2, 0.8, 1.2, 0, 0),  # 0.4m x 0.4m burner
        surface="burner",
    )

    # Add species concentration devices
    print("   - Adding species concentration monitors")
    sim.device(id="O2_SENSOR", xyz=(1.0, 1.0, 0.5), quantity="VOLUME FRACTION OXYGEN")

    sim.device(id="CO2_SENSOR", xyz=(1.0, 1.0, 0.5), quantity="VOLUME FRACTION CARBON DIOXIDE")

    sim.device(id="TEMP_SENSOR", xyz=(1.0, 1.0, 0.5), quantity="TEMPERATURE")

    # Generate FDS input
    print("\n3. Generated FDS Input:")
    fds_content = sim.to_fds()
    print(f"   FDS file length: {len(fds_content)} characters")

    # Show key sections
    print("\n4. Key FDS Sections:")
    lines = fds_content.split("\n")

    sections_to_show = ["&SPEC", "&REAC", "&DEVC"]
    for section in sections_to_show:
        section_lines = [line for line in lines if line.strip().startswith(section)]
        if section_lines:
            print(f"\n   {section} entries:")
            for line in section_lines[:3]:  # Show first 3 entries
                print(f"     {line.strip()}")
            if len(section_lines) > 3:
                print(f"     ... and {len(section_lines) - 3} more")

    # Save FDS file
    output_file = Path("/home/gbellamy/git/pyfds/predefined_species_example.fds")
    with output_file.open("w") as f:
        f.write(fds_content)

    print(f"\n5. FDS file saved to: {output_file}")
    print("   Run with: fds predefined_species_example.fds")

    print("\n6. Expected Results:")
    print("   - Oxygen concentration should decrease near fire")
    print("   - CO2 concentration should increase near fire")
    print("   - Temperature should rise significantly")
    print("   - Species conservation should be maintained")

    print("\nExample completed successfully!")


if __name__ == "__main__":
    main()
