#!/usr/bin/env python3
"""
02_custom_fuel.py

Example demonstrating custom fuel species definition and usage.

This example shows:
- Defining custom fuel species with chemical formulas
- Setting molecular weights and elemental composition
- Custom reaction stoichiometry
- Heat of combustion calculations

Run with: python examples/species/02_custom_fuel.py
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Species
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.reac import Reaction
from pyfds.core.namelists.time import Time
from pyfds.core.namelists.vent import Vent


def main():
    """Demonstrate custom fuel species."""

    print("PyFDS Chemical Species Example 02: Custom Fuel")
    print("=" * 48)

    # Define custom fuel: Ethanol (C2H5OH)
    print("\n1. Defining Custom Fuel: Ethanol (C2H5OH)")
    ethanol = Species(
        id="ETHANOL",
        formula="C2H5OH",  # Chemical formula
        mw=46.069,  # Molecular weight [g/mol]
        c=2,
        h=6,
        o=1,  # Elemental composition (atoms per molecule)
        mass_fraction_0=0.0,  # Not present initially
    )

    print(f"   Formula: {ethanol.formula}")
    print(f"   Molecular weight: {ethanol.mw} g/mol")
    print(f"   Elemental composition: C{ethanol.c}H{ethanol.h}O{ethanol.o}")

    # Calculate theoretical air-fuel ratio
    # C2H5OH + 3O2 -> 2CO2 + 3H2O
    # MW: 46.069 + 3*31.998 = 46.069 + 95.994 = 142.063
    # Air is ~21% O2, 79% N2 by volume
    # Air molecular weight ≈ 28.97 g/mol
    # Stoichiometric AFR = (142.063 / 46.069) * (100/21) * (28.97/31.998) ≈ 9.0
    print(".1f")

    # Create simulation
    print("\n2. Creating Simulation with Custom Fuel:")
    sim = Simulation(chid="custom_fuel_example")

    # Set up basic simulation parameters
    sim.add(Time(t_end=30.0))
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

    # Add species
    print("   - Adding ethanol fuel")
    sim.add(ethanol)

    # Add air components (simplified - using predefined species)
    print("   - Adding air components")
    sim.add(Species(id="OXYGEN", mass_fraction_0=0.23))
    sim.add(Species(id="NITROGEN", mass_fraction_0=0.77))

    # Define combustion reaction for ethanol
    print("   - Setting up ethanol combustion reaction")
    # C2H5OH + 3O2 -> 2CO2 + 3H2O
    sim.add(
        Reaction(
            fuel="ETHANOL",
            heat_of_combustion=26700,  # kJ/kg (experimental value for ethanol)
            c=2,
            h=6,
            o=1,  # Fuel composition
            soot_yield=0.0,  # Ethanol produces negligible soot
            co_yield=0.001,  # Small CO yield
            # Product species stoichiometry
            spec_id_nu=["ETHANOL", "OXYGEN", "CARBON_DIOXIDE", "WATER_VAPOR"],
            nu=[-1, -3, 2, 3],  # Stoichiometric coefficients
        )
    )

    # Add fire source
    print("   - Adding ethanol fuel source")
    sim.add(Vent(id="FUEL_SOURCE", xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0, 0), surface="burner"))

    # Add monitoring devices
    print("   - Adding species concentration monitors")
    sim.add(Device(id="FUEL_CONC", xyz=Point3D.of(1.0, 1.0, 0.5), quantity="MASS FRACTION ETHANOL"))

    sim.add(Device(id="O2_CONC", xyz=Point3D.of(1.0, 1.0, 0.5), quantity="VOLUME FRACTION OXYGEN"))

    sim.add(
        Device(
            id="CO2_CONC", xyz=Point3D.of(1.0, 1.0, 0.5), quantity="VOLUME FRACTION CARBON DIOXIDE"
        )
    )
    sim.add(Device(id="TEMP", xyz=Point3D.of(1.0, 1.0, 0.5), quantity="TEMPERATURE"))

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
    output_file = Path("/home/gbellamy/git/pyfds/custom_fuel_example.fds")
    with output_file.open("w") as f:
        f.write(fds_content)

    print(f"\n5. FDS file saved to: {output_file}")
    print("   Run with: fds custom_fuel_example.fds")

    print("\n6. Expected Results:")
    print("   - Ethanol fuel concentration decreases as it burns")
    print("   - Oxygen concentration decreases, CO2 increases")
    print("   - Water vapor production from combustion")
    print("   - Heat release from ethanol combustion")

    # Show reaction stoichiometry
    print("\n7. Reaction Stoichiometry:")
    print("   C₂H₅OH + 3O₂ → 2CO₂ + 3H₂O")
    print("   (Ethanol + Oxygen → Carbon Dioxide + Water)")
    print(".1f")
    print(".1f")

    print("\nExample completed successfully!")


if __name__ == "__main__":
    main()
