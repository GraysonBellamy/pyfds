#!/usr/bin/env python3
"""
03_lumped_species.py

Example demonstrating lumped species for modeling gas mixtures.

This example shows:
- Creating lumped species mixtures
- Component species definitions
- Volume/mass fraction calculations
- Background species with multiple components

Run with: python examples/species/03_lumped_species.py
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
    """Demonstrate lumped species usage."""

    print("PyFDS Chemical Species Example 03: Lumped Species")
    print("=" * 52)

    # Define component species for air mixture
    print("\n1. Defining Component Species for Air Mixture:")

    nitrogen_component = Species(
        id="N2_COMPONENT",
        lumped_component_only=True,  # Can only be used in lumped mixtures
        mass_fraction_0=0.0,
    )

    oxygen_component = Species(id="O2_COMPONENT", lumped_component_only=True, mass_fraction_0=0.0)

    argon_component = Species(id="AR_COMPONENT", lumped_component_only=True, mass_fraction_0=0.0)

    print("   Component species defined:")
    print("   - N2_COMPONENT (Nitrogen component)")
    print("   - O2_COMPONENT (Oxygen component)")
    print("   - AR_COMPONENT (Argon component)")
    print("   - All marked as lumped_component_only=True")

    # Define lumped air mixture
    print("\n2. Creating Lumped Air Mixture:")
    air_composition = {
        "N2_COMPONENT": 0.78084,  # 78.084% Nitrogen
        "O2_COMPONENT": 0.20946,  # 20.946% Oxygen
        "AR_COMPONENT": 0.00934,  # 0.934% Argon
    }

    # Verify fractions sum to 1.0
    print(".4f")
    print("   Composition:")
    for _component, _fraction in air_composition.items():
        print(".4f")

    lumped_air = Species(
        id="DRY_AIR",
        background=True,  # This is the ambient atmosphere
        spec_id=list(air_composition.keys()),
        volume_fraction=list(air_composition.values()),
    )

    print("   - Created lumped species 'DRY_AIR'")
    print("   - Marked as background=True (ambient atmosphere)")
    print("   - Using volume fractions for accurate mixing")

    # Create simulation
    print("\n3. Creating Simulation with Lumped Species:")
    sim = Simulation(chid="lumped_species_example")

    # Set up basic simulation parameters
    sim.add(Time(t_end=30.0))
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

    # Add component species
    print("   - Adding component species")
    sim.add(nitrogen_component)
    sim.add(oxygen_component)
    sim.add(argon_component)

    # Add lumped air mixture
    print("   - Adding lumped air mixture")
    sim.add(lumped_air)
    # Add fuel species
    print("   - Adding methane fuel")
    sim.add(Species(id="METHANE", mass_fraction_0=0.0))

    # Define combustion reaction
    print("   - Setting up methane combustion")
    sim.add(
        Reaction(
            fuel="METHANE",
            heat_of_combustion=50000,  # kJ/kg
            c=1,
            h=4,  # CH4 composition
            soot_yield=0.01,
            co_yield=0.005,
        )
    )

    # Add fire source
    print("   - Adding methane fuel source")
    sim.add(Vent(id="CH4_BURNER", xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0, 0), surface="burner"))

    # Add monitoring devices
    print("   - Adding species concentration monitors")
    sim.add(Device(id="CH4_CONC", xyz=Point3D.of(1.0, 1.0, 0.5), quantity="MASS FRACTION METHANE"))
    sim.add(
        Device(
            id="O2_CONC",
            xyz=Point3D.of(1.0, 1.0, 0.5),
            quantity="VOLUME FRACTION O2 COMPONENT",  # Monitor oxygen component
        )
    )
    sim.add(
        Device(
            id="N2_CONC",
            xyz=Point3D.of(1.0, 1.0, 0.5),
            quantity="VOLUME FRACTION N2 COMPONENT",  # Monitor nitrogen component
        )
    )

    sim.add(Device(id="TEMP", xyz=Point3D.of(1.0, 1.0, 0.5), quantity="TEMPERATURE"))

    # Generate FDS input
    print("\n4. Generated FDS Input:")
    fds_content = sim.to_fds()
    print(f"   FDS file length: {len(fds_content)} characters")

    # Show key sections
    print("\n5. Key FDS Sections:")
    lines = fds_content.split("\n")

    sections_to_show = ["&SPEC", "&REAC", "&DEVC"]
    for section in sections_to_show:
        section_lines = [line for line in lines if line.strip().startswith(section)]
        if section_lines:
            print(f"\n   {section} entries:")
            for line in section_lines[:4]:  # Show first 4 entries
                print(f"     {line.strip()}")
            if len(section_lines) > 4:
                print(f"     ... and {len(section_lines) - 4} more")

    # Save FDS file
    output_file = Path("/home/gbellamy/git/pyfds/lumped_species_example.fds")
    with output_file.open("w") as f:
        f.write(fds_content)

    print(f"\n6. FDS file saved to: {output_file}")
    print("   Run with: fds lumped_species_example.fds")

    print("\n7. Expected Results:")
    print("   - Methane fuel burns, concentration decreases")
    print("   - Oxygen component decreases as it's consumed")
    print("   - Nitrogen component remains relatively constant")
    print("   - Argon component tracks nitrogen (both inert)")
    print("   - Heat release from methane combustion")

    print("\n8. Lumped Species Benefits:")
    print("   - Accurate representation of gas mixtures")
    print("   - Individual component tracking")
    print("   - Proper stoichiometric calculations")
    print("   - Background atmosphere definition")

    print("\nExample completed successfully!")


if __name__ == "__main__":
    main()
