#!/usr/bin/env python3
"""
04_finite_rate.py

Example demonstrating finite-rate chemical kinetics in PyFDS.

This example shows:
- Arrhenius reaction parameters
- Concentration exponents and reaction orders
- Complex reaction equations
- Finite-rate combustion modeling

Run with: python examples/species/04_finite_rate.py
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.core.namelists import Species


def main():
    """Demonstrate finite-rate chemical kinetics."""

    print("PyFDS Chemical Species Example 04: Finite-Rate Chemistry")
    print("=" * 58)

    # Define species for hydrogen combustion
    print("\n1. Defining Species for Hydrogen Combustion:")

    hydrogen = Species(id="HYDROGEN", formula="H2", mw=2.016, mass_fraction_0=0.0)

    oxygen = Species(id="OXYGEN", formula="O2", mw=31.999, mass_fraction_0=0.23)

    water_vapor = Species(id="WATER_VAPOR", formula="H2O", mw=18.015, mass_fraction_0=0.0)

    nitrogen = Species(id="NITROGEN", formula="N2", mw=28.013, mass_fraction_0=0.77)

    print("   Species defined:")
    print("   - HYDROGEN (H₂): Fuel")
    print("   - OXYGEN (O₂): Oxidizer")
    print("   - WATER_VAPOR (H₂O): Product")
    print("   - NITROGEN (N₂): Inert diluent")

    # Create simulation
    print("\n2. Creating Simulation with Finite-Rate Chemistry:")
    sim = Simulation(chid="finite_rate_example")

    # Set up basic simulation parameters
    sim.time(t_end=10.0)  # Shorter time for fast chemistry
    sim.mesh(ijk=(20, 20, 20), xb=(0, 1, 0, 1, 0, 1))

    # Add species
    print("   - Adding species to simulation")
    sim.add_species(hydrogen)
    sim.add_species(oxygen)
    sim.add_species(water_vapor)
    sim.add_species(nitrogen)

    # Define finite-rate reaction: H2 + O2 -> 2OH (chain initiation)
    # This is a simplified reaction for demonstration
    print("   - Setting up finite-rate reaction")
    print("   - Reaction: H₂ + ½O₂ → H₂O")
    print("   - Using Arrhenius kinetics")

    sim.reaction(
        fuel="HYDROGEN",
        # Basic stoichiometry
        c=0,
        h=2,
        o=0,  # H2 composition
        spec_id_nu=["HYDROGEN", "OXYGEN", "WATER_VAPOR"],
        nu=[-1, -0.5, 1],  # H2 + 0.5O2 -> H2O
        # Finite-rate parameters
        a=1e12,  # Pre-exponential factor [cm³/mol·s]
        e=30000,  # Activation energy [J/mol]
        n_t=0.0,  # Temperature exponent
        # Concentration exponents
        spec_id_n_s=["HYDROGEN", "OXYGEN"],
        n_s=[1.0, 0.5],  # Reaction orders: first order in H2, half order in O2
        # Heat release
        heat_of_combustion=120000,  # kJ/kg for H2
        # Reaction priority
        priority=1,
    )

    # Set combustion parameters for finite-rate chemistry
    print("   - Configuring combustion model")
    sim.combustion(
        finite_rate_min_temp=300.0,  # Minimum temperature for reaction [°C]
        zz_min_global=1e-10,  # Species threshold
        initial_unmixed_fraction=0.1,  # Some mixing
    )

    # Add fuel injection source
    print("   - Adding hydrogen fuel injection")
    sim.vent(
        id="H2_INLET",
        xb=(0.0, 0.0, 0.4, 0.6, 0.4, 0.6),  # Small inlet on left wall
        surface="injector",  # Need to define this surface
    )

    # Define injector surface for hydrogen injection
    sim.surface(
        id="injector",
        mass_flux=(0.01,),  # kg/m²/s hydrogen injection
        species="HYDROGEN",
    )

    # Add monitoring devices
    print("   - Adding reaction monitoring devices")
    sim.device(id="H2_CONC", xyz=(0.5, 0.5, 0.5), quantity="MASS FRACTION HYDROGEN")

    sim.device(id="O2_CONC", xyz=(0.5, 0.5, 0.5), quantity="VOLUME FRACTION OXYGEN")

    sim.device(id="H2O_CONC", xyz=(0.5, 0.5, 0.5), quantity="MASS FRACTION WATER VAPOR")

    sim.device(id="TEMP", xyz=(0.5, 0.5, 0.5), quantity="TEMPERATURE")

    # Add reaction rate monitoring (if available)
    sim.device(
        id="HRR",
        xyz=(0.5, 0.5, 0.5),
        quantity="HRRPUV",  # Heat release rate per unit volume
    )

    # Generate FDS input
    print("\n3. Generated FDS Input:")
    fds_content = sim.to_fds()
    print(f"   FDS file length: {len(fds_content)} characters")

    # Show key sections
    print("\n4. Key FDS Sections:")
    lines = fds_content.split("\n")

    sections_to_show = ["&SPEC", "&REAC", "&COMB", "&SURF", "&DEVC"]
    for section in sections_to_show:
        section_lines = [line for line in lines if line.strip().startswith(section)]
        if section_lines:
            print(f"\n   {section} entries:")
            for line in section_lines[:3]:  # Show first 3 entries
                print(f"     {line.strip()}")
            if len(section_lines) > 3:
                print(f"     ... and {len(section_lines) - 3} more")

    # Save FDS file
    output_file = Path("/home/gbellamy/git/pyfds/finite_rate_example.fds")
    with output_file.open("w") as f:
        f.write(fds_content)

    print(f"\n5. FDS file saved to: {output_file}")
    print("   Run with: fds finite_rate_example.fds")

    print("\n6. Expected Results:")
    print("   - Hydrogen injection creates concentration gradients")
    print("   - Chemical reaction consumes H₂ and O₂")
    print("   - Water vapor production from combustion")
    print("   - Heat release and temperature increase")
    print("   - Reaction rates depend on local concentrations and temperature")

    print("\n7. Finite-Rate Chemistry Features:")
    print("   - Arrhenius rate expression: k = A * T^n_t * exp(-E/RT)")
    print("   - Concentration dependence: rate ∝ [H₂]^1.0 * [O₂]^0.5")
    print("   - Temperature threshold prevents reaction at low T")
    print("   - Detailed reaction kinetics instead of equilibrium")

    print("\n8. Notes:")
    print("   - This is a simplified reaction for demonstration")
    print("   - Real hydrogen combustion involves multiple reaction steps")
    print("   - Reaction rates are highly temperature-dependent")
    print("   - Finite-rate chemistry is computationally expensive")

    print("\nExample completed successfully!")


if __name__ == "__main__":
    main()
