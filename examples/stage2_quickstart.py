"""
Stage 2 Particle Systems - Quickstart Examples
===============================================

Simple, focused examples of each Stage 2 feature.
"""

from pyfds.builders import MaterialBuilder, PartBuilder, PropBuilder, SurfBuilder


def example_1_water_droplets():
    """Create water droplet particles for sprinkler spray."""
    print("Example 1: Water Droplets")
    print("-" * 50)

    # Simple water droplet
    water = (
        PartBuilder("WATER_DROP")
        .as_water_droplet(diameter=0.001)  # 1mm droplets
        .with_color("BLUE")
        .build()
    )

    print(water.to_fds())
    print()


def example_2_smoke_particles():
    """Create smoke particles for fire visualization."""
    print("Example 2: Smoke Particles")
    print("-" * 50)

    # Smoke aerosol
    smoke = (
        PartBuilder("SMOKE")
        .as_aerosol(diameter=0.00001, spec_id="SOOT")
        .with_color("GRAY 50")
        .build()
    )

    print(smoke.to_fds())
    print()


def example_3_sprinkler_surface():
    """Create a sprinkler spray surface."""
    print("Example 3: Sprinkler Surface")
    print("-" * 50)

    # Sprinkler that generates water droplets
    sprinkler = (
        SurfBuilder("SPRINKLER_HEAD")
        .as_sprinkler(
            part_id="WATER_DROP",
            mass_flux=0.015,  # kg/s/m²
            median_diameter=0.001,  # 1mm median droplet
            velocity=5.0,  # 5 m/s downward
        )
        .build()
    )

    print(sprinkler.to_fds())
    print()


def example_4_fire_with_smoke():
    """Create a fire surface that generates smoke."""
    print("Example 4: Fire with Smoke Generation")
    print("-" * 50)

    # Burning surface producing smoke
    fire = (
        SurfBuilder("FIRE")
        .with_heat_release(1000.0, ramp_id="fire_ramp")
        .with_particle_generation("SMOKE", mass_flux=0.002, nppc=2)
        .with_color("ORANGE")
        .build()
    )

    print(fire.to_fds())
    print()


def example_5_sprinkler_property():
    """Create sprinkler device properties."""
    print("Example 5: Sprinkler Properties")
    print("-" * 50)

    # Quick-response sprinkler
    qr = PropBuilder.quick_response_sprinkler()
    print("Quick-response sprinkler:")
    print(qr.to_fds())
    print()

    # Standard-response sprinkler
    sr = PropBuilder.standard_response_sprinkler()
    print("Standard-response sprinkler:")
    print(sr.to_fds())
    print()

    # Custom sprinkler
    custom = PropBuilder.sprinkler(
        id="CUSTOM", activation_temp=74, rti=100, k_factor=80, pressure=200000
    )
    print("Custom sprinkler:")
    print(custom.to_fds())
    print()


def example_6_smoke_detector():
    """Create smoke detector properties."""
    print("Example 6: Smoke Detector")
    print("-" * 50)

    # Standard photoelectric detector
    detector = PropBuilder.smoke_detector(id="SMOKE_DET")
    print(detector.to_fds())
    print()


def example_7_pyrolysis_material():
    """Create material with pyrolysis."""
    print("Example 7: Material with Pyrolysis")
    print("-" * 50)

    # PMMA with single pyrolysis reaction
    pmma = (
        MaterialBuilder("PMMA")
        .density(1200)
        .thermal_conductivity(0.19)
        .specific_heat(1.4)
        .with_pyrolysis_product("MMA_VAPOR", yield_fraction=1.0)
        .with_heat_of_combustion(25000)
        .build()
    )

    print(pmma.to_fds())
    print()


def example_8_multi_reaction_pyrolysis():
    """Create material with multiple pyrolysis reactions."""
    print("Example 8: Multi-Reaction Pyrolysis")
    print("-" * 50)

    # Wood decomposing into vapor and char
    wood = (
        MaterialBuilder("WOOD")
        .density(500)
        .thermal_conductivity(0.13)
        .specific_heat(2.5)
        .add_pyrolysis_reaction(
            a=1e10, e=100000, heat_of_reaction=1800, product_species="WOOD_VAPOR"
        )
        .add_pyrolysis_reaction(a=5e8, e=120000, heat_of_reaction=500, residue_material="CHAR")
        .build()
    )

    print(wood.to_fds())
    print()


def example_9_predefined_materials():
    """Use predefined common materials."""
    print("Example 9: Predefined Materials")
    print("-" * 50)

    # Quick access to common materials
    concrete = MaterialBuilder.concrete()
    steel = MaterialBuilder.steel()
    wood = MaterialBuilder.wood()

    print("Concrete:")
    print(concrete.to_fds())
    print("\nSteel:")
    print(steel.to_fds())
    print("\nWood:")
    print(wood.to_fds())
    print()


def example_10_spray_distribution():
    """Create spray with custom droplet distribution."""
    print("Example 10: Spray with Droplet Distribution")
    print("-" * 50)

    # Nozzle with Gaussian spray pattern
    spray = (
        SurfBuilder("SPRAY_NOZZLE")
        .with_particle_generation("WATER_DROP", mass_flux=0.01, nppc=5)
        .with_droplet_distribution(median_diameter=0.0008, gamma_d=2.4, spray_pattern="GAUSSIAN")
        .with_particle_velocity((0.0, 0.0, -3.0))  # Downward at 3 m/s
        .build()
    )

    print(spray.to_fds())
    print()


def run_all_examples():
    """Run all quickstart examples."""
    examples = [
        example_1_water_droplets,
        example_2_smoke_particles,
        example_3_sprinkler_surface,
        example_4_fire_with_smoke,
        example_5_sprinkler_property,
        example_6_smoke_detector,
        example_7_pyrolysis_material,
        example_8_multi_reaction_pyrolysis,
        example_9_predefined_materials,
        example_10_spray_distribution,
    ]

    print("=" * 70)
    print("STAGE 2 PARTICLE SYSTEMS - QUICKSTART EXAMPLES")
    print("=" * 70)
    print()

    for example in examples:
        example()

    print("=" * 70)
    print("All examples complete!")
    print("=" * 70)


if __name__ == "__main__":
    run_all_examples()
