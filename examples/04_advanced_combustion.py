"""
Advanced Combustion Modeling Example
=====================================

This example demonstrates advanced combustion features:
- Custom fuel composition
- Extinction modeling
- Suppression modeling
- Species stoichiometry tracking
- Chemical time scales
- Non-ideal heat of combustion

This showcases the advanced combustion features added in Stage 1.3 (REAC enhancements).
"""

from pyfds.builders import DevcBuilder, MeshBuilder, ReactionBuilder, SurfBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Time
from pyfds.core.simulation import Simulation


def create_advanced_combustion_fire():
    """
    Create a fire simulation with advanced combustion modeling.

    Features:
    - Custom fuel mixture
    - Extinction model EXTINCTION_1
    - Suppression modeling
    - Species tracking
    - Chemical time scale control
    """
    # Create simulation with metadata
    sim = Simulation(
        chid="advanced_combustion", title="Advanced Combustion Modeling - PyFDS Stage 1 Example"
    )
    sim.add(Time(t_end=400.0))

    # Computational mesh
    mesh = (
        MeshBuilder()
        .with_id("BURN_ROOM")
        .with_bounds(Bounds3D(0, 6, 0, 6, 0, 3))
        .with_grid(Grid3D(60, 60, 30))
        .with_stability_control(cfl_max=0.95)
        .build()
    )
    sim.add(mesh)

    # Advanced reaction with all Stage 1.3 features
    reaction = (
        ReactionBuilder()
        .custom_fuel(
            c=7,  # Heptane-like fuel
            h=16,
            o=0,
            n=0,
            heat_of_combustion=44600,  # kJ/kg
        )
        .use_non_ideal_hoc()  # Use non-ideal heat of combustion
        .with_extinction(
            model="EXTINCTION_1",
            critical_temp=1150.0,  # Critical flame temperature in K
        )
        .with_suppression(k_suppression=0.3)
        .with_species_stoichiometry(
            species=["CO2", "H2O", "CO"],
            coefficients=[7.0, 8.0, 0.1],  # Stoichiometric coefficients
        )
        .with_time_scales(
            tau_chem=0.15,  # Chemical time scale (s)
            tau_flame=0.6,  # Flame time scale (s)
        )
        .radiative_fraction(0.33)
        .yields(soot=0.037, co=0.01)
        .auto_ignition_temperature(220.0)
        .build()
    )
    sim.add(reaction)

    # Fire surface with mass flux
    fire_surf = (
        SurfBuilder("LIQUID_POOL")
        .with_mass_flux(0.02)  # kg/s/m²
        .with_ignition(temperature=220.0, burn_away=False)
        .with_radiation(emissivity=0.95, absorptivity=0.90)
        .with_heat_of_combustion(44600.0)
        .build()
    )
    sim.add(fire_surf)

    # Temperature monitoring - detailed grid
    x_positions = [1.5, 3.0, 4.5]
    y_positions = [1.5, 3.0, 4.5]
    z_positions = [0.5, 1.5, 2.5]

    for x in x_positions:
        for y in y_positions:
            for z in z_positions:
                temp_id = f"T_X{int(x * 10):02d}_Y{int(y * 10):02d}_Z{int(z * 10):02d}"
                temp_sensor = (
                    DevcBuilder(temp_id)
                    .with_quantity("TEMPERATURE")
                    .at_point(Point3D(x, y, z))
                    .build()
                )
                sim.add(temp_sensor)

    # Species concentration measurements (CO2, O2, CO)
    species_quantities = ["VOLUME FRACTION CO2", "VOLUME FRACTION O2", "VOLUME FRACTION CO"]
    measurement_point = Point3D(3.0, 3.0, 1.5)

    for species in species_quantities:
        species_name = species.replace("VOLUME FRACTION ", "").replace(" ", "_")
        species_sensor = (
            DevcBuilder(f"SPECIES_{species_name}")
            .with_quantity(species)
            .at_point(measurement_point)
            .with_time_history(True)
            .build()
        )
        sim.add(species_sensor)

    # Heat release rate measurement
    hrr_sensor = (
        DevcBuilder("HRR_TOTAL")
        .with_quantity("HRR")
        .in_bounds(Bounds3D(0, 6, 0, 6, 0, 3))
        .with_statistics("SUM")
        .build()
    )
    sim.add(hrr_sensor)

    # Extinction indicator - monitors flame temperature
    flame_temp = (
        DevcBuilder("FLAME_TEMP")
        .with_quantity("TEMPERATURE")
        .at_point(Point3D(3.0, 3.0, 0.5))
        .with_time_history(True)
        .build()
    )
    sim.add(flame_temp)

    # Average upper layer temperature
    upper_layer = (
        DevcBuilder("UPPER_LAYER_TEMP")
        .with_quantity("TEMPERATURE")
        .with_statistics("MEAN", start_time=10.0)
        .in_bounds(Bounds3D(0, 6, 0, 6, 2.0, 3.0))
        .build()
    )
    sim.add(upper_layer)

    # Suppression effectiveness monitor
    suppression_monitor = (
        DevcBuilder("SUPPRESSION_RATE")
        .with_quantity("SUPPRESSION")
        .at_point(Point3D(3.0, 3.0, 1.0))
        .with_time_history(True)
        .build()
    )
    sim.add(suppression_monitor)

    return sim


if __name__ == "__main__":
    simulation = create_advanced_combustion_fire()
    simulation.write("advanced_combustion.fds")

    print("Advanced combustion simulation created!")
    print("Output file: advanced_combustion.fds")
    print("\nFuel properties:")
    print("  - Custom hydrocarbon: C7H16 (heptane-like)")
    print("  - Heat of combustion: 44,600 kJ/kg (non-ideal)")
    print("  - Auto-ignition temp: 220°C")
    print("  - Soot yield: 0.037, CO yield: 0.01")
    print("\nCombustion modeling:")
    print("  - Extinction model: EXTINCTION_1")
    print("  - Critical flame temperature: 1150 K")
    print("  - Suppression enabled (k = 0.3)")
    print("  - Chemical time scale: 0.15 s")
    print("  - Flame time scale: 0.6 s")
    print("\nSpecies tracking:")
    print("  - CO2 (stoich coeff: 7.0)")
    print("  - H2O (stoich coeff: 8.0)")
    print("  - CO (stoich coeff: 0.1)")
    print("\nFire source:")
    print("  - Type: Liquid pool")
    print("  - Mass flux: 0.02 kg/s/m²")
    print("  - Emissivity: 0.95, Absorptivity: 0.90")
    print("\nMonitoring:")
    print("  - 27 temperature sensors (3D grid)")
    print("  - 3 species concentration sensors (CO2, O2, CO)")
    print("  - Total HRR measurement")
    print("  - Flame temperature (extinction indicator)")
    print("  - Suppression rate monitor")
