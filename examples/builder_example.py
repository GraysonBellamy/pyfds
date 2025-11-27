"""
Example demonstrating the builder pattern API for creating FDS simulations.

This example shows how to use builders to create a complete fire simulation
with materials, reactions, ramps, controls, and vents using a fluent API.
"""

from pyfds import Simulation
from pyfds.builders import (
    ControlBuilder,
    MaterialBuilder,
    PropBuilder,
    RampBuilder,
    ReactionBuilder,
    VentBuilder,
)
from pyfds.builders.libraries import CommonMaterials


def main():
    """Create a complete apartment fire simulation using builders."""
    # Create simulation
    print("Creating simulation...")
    sim = Simulation("apartment_fire", title="Apartment Fire with Builders")

    # Time and mesh
    sim.time(t_end=600.0)
    sim.mesh(ijk=(100, 100, 50), xb=(0, 10, 0, 10, 0, 5))

    # ========================================================================
    # Combustion Reaction (using builder)
    # ========================================================================
    print("Adding combustion reaction...")
    reaction = (
        ReactionBuilder()
        .fuel("POLYURETHANE")  # Use predefined fuel from database
        .yields(soot=0.10, co=0.02)
        .radiative_fraction(0.30)
        .build()
    )
    sim.add_reaction(reaction)

    # ========================================================================
    # Fire Growth Ramp (t-squared)
    # ========================================================================
    print("Adding fire growth ramp...")
    fire_ramp = RampBuilder("HRR_GROWTH").t_squared("FAST", peak_hrr=2500, t_peak=300).build()
    sim.add_ramp(fire_ramp)

    # ========================================================================
    # Temperature-Dependent Material Properties
    # ========================================================================
    print("Adding temperature-dependent steel...")

    # Create temperature ramp for thermal conductivity
    steel_k_ramp = (
        RampBuilder("STEEL_K")
        .temperature_table({20: 45.8, 100: 43.3, 200: 40.7, 400: 36.4, 600: 31.0})
        .build()
    )
    sim.add_ramp(steel_k_ramp)

    # Create steel material using the ramp
    steel = (
        MaterialBuilder("STEEL")
        .density(7850)
        .thermal_conductivity_ramp("STEEL_K")  # Reference the ramp
        .specific_heat(0.46)
        .emissivity(0.7)
        .build()
    )
    sim.add_material(steel)

    # ========================================================================
    # Pyrolysis Material (multi-reaction)
    # ========================================================================
    print("Adding complex pyrolysis material...")
    foam = (
        MaterialBuilder("FOAM")
        .density(40)
        .thermal_conductivity(0.04)
        .specific_heat(1.5)
        .add_pyrolysis_reaction(
            a=1e10, e=80000, heat_of_reaction=1000, product_species="FUEL_VAPOR"
        )
        .add_pyrolysis_reaction(a=5e8, e=120000, heat_of_reaction=1500, residue_material="CHAR")
        .build()
    )
    sim.add_material(foam)

    # ========================================================================
    # Use Predefined Materials from Library
    # ========================================================================
    print("Adding predefined materials...")
    concrete = CommonMaterials.concrete()
    gypsum = CommonMaterials.gypsum()
    sim.add_material(concrete)
    sim.add_material(gypsum)

    # ========================================================================
    # Vents (using factory methods)
    # ========================================================================
    print("Adding vents...")

    # Door using convenience method
    door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, z_min=0.0, z_max=2.1, id="DOOR_1")
    sim.add_vent(door)

    # Window
    window = VentBuilder.window(x=0.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5, id="WINDOW_1")
    sim.add_vent(window)

    # HVAC supply
    supply = VentBuilder.hvac_supply(xb=(5, 6, 5, 6, 3, 3), volume_flow=0.5, id="SUPPLY_1")
    sim.add_vent(supply)

    # HVAC exhaust
    exhaust = VentBuilder.hvac_exhaust(xb=(0, 1, 0, 1, 3, 3), volume_flow=0.3, id="EXHAUST_1")
    sim.add_vent(exhaust)

    # Circular burner
    burner = VentBuilder.circular_burner(center=(5, 5, 0), radius=0.5, surf_id="FIRE", id="BURNER")
    sim.add_vent(burner)

    # ========================================================================
    # Device Properties (using factory methods)
    # ========================================================================
    print("Adding device properties...")

    # Sprinkler using predefined quick-response
    sprinkler = PropBuilder.quick_response_sprinkler(id="SPRINKLER_QR")
    sim.add_prop(sprinkler)

    # Smoke detector
    smoke_det = PropBuilder.smoke_detector(id="SMOKE_DET", activation_obscuration=3.28)
    sim.add_prop(smoke_det)

    # Heat detector
    heat_det = PropBuilder.heat_detector(id="HEAT_DET", activation_temp=74, rti=5.0)
    sim.add_prop(heat_det)

    # ========================================================================
    # Control Logic
    # ========================================================================
    print("Adding control logic...")

    # ANY logic - alarm if any detector activates
    alarm = ControlBuilder("SMOKE_ALARM").any(["SMOKE_DET_1", "SMOKE_DET_2", "SMOKE_DET_3"]).build()
    sim.add_ctrl(alarm)

    # Time-delayed sprinkler activation
    sprinkler_ctrl = (
        ControlBuilder("DELAYED_SPRINKLER")
        .time_delay("HEAT_DET", delay=10.0)
        .with_latch(True)
        .build()
    )
    sim.add_ctrl(sprinkler_ctrl)

    # ========================================================================
    # Add Surfaces and Obstructions
    # ========================================================================
    print("Adding surfaces and obstructions...")

    # Fire surface
    sim.surface(id="FIRE", hrrpua=1000.0, color="RED")

    # Wall surface with material
    sim.surface(id="WALL", matl_id="GYPSUM", thickness=0.012, color="GRAY")

    # Burner obstruction
    sim.obstruction(xb=(4.5, 5.5, 4.5, 5.5, 0, 0.1), surf_id="FIRE")

    # Wall obstructions
    sim.obstruction(xb=(0, 0, 0, 10, 0, 3), surf_id="WALL")
    sim.obstruction(xb=(10, 10, 0, 10, 0, 3), surf_id="WALL")

    # ========================================================================
    # Add Devices
    # ========================================================================
    print("Adding measurement devices...")

    # Temperature measurements
    sim.device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=(5, 5, 2.5))
    sim.device(id="TEMP_CORNER", quantity="TEMPERATURE", xyz=(1, 1, 2.5))

    # HRR measurement
    sim.device(id="HRR", quantity="HRR", xb=(0, 10, 0, 10, 0, 5))

    # ========================================================================
    # Write FDS File
    # ========================================================================
    print("\nWriting FDS file...")
    fds_file = sim.write("apartment_fire.fds")
    print(f"✅ FDS file written to: {fds_file}")

    # ========================================================================
    # Validation
    # ========================================================================
    print("\nValidating simulation...")
    warnings = sim.validate()
    if warnings:
        print(f"⚠️  Found {len(warnings)} warning(s):")
        for warning in warnings:
            print(f"   - {warning}")
    else:
        print("✅ Validation passed!")

    # ========================================================================
    # Summary
    # ========================================================================
    print("\n" + "=" * 70)
    print("SIMULATION SUMMARY")
    print("=" * 70)
    print(f"CHID: {sim.chid}")
    print(f"Title: {sim.head.title}")
    print(f"Meshes: {len(sim.geometry.meshes)}")
    print(f"Materials: {len(sim.material_mgr.materials)}")
    print(f"Ramps: {len(sim.ramps.ramps)}")
    print(f"Surfaces: {len(sim.material_mgr.surfaces)}")
    print(f"Obstructions: {len(sim.geometry.obstructions)}")
    print(f"Vents: {len(sim.geometry.vents)}")
    print(f"Devices: {len(sim.instrumentation.devices)}")
    print(f"Controls: {len(sim.controls.ctrls)}")
    print(f"Props: {len(sim.props)}")
    print("=" * 70)

    # Show available fuels
    print("\n" + "=" * 70)
    print("AVAILABLE FUELS FROM DATABASE")
    print("=" * 70)
    fuels = ReactionBuilder.list_fuels()
    for i, fuel in enumerate(fuels, 1):
        info = ReactionBuilder.get_fuel_info(fuel)
        print(f"{i:2d}. {fuel:20s} - HOC: {info['hoc']:6.0f} kJ/kg, Soot: {info['soot_yield']:.3f}")
    print("=" * 70)


if __name__ == "__main__":
    main()
