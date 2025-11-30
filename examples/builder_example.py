"""
Example demonstrating the builder pattern API for creating FDS simulations.

This example shows how to use builders to create a complete fire simulation
with materials, reactions, ramps, controls, and vents using a fluent API.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders import (
    ControlBuilder,
    MaterialBuilder,
    PropBuilder,
    RampBuilder,
    ReactionBuilder,
)
from pyfds.builders.libraries import CommonMaterials
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Device, Obstruction, Surface
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.time import Time


def main():
    """Create a complete apartment fire simulation using builders."""
    # Create simulation
    print("Creating simulation...")
    sim = Simulation("apartment_fire", title="Apartment Fire with Builders")

    # Time and mesh
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(100, 100, 50), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

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
    sim.add(reaction)

    # ========================================================================
    # Fire Growth Ramp (t-squared)
    # ========================================================================
    print("Adding fire growth ramp...")
    fire_ramp = RampBuilder("HRR_GROWTH").t_squared("FAST", peak_hrr=2500, t_peak=300).build()
    sim.add(fire_ramp)

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
    sim.add(steel_k_ramp)

    # Create steel material using the ramp
    steel = (
        MaterialBuilder("STEEL")
        .density(7850)
        .thermal_conductivity_ramp("STEEL_K")  # Reference the ramp
        .specific_heat(0.46)
        .emissivity(0.7)
        .build()
    )
    sim.add(steel)

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
    sim.add(foam)

    # ========================================================================
    # Use Predefined Materials from Library
    # ========================================================================
    print("Adding predefined materials...")
    concrete = CommonMaterials.concrete()
    gypsum = CommonMaterials.gypsum()
    sim.add(concrete)
    sim.add(gypsum)

    # ========================================================================
    # Vents (using factory methods)
    # ========================================================================
    print("Adding vents...")

    # Door using convenience method (on X=10 boundary)
    from pyfds.core.namelists import Vent

    door = Vent(xb=Bounds3D.of(10.0, 10.0, 4.0, 6.0, 0.0, 2.1), surf_id="OPEN", id="DOOR_1")
    sim.add(door)

    # Window (on X=0 boundary)
    window = Vent(xb=Bounds3D.of(0.0, 0.0, 2.0, 4.0, 1.0, 2.0), surf_id="OPEN", id="WINDOW_1")
    sim.add(window)

    # HVAC vents - Note: VOLUME_FLOW must be on SURF, not VENT per FDS requirements
    # Create surfaces with volume flow
    sim.add(Surface(id="HVAC_SUPPLY", volume_flow=-0.5, color="BLUE"))  # negative = into domain
    sim.add(Surface(id="HVAC_EXHAUST", volume_flow=0.3, color="RED"))  # positive = out of domain

    # Create vents referencing the surfaces (on ceiling at Z=5)
    from pyfds.core.namelists import Vent

    supply = Vent(xb=Bounds3D.of(5, 6, 5, 6, 5, 5), surf_id="HVAC_SUPPLY", id="SUPPLY_1")
    sim.add(supply)

    exhaust = Vent(xb=Bounds3D.of(0, 1, 0, 1, 5, 5), surf_id="HVAC_EXHAUST", id="EXHAUST_1")
    sim.add(exhaust)

    # Circular burner
    # Approximate circular burner with square vent
    burner = Vent(xb=Bounds3D.of(4.5, 5.5, 4.5, 5.5, 0.0, 0.0), surf_id="FIRE", id="BURNER")
    sim.add(burner)

    # ========================================================================
    # Device Properties (using factory methods)
    # ========================================================================
    print("Adding device properties...")

    # Sprinkler using predefined quick-response
    sprinkler = PropBuilder.quick_response_sprinkler(id="SPRINKLER_QR")
    sim.add(sprinkler)

    # Smoke detector
    smoke_det = PropBuilder.smoke_detector(id="SMOKE_DET", activation_obscuration=3.28)
    sim.add(smoke_det)

    # Heat detector
    heat_det = PropBuilder.heat_detector(id="HEAT_DET", activation_temp=74, rti=5.0)
    sim.add(heat_det)

    # ========================================================================
    # Control Logic
    # ========================================================================
    print("Adding control logic...")

    # ANY logic - alarm if any detector activates
    alarm = ControlBuilder("SMOKE_ALARM").any(["SMOKE_DET_1", "SMOKE_DET_2", "SMOKE_DET_3"]).build()
    sim.add(alarm)

    # Time-delayed sprinkler activation
    sprinkler_ctrl = (
        ControlBuilder("DELAYED_SPRINKLER")
        .time_delay("HEAT_DET", delay=10.0)
        .with_latch(True)
        .build()
    )
    sim.add(sprinkler_ctrl)

    # ========================================================================
    # Add Surfaces and Obstructions
    # ========================================================================
    print("Adding surfaces and obstructions...")

    # Fire surface
    sim.add(Surface(id="FIRE", hrrpua=1000.0, color="RED"))

    # Wall surface with material
    sim.add(Surface(id="WALL", matl_id="GYPSUM", thickness=0.012, color="GRAY"))

    # Burner obstruction
    sim.add(Obstruction(xb=Bounds3D.of(4.5, 5.5, 4.5, 5.5, 0, 0.1), surf_id="FIRE"))

    # Wall obstructions
    sim.add(Obstruction(xb=Bounds3D.of(0, 0, 0, 10, 0, 3), surf_id="WALL"))
    sim.add(Obstruction(xb=Bounds3D.of(10, 10, 0, 10, 0, 3), surf_id="WALL"))

    # ========================================================================
    # Add Devices
    # ========================================================================
    print("Adding measurement devices...")

    # Temperature measurements
    sim.add(Device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=Point3D.of(5, 5, 2.5)))
    sim.add(Device(id="TEMP_CORNER", quantity="TEMPERATURE", xyz=Point3D.of(1, 1, 2.5)))

    # HRR measurement
    sim.add(Device(id="HRR", quantity="HRR", xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

    # Actual detector devices (referenced by controls)
    sim.add(
        Device(
            id="SMOKE_DET_1",
            quantity="CHAMBER OBSCURATION",
            xyz=Point3D.of(3, 3, 2.5),
            prop_id="SMOKE_DET",
        )
    )
    sim.add(
        Device(
            id="SMOKE_DET_2",
            quantity="CHAMBER OBSCURATION",
            xyz=Point3D.of(7, 3, 2.5),
            prop_id="SMOKE_DET",
        )
    )
    sim.add(
        Device(
            id="SMOKE_DET_3",
            quantity="CHAMBER OBSCURATION",
            xyz=Point3D.of(5, 7, 2.5),
            prop_id="SMOKE_DET",
        )
    )
    sim.add(
        Device(
            id="HEAT_DET",
            quantity="LINK TEMPERATURE",
            xyz=Point3D.of(5, 5, 2.5),
            prop_id="HEAT_DET",
        )
    )

    # ========================================================================
    # Write FDS File
    # ========================================================================
    print("\nWriting FDS file...")
    output_dir = Path(__file__).parent / "fds"
    output_dir.mkdir(exist_ok=True)
    fds_file = sim.write(output_dir / "apartment_fire.fds")
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
    print(f"Meshes: {len(sim.meshes)}")
    print(f"Materials: {len(sim.materials)}")
    print(f"Ramps: {len(sim.ramps.all())}")
    print(f"Surfaces: {len(sim.surfaces)}")
    print(f"Obstructions: {len(sim.obstructions)}")
    print(f"Vents: {len(sim.vents)}")
    print(f"Devices: {len(sim.devices)}")
    print(f"Controls: {len(sim.ctrls)}")
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
