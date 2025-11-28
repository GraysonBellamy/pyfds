"""
Sprinkler Suppression System Example
=====================================

This example demonstrates a complete sprinkler suppression system including:
- Water droplet particles
- Quick-response sprinkler properties
- Sprinkler spray surface
- Fire source with smoke generation
- Smoke detector
- Compartment geometry

This is a simplified demonstration. For production use, consult FDS documentation
and fire protection engineering guidelines.
"""

from pathlib import Path

from pyfds.builders import (
    DevcBuilder,
    MaterialBuilder,
    MeshBuilder,
    PartBuilder,
    PropBuilder,
    RampBuilder,
    SurfBuilder,
    VentBuilder,
)
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Head, Time
from pyfds.core.simulation import Simulation


def create_sprinkler_simulation():
    """Create a complete sprinkler suppression simulation."""

    # ==================== HEADER ====================
    head = Head(chid="sprinkler_demo", title="Residential Room Fire with Sprinkler Suppression")

    # ==================== TIME ====================
    time = Time(t_end=60.0)  # 60 second simulation

    # ==================== MESH ====================
    # Simple 6m x 4m x 3m room
    mesh = (
        MeshBuilder()
        .with_id("ROOM")
        .with_bounds(Bounds3D(0, 6, 0, 4, 0, 3))
        .with_grid(Grid3D(60, 40, 30))
        .build()
    )

    # ==================== MATERIALS ====================
    # Wood material with pyrolysis
    wood = (
        MaterialBuilder("PINE_WOOD")
        .density(500)
        .thermal_conductivity(0.13)
        .specific_heat(2.5)
        .add_pyrolysis_reaction(
            a=1e10, e=100000, heat_of_reaction=1800, product_species="WOOD_VAPOR"
        )
        .build()
    )

    # ==================== PARTICLES ====================
    # Smoke particles for visualization
    smoke = (
        PartBuilder("SMOKE")
        .as_aerosol(diameter=0.00001, spec_id="SOOT")
        .with_color("GRAY 50")
        .build()
    )

    # Water droplets from sprinkler
    water = PartBuilder("WATER_DROP").as_water_droplet(diameter=0.001).with_color("BLUE").build()

    # ==================== RAMPS ====================
    # t-squared fire growth (medium growth rate)
    fire_ramp = (
        RampBuilder("FIRE_GROWTH")
        .t_squared(alpha=0.0117, t_end=60.0, steps=20)  # Medium: 300 kW in 300s
        .build()
    )

    # ==================== SURFACES ====================
    # Fire source that generates smoke
    fire_surf = (
        SurfBuilder("FIRE_SOURCE")
        .with_particle_generation("SMOKE", mass_flux=0.002, nppc=2)
        .with_heat_release(500.0, ramp_id="FIRE_GROWTH")
        .with_color("ORANGE")
        .build()
    )

    # Sprinkler spray surface
    sprinkler_surf = (
        SurfBuilder("SPRINKLER_SPRAY")
        .as_sprinkler(part_id="WATER_DROP", mass_flux=0.02, median_diameter=0.001, velocity=6.0)
        .build()
    )

    # ==================== DEVICE PROPERTIES ====================
    # Quick-response sprinkler (68°C, RTI=50)
    sprinkler_prop = PropBuilder.quick_response_sprinkler(id="QR_SPRINKLER")

    # Photoelectric smoke detector
    smoke_detector_prop = PropBuilder.smoke_detector(
        id="SMOKE_DETECTOR",
        activation_obscuration=3.28,  # UL standard
    )

    # ==================== DEVICES ====================
    # Ceiling-mounted sprinkler (center of room)
    sprinkler = (
        DevcBuilder("SPRINKLER_1")
        .with_quantity("SPRINKLER_LINK_TEMPERATURE")
        .with_control(setpoint=68.0, trip_direction=1, latch=True, delay=2.0)
        .at_point(Point3D(3.0, 2.0, 2.8))
        .with_prop("QR_SPRINKLER")
        .build()
    )

    # Smoke detector (also ceiling-mounted)
    smoke_detector = (
        DevcBuilder("SMOKE_DET_1")
        .with_quantity("CHAMBER_OBSCURATION")
        .with_control(setpoint=3.28, trip_direction=1, latch=True)
        .at_point(Point3D(1.5, 2.0, 2.9))
        .with_prop("SMOKE_DETECTOR")
        .build()
    )

    # Temperature measurement at ceiling
    temp_ceiling = (
        DevcBuilder("TEMP_CEILING")
        .with_quantity("TEMPERATURE")
        .at_point(Point3D(3.0, 2.0, 2.9))
        .with_time_history(True)
        .build()
    )

    # ==================== VENTS ====================
    # Fire source (1m x 1m on floor, corner of room)
    fire_vent = (
        VentBuilder("FIRE")
        .with_bounds(Bounds3D(0.5, 1.5, 0.5, 1.5, 0.0, 0.0))
        .with_surf("FIRE_SOURCE")
        .build()
    )

    # Sprinkler spray (0.3m x 0.3m on ceiling)
    sprinkler_vent = (
        VentBuilder("SPRINKLER")
        .with_bounds(Bounds3D(2.85, 3.15, 1.85, 2.15, 3.0, 3.0))
        .with_surf("SPRINKLER_SPRAY")
        .with_control("SPRINKLER_1")  # Activated by sprinkler device
        .build()
    )

    # Door opening (0.9m wide, 2.0m tall)
    door = (
        VentBuilder("DOOR")
        .with_bounds(Bounds3D(6.0, 6.0, 1.5, 2.4, 0.0, 2.0))
        .with_surf("OPEN")
        .build()
    )

    # ==================== BUILD SIMULATION ====================
    sim = Simulation(head=head, time=time)
    sim.add_mesh(mesh)
    sim.add_material(wood)
    sim.add_particle(smoke)
    sim.add_particle(water)
    sim.add_ramp(fire_ramp)
    sim.add_surface(fire_surf)
    sim.add_surface(sprinkler_surf)
    sim.add_prop(sprinkler_prop)
    sim.add_prop(smoke_detector_prop)
    sim.add_device(sprinkler)
    sim.add_device(smoke_detector)
    sim.add_device(temp_ceiling)
    sim.add_vent(fire_vent)
    sim.add_vent(sprinkler_vent)
    sim.add_vent(door)

    return sim


if __name__ == "__main__":
    # Create simulation
    sim = create_sprinkler_simulation()

    # Create output directory
    output_dir = Path(__file__).parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write to FDS input file
    output_file = output_dir / "sprinkler_demo.fds"
    with output_file.open("w") as f:
        f.write(sim.to_fds())

    print(f"Simulation file written to: {output_file}")
    print("\nSimulation Summary:")
    print("  - Domain: 6m x 4m x 3m room")
    print("  - Fire: t-squared growth, max 500 kW")
    print("  - Sprinkler: Quick-response, 68°C activation")
    print("  - Smoke detector: 3.28%/m activation")
    print("  - Duration: 60 seconds")
    print("\nTo run with FDS:")
    print(f"  fds {output_file}")
