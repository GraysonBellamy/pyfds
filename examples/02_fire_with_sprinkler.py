"""
Fire with Sprinkler Activation Example
=======================================

This example demonstrates a fire scenario with sprinkler activation:
- Growing fire with t-squared growth curve
- Sprinkler link temperature control
- Fire extinction after sprinkler activation
- Multiple monitoring devices

This shows the control features added in Stage 1.2 (DEVC enhancements).
"""

from pathlib import Path

from pyfds.builders import MeshBuilder, ReactionBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Device, Ramp, Surface, Time
from pyfds.core.simulation import Simulation


def create_fire_with_sprinkler():
    """
    Create a fire scenario with sprinkler activation.

    Features:
    - t-squared fire growth (MEDIUM growth rate)
    - Sprinkler activation at 74°C
    - Fire suppression after activation
    - Comprehensive temperature monitoring
    """
    # Create simulation with metadata
    sim = Simulation(
        chid="fire_with_sprinkler", title="Fire with Sprinkler Activation - PyFDS Stage 1 Example"
    )
    sim.add(Time(t_end=600.0))

    # Computational mesh - finer grid for better accuracy
    mesh = (
        MeshBuilder()
        .with_id("COMPARTMENT")
        .with_bounds(Bounds3D.of(0, 8, 0, 6, 0, 3))
        .with_grid(Grid3D.of(80, 60, 30))
        .build()
    )
    sim.add(mesh)

    # Fire growth ramp - t-squared MEDIUM
    # t² = Q/a, where a_medium = 0.01172 kW/s²
    # Peak HRR = 2500 kW at t=300s
    fire_ramp = Ramp(
        id="T2_MEDIUM",
        points=[(0, 0), (60, 42), (120, 170), (180, 380), (240, 675), (300, 1055)],
    )
    sim.add(fire_ramp)

    # Fire surface with ramped HRR and suppression
    fire_surf = Surface(id="GROWING_FIRE", hrrpua=2500.0, ramp_q="T2_MEDIUM", emissivity=0.9)
    sim.add(fire_surf)

    # Reaction with suppression model
    reaction = (
        ReactionBuilder()
        .fuel("POLYURETHANE")
        .radiative_fraction(0.30)
        .yields(soot=0.10, co=0.04)
        .build()
    )
    sim.add(reaction)

    # Sprinkler system - 4 sprinklers in a square pattern
    sprinkler_locations = [
        (2.0, 1.5, 2.9),
        (6.0, 1.5, 2.9),
        (2.0, 4.5, 2.9),
        (6.0, 4.5, 2.9),
    ]

    for i, (x, y, z) in enumerate(sprinkler_locations, 1):
        sprinkler = Device(
            id=f"SPRINKLER_{i}",
            quantity="TEMPERATURE",
            setpoint=74.0,
            trip_direction=1,
            latch=True,
            delay=1.0,
            xyz=Point3D.of(x, y, z),
        )
        sim.add(sprinkler)

    # Temperature monitoring grid - 3x3 array at ceiling level
    x_positions = [2.0, 4.0, 6.0]
    y_positions = [1.5, 3.0, 4.5]

    for i, x in enumerate(x_positions):
        for j, y in enumerate(y_positions):
            temp_sensor = Device(
                id=f"TEMP_CEILING_{i}_{j}",
                quantity="TEMPERATURE",
                xyz=Point3D.of(x, y, 2.8),
                time_history=True,
            )
            sim.add(temp_sensor)

    # Average compartment temperature by layer
    layers = [
        ("UPPER", Bounds3D.of(0, 8, 0, 6, 2.0, 3.0)),
        ("MIDDLE", Bounds3D.of(0, 8, 0, 6, 1.0, 2.0)),
        ("LOWER", Bounds3D.of(0, 8, 0, 6, 0.0, 1.0)),
    ]

    for layer_name, bounds in layers:
        avg_temp = Device(
            id=f"AVG_TEMP_{layer_name}",
            quantity="TEMPERATURE",
            statistics="MEAN",
            stat_start=5.0,
            xb=Bounds3D.of(
                bounds.x_min, bounds.x_max, bounds.y_min, bounds.y_max, bounds.z_min, bounds.z_max
            ),
        )
        sim.add(avg_temp)

        max_temp = Device(
            id=f"MAX_TEMP_{layer_name}",
            quantity="TEMPERATURE",
            statistics="MAX",
            stat_start=5.0,
            xb=Bounds3D.of(
                bounds.x_min, bounds.x_max, bounds.y_min, bounds.y_max, bounds.z_min, bounds.z_max
            ),
        )
        sim.add(max_temp)

    return sim


if __name__ == "__main__":
    simulation = create_fire_with_sprinkler()

    # Create output directory
    output_dir = Path(__file__).parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write FDS input file
    output_file = simulation.write(output_dir / "fire_with_sprinkler.fds")

    print("Fire with sprinkler simulation created!")
    print(f"Output file: {output_file}")
    print("\nSimulation features:")
    print("  - Growing fire (t² MEDIUM)")
    print("  - Peak HRR: 2500 kW at 300s")
    print("  - Suppression model enabled")
    print("  - 4 sprinklers (setpoint 74°C, 1s delay)")
    print("  - 9 ceiling temperature sensors")
    print("  - 6 layer-averaged devices (MEAN + MAX for 3 layers)")
    print("\nPhysics:")
    print("  - Fuel: Polyurethane")
    print("  - Suppression constant: 0.5")
    print("  - Radiative fraction: 0.30")
    print("  - Soot yield: 0.10, CO yield: 0.04")
