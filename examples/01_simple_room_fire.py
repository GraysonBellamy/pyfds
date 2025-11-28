"""
Simple Room Fire Example
=========================

This example demonstrates a basic room fire simulation using PyFDS Stage 1 features:
- Single computational mesh
- Fire surface with constant heat release rate
- Temperature measurement devices
- Heat detector for monitoring

This uses the Simulation class convenience methods.
"""

from pathlib import Path

from pyfds import Simulation


def create_simple_room_fire():
    """
    Create a simple room fire simulation.

    Room dimensions: 5m x 4m x 3m (L x W x H)
    Fire: 1m x 1m burner at center of floor
    HRR: 500 kW constant
    Duration: 300 seconds (5 minutes)
    """
    # Create simulation with metadata
    sim = Simulation(chid="simple_room_fire", title="Simple Room Fire - PyFDS Stage 1 Example")
    sim.time(t_end=300.0)

    # Computational mesh
    sim.mesh(ijk=(50, 40, 30), xb=(0, 5, 0, 4, 0, 3), id="ROOM")

    # Fire surface - 500 kW constant heat release (Stage 1.1 feature)
    sim.surface(id="FIRE", hrrpua=500.0, color="ORANGE", emissivity=0.9)

    # Fire source (burner at floor center)
    sim.obstruction(xb=(2.0, 3.0, 1.5, 2.5, 0.0, 0.1), surf_id="FIRE")

    # Temperature sensors at various heights
    heights = [0.5, 1.5, 2.5]
    for z in heights:
        sim.device(id=f"TEMP_{int(z * 10):02d}", quantity="TEMPERATURE", xyz=(2.5, 2.0, z))

    # Heat detector at ceiling with control logic (Stage 1.2 feature)
    sim.device(
        id="HEAT_DETECTOR",
        quantity="TEMPERATURE",
        xyz=(2.5, 2.0, 2.9),
        setpoint=75.0,  # Activate at 75°C
        trip_direction=1,  # Trip when temperature exceeds setpoint
        latch=True,  # Stay activated
    )

    return sim


if __name__ == "__main__":
    # Create simulation
    simulation = create_simple_room_fire()

    # Create output directory
    output_dir = Path(__file__).parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write FDS input file
    output_file = simulation.write(output_dir / "simple_room_fire.fds")

    print("Simple room fire simulation created!")
    print(f"Output file: {output_file}")
    print("\nSimulation parameters:")
    print("  - Room size: 5m x 4m x 3m")
    print("  - Fire HRR: 500 kW (constant)")
    print("  - Duration: 300 seconds")
    print("  - Grid cells: 50 x 40 x 30 = 60,000")
    print("\nStage 1 Features Demonstrated:")
    print("  ✓ SURF with heat release rate (Phase 1.1)")
    print("  ✓ SURF with radiation properties (Phase 1.1)")
    print("  ✓ DEVC with control logic (Phase 1.2)")
    print("\nDevices:")
    print("  - 1 heat detector (setpoint 75°C)")
    print("  - 3 temperature sensors at different heights")
