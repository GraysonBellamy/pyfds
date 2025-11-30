"""
Basic Room Fire Example
========================

This example demonstrates how to create a simple room fire simulation
using PyFDS. It follows the example from Section 7.2.1 of the implementation plan.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.reac import Reaction
from pyfds.core.namelists.surf import Surface
from pyfds.core.namelists.time import Time

# Create simulation
sim = Simulation(chid="room_fire", title="Room Fire Test")

# Set time parameters
sim.add(Time(t_end=600.0))  # 10 minutes simulation

# Define computational domain
# Room dimensions: 5m x 5m x 2.5m high
# Grid resolution: 50 x 50 x 25 cells (10 cm cell size)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Create fire surface
# Heat release rate per unit area: 1000 kW/m²
sim.add(
    Surface(
        id="BURNER",
        hrrpua=1000.0,  # kW/m²
        color="RED",
    )
)

# Add reaction for combustion
sim.add(Reaction(fuel="PROPANE"))

# Add fire source (1m x 1m burner at floor level)
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id="BURNER"))

# Add measurement device at ceiling
# Using Point3D for better type safety and operations
ceiling_sensor = Point3D.of(2.5, 2.5, 2.4)
sim.add(Device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=ceiling_sensor))

# You can also use tuples (backward compatible):
# sim.add(Device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=Point3D.of(2.5, 2.5, 2.4)))

# Validate the simulation
warnings = sim.validate()
if warnings:
    print("Validation warnings:")
    for w in warnings:
        print(f"  - {w}")
else:
    print("Validation passed!")

# Generate and save FDS input file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "room_fire.fds")
print(f"\nFDS input file written to: {output_file}")

# Print preview of the FDS file
print("\nFDS File Preview:")
print("=" * 60)
print(sim.to_fds())
print("=" * 60)

print("\nTo run this simulation with FDS, execute:")
print(f"  fds {output_file}")
