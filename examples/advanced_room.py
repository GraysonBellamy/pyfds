"""
Advanced Room Example
======================

This example demonstrates more advanced PyFDS features including:
- Multiple surfaces
- Multiple obstructions
- Device arrays
- Method chaining
"""

from pyfds import Simulation

# Create simulation using method chaining
sim = (
    Simulation(chid="advanced_room", title="Advanced Room Fire with Walls")
    .time(t_end=600.0, dt=0.1)
    .mesh(ijk=(60, 60, 30), xb=(0, 6, 0, 6, 0, 3))
)

# Define multiple surfaces
sim.surface(id="WALL", color="GRAY", tmp_front=20.0)
sim.surface(id="CEILING", color="WHITE", tmp_front=20.0)
sim.surface(id="FLOOR", color="TAN", tmp_front=20.0)
sim.surface(id="FIRE", hrrpua=1000.0, color="RED")

# Build room geometry
# Floor
sim.obstruction(xb=(0, 6, 0, 6, 0, 0), surf_id="FLOOR")

# Ceiling
sim.obstruction(xb=(0, 6, 0, 6, 3, 3), surf_id="CEILING")

# Walls (with doorway opening at x=0)
sim.obstruction(xb=(0, 0, 0, 6, 0, 3), surf_id="WALL")  # Left wall
sim.obstruction(xb=(6, 6, 0, 6, 0, 3), surf_id="WALL")  # Right wall
sim.obstruction(xb=(0, 6, 0, 0, 0, 3), surf_id="WALL")  # Front wall
sim.obstruction(xb=(0, 6, 6, 6, 0, 3), surf_id="WALL")  # Back wall

# Door opening (remove section of left wall)
# This would typically be done with a VENT, but showing obstruction approach
# Note: In a real scenario, you'd use VENT with SURF_ID='OPEN' for openings

# Fire source in corner
sim.obstruction(xb=(2.5, 3.5, 2.5, 3.5, 0, 0.2), surf_id="FIRE")

# Add measurement devices in a grid at ceiling level
print("Adding temperature measurement grid...")
x_positions = [1.5, 3.0, 4.5]
y_positions = [1.5, 3.0, 4.5]
z_ceiling = 2.9

device_count = 0
for i, x in enumerate(x_positions):
    for j, y in enumerate(y_positions):
        device_count += 1
        sim.device(id=f"TEMP_X{i + 1}Y{j + 1}", quantity="TEMPERATURE", xyz=(x, y, z_ceiling))

print(f"Added {device_count} temperature sensors")

# Add vertical temperature profile at room center
z_positions = [0.5, 1.0, 1.5, 2.0, 2.5, 2.9]
for i, z in enumerate(z_positions, start=1):
    sim.device(id=f"TEMP_VERT_{i}", quantity="TEMPERATURE", xyz=(3.0, 3.0, z))

print(f"Added {len(z_positions)} vertical profile sensors")

# Validate
warnings = sim.validate()
if warnings:
    print("\nValidation warnings:")
    for w in warnings:
        print(f"  ⚠ {w}")
else:
    print("\n✓ Validation passed!")

# Write file
output_file = sim.write("advanced_room.fds")
print(f"\n✓ FDS input file written to: {output_file}")

# Summary
print("\nSimulation Summary:")
print("  - Domain: 6m x 6m x 3m")
print("  - Grid cells: 60 x 60 x 30 (10 cm resolution)")
print(f"  - Surfaces defined: {len(sim.surfaces)}")
print(f"  - Obstructions: {len(sim.obstructions)}")
print(f"  - Devices: {len(sim.devices)}")
print(f"  - Simulation time: {sim.time_params.t_end} seconds")
