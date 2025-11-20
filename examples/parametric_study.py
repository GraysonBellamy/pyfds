"""
Parametric Study Example
=========================

This example demonstrates how to create multiple simulations with varying
parameters to study the effect of heat release rate on fire behavior.
"""

from pathlib import Path

from pyfds import Simulation

# Create output directory
output_dir = Path("parametric_study")
output_dir.mkdir(exist_ok=True)

# Define parameter values to study
hrr_values = [500, 1000, 1500, 2000]  # kW/m²

print("Creating parametric study simulations...")
print(f"Heat Release Rate values: {hrr_values} kW/m²\n")

for hrr in hrr_values:
    # Create unique simulation for each HRR value
    sim = Simulation(chid=f"fire_hrr_{hrr}", title=f"Parametric Study - HRR {hrr} kW/m²")

    # Time parameters
    sim.time(t_end=300.0)  # 5 minutes

    # Mesh - 3m x 3m x 1.5m domain
    sim.mesh(ijk=(30, 30, 15), xb=(0, 3, 0, 3, 0, 1.5))

    # Fire surface with current HRR
    sim.surface(id="FIRE", hrrpua=float(hrr), color="RED")

    # 1m x 1m fire source
    sim.obstruction(xb=(1, 2, 1, 2, 0, 0.1), surf_id="FIRE")

    # Temperature measurement devices at different heights
    heights = [0.5, 1.0, 1.4]
    for i, z in enumerate(heights, start=1):
        sim.device(id=f"TEMP_{i}", quantity="TEMPERATURE", xyz=(1.5, 1.5, z))

    # Write simulation file
    output_file = output_dir / f"fire_hrr_{hrr}.fds"
    sim.write(output_file)

    print(f"✓ Created: {output_file}")

print(f"\nAll simulation files created in: {output_dir}")
print("\nTo run all simulations, you can use:")
print(f'  for f in {output_dir}/*.fds; do fds "$f"; done')

# Create a summary file
summary_file = output_dir / "README.txt"
with summary_file.open("w") as f:
    f.write("Parametric Study - Heat Release Rate Sensitivity\n")
    f.write("=" * 60 + "\n\n")
    f.write("This directory contains FDS simulations with varying HRR:\n\n")
    for hrr in hrr_values:
        f.write(f"  - fire_hrr_{hrr}.fds: {hrr} kW/m² HRRPUA\n")
    f.write("\n")
    f.write("Each simulation measures temperature at 3 heights:\n")
    f.write("  - TEMP_1: 0.5 m (lower layer)\n")
    f.write("  - TEMP_2: 1.0 m (mid-height)\n")
    f.write("  - TEMP_3: 1.4 m (ceiling layer)\n")

print(f"Summary written to: {summary_file}")
