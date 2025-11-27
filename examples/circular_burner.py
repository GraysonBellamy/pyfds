"""
Circular Burner Example - Demonstrating Circular VENT Geometry

This example shows:
- Circular burner using xyz and radius
- Open mesh boundaries
- Area calculation for circular vents
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Vent

# Create simulation
sim = Simulation(chid="circular_burner", title="Circular Burner Demo")

# Standard ambient conditions
sim.set_misc(tmpa=20.0, humidity=40.0)

# Time parameters
sim.time(t_end=120.0, dt=0.05)

# Mesh centered around burner - 6m x 6m x 6m
sim.mesh(ijk=Grid3D(60, 60, 60), xb=Bounds3D(-3, 3, -3, 3, 0, 6))

# Add reaction for combustion
sim.reaction(fuel="PROPANE")

# ===== Burner surfaces =====
sim.surface(id="BURNER", hrrpua=500.0, color="RED")

# ===== Circular burner =====
# Circular vents are defined as intersection of XB rectangle and XYZ+RADIUS circle
circular_burner = Vent(
    xb=Bounds3D(-1, 1, -1, 1, 0, 0),  # Bounding rectangle
    surf_id="BURNER",
    xyz=Point3D(0, 0, 0),  # Circle center point
    radius=1.0,  # Circle radius 1m
    id="CIRCULAR_BURNER",
)
sim.add_vent(circular_burner)

# Calculate and display area
circular_area = circular_burner.get_area()
print(f"Circular burner area: {circular_area:.3f} m²")


# ===== Open boundaries =====
# All mesh boundaries open to ambient
for boundary in ["XMIN", "XMAX", "YMIN", "YMAX"]:
    sim.vent(mb=boundary, surf_id="OPEN")

# ===== Monitoring devices =====
# Temperature above circular burner
for i, z in enumerate([1, 2, 3, 4, 5], start=1):
    sim.device(id=f"TEMP_CIRC_{i}", quantity="TEMPERATURE", xyz=Point3D(0, 0, z))

# Velocity measurement
sim.device(id="VEL_CENTER", quantity="VELOCITY", xyz=Point3D(0, 0, 3))

# Generate FDS file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "circular_burner.fds")
print(f"\n✓ Created FDS file: {output_file}")
print(f"✓ Circular burner: radius={circular_burner.radius}m, area={circular_area:.3f}m²")
print(f"✓ Total vents: {len(sim.geometry.vents)}")
print(f"✓ Circular vent shape: {circular_burner.get_shape().value}")
