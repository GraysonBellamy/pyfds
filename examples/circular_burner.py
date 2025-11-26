"""
Circular Burner Example - Demonstrating Circular and Annular VENT Geometries

This example shows:
- Circular burner using xyz and radius
- Annular (ring-shaped) burner
- Open mesh boundaries
- Area calculation for different vent shapes
"""

from pyfds import Simulation
from pyfds.core.namelists import Vent

# Create simulation
sim = Simulation(chid="circular_burner", title="Circular and Annular Burner Demo")

# Standard ambient conditions
sim.set_misc(tmpa=20.0, humidity=40.0)

# Time parameters
sim.time(t_end=120.0, dt=0.05)

# Mesh centered around burner - 6m x 6m x 6m
sim.mesh(ijk=(60, 60, 60), xb=(-3, 3, -3, 3, 0, 6))

# ===== Burner surfaces =====
sim.surface(id="BURNER", hrrpua=500.0, color="RED")

# ===== Circular burner at center =====
# Using Vent for circular geometry
burner = Vent(
    xb=(-2, 2, -2, 2, 0, 0),  # Bounding box
    surf_id="BURNER",
    xyz=(0, 0, 0),  # Center point
    radius=1.0,  # 1m radius
    id="CIRCULAR_BURNER",
)
sim.add_vent(burner)

# Calculate and display area
burner_area = burner.get_area()
print(f"Circular burner area: {burner_area:.3f} m²")

# ===== Annular (ring-shaped) burner =====
# Create a second mesh for the annular burner demonstration
sim.mesh(ijk=(60, 60, 60), xb=(-3, 3, -3, 3, 7, 13), id="MESH_02")

# Annular burner (ring with inner hole)
annular_burner = Vent(
    xb=(-2, 2, -2, 2, 7, 7),  # Bounding box
    surf_id="BURNER",
    xyz=(0, 0, 7),  # Center point
    radius=1.5,  # Outer radius 1.5m
    radius_inner=0.5,  # Inner radius 0.5m
    id="ANNULAR_BURNER",
)
sim.add_vent(annular_burner)

# Calculate annular area
annular_area = annular_burner.get_area()
print(f"Annular burner area: {annular_area:.3f} m²")

# ===== Open boundaries =====
# All mesh boundaries open to ambient
for boundary in ["XMIN", "XMAX", "YMIN", "YMAX"]:
    sim.vent(mb=boundary, surf_id="OPEN")

# ===== Monitoring devices =====
# Temperature above circular burner
for i, z in enumerate([1, 2, 3, 4, 5], start=1):
    sim.device(id=f"TEMP_CIRC_{i}", quantity="TEMPERATURE", xyz=(0, 0, z))

# Temperature above annular burner
for i, z in enumerate([8, 9, 10, 11, 12], start=1):
    sim.device(id=f"TEMP_ANN_{i}", quantity="TEMPERATURE", xyz=(0, 0, z))

# Velocity measurements
sim.device(id="VEL_CENTER", quantity="VELOCITY", xyz=(0, 0, 3))
sim.device(id="VEL_ANNULAR", quantity="VELOCITY", xyz=(0, 0, 10))

# Generate FDS file
output_file = sim.write("circular_burner.fds")
print(f"\n✓ Created FDS file: {output_file}")
print(f"✓ Circular burner: radius={burner.radius}m, area={burner_area:.3f}m²")
print(
    f"✓ Annular burner: outer={annular_burner.radius}m, inner={annular_burner.radius_inner}m, area={annular_area:.3f}m²"
)
print(f"✓ Total vents: {len(sim.vents)}")
print(
    f"✓ Vent types: {sum(1 for v in sim.vents if v.get_shape().value == 'CIRCULAR')} circular, {sum(1 for v in sim.vents if v.get_shape().value == 'ANNULAR')} annular"
)
