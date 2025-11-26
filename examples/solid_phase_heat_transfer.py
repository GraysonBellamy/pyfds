"""
Solid Phase Heat Transfer Example - SOLID_PHASE_ONLY Mode

This example demonstrates:
- Solid phase only mode (no fluid flow)
- Pure heat conduction simulation
- Radiation disabled for faster computation
- Temperature-controlled surface vents
- Material heat transfer
"""

from pyfds import Simulation
from pyfds.core.namelists import Material, Vent

# Create simulation
sim = Simulation(chid="heat_transfer", title="Solid Phase Heat Conduction")

# Configure for solid phase only
sim.set_misc(
    solid_phase_only=True,  # Only solve solid heat transfer
    radiation=False,  # Disable radiation for speed
    tmpa=20.0,  # Initial/ambient temperature
)

# Time parameters - heat conduction is slower
sim.time(t_end=3600.0, dt=1.0)  # 1 hour simulation

# Small domain - 1m x 1m x 1m
sim.mesh(ijk=(50, 50, 50), xb=(0, 1, 0, 1, 0, 1))

# ===== Material definitions =====
# Concrete
concrete = Material(
    id="CONCRETE", density=2300.0, conductivity=1.4, specific_heat=0.88, emissivity=0.9
)
sim.add_material(concrete)

# Steel
steel = Material(id="STEEL", density=7850.0, conductivity=45.0, specific_heat=0.46, emissivity=0.7)
sim.add_material(steel)

# ===== Surface definitions =====
# Hot boundary (500°C)
sim.surface(id="HOT", tmp_front=500.0, color="RED")

# Cold boundary (0°C)
sim.surface(id="COLD", tmp_front=0.0, color="BLUE")

# Concrete surface
sim.surface(id="CONCRETE_SURF", matl_id="CONCRETE", thickness=0.2, color="GRAY")

# Steel plate
sim.surface(id="STEEL_PLATE", matl_id="STEEL", thickness=0.01, color="SILVER")

# ===== Geometry =====
# Hot plate at bottom
hot_vent = Vent(xb=(0.3, 0.7, 0.3, 0.7, 0, 0), surf_id="HOT", id="HOT_PLATE")
sim.add_vent(hot_vent)

# Concrete block in center
sim.obstruction(xb=(0.4, 0.6, 0.4, 0.6, 0, 0.4), surf_id="CONCRETE_SURF")

# Steel plate on top
steel_vent = Vent(xb=(0.35, 0.65, 0.35, 0.65, 0.4, 0.4), surf_id="STEEL_PLATE", id="STEEL_TOP")
sim.add_vent(steel_vent)

# Insulated walls (default INERT)
sim.vent(xb=(0, 0, 0, 1, 0, 1), surf_id="INERT", id="WALL_XMIN")
sim.vent(xb=(1, 1, 0, 1, 0, 1), surf_id="INERT", id="WALL_XMAX")
sim.vent(xb=(0, 1, 0, 0, 0, 1), surf_id="INERT", id="WALL_YMIN")
sim.vent(xb=(0, 1, 1, 1, 0, 1), surf_id="INERT", id="WALL_YMAX")
sim.vent(xb=(0, 1, 0, 1, 1, 1), surf_id="INERT", id="WALL_ZMAX")

# ===== Monitoring devices =====
# Temperature at various heights in the concrete block
for i, z in enumerate([0.05, 0.1, 0.2, 0.3, 0.35], start=1):
    sim.device(id=f"TEMP_CONCRETE_{i}", quantity="WALL TEMPERATURE", xyz=(0.5, 0.5, z))

# Temperature at steel plate
sim.device(id="TEMP_STEEL", quantity="WALL TEMPERATURE", xyz=(0.5, 0.5, 0.4))

# Heat flux measurements
sim.device(id="HEAT_FLUX_BOTTOM", quantity="WALL HEAT FLUX", xyz=(0.5, 0.5, 0.05))

sim.device(id="HEAT_FLUX_TOP", quantity="WALL HEAT FLUX", xyz=(0.5, 0.5, 0.35))

# Generate FDS file
output_file = sim.write("heat_transfer.fds")
print(f"✓ Created FDS file: {output_file}")
print(f"✓ Mode: SOLID_PHASE_ONLY={sim.misc_params.solid_phase_only}")
print(f"✓ Radiation: {sim.misc_params.radiation}")
print(f"✓ Materials: {len(sim.materials)}")
print(f"✓ Vents: {len(sim.vents)} (boundary conditions)")
print(f"✓ Simulation time: {sim.time_params.t_end}s (1 hour)")
print("\nThis simulation models pure heat conduction without fluid flow.")
