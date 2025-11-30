"""
Solid Phase Heat Transfer Example - SOLID_PHASE_ONLY Mode

This example demonstrates:
- Solid phase only mode (no fluid flow)
- Pure heat conduction simulation
- Radiation disabled for faster computation
- Temperature-controlled surface vents
- Material heat transfer
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Material, Misc, Vent
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.surf import Surface
from pyfds.core.namelists.time import Time

# Create simulation
sim = Simulation(chid="heat_transfer", title="Solid Phase Heat Conduction")

# Configure for solid phase only
sim.add(
    Misc(
        solid_phase_only=True,  # Only solve solid heat transfer
        radiation=False,  # Disable radiation for speed
        tmpa=20.0,  # Initial/ambient temperature
    )
)

# Time parameters - heat conduction is slower
sim.add(Time(t_end=3600.0, dt=1.0))  # 1 hour simulation

# Small domain - 1m x 1m x 1m
sim.add(Mesh(ijk=Grid3D.of(50, 50, 50), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

# ===== Material definitions =====
# Concrete
concrete = Material(
    id="CONCRETE", density=2300.0, conductivity=1.4, specific_heat=0.88, emissivity=0.9
)
sim.add(concrete)

# Steel
steel = Material(id="STEEL", density=7850.0, conductivity=45.0, specific_heat=0.46, emissivity=0.7)
sim.add(steel)

# ===== Surface definitions =====
# Hot boundary (500°C)
sim.add(Surface(id="HOT", tmp_front=500.0, color="RED"))

# Cold boundary (0°C)
sim.add(Surface(id="COLD", tmp_front=0.0, color="BLUE"))

# Concrete surface
sim.add(Surface(id="CONCRETE_SURF", matl_id="CONCRETE", thickness=0.2, color="GRAY"))

# Steel plate
sim.add(Surface(id="STEEL_PLATE", matl_id="STEEL", thickness=0.01, color="SILVER"))

# ===== Geometry =====
# Hot plate at bottom
hot_vent = Vent(xb=Bounds3D.of(0.3, 0.7, 0.3, 0.7, 0, 0), surf_id="HOT", id="HOT_PLATE")
sim.add(hot_vent)

# Concrete block in center
sim.add(Obstruction(xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0.4), surf_id="CONCRETE_SURF"))
# Steel plate on top
steel_vent = Vent(
    xb=Bounds3D.of(0.35, 0.65, 0.35, 0.65, 0.4, 0.4), surf_id="STEEL_PLATE", id="STEEL_TOP"
)
sim.add(steel_vent)

# Insulated walls (default INERT)
sim.add(Vent(xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="INERT", id="WALL_XMIN"))
sim.add(Vent(xb=Bounds3D.of(1, 1, 0, 1, 0, 1), surf_id="INERT", id="WALL_XMAX"))
sim.add(Vent(xb=Bounds3D.of(0, 1, 0, 0, 0, 1), surf_id="INERT", id="WALL_YMIN"))
sim.add(Vent(xb=Bounds3D.of(0, 1, 1, 1, 0, 1), surf_id="INERT", id="WALL_YMAX"))
sim.add(Vent(xb=Bounds3D.of(0, 1, 0, 1, 1, 1), surf_id="INERT", id="WALL_ZMAX"))
# ===== Monitoring devices =====
# Temperature measurements near the surfaces (since it's solid phase only)
sim.add(Device(id="TEMP_NEAR_BOTTOM", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.01)))

# Temperature at mid-height
sim.add(Device(id="TEMP_MID", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.2)))

# Temperature near top
sim.add(Device(id="TEMP_NEAR_TOP", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.39)))

# Generate FDS file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "heat_transfer.fds")
print(f"✓ Created FDS file: {output_file}")
if sim.misc_config:
    print(f"✓ Mode: SOLID_PHASE_ONLY={sim.misc_config.solid_phase_only}")
    print(f"✓ Radiation: {sim.misc_config.radiation}")
print(f"✓ Materials: {len(sim.materials)}")
print(f"✓ Vents: {len(sim.vents)} (boundary conditions)")
if sim.time_params:
    print(f"✓ Simulation time: {sim.time_params.t_end}s (1 hour)")
print("\nThis simulation models pure heat conduction without fluid flow.")
