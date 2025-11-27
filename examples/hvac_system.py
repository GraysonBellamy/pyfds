"""
HVAC System Example - Demonstrating VENT and MISC Namelists

This example creates a simple room with an HVAC system including:
- Supply and exhaust vents with volume flow rates
- Door opening to ambient
- Custom ambient conditions using MISC
- Temperature monitoring at various locations
"""

from pathlib import Path

from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Vent
from pyfds.core.simulation import Simulation

# Create simulation
sim = Simulation(chid="hvac_system", title="Room with HVAC System")

# Set custom ambient conditions
sim.set_misc(
    tmpa=22.0,  # 22°C ambient temperature
    humidity=50.0,  # 50% relative humidity
    p_inf=101325.0,  # Standard atmospheric pressure
)

# Time parameters
sim.time(t_end=600.0, dt=0.1)

# Define computational domain - 10m x 10m x 3m room
sim.mesh(ijk=Grid3D(50, 50, 15), xb=Bounds3D(0, 10, 0, 10, 0, 3))

# ===== Room geometry =====
# Floor
sim.obstruction(xb=Bounds3D(0, 10, 0, 10, 0, 0))

# Ceiling
sim.obstruction(xb=Bounds3D(0, 10, 0, 10, 3, 3))

# Walls (with door opening on one side)
sim.obstruction(xb=Bounds3D(0, 0, 0, 10, 0, 3))  # West wall
sim.obstruction(xb=Bounds3D(10, 10, 0, 10, 0, 3))  # East wall
sim.obstruction(xb=Bounds3D(0, 10, 0, 0, 0, 3))  # South wall

# North wall with door opening (2m wide, 2m high)
sim.obstruction(xb=Bounds3D(0, 10, 10, 10, 0, 0.5))  # Bottom part
sim.obstruction(xb=Bounds3D(0, 10, 10, 10, 2, 3))  # Top part
sim.obstruction(xb=Bounds3D(0, 4, 10, 10, 0.5, 2))  # Left side
sim.obstruction(xb=Bounds3D(6, 10, 10, 10, 0.5, 2))  # Right side

# ===== HVAC VENTS =====
# Supply vent (ceiling-mounted, providing fresh air)
# VOLUME_FLOW must be on SURF, not VENT
sim.surface(id="SUPPLY", volume_flow=-0.5, color="BLUE")  # negative = into domain
supply_vent = Vent(
    xb=Bounds3D(2, 2.5, 2, 2.5, 3, 3),
    surf_id="SUPPLY",
    id="SUPPLY_VENT",
)
sim.add_vent(supply_vent)

# Exhaust vent (ceiling-mounted, removing air)
sim.surface(id="EXHAUST", volume_flow=0.4, color="RED")  # positive = out of domain
exhaust_vent = Vent(
    xb=Bounds3D(7.5, 8, 7.5, 8, 3, 3),
    surf_id="EXHAUST",
    id="EXHAUST_VENT",
)
sim.add_vent(exhaust_vent)

# ===== DOOR OPENING =====
# Opening to ambient (using convenience method)
sim.vent(xb=Bounds3D(4, 6, 10, 10, 0.5, 2), surf_id="OPEN", id="DOOR")

# Add reaction for combustion
sim.reaction(fuel="PROPANE")

# ===== Heat source (simulating occupant/equipment heat) =====
sim.surface(id="HEAT_SOURCE", hrrpua=100.0, color="ORANGE")
sim.obstruction(xb=Bounds3D(5, 6, 5, 6, 0, 0.5), surf_id="HEAT_SOURCE")

# ===== Monitoring devices =====
# Temperature at various heights near center of room
for i, z in enumerate([0.5, 1.0, 1.5, 2.0, 2.5], start=1):
    sim.device(id=f"TEMP_CENTER_{i}", quantity="TEMPERATURE", xyz=Point3D(5, 5, z))

# Temperature near supply vent
sim.device(id="TEMP_SUPPLY", quantity="TEMPERATURE", xyz=Point3D(2.25, 2.25, 2.8))

# Temperature near exhaust vent
sim.device(id="TEMP_EXHAUST", quantity="TEMPERATURE", xyz=Point3D(7.75, 7.75, 2.8))

# Velocity near door
sim.device(id="VEL_DOOR", quantity="VELOCITY", xyz=Point3D(4, 5, 1.25))

# Generate FDS file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "hvac_system.fds")
print(f"✓ Created FDS file: {output_file}")
if sim.physics.misc_params:
    print(f"✓ Ambient temperature: {sim.physics.misc_params.tmpa}°C")
    print(f"✓ Humidity: {sim.physics.misc_params.humidity}%")
print(f"✓ Number of vents: {len(sim.geometry.vents)}")
print("✓ Supply flow: 0.5 m³/s")
print("✓ Exhaust flow: -0.4 m³/s")
print("✓ Net flow: 0.1 m³/s (slight positive pressure)")
