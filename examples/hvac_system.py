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
from pyfds.core.namelists import Misc, Surface, Time, Vent
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.reac import Reaction
from pyfds.core.simulation import Simulation

# Create simulation
sim = Simulation(chid="hvac_system", title="Room with HVAC System")

# Set custom ambient conditions
sim.add(Misc(tmpa=22.0, humidity=50.0, p_inf=101325.0))

# Time parameters
sim.add(Time(t_end=600.0, dt=0.1))

# Define computational domain - 10m x 10m x 3m room
sim.add(Mesh(ijk=Grid3D.of(50, 50, 15), xb=Bounds3D.of(0, 10, 0, 10, 0, 3)))

# ===== Room geometry =====
# Floor
sim.add(Obstruction(xb=Bounds3D.of(0, 10, 0, 10, 0, 0)))
# Ceiling
sim.add(Obstruction(xb=Bounds3D.of(0, 10, 0, 10, 3, 3)))

# Walls (with door opening on one side)
sim.add(Obstruction(xb=Bounds3D.of(0, 0, 0, 10, 0, 3)))  # West wall
sim.add(Obstruction(xb=Bounds3D.of(10, 10, 0, 10, 0, 3)))  # East wall
sim.add(Obstruction(xb=Bounds3D.of(0, 10, 0, 0, 0, 3)))  # South wall

# North wall with door opening (2m wide, 2m high)
sim.add(Obstruction(xb=Bounds3D.of(0, 10, 10, 10, 0, 0.5)))  # Bottom part
sim.add(Obstruction(xb=Bounds3D.of(0, 10, 10, 10, 2, 3)))  # Top part
sim.add(Obstruction(xb=Bounds3D.of(0, 4, 10, 10, 0.5, 2)))  # Left side
sim.add(Obstruction(xb=Bounds3D.of(6, 10, 10, 10, 0.5, 2)))  # Right side

# ===== HVAC VENTS =====
# Supply vent (ceiling-mounted, providing fresh air)
# VOLUME_FLOW must be on SURF, not VENT
sim.add(Surface(id="SUPPLY", volume_flow=-0.5, color="BLUE"))  # negative = into domain
supply_vent = Vent(
    xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3),
    surf_id="SUPPLY",
    id="SUPPLY_VENT",
)
sim.add(supply_vent)

# Exhaust vent (ceiling-mounted, removing air)
sim.add(Surface(id="EXHAUST", volume_flow=0.4, color="RED"))  # positive = out of domain
exhaust_vent = Vent(
    xb=Bounds3D.of(7.5, 8, 7.5, 8, 3, 3),
    surf_id="EXHAUST",
    id="EXHAUST_VENT",
)
sim.add(exhaust_vent)

# ===== DOOR OPENING =====
# Opening to ambient (using convenience method)
sim.add(Vent(xb=Bounds3D.of(4, 6, 10, 10, 0.5, 2), surf_id="OPEN", id="DOOR"))

# Add reaction for combustion
sim.add(Reaction(fuel="PROPANE"))

# ===== Heat source (simulating occupant/equipment heat) =====
sim.add(Surface(id="HEAT_SOURCE", hrrpua=100.0, color="ORANGE"))
sim.add(Obstruction(xb=Bounds3D.of(5, 6, 5, 6, 0, 0.5), surf_id="HEAT_SOURCE"))

# ===== Monitoring devices =====
# Temperature at various heights near center of room
for i, z in enumerate([0.5, 1.0, 1.5, 2.0, 2.5], start=1):
    sim.add(Device(id=f"TEMP_CENTER_{i}", quantity="TEMPERATURE", xyz=Point3D.of(5, 5, z)))

# Temperature near supply vent
sim.add(Device(id="TEMP_SUPPLY", quantity="TEMPERATURE", xyz=Point3D.of(2.25, 2.25, 2.8)))

# Temperature near exhaust vent
sim.add(Device(id="TEMP_EXHAUST", quantity="TEMPERATURE", xyz=Point3D.of(7.75, 7.75, 2.8)))

# Velocity near door
sim.add(Device(id="VEL_DOOR", quantity="VELOCITY", xyz=Point3D.of(4, 5, 1.25)))

# Generate FDS file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "hvac_system.fds")
print(f"✓ Created FDS file: {output_file}")
if sim.misc_config:
    print(f"✓ Ambient temperature: {sim.misc_config.tmpa}°C")
    print(f"✓ Humidity: {sim.misc_config.humidity}%")
print(f"✓ Number of vents: {len(sim.vents)}")
print("✓ Supply flow: 0.5 m³/s")
print("✓ Exhaust flow: -0.4 m³/s")
print("✓ Net flow: 0.1 m³/s (slight positive pressure)")
