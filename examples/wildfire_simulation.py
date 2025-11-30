"""
Wildfire Simulation Example - Advanced MISC and VENT Features

This example demonstrates:
- Wildfire simulation mode (LEVEL_SET_MODE)
- Custom turbulence model (Vreman)
- High ambient temperature and low humidity
- Open mesh boundaries for wind flow
- Large outdoor domain
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.core.enums import TurbulenceModel
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Misc
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.reac import Reaction
from pyfds.core.namelists.surf import Surface
from pyfds.core.namelists.time import Time
from pyfds.core.namelists.vent import Vent

# Create simulation
sim = Simulation(chid="wildfire", title="Wildfire Spread Simulation")

# Configure for wildfire simulation
sim.add(
    Misc(
        level_set_mode=1,  # Enable wildfire spread model
        tmpa=35.0,  # Hot ambient (35°C)
        humidity=15.0,  # Low humidity (15%)
        turbulence_model=TurbulenceModel.VREMAN,  # Use Vreman turbulence model
        c_vreman=0.07,  # Vreman constant
    )
)

# Time parameters - longer simulation for fire spread
sim.add(Time(t_end=600.0, dt=0.1))

# Large outdoor mesh - 100m x 100m x 30m
sim.add(Mesh(ijk=Grid3D.of(100, 100, 30), xb=Bounds3D.of(0, 100, 0, 100, 0, 30)))

# Add reaction for combustion
sim.add(Reaction(fuel="PROPANE"))

# ===== Terrain/Ground surface =====
sim.add(Surface(id="GROUND", color="BROWN"))
sim.add(Obstruction(xb=Bounds3D.of(0, 100, 0, 100, 0, 0), surf_id="GROUND"))

# ===== Vegetation/Fuel =====
sim.add(Surface(id="VEGETATION", hrrpua=500.0, color="GREEN"))
# Initial fuel patch
sim.add(Obstruction(xb=Bounds3D.of(45, 55, 45, 55, 0, 0.5), surf_id="VEGETATION"))

# ===== Open boundaries for wind =====
# West boundary (wind inlet)
sim.add(Vent(mb="XMIN", surf_id="OPEN", id="WIND_INLET"))

# East boundary (outlet)
sim.add(Vent(mb="XMAX", surf_id="OPEN", id="OUTLET"))

# South and North boundaries (open)
sim.add(Vent(mb="YMIN", surf_id="OPEN", id="SOUTH_OPEN"))
sim.add(Vent(mb="YMAX", surf_id="OPEN", id="NORTH_OPEN"))
# Top boundary (open)
sim.add(Vent(mb="ZMAX", surf_id="OPEN", id="TOP_OPEN"))

# ===== Monitoring devices =====
# Temperature grid at 2m height
for x in [25, 50, 75]:
    for y in [25, 50, 75]:
        sim.add(Device(id=f"TEMP_X{x}_Y{y}", quantity="TEMPERATURE", xyz=Point3D.of(x, y, 2)))

# Heat release rate (HRR requires xb for spatial statistic)
sim.add(Device(id="HRR", quantity="HRR", xb=Bounds3D.of(0, 100, 0, 100, 0, 30)))

# Wind velocity
sim.add(Device(id="WIND_VEL", quantity="VELOCITY", xyz=Point3D.of(50, 50, 5)))

# Generate FDS file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "wildfire.fds")
print(f"✓ Created FDS file: {output_file}")
if sim.misc_config:
    print(f"✓ Wildfire mode: LEVEL_SET_MODE={sim.misc_config.level_set_mode}")
    print(f"✓ Turbulence model: {sim.misc_config.turbulence_model.value}")
    print(f"✓ Ambient: {sim.misc_config.tmpa}°C, {sim.misc_config.humidity}% RH")
print("✓ Domain: 100m x 100m x 30m")
print(f"✓ Open boundaries: {len(sim.vents)}")
