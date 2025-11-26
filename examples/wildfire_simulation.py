"""
Wildfire Simulation Example - Advanced MISC and VENT Features

This example demonstrates:
- Wildfire simulation mode (LEVEL_SET_MODE)
- Custom turbulence model (Vreman)
- High ambient temperature and low humidity
- Open mesh boundaries for wind flow
- Large outdoor domain
"""

from pyfds import Simulation
from pyfds.core.namelists import TurbulenceModel

# Create simulation
sim = Simulation(chid="wildfire", title="Wildfire Spread Simulation")

# Configure for wildfire simulation
sim.set_misc(
    level_set_mode=1,  # Enable wildfire spread model
    tmpa=35.0,  # Hot ambient (35°C)
    humidity=15.0,  # Low humidity (15%)
    turbulence_model=TurbulenceModel.VREMAN,  # Use Vreman turbulence model
    c_vreman=0.07,  # Vreman constant
)

# Time parameters - longer simulation for fire spread
sim.time(t_end=600.0, dt=0.1)

# Large outdoor mesh - 100m x 100m x 30m
sim.mesh(ijk=(100, 100, 30), xb=(0, 100, 0, 100, 0, 30))

# ===== Terrain/Ground surface =====
sim.surface(id="GROUND", color="BROWN")
sim.obstruction(xb=(0, 100, 0, 100, 0, 0), surf_id="GROUND")

# ===== Vegetation/Fuel =====
sim.surface(id="VEGETATION", hrrpua=500.0, color="GREEN")

# Initial fuel patch
sim.obstruction(xb=(45, 55, 45, 55, 0, 0.5), surf_id="VEGETATION")

# ===== Open boundaries for wind =====
# West boundary (wind inlet)
sim.vent(mb="XMIN", surf_id="OPEN", id="WIND_INLET")

# East boundary (outlet)
sim.vent(mb="XMAX", surf_id="OPEN", id="OUTLET")

# South and North boundaries (open)
sim.vent(mb="YMIN", surf_id="OPEN", id="SOUTH_OPEN")
sim.vent(mb="YMAX", surf_id="OPEN", id="NORTH_OPEN")

# Top boundary (open)
sim.vent(mb="ZMAX", surf_id="OPEN", id="TOP_OPEN")

# ===== Monitoring devices =====
# Temperature grid at 2m height
for x in [25, 50, 75]:
    for y in [25, 50, 75]:
        sim.device(id=f"TEMP_X{x}_Y{y}", quantity="TEMPERATURE", xyz=(x, y, 2))

# Heat release rate
sim.device(id="HRR", quantity="HRR", xyz=(50, 50, 2))

# Wind velocity
sim.device(id="WIND_VEL", quantity="VELOCITY", xyz=(50, 50, 5))

# Generate FDS file
output_file = sim.write("wildfire.fds")
print(f"✓ Created FDS file: {output_file}")
print(f"✓ Wildfire mode: LEVEL_SET_MODE={sim.misc_params.level_set_mode}")
print(f"✓ Turbulence model: {sim.misc_params.turbulence_model.value}")
print(f"✓ Ambient: {sim.misc_params.tmpa}°C, {sim.misc_params.humidity}% RH")
print("✓ Domain: 100m x 100m x 30m")
print(f"✓ Open boundaries: {len(sim.vents)}")
