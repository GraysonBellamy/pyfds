# Special Applications

Specialized simulation types for specific scenarios and research applications.

## Overview

These examples demonstrate PyFDS for specialized applications:

- Wildfire and vegetation fires
- Heat transfer only (no combustion)
- Sprinkler system activation
- Aircraft and vehicle fires
- Industrial scenarios

## Wildfire Simulation

Large-scale outdoor fire with vegetation and wind.

```python
from pyfds import Simulation

sim = Simulation(chid='wildfire')
sim.time(t_end=1800.0)  # 30 minutes

# Large outdoor domain (100m × 100m × 30m)
sim.mesh(ijk=(100, 100, 30), xb=(0, 100, 0, 100, 0, 30))

# Hot, dry, windy conditions
sim.set_misc(
    tmpa=35.0,              # Hot day (35°C)
    humidity=15.0,          # Low humidity
    wind_speed=5.0,         # 5 m/s wind
    wind_direction=270.0    # From west (blowing east)
)

# Open boundaries for wind flow
for mb in ['XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMAX']:
    sim.vent(mb=mb, surf_id='OPEN')

# Vegetation fuel (grass/brush)
sim.reaction(
    id='VEGETATION',
    fuel='CELLULOSE',
    heat_of_combustion=15000.0,
    soot_yield=0.015,
    co_yield=0.004
)

# Initial fire line (ignition source)
sim.surface(
    id='IGNITION',
    hrrpua=500.0,
    reac_id='VEGETATION',
    color='ORANGE'
)

# Fire starts along western edge
sim.obstruction(
    xb=(10, 15, 45, 55, 0, 0.5),
    surf_id='IGNITION'
)

# Temperature measurements downwind
for x in [30, 50, 70, 90]:
    sim.device(
        id=f'TEMP_X{x}',
        quantity='TEMPERATURE',
        xyz=(x, 50, 2)
    )

# Heat flux to potential targets
for x in [60, 80]:
    sim.device(
        id=f'HF_X{x}',
        quantity='GAUGE HEAT FLUX',
        xyz=(x, 50, 2),
        ior=1  # Facing fire
    )

sim.write('wildfire.fds')
```

**Features**:
- Large outdoor domain
- Wind-driven fire spread
- Vegetation fuel properties
- Downwind temperature and heat flux monitoring

## Heat Transfer Only

Thermal analysis without combustion (pure conduction/radiation).

```python
from pyfds import Simulation

sim = Simulation(chid='heat_transfer')
sim.time(t_end=3600.0)  # 1 hour
sim.mesh(ijk=(60, 40, 30), xb=(0, 3, 0, 2, 0, 1.5))

# Disable combustion
sim.set_misc(no_combustion=True)

# Steel structure properties
sim.material(
    id='STEEL',
    conductivity=45.8,
    specific_heat=0.46,
    density=7850.0
)

sim.surface(
    id='STEEL_SURF',
    matl_id='STEEL',
    thickness=0.01,  # 10mm steel plate
    emissivity=0.85
)

# Hot boundary (fire exposure side)
sim.surface(
    id='HOT_BC',
    tmp_front=800.0,  # 800°C constant temperature
    color='RED'
)

# Steel plate between hot and cold sides
sim.obstruction(
    xb=(1.45, 1.55, 0, 2, 0, 1.5),
    surf_id='STEEL_SURF'
)

# Hot side exposure
sim.vent(
    xb=(0, 0, 0, 2, 0, 1.5),
    surf_id='HOT_BC'
)

# Cold side (ambient)
sim.vent(
    xb=(3, 3, 0, 2, 0, 1.5),
    surf_id='OPEN'
)

# Temperature through steel plate
for i, x in enumerate([1.45, 1.475, 1.50, 1.525, 1.55]):
    sim.device(
        id=f'TEMP_STEEL_{i}',
        quantity='TEMPERATURE',
        xyz=(x, 1, 0.75)
    )

# Heat flux on cold side
sim.device(
    id='HF_COLD_SIDE',
    quantity='GAUGE HEAT FLUX',
    xyz=(1.56, 1, 0.75),
    ior=1
)

sim.write('heat_transfer.fds')
```

**Features**:
- No combustion (thermal only)
- Temperature boundary conditions
- Heat conduction through materials
- Temperature gradient measurement

## Sprinkler System

Automatic sprinkler activation with suppression.

```python
from pyfds import Simulation

sim = Simulation(chid='sprinkler_system')
sim.time(t_end=600.0)
sim.mesh(ijk=(80, 60, 30), xb=(0, 8, 0, 6, 0, 3))

# Fire growing over time
sim.ramp(id='FIRE_GROWTH', t=[0, 120, 300], f=[0, 0.5, 1.0])
sim.surface(id='FIRE', hrrpua=2000.0, ramp_q='FIRE_GROWTH')
sim.obstruction(xb=(3.5, 4.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE')

# Sprinkler heads (4 heads, K-factor 80)
sprinkler_locations = [
    (2, 2, 2.95),
    (6, 2, 2.95),
    (2, 4, 2.95),
    (6, 4, 2.95)
]

for i, (x, y, z) in enumerate(sprinkler_locations):
    # Temperature link for each sprinkler
    sim.device(
        id=f'LINK_{i+1}',
        quantity='TEMPERATURE',
        xyz=(x, y, z)
    )

    # Control: Activate at 68°C (typical sprinkler)
    sim.control(
        id=f'SPRINKLER_{i+1}_CTRL',
        input_id=f'LINK_{i+1}',
        setpoint=68.0,
        latch=True  # Once activated, stays on
    )

    # Water spray surface
    sim.surface(
        id=f'SPRAY_{i+1}',
        mass_flux=0.05,  # Water spray rate (kg/m²/s)
        tmp_front=20.0,  # Water temperature
        ctrl_id=f'SPRINKLER_{i+1}_CTRL'
    )

    # Spray vent
    sim.vent(
        xb=(x-0.2, x+0.2, y-0.2, y+0.2, z, z),
        surf_id=f'SPRAY_{i+1}'
    )

# Monitor temperatures
for loc, (x, y) in [('FIRE', (4, 3)), ('CORNER', (1, 1))]:
    sim.device(
        id=f'TEMP_{loc}',
        quantity='TEMPERATURE',
        xyz=(x, y, 2.5)
    )

# Monitor HRR (will decrease after sprinkler activation)
sim.device(
    id='HRR',
    quantity='HRR',
    xyz=(4, 3, 1.5)
)

sim.write('sprinkler_system.fds')
```

**Features**:
- Multiple sprinkler heads with thermal links
- Automatic activation controls
- Water spray suppression
- Fire suppression monitoring

## Aircraft Cabin Fire

Aircraft interior fire scenario.

```python
from pyfds import Simulation

sim = Simulation(chid='aircraft_fire')
sim.time(t_end=600.0)

# Aircraft cabin dimensions (20m long × 3m wide × 2m high)
sim.mesh(ijk=(100, 30, 20), xb=(0, 20, 0, 3, 0, 2))

# Cabin materials (composite/aluminum)
sim.material(
    id='AIRCRAFT_PANEL',
    conductivity=0.25,
    specific_heat=1.0,
    density=1600.0
)

sim.surface(
    id='CABIN_WALL',
    matl_id='AIRCRAFT_PANEL',
    thickness=0.003,  # 3mm panels
    color='GRAY'
)

# Cabin walls
sim.obstruction(xb=(0, 20, 0, 0.1, 0, 2), surf_id='CABIN_WALL')
sim.obstruction(xb=(0, 20, 2.9, 3, 0, 2), surf_id='CABIN_WALL')
sim.obstruction(xb=(0, 20, 0, 3, 1.95, 2), surf_id='CABIN_WALL')

# Seat fire (polyurethane foam)
sim.reaction(
    id='FOAM',
    fuel='POLYURETHANE',
    heat_of_combustion=25000.0,
    soot_yield=0.131,
    co_yield=0.042,
    radiative_fraction=0.40
)

sim.surface(id='SEAT_FIRE', hrrpua=800.0, reac_id='FOAM')

# Burning seat at row 10
sim.obstruction(
    xb=(9.5, 10, 0.5, 1.5, 0.4, 1.2),
    surf_id='SEAT_FIRE'
)

# Emergency exits
sim.vent(xb=(0, 0, 1, 2, 0, 1.8), surf_id='OPEN')      # Forward
sim.vent(xb=(20, 20, 1, 2, 0, 1.8), surf_id='OPEN')    # Aft

# Dense monitoring along aisle
for x in range(2, 19, 2):
    # Temperature at head height (seated)
    sim.device(
        id=f'TEMP_X{x:02d}',
        quantity='TEMPERATURE',
        xyz=(x, 1.5, 1.0)
    )

    # Visibility (critical for evacuation)
    sim.device(
        id=f'VIS_X{x:02d}',
        quantity='VISIBILITY',
        xyz=(x, 1.5, 1.0)
    )

    # CO concentration (toxic)
    sim.device(
        id=f'CO_X{x:02d}',
        quantity='VOLUME FRACTION',
        spec_id='CARBON MONOXIDE',
        xyz=(x, 1.5, 1.0)
    )

sim.write('aircraft_fire.fds')
```

**Features**:
- Confined aircraft geometry
- Polyurethane foam fire (high soot)
- Visibility along evacuation path
- Toxic gas monitoring

## Battery Fire (Thermal Runaway)

Lithium-ion battery thermal runaway simulation.

```python
from pyfds import Simulation

sim = Simulation(chid='battery_fire')
sim.time(t_end=600.0)
sim.mesh(ijk=(60, 60, 40), xb=(0, 3, 0, 3, 0, 2))

# Battery thermal runaway characteristics
sim.ramp(
    id='THERMAL_RUNAWAY',
    t=[0, 30, 60, 90, 120, 600],
    f=[0, 0.5, 1.0, 0.8, 0.3, 0.1]  # Rapid onset, gradual decay
)

# Battery fire (high heat, moderate smoke)
sim.surface(
    id='BATTERY_FIRE',
    hrrpua=1500.0,
    ramp_q='THERMAL_RUNAWAY',
    color='ORANGE'
)

# Battery pack (small footprint, intense heat)
sim.obstruction(
    xb=(1.4, 1.6, 1.4, 1.6, 0.5, 0.7),
    surf_id='BATTERY_FIRE'
)

# Thermal propagation to adjacent batteries
sim.device(
    id='TEMP_ADJACENT_1',
    quantity='TEMPERATURE',
    xyz=(1.7, 1.5, 0.6)
)

sim.device(
    id='TEMP_ADJACENT_2',
    quantity='TEMPERATURE',
    xyz=(1.3, 1.5, 0.6)
)

# Heat flux to surrounding objects
for angle in [0, 90, 180, 270]:
    import math
    r = 0.5  # 0.5m from battery
    x = 1.5 + r * math.cos(math.radians(angle))
    y = 1.5 + r * math.sin(math.radians(angle))

    sim.device(
        id=f'HF_{angle}DEG',
        quantity='GAUGE HEAT FLUX',
        xyz=(x, y, 0.6),
        ior=1
    )

# Toxic gas monitoring (HF from electrolyte)
# Note: Simplified - real battery fires produce complex chemistry
sim.device(
    id='SMOKE_DETECTOR',
    quantity='OPTICAL DENSITY',
    xyz=(1.5, 1.5, 1.8)
)

sim.write('battery_fire.fds')
```

**Features**:
- Thermal runaway growth curve
- Small, intense heat source
- Thermal propagation monitoring
- Radial heat flux measurements

## Industrial Process Fire

Chemical plant/industrial scenario.

```python
from pyfds import Simulation

sim = Simulation(chid='industrial_fire')
sim.time(t_end=900.0)

# Large industrial space
sim.mesh(ijk=(120, 80, 50), xb=(0, 24, 0, 16, 0, 10))

# Concrete structure
sim.material(id='CONCRETE', conductivity=1.8, specific_heat=0.88, density=2400.0)
sim.surface(id='CONCRETE_WALL', matl_id='CONCRETE', thickness=0.3)

# Walls
sim.obstruction(xb=(0, 0.3, 0, 16, 0, 10), surf_id='CONCRETE_WALL')
sim.obstruction(xb=(23.7, 24, 0, 16, 0, 10), surf_id='CONCRETE_WALL')

# Flammable liquid pool fire (heptane)
sim.reaction(
    id='HEPTANE',
    fuel='N-HEPTANE',
    heat_of_combustion=44600.0,
    soot_yield=0.037,
    co_yield=0.010,
    radiative_fraction=0.33
)

# Large pool fire
sim.surface(
    id='POOL_FIRE',
    hrrpua=2500.0,
    reac_id='HEPTANE',
    color='ORANGE'
)

# 4m diameter pool
sim.vent(
    xb=(10, 14, 6, 10, 0, 0),
    surf_id='POOL_FIRE',
    xyz=(12, 8, 0),
    radius=2.0
)

# Pressure relief vents at roof
sim.vent(xb=(8, 10, 6, 8, 10, 10), surf_id='OPEN')
sim.vent(xb=(14, 16, 6, 8, 10, 10), surf_id='OPEN')

# Heat flux to critical equipment
equipment_locations = [(6, 4, 3), (18, 4, 3), (12, 12, 3)]

for i, (x, y, z) in enumerate(equipment_locations):
    sim.device(
        id=f'HF_EQUIPMENT_{i+1}',
        quantity='GAUGE HEAT FLUX',
        xyz=(x, y, z),
        ior=2  # Facing toward center
    )

# High-level temperature (roof structure)
for x in [6, 12, 18]:
    sim.device(
        id=f'TEMP_ROOF_X{x}',
        quantity='TEMPERATURE',
        xyz=(x, 8, 9.5)
    )

sim.write('industrial_fire.fds')
```

**Features**:
- Large industrial space
- Flammable liquid pool fire
- Pressure relief venting
- Equipment heat exposure

## Best Practices for Special Applications

### 1. Wildfire Simulations

```python
# Use large domain with open boundaries
sim.mesh(ijk=(100, 100, 30), xb=(0, 100, 0, 100, 0, 30))

# Realistic wind conditions
sim.set_misc(wind_speed=5.0, wind_direction=270.0, roughness_length=0.1)

# Open all boundaries
for mb in ['XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMAX']:
    sim.vent(mb=mb, surf_id='OPEN')
```

### 2. Heat Transfer Studies

```python
# Disable combustion for pure thermal analysis
sim.set_misc(no_combustion=True)

# Use temperature boundary conditions
sim.surface(id='HOT_BC', tmp_front=800.0)
```

### 3. Sprinkler Systems

```python
# Realistic activation temperature
sim.control(id='SPRINKLER', input_id='LINK', setpoint=68.0, latch=True)

# Appropriate water flux
sim.surface(id='SPRAY', mass_flux=0.05)  # kg/m²/s
```

### 4. Confined Spaces

```python
# Ensure adequate resolution in small spaces
# Aircraft: 0.1-0.2m cells
# Battery enclosure: 0.05-0.1m cells

# Monitor visibility and toxicity
sim.device(id='VIS', quantity='VISIBILITY', xyz=(...))
sim.device(id='CO', quantity='VOLUME FRACTION', spec_id='CARBON MONOXIDE', xyz=(...))
```

## Next Steps

- [Basic Examples](basic.md) - Fundamental simulations
- [Advanced Examples](advanced.md) - Complex scenarios
- [Parametric Studies](parametric.md) - Sensitivity analysis
- [User Guide](../guide/index.md) - Detailed documentation

---

[Parametric Studies →](parametric.md){ .md-button .md-button--primary }
