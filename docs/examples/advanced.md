# Advanced Examples

Complex PyFDS simulations demonstrating advanced features and techniques.

## Overview

These examples showcase:

- Multi-room scenarios with complex geometry
- HVAC systems with time-varying controls
- Multi-mesh simulations
- Advanced fire modeling
- Realistic building simulations

## Multi-Room Fire Spread

Simulate fire spreading between connected rooms.

```python
from pyfds import Simulation
from pyfds.core.geometry import Point3D

sim = Simulation(chid='multi_room_fire')
sim.time(t_end=900.0)

# Two-room domain
sim.mesh(id='ROOM1', ijk=(60, 50, 25), xb=(0, 6, 0, 5, 0, 2.5))
sim.mesh(id='ROOM2', ijk=(60, 50, 25), xb=(6, 12, 0, 5, 0, 2.5))

# Ambient conditions
sim.set_misc(tmpa=20.0, humidity=50.0)

# Material: Gypsum walls
sim.material(
    id='GYPSUM',
    conductivity=0.48,
    specific_heat=0.84,
    density=1440.0
)
sim.surface(
    id='GYPSUM_WALL',
    matl_id='GYPSUM',
    thickness=0.0127,
    color='WHITE'
)

# Room 1 walls (with door opening)
sim.obstruction(xb=(0, 0.1, 0, 5, 0, 2.5), surf_id='GYPSUM_WALL')  # West wall
sim.obstruction(xb=(0, 6, 0, 0.1, 0, 2.5), surf_id='GYPSUM_WALL')   # South wall
sim.obstruction(xb=(0, 6, 4.9, 5, 0, 2.5), surf_id='GYPSUM_WALL')   # North wall

# Dividing wall with door (at x=6)
sim.obstruction(xb=(5.9, 6.1, 0, 1.5, 0, 2.5), surf_id='GYPSUM_WALL')     # Below door
sim.obstruction(xb=(5.9, 6.1, 1.5, 2.5, 2.1, 2.5), surf_id='GYPSUM_WALL') # Above door
sim.obstruction(xb=(5.9, 6.1, 2.5, 5, 0, 2.5), surf_id='GYPSUM_WALL')     # Above door

# Room 2 walls
sim.obstruction(xb=(11.9, 12, 0, 5, 0, 2.5), surf_id='GYPSUM_WALL')  # East wall
sim.obstruction(xb=(6, 12, 0, 0.1, 0, 2.5), surf_id='GYPSUM_WALL')    # South wall
sim.obstruction(xb=(6, 12, 4.9, 5, 0, 2.5), surf_id='GYPSUM_WALL')    # North wall

# Fire in Room 1 (grows over time)
sim.ramp(id='FIRE_GROWTH', t=[0, 180, 600, 900], f=[0, 1, 1, 0.8])
sim.surface(id='FIRE', hrrpua=1500.0, ramp_q='FIRE_GROWTH', color='ORANGE')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# External door in Room 2
sim.vent(xb=(12, 12, 2, 3, 0, 2.1), surf_id='OPEN')

# Devices: Temperature in each room
sim.device(
    id='TEMP_ROOM1',
    quantity='TEMPERATURE',
    xyz=Point3D(3, 2.5, 2.0)
)
sim.device(
    id='TEMP_ROOM2',
    quantity='TEMPERATURE',
    xyz=Point3D(9, 2.5, 2.0)
)

# Smoke layer height
sim.device(
    id='LAYER_ROOM1',
    quantity='LAYER HEIGHT',
    xyz=Point3D(3, 2.5, 1.25)
)
sim.device(
    id='LAYER_ROOM2',
    quantity='LAYER HEIGHT',
    xyz=Point3D(9, 2.5, 1.25)
)

sim.write('multi_room_fire.fds')
```

**Features**:
- Two connected rooms with shared door
- Fire originates in Room 1
- Heat and smoke spread to Room 2
- Temperature and layer height monitoring

## HVAC System with Control Logic

Automatic HVAC shutdown when smoke is detected.

```python
from pyfds import Simulation

sim = Simulation(chid='hvac_control')
sim.time(t_end=900.0)
sim.mesh(ijk=(80, 60, 30), xb=(0, 8, 0, 6, 0, 3))

sim.set_misc(tmpa=22.0, humidity=50.0)

# Fire starts at t=120s
sim.ramp(id='FIRE_START', t=[0, 120, 240], f=[0, 0, 1])
sim.surface(id='FIRE', hrrpua=1200.0, ramp_q='FIRE_START')
sim.obstruction(xb=(3.5, 4.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE')

# Smoke detector at ceiling
sim.device(
    id='SMOKE_DET',
    quantity='OPTICAL DENSITY',
    xyz=Point3D(4, 3, 2.9)
)

# Control: HVAC operates until smoke detector activates
# In real PyFDS, this would use CTRL namelist
# Simplified: HVAC operates for first 300s, then shuts off
sim.ramp(id='HVAC_CTRL', t=[0, 300, 301], f=[1, 1, 0])

# Supply vent (with control)
sim.vent(
    xb=(1, 1.5, 1, 1.5, 3, 3),
    surf_id='HVAC',
    volume_flow=0.6,
    volume_flow_ramp='HVAC_CTRL'
)

# Exhaust vent (with control)
sim.vent(
    xb=(6.5, 7, 4.5, 5, 3, 3),
    surf_id='HVAC',
    volume_flow=-0.5,
    volume_flow_ramp='HVAC_CTRL'
)

# Emergency exit door
sim.vent(xb=(8, 8, 2.5, 3.5, 0, 2.1), surf_id='OPEN')

# Temperature devices
for i, x in enumerate([2, 4, 6]):
    sim.device(
        id=f'TEMP_{i+1}',
        quantity='TEMPERATURE',
        xyz=Point3D(x, 3, 2.5)
    )

sim.write('hvac_control.fds')
```

**Features**:
- HVAC supply and exhaust
- Time-based control (simplified)
- Smoke detection
- Multiple temperature sensors

## Atrium Fire

Large open space with high ceiling.

```python
from pyfds import Simulation

sim = Simulation(chid='atrium_fire')
sim.time(t_end=600.0)

# Tall atrium space (10m high)
sim.mesh(id='LOWER', ijk=(80, 80, 40), xb=(0, 16, 0, 16, 0, 5))
sim.mesh(id='UPPER', ijk=(80, 80, 40), xb=(0, 16, 0, 16, 5, 10))

sim.set_misc(tmpa=20.0)

# Concrete floor/walls
sim.material(
    id='CONCRETE',
    conductivity=1.8,
    specific_heat=0.88,
    density=2400.0
)
sim.surface(
    id='CONCRETE_SURF',
    matl_id='CONCRETE',
    thickness=0.3,
    color='GRAY'
)

# Walls
sim.obstruction(xb=(0, 0.3, 0, 16, 0, 10), surf_id='CONCRETE_SURF')    # West
sim.obstruction(xb=(15.7, 16, 0, 16, 0, 10), surf_id='CONCRETE_SURF')  # East
sim.obstruction(xb=(0, 16, 0, 0.3, 0, 10), surf_id='CONCRETE_SURF')    # South
sim.obstruction(xb=(0, 16, 15.7, 16, 0, 10), surf_id='CONCRETE_SURF')  # North

# Growing fire at floor level
sim.ramp(
    id='T_SQUARED',
    t=[0, 60, 120, 180, 240, 300, 600],
    f=[0, 0.25, 0.5, 0.75, 0.9, 1.0, 1.0]
)
sim.surface(id='FIRE', hrrpua=2500.0, ramp_q='T_SQUARED', color='RED')
sim.obstruction(xb=(7, 9, 7, 9, 0, 0.2), surf_id='FIRE')

# Openings at ground level (four doors)
sim.vent(xb=(0, 0, 6, 8, 0, 2.1), surf_id='OPEN')      # West door
sim.vent(xb=(16, 16, 6, 8, 0, 2.1), surf_id='OPEN')    # East door
sim.vent(xb=(6, 8, 0, 0, 0, 2.1), surf_id='OPEN')      # South door
sim.vent(xb=(6, 8, 16, 16, 0, 2.1), surf_id='OPEN')    # North door

# Vertical temperature profile
for z in [2, 4, 6, 8, 9.5]:
    sim.device(
        id=f'TEMP_Z{z:.1f}',
        quantity='TEMPERATURE',
        xyz=Point3D(8, 8, z)
    )

# Ceiling temperature
sim.device(
    id='CEILING_TEMP',
    quantity='CEILING TEMPERATURE',
    xyz=Point3D(8, 8, 9.8)
)

sim.write('atrium_fire.fds')
```

**Features**:
- Multi-mesh for tall space
- Growing fire (t-squared)
- Vertical temperature profile
- Multiple exits

## Data Center with Racks

Realistic data center with equipment racks and cooling.

```python
from pyfds import Simulation

sim = Simulation(chid='data_center')
sim.time(t_end=600.0)
sim.mesh(ijk=(100, 80, 30), xb=(0, 10, 0, 8, 0, 3))

sim.set_misc(tmpa=18.0, humidity=40.0)  # Cool, dry environment

# Raised floor
sim.obstruction(xb=(0, 10, 0, 8, 0, 0.3), surf_id='INERT', color='GRAY')

# Server racks (4 rows)
rack_width = 0.6
rack_depth = 1.0
rack_height = 2.1

for row in range(4):
    y_start = 1.5 + row * 1.6
    for col in range(8):
        x_start = 1.0 + col * 1.1
        sim.obstruction(
            xb=(x_start, x_start + rack_width,
                y_start, y_start + rack_depth,
                0.3, 0.3 + rack_height),
            surf_id='INERT',
            color='BLACK'
        )

# Cooling vents (under raised floor)
for x in [2, 5, 8]:
    sim.vent(
        xb=(x, x + 0.5, 3.5, 4, 0.3, 0.3),
        surf_id='HVAC',
        volume_flow=0.8
    )

# Fire starts in one rack (electrical)
sim.ramp(id='ELEC_FIRE', t=[0, 30, 120, 600], f=[0, 1, 1, 0.8])
sim.surface(id='FIRE', hrrpua=800.0, ramp_q='ELEC_FIRE', color='YELLOW')

# Fire on rack surface
sim.obstruction(
    xb=(4.4, 4.45, 3.1, 4.1, 1.5, 2.0),
    surf_id='FIRE'
)

# Exit doors
sim.vent(xb=(0, 0, 3.5, 4.5, 0.3, 2.4), surf_id='OPEN')
sim.vent(xb=(10, 10, 3.5, 4.5, 0.3, 2.4), surf_id='OPEN')

# Temperature sensors at rack height
for x in [2, 5, 8]:
    for y in [2, 4, 6]:
        sim.device(
            id=f'T_X{x}Y{y}',
            quantity='TEMPERATURE',
            xyz=Point3D(x, y, 1.5)
        )

sim.write('data_center.fds')
```

**Features**:
- Raised floor with under-floor cooling
- Array of server racks
- Electrical fire with realistic growth
- Comprehensive temperature monitoring

## Tunnel Fire with Ventilation

Road tunnel with longitudinal ventilation.

```python
from pyfds import Simulation

sim = Simulation(chid='tunnel_fire')
sim.time(t_end=900.0)

# Long tunnel (100m × 8m × 5m)
# Use multiple meshes for efficiency
for i in range(5):
    x_start = i * 20
    sim.mesh(
        id=f'TUNNEL_{i+1}',
        ijk=(100, 40, 25),
        xb=(x_start, x_start + 20, 0, 8, 0, 5)
    )

sim.set_misc(tmpa=15.0)  # Cool tunnel environment

# Concrete tunnel
sim.material(
    id='CONCRETE',
    conductivity=1.8,
    specific_heat=0.88,
    density=2400.0
)
sim.surface(
    id='TUNNEL_WALL',
    matl_id='CONCRETE',
    thickness=0.4
)

# Tunnel walls and ceiling
sim.obstruction(xb=(0, 100, 0, 0.3, 0, 5), surf_id='TUNNEL_WALL')       # South wall
sim.obstruction(xb=(0, 100, 7.7, 8, 0, 5), surf_id='TUNNEL_WALL')       # North wall
sim.obstruction(xb=(0, 100, 0, 8, 4.7, 5), surf_id='TUNNEL_WALL')       # Ceiling

# Vehicle fire at x=50m (truck fire)
sim.ramp(
    id='TRUCK_FIRE',
    t=[0, 120, 600, 900],
    f=[0, 1.0, 1.0, 0.7]
)
sim.surface(id='FIRE', hrrpua=3000.0, ramp_q='TRUCK_FIRE', color='ORANGE')
sim.obstruction(xb=(48, 52, 3, 5, 0, 2), surf_id='FIRE')

# Longitudinal ventilation (jet fans create flow from west to east)
sim.set_misc(wind_speed=3.0, wind_direction=0.0)  # 3 m/s eastward flow

# Portal openings
sim.vent(xb=(0, 0, 0.5, 7.5, 0.5, 4.5), surf_id='OPEN')      # West portal
sim.vent(xb=(100, 100, 0.5, 7.5, 0.5, 4.5), surf_id='OPEN')  # East portal

# Temperature array along tunnel centerline
for x in range(10, 91, 10):
    sim.device(
        id=f'TEMP_X{x}',
        quantity='TEMPERATURE',
        xyz=Point3D(x, 4, 3.5)
    )

# Visibility devices (critical for evacuation)
for x in [30, 50, 70]:
    sim.device(
        id=f'VIS_X{x}',
        quantity='VISIBILITY',
        xyz=Point3D(x, 4, 1.5)
    )

sim.write('tunnel_fire.fds')
```

**Features**:
- Multi-mesh long tunnel
- Vehicle fire (high HRR)
- Longitudinal ventilation
- Temperature and visibility monitoring

## Outdoor Pool Fire

Large liquid pool fire outdoors with wind.

```python
from pyfds import Simulation

sim = Simulation(chid='pool_fire')
sim.time(t_end=600.0)

# Large outdoor domain
sim.mesh(ijk=(120, 120, 60), xb=(-30, 30, -30, 30, 0, 30))

# Outdoor conditions with wind
sim.set_misc(
    tmpa=25.0,
    humidity=60.0,
    wind_speed=5.0,         # 5 m/s wind
    wind_direction=270.0,   # From west
    roughness_length=0.03   # Short grass
)

# Open boundaries (all sides)
for mb in ['XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMAX']:
    sim.vent(mb=mb, surf_id='OPEN')

# Circular pool fire (10m diameter)
sim.surface(
    id='POOL_FIRE',
    hrrpua=2000.0,  # Liquid fuel
    color='ORANGE'
)

sim.vent(
    xb=(-10, 10, -10, 10, 0, 0),
    surf_id='POOL_FIRE',
    xyz=(0, 0, 0),
    radius=5.0  # 5m radius = 10m diameter
)

# Downwind structures (to see radiation effects)
sim.material(id='WOOD', conductivity=0.12, specific_heat=1.0, density=500.0)
sim.surface(id='WOOD_SURF', matl_id='WOOD', thickness=0.02)

# Building at x=15m (downwind)
sim.obstruction(
    xb=(15, 20, -5, 5, 0, 6),
    surf_id='WOOD_SURF',
    color='TAN'
)

# Heat flux meters at various distances
for dist in [10, 15, 20, 25]:
    sim.device(
        id=f'HF_{dist}m',
        quantity='GAUGE HEAT FLUX',
        xyz=Point3D(dist, 0, 3),
        ior=1  # Facing fire (-X direction)
    )

# Temperature at various heights above fire
for z in [5, 10, 15, 20]:
    sim.device(
        id=f'TEMP_Z{z}',
        quantity='TEMPERATURE',
        xyz=Point3D(0, 0, z)
    )

sim.write('pool_fire.fds')
```

**Features**:
- Circular pool fire (annular vent)
- Wind effects on plume
- Radiation to downwind structure
- Heat flux measurements

## Pyrolysis Material Example

Simulate a charring wood fire with detailed pyrolysis kinetics.

```python
from pyfds import Simulation
from pyfds.builders import MaterialBuilder
from pyfds.core.namelists.pyrolysis import PyrolysisReaction, PyrolysisProduct

sim = Simulation(chid='pyrolysis_fire', title='Wood Pyrolysis Fire')
sim.time(t_end=600.0)

# Domain: 4m × 4m × 2m room
sim.mesh(ijk=(40, 40, 20), xb=(0, 4, 0, 4, 0, 2))

# Define char residue first (referenced by pyrolysis)
char = MaterialBuilder("CHAR") \
    .density(150) \
    .thermal_conductivity(0.1) \
    .specific_heat(1.0) \
    .build()

# Wood with structured pyrolysis reactions
wood = MaterialBuilder("WOOD") \
    .density(500) \
    .thermal_conductivity(0.13) \
    .specific_heat(2.5) \
    .add_reaction(
        PyrolysisReaction(
            a=1e10,                    # Pre-exponential factor [1/s]
            e=100000,                  # Activation energy [kJ/kmol]
            heat_of_reaction=500,      # Endothermic [kJ/kg]
            products=[
                PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
            ]
        )
    ) \
    .build()

# Add materials to simulation
sim.add_material(char)
sim.add_material(wood)

# Wood surface (pyrolyzing)
sim.surface(id='WOOD_SURF', matl_id='WOOD', thickness=0.02, color='BROWN')

# Concrete floor
sim.material(id='CONCRETE', conductivity=1.8, specific_heat=0.88, density=2400)
sim.surface(id='CONCRETE_SURF', matl_id='CONCRETE', thickness=0.1, color='GRAY')

# Room boundaries
sim.obstruction(xb=(0, 4, 0, 4, 0, 0.1), surf_id='CONCRETE_SURF')  # Floor
sim.obstruction(xb=(0, 4, 0, 4, 1.9, 2), surf_id='WOOD_SURF')      # Ceiling
sim.obstruction(xb=(0, 0.1, 0, 4, 0, 2), surf_id='WOOD_SURF')      # Walls
sim.obstruction(xb=(3.9, 4, 0, 4, 0, 2), surf_id='WOOD_SURF')
sim.obstruction(xb=(0, 4, 0, 0.1, 0, 2), surf_id='WOOD_SURF')
sim.obstruction(xb=(0, 4, 3.9, 4, 0, 2), surf_id='WOOD_SURF')

# Ignition source (small fire to start pyrolysis)
sim.surface(id='IGNITER', hrrpua=50.0, color='RED')
sim.obstruction(xb=(1.9, 2.1, 1.9, 2.1, 0, 0.05), surf_id='IGNITER')

# Measurements
sim.device(id='TEMP_CENTER', quantity='TEMPERATURE', xyz=(2, 2, 1.5))
sim.device(id='MLRPUA', quantity='MLRPUA', xyz=(2, 2, 0.01))  # Mass loss rate

sim.write('pyrolysis_fire.fds')
```

**Features Demonstrated:**
- Structured pyrolysis API with `PyrolysisReaction` and `PyrolysisProduct`
- Multi-step material decomposition (wood → char + gases)
- Material cross-references (wood references char)
- Pyrolysis-driven fire spread
- Mass loss rate measurement

**Expected Results:**
- Initial ignition followed by pyrolysis-driven fire growth
- Char layer formation on wood surfaces
- Peak temperatures: 800-1000°C
- Total mass loss: ~25% (char residue)

## Best Practices for Advanced Simulations

### 1. Multi-Mesh Alignment

Ensure meshes align at boundaries:

```python
# Good: Meshes align perfectly at x=10
sim.mesh(id='MESH1', ijk=(50, 40, 25), xb=(0, 10, 0, 8, 0, 5))
sim.mesh(id='MESH2', ijk=(50, 40, 25), xb=(10, 20, 0, 8, 0, 5))

# Bad: Meshes overlap or have gaps
sim.mesh(id='MESH1', ijk=(50, 40, 25), xb=(0, 10, 0, 8, 0, 5))
sim.mesh(id='MESH2', ijk=(50, 40, 25), xb=(9.9, 20, 0, 8, 0, 5))  # Gap/overlap
```

### 2. Appropriate Fire Growth

Use realistic fire growth curves:

```python
# Realistic t-squared growth
sim.ramp(id='GROWTH', t=[0, 100, 200, 300], f=[0, 0.33, 0.67, 1.0])

# Avoid instantaneous fires (unless justified)
sim.ramp(id='INSTANT', t=[0, 1], f=[0, 1])  # Usually unrealistic
```

### 3. Adequate Device Coverage

```python
# Good: Distributed measurements
for x in range(2, 9, 2):
    for y in range(2, 7, 2):
        sim.device(id=f'T_X{x}Y{y}', quantity='TEMPERATURE', xyz=Point3D(x, y, 2))

# Poor: Single measurement point
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D(5, 4, 2))
```

## Next Steps

- [User Guide](../guide/index.md) - Detailed feature documentation
- [Parametric Studies](parametric.md) - Sensitivity analysis
- [Workflows](workflows.md) - Complete simulation workflows
- [API Reference](../api/index.md) - Full API documentation

---

[Parametric Studies →](parametric.md){ .md-button .md-button--primary }
