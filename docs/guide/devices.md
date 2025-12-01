# Devices & Measurement

Monitor simulation quantities using device measurements.

## Overview

**Devices** (DEVC namelist) record time-series data at specific locations or surfaces. They are essential for:

- Temperature monitoring
- Heat flux measurements
- Velocity tracking
- Pressure monitoring
- Species concentration
- Layer height detection

```python
from pyfds import Device
from pyfds.core.geometry import Point3D

# Temperature device at a point
sim.add(Device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=Point3D.of(2.5, 2.5, 2.4)
)
```

## Device Types

### Point Devices

Measure quantities at a single point in space:

```python
# Temperature at ceiling
sim.add(Device(
    id='TEMP_1',
    quantity='TEMPERATURE',
    xyz=Point3D.of(3.0, 2.5, 2.4)
)

# Velocity at doorway
sim.add(Device(
    id='VEL_DOOR',
    quantity='VELOCITY',
    xyz=Point3D.of(5.0, 2.5, 1.0)
)

# Pressure at floor
sim.add(Device(
    id='PRES_FLOOR',
    quantity='PRESSURE',
    xyz=Point3D.of(2.5, 2.5, 0.1)
)
```

### Surface Devices

Measure quantities on a surface using `ior` (index of orientation):

```python
# Heat flux on wall (facing into room)
sim.add(Device(
    id='HF_WALL',
    quantity='GAUGE HEAT FLUX',
    xyz=Point3D.of(0.1, 2.5, 1.5),
    ior=1  # +X direction (surface normal)
)

# Wall temperature
sim.add(Device(
    id='TEMP_WALL',
    quantity='WALL TEMPERATURE',
    xyz=Point3D.of(0.1, 2.5, 1.5),
    ior=1
)
```

**IOR Values**:
- `1` = +X direction (east face)
- `-1` = -X direction (west face)
- `2` = +Y direction (north face)
- `-2` = -Y direction (south face)
- `3` = +Z direction (ceiling)
- `-3` = -Z direction (floor)

### Area/Volume Devices

Average over a region or line:

```python
# Average temperature in a line
sim.add(Device(
    id='TEMP_LINE',
    quantity='TEMPERATURE',
    xyz=Point3D.of(2.5, 2.5, 0.5),
    xyz2=(2.5, 2.5, 2.4)  # Vertical line
)

# Average over an area
sim.add(Device(
    id='TEMP_AREA',
    quantity='TEMPERATURE',
    xb=Bounds3D.of(2, 3, 2, 3, 2.4, 2.4)  # Ceiling area
)
```

## Common Quantities

### Temperature

```python
# Gas temperature
sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(3, 2, 2)))

# Wall temperature
sim.add(Device(id='WALL_TEMP', quantity='WALL TEMPERATURE', xyz=Point3D.of(0, 2, 1), ior=1))

# Ceiling temperature
sim.add(Device(id='CEIL_TEMP', quantity='CEILING TEMPERATURE', xyz=Point3D.of(3, 2, 2.4)))
```

### Velocity

```python
# Total velocity magnitude
sim.add(Device(id='VEL', quantity='VELOCITY', xyz=Point3D.of(3, 2, 2)))

# U-velocity (X-component)
sim.add(Device(id='U_VEL', quantity='U-VELOCITY', xyz=Point3D.of(3, 2, 2)))

# V-velocity (Y-component)
sim.add(Device(id='V_VEL', quantity='V-VELOCITY', xyz=Point3D.of(3, 2, 2)))

# W-velocity (Z-component)
sim.add(Device(id='W_VEL', quantity='W-VELOCITY', xyz=Point3D.of(3, 2, 2)))
```

### Heat Flux

```python
# Gauge heat flux (incident radiation + convection)
sim.add(Device(
    id='HF_GAUGE',
    quantity='GAUGE HEAT FLUX',
    xyz=Point3D.of(0.1, 2, 1.5),
    ior=1
)

# Radiative heat flux only
sim.add(Device(
    id='HF_RAD',
    quantity='RADIATIVE HEAT FLUX',
    xyz=Point3D.of(0.1, 2, 1.5),
    ior=1
)

# Convective heat flux only
sim.add(Device(
    id='HF_CONV',
    quantity='CONVECTIVE HEAT FLUX',
    xyz=Point3D.of(0.1, 2, 1.5),
    ior=1
)
```

### Pressure

```python
# Static pressure
sim.add(Device(id='PRES', quantity='PRESSURE', xyz=Point3D.of(3, 2, 1)))

# Pressure coefficient
sim.add(Device(id='PRES_COEF', quantity='PRESSURE COEFFICIENT', xyz=Point3D.of(3, 2, 1)))
```

### Species and Smoke

```python
# Oxygen concentration
sim.add(Device(id='O2', quantity='VOLUME FRACTION', spec_id='OXYGEN', xyz=Point3D.of(3, 2, 2)))

# CO2 concentration
sim.add(Device(id='CO2', quantity='VOLUME FRACTION', spec_id='CARBON DIOXIDE', xyz=Point3D.of(3, 2, 2)))

# Soot density
sim.add(Device(id='SOOT', quantity='SOOT DENSITY', xyz=Point3D.of(3, 2, 2)))

# Visibility
sim.add(Device(id='VIS', quantity='VISIBILITY', xyz=Point3D.of(3, 2, 1.5)))

# Optical density
sim.add(Device(id='OD', quantity='OPTICAL DENSITY', xyz=Point3D.of(3, 2, 2.4)))
```

### Layer Height

```python
# Smoke layer height
sim.add(Device(
    id='LAYER_HEIGHT',
    quantity='LAYER HEIGHT',
    xyz=Point3D.of(3, 2, 1.25)  # Reference height (typically mid-height)
)
```

## Quantity Reference Table

| Quantity | Units | Type | Description |
|----------|-------|------|-------------|
| `TEMPERATURE` | °C | Point | Gas temperature |
| `WALL TEMPERATURE` | °C | Surface | Solid surface temperature |
| `CEILING TEMPERATURE` | °C | Point | Temperature at ceiling |
| `VELOCITY` | m/s | Point | Total velocity magnitude |
| `U-VELOCITY` | m/s | Point | X-component velocity |
| `V-VELOCITY` | m/s | Point | Y-component velocity |
| `W-VELOCITY` | m/s | Point | Z-component velocity |
| `PRESSURE` | Pa | Point | Static pressure |
| `GAUGE HEAT FLUX` | kW/m² | Surface | Total incident heat flux |
| `RADIATIVE HEAT FLUX` | kW/m² | Surface | Radiative component |
| `CONVECTIVE HEAT FLUX` | kW/m² | Surface | Convective component |
| `VOLUME FRACTION` | - | Point | Species concentration |
| `SOOT DENSITY` | kg/m³ | Point | Soot concentration |
| `VISIBILITY` | m | Point | Visibility distance |
| `OPTICAL DENSITY` | 1/m | Point | Smoke obscuration |
| `LAYER HEIGHT` | m | Point | Smoke layer interface |
| `DENSITY` | kg/m³ | Point | Gas density |
| `INTEGRATED INTENSITY` | kW/m² | Surface | Time-integrated heat flux |

## Device Arrays

### Grid of Devices

Create multiple devices in a loop:

```python
# Temperature grid at ceiling
for i in range(5):
    for j in range(4):
        x = 1 + i * 1.0
        y = 1 + j * 1.0
        sim.add(Device(
            id=f'TEMP_{i}_{j}',
            quantity='TEMPERATURE',
            xyz=Point3D.of(x, y, 2.4)
        )
```

### Vertical Profile

```python
# Temperature profile along vertical line
x, y = 2.5, 2.5
for k, z in enumerate([0.5, 1.0, 1.5, 2.0, 2.4]):
    sim.add(Device(
        id=f'TEMP_Z{k}',
        quantity='TEMPERATURE',
        xyz=Point3D.of(x, y, z)
    )
```

### Line of Devices

```python
# Temperature along centerline
y, z = 2.5, 2.0
for i, x in enumerate(range(1, 5)):
    sim.add(Device(
        id=f'TEMP_X{i}',
        quantity='TEMPERATURE',
        xyz=Point3D.of(x, y, z)
    )
```

## Output Control

### Statistics

Compute time-averaged quantities:

```python
# Time-averaged temperature
sim.add(Device(
    id='TEMP_AVG',
    quantity='TEMPERATURE',
    xyz=Point3D.of(3, 2, 2),
    statistics='MEAN'
)

# Standard deviation
sim.add(Device(
    id='TEMP_STD',
    quantity='TEMPERATURE',
    xyz=Point3D.of(3, 2, 2),
    statistics='STANDARD DEVIATION'
)
```

### Output Intervals

Control output frequency:

```python
# Output every 10 seconds
sim.add(Device(
    id='TEMP',
    quantity='TEMPERATURE',
    xyz=Point3D.of(3, 2, 2),
    dt=10.0  # Output interval
)
```

## Complete Examples

### Comprehensive Room Monitoring

```python
from pyfds import Simulation

sim = Simulation(chid='monitored_room')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 40, 25), xb=Bounds3D.of(0, 5, 0, 4, 0, 2.5)))

# Fire
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 1.5, 2.5, 0, 0.1), surf_id='FIRE'))

# Temperature at different heights
for z, label in [(0.5, 'LOW'), (1.5, 'MID'), (2.4, 'HIGH')]:
    sim.add(Device(
        id=f'TEMP_{label}',
        quantity='TEMPERATURE',
        xyz=Point3D.of(2.5, 2, z)
    )

# Velocity at door
sim.add(Device(
    id='VEL_DOOR',
    quantity='VELOCITY',
    xyz=Point3D.of(5, 2, 1.0)
)

# Heat flux on walls (all 4 walls)
walls = [
    ('WEST', 0.1, 2, 1.5, 1),
    ('EAST', 4.9, 2, 1.5, -1),
    ('SOUTH', 2.5, 0.1, 1.5, 2),
    ('NORTH', 2.5, 3.9, 1.5, -2)
]

for name, x, y, z, ior in walls:
    sim.add(Device(
        id=f'HF_{name}',
        quantity='GAUGE HEAT FLUX',
        xyz=Point3D.of(x, y, z),
        ior=ior
    )

# Smoke layer height
sim.add(Device(
    id='LAYER_HEIGHT',
    quantity='LAYER HEIGHT',
    xyz=Point3D.of(2.5, 2, 1.25)
)

# Visibility at eye level
sim.add(Device(
    id='VISIBILITY',
    quantity='VISIBILITY',
    xyz=Point3D.of(2.5, 2, 1.5)
)

# Oxygen concentration
sim.add(Device(
    id='O2_CONC',
    quantity='VOLUME FRACTION',
    spec_id='OXYGEN',
    xyz=Point3D.of(2.5, 2, 1.5)
)

sim.write('monitored_room.fds')
```

### Heat Flux Array on Target Surface

```python
sim = Simulation(chid='heat_flux_array')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(80, 60, 30), xb=Bounds3D.of(0, 8, 0, 6, 0, 3)))

# Fire
sim.add(Surface(id='FIRE', hrrpua=2000.0))
sim.add(Obstruction(xb=Bounds3D.of(3.5, 4.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE'))

# Target wall at x=0
# Heat flux measurement array (3×3 grid)
for i in range(3):
    for j in range(3):
        y = 1.5 + i * 1.0  # 1.5, 2.5, 3.5
        z = 0.5 + j * 1.0  # 0.5, 1.5, 2.5

        sim.add(Device(
            id=f'HF_Y{i}_Z{j}',
            quantity='GAUGE HEAT FLUX',
            xyz=Point3D.of(0.1, y, z),
            ior=1  # Facing into domain (+X)
        )

        # Also measure wall temperature
        sim.add(Device(
            id=f'WALL_TEMP_Y{i}_Z{j}',
            quantity='WALL TEMPERATURE',
            xyz=Point3D.of(0.1, y, z),
            ior=1
        )

sim.write('heat_flux_array.fds')
```

### Vertical Temperature Profile

```python
sim = Simulation(chid='vertical_profile')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 50), xb=Bounds3D.of(0, 5, 0, 5, 0, 5)))

# Fire at floor
sim.add(Surface(id='FIRE', hrrpua=1500.0))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE'))

# Vertical profile above fire (every 0.5m from 0.5 to 4.5m)
x_center, y_center = 2.5, 2.5

for i, z in enumerate([0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5]):
    # Temperature
    sim.add(Device(
        id=f'TEMP_Z{int(z*10):02d}',
        quantity='TEMPERATURE',
        xyz=Point3D.of(x_center, y_center, z)
    )

    # Vertical velocity
    sim.add(Device(
        id=f'W_VEL_Z{int(z*10):02d}',
        quantity='W-VELOCITY',
        xyz=Point3D.of(x_center, y_center, z)
    )

    # Oxygen concentration
    sim.add(Device(
        id=f'O2_Z{int(z*10):02d}',
        quantity='VOLUME FRACTION',
        spec_id='OXYGEN',
        xyz=Point3D.of(x_center, y_center, z)
    )

sim.write('vertical_profile.fds')
```

### Corridor Smoke Spread

```python
sim = Simulation(chid='corridor_smoke')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(200, 40, 25), xb=Bounds3D.of(0, 20, 0, 4, 0, 2.5)))

# Fire at one end
sim.add(Surface(id='FIRE', hrrpua=800.0))
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1.5, 2.5, 0, 0.1), surf_id='FIRE'))

# Visibility and temperature along corridor
for x in range(2, 19, 2):  # Every 2m along corridor
    # Visibility at head height
    sim.add(Device(
        id=f'VIS_X{x:02d}',
        quantity='VISIBILITY',
        xyz=Point3D.of(x, 2, 1.5)
    )

    # Temperature at ceiling
    sim.add(Device(
        id=f'TEMP_X{x:02d}',
        quantity='TEMPERATURE',
        xyz=Point3D.of(x, 2, 2.4)
    )

    # Smoke layer height
    sim.add(Device(
        id=f'LAYER_X{x:02d}',
        quantity='LAYER HEIGHT',
        xyz=Point3D.of(x, 2, 1.25)
    )

# Door at far end
sim.add(Vent(xb=Bounds3D.of(20, 20, 1.5, 2.5, 0, 2.1), surf_id='OPEN'))

sim.write('corridor_smoke.fds')
```

## Best Practices

### 1. Strategic Device Placement

```python
# Good: Devices at key locations
sim.add(Device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2, 2.4)))
sim.add(Device(id='TEMP_EYE', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2, 1.5)))
sim.add(Device(id='TEMP_FLOOR', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2, 0.5)))

# Avoid: Too many redundant devices
# (creates large output files without additional insight)
```

### 2. Consistent Naming

```python
# Good: Descriptive, consistent naming
sim.add(Device(id='TEMP_ROOM1_CEILING', quantity='TEMPERATURE', xyz=Point3D.of(3, 2, 2.4)))
sim.add(Device(id='TEMP_ROOM2_CEILING', quantity='TEMPERATURE', xyz=Point3D.of(8, 2, 2.4)))

# Poor: Unclear naming
sim.add(Device(id='D1', quantity='TEMPERATURE', xyz=Point3D.of(3, 2, 2.4)))
sim.add(Device(id='D2', quantity='TEMPERATURE', xyz=Point3D.of(8, 2, 2.4)))
```

### 3. Appropriate IOR for Surface Devices

```python
# Correct: Heat flux meter facing INTO the domain
# Wall at x=0, device just inside
sim.add(Device(
    id='HF_WALL',
    quantity='GAUGE HEAT FLUX',
    xyz=Point3D.of(0.1, 2, 1.5),
    ior=1  # +X direction (away from wall)
)

# Wrong: IOR pointing wrong direction
sim.add(Device(
    id='HF_WALL',
    quantity='GAUGE HEAT FLUX',
    xyz=Point3D.of(0.1, 2, 1.5),
    ior=-1  # Points into wall (wrong!)
)
```

### 4. Output Control

```python
# For large simulations, reduce output frequency
sim.add(Device(
    id='TEMP',
    quantity='TEMPERATURE',
    xyz=Point3D.of(3, 2, 2),
    dt=10.0  # Output every 10s instead of every timestep
)
```

## Common Issues

??? question "Device shows no data"
    **Cause**: Device outside computational domain or on wrong surface

    **Solution**: Verify coordinates are within mesh bounds
    ```python
    # Check mesh bounds
    sim.add(Mesh(xb=Bounds3D.of(0, 5, 0, 4, 0, 2.5)))

    # Device must be inside: 0 < x < 5, 0 < y < 4, 0 < z < 2.5
    sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2, 1.5))  # ✓
    sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(6, 2, 1.5))    # ✗ Outside
    ```

??? question "Heat flux always zero"
    **Cause**: Missing IOR parameter or device not on surface

    **Solution**: Add IOR and position on surface
    ```python
    # Correct: Surface device with IOR
    sim.add(Device(
        id='HF',
        quantity='GAUGE HEAT FLUX',
        xyz=Point3D.of(0.1, 2, 1.5),
        ior=1
    )
    ```

??? question "Layer height not working"
    **Cause**: Reference height not at mid-height of room

    **Solution**: Place at approximate mid-height
    ```python
    # Room height 2.5m, place layer height device at ~1.25m
    sim.add(Device(
        id='LAYER',
        quantity='LAYER HEIGHT',
        xyz=Point3D.of(2.5, 2, 1.25)  # Mid-height reference
    )
    ```

## Tenability Criteria

Use devices to monitor tenability limits:

```python
# Temperature tenability (60°C for unprotected skin)
sim.add(Device(id='TEMP_EYES', quantity='TEMPERATURE', xyz=Point3D.of(2, 2, 1.5)))

# Visibility tenability (>10m for unfamiliar, >5m for familiar)
sim.add(Device(id='VIS', quantity='VISIBILITY', xyz=Point3D.of(2, 2, 1.5)))

# Heat flux tenability (2.5 kW/m² for extended exposure)
sim.add(Device(id='HF_PERSON', quantity='GAUGE HEAT FLUX', xyz=Point3D.of(2, 2, 1), ior=1))

# Oxygen concentration (>15% required)
sim.add(Device(id='O2', quantity='VOLUME FRACTION', spec_id='OXYGEN', xyz=Point3D.of(2, 2, 1.5)))
```

## Next Steps

- [RAMP Guide](ramps.md) - Time-varying device properties
- [Fire Sources](fire-sources.md) - What to monitor
- [Analysis](../execution/analysis.md) - Analyzing device output
- [Examples](../examples/basic.md) - Complete examples with devices

---

[RAMP →](ramps.md){ .md-button .md-button--primary }
