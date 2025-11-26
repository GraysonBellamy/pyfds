# Devices & Measurement

Add measurement devices to record simulation data.

## Overview

Devices (DEVC) measure quantities at specific locations or over areas/volumes.

```python
sim.device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=(2.5, 2.5, 2.4)
)
```

## Point Measurements

### Temperature

```python
sim.device(id='TEMP_1', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))
```

### Velocity

```python
sim.device(id='VEL_1', quantity='VELOCITY', xyz=(2.5, 2.5, 2.4))
```

### Pressure

```python
sim.device(id='PRES_1', quantity='PRESSURE', xyz=(2.5, 2.5, 1.5))
```

## Area Measurements

```python
# Heat flux over floor
sim.device(
    id='HF_FLOOR',
    quantity='HEAT FLUX',
    xb=(0, 5, 0, 5, 0, 0)
)
```

## Common Quantities

| Quantity | Units | Description |
|----------|-------|-------------|
| `TEMPERATURE` | °C | Gas temperature |
| `VELOCITY` | m/s | Gas velocity magnitude |
| `PRESSURE` | Pa | Static pressure |
| `HEAT FLUX` | kW/m² | Heat flux to surface |
| `DENSITY` | kg/m³ | Gas density |
| `U-VELOCITY` | m/s | X-direction velocity |
| `V-VELOCITY` | m/s | Y-direction velocity |
| `W-VELOCITY` | m/s | Z-direction velocity |

## Device Arrays

```python
# Temperature profile along ceiling
for i, x in enumerate([1, 2, 3, 4]):
    sim.device(
        id=f'TEMP_{i+1}',
        quantity='TEMPERATURE',
        xyz=(x, 2.5, 2.4)
    )
```

## Output Control

Devices write to `{chid}_devc.csv` file.

```python
# Results analysis
from pyfds import Results

results = Results(chid='simulation')
temp_data = results.devices['TEMP_1']
print(f"Peak temperature: {temp_data.max():.1f} °C")
```

## Complete Example

```python
from pyfds import Simulation

sim = Simulation(chid='measurements')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# Measurements
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))
sim.device(id='TEMP_DOOR', quantity='TEMPERATURE', xyz=(5, 2.5, 2.0))
sim.device(id='VEL_DOOR', quantity='VELOCITY', xyz=(5, 2.5, 2.0))
sim.device(id='HF_FLOOR', quantity='HEAT FLUX', xb=(0, 5, 0, 5, 0, 0))

sim.write('measurements.fds')
```

## Next Steps

- [RAMP Guide](ramps.md) - Time-varying properties
- [Results Analysis](../execution/analysis.md) - Working with device data

---

[RAMP Guide →](ramps.md){ .md-button .md-button--primary }
