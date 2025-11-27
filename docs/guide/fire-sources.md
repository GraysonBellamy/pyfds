# Fire Sources

Learn how to create and configure fire sources in FDS simulations.

## Overview

Fire sources in FDS are created by applying fire surfaces to obstructions or vents. The fire is defined by its **heat release rate per unit area (HRRPUA)**.

```python
# Create fire surface
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')

# Apply to obstruction
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
```

## Basic Fire Surface

### Simple Fire

```python
sim.surface(
    id='BURNER',
    hrrpua=1000.0,      # Heat release rate per unit area (kW/m²)
    color='RED'          # Visualization color
)

# Apply to 1m x 1m burner
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='BURNER')
```

Total HRR = HRRPUA × Area = 1000 kW/m² × 1 m² = 1000 kW

## Fire Intensity Levels

| Fire Type | HRRPUA (kW/m²) | Description |
|-----------|----------------|-------------|
| **Small** | 250 - 500 | Smoldering, paper fire |
| **Medium** | 500 - 1000 | Wood crib, office materials |
| **Large** | 1000 - 2000 | Pool fire, intense burning |
| **Very Large** | 2000 - 5000 | Liquid fuel pool |

## Time-Varying Fires

Create a fire that grows over time:

```python
# Define fire growth ramp
sim.ramp(
    id='FIRE_GROWTH',
    t=[0, 60, 120, 180, 300],
    f=[0.0, 0.25, 0.5, 1.0, 1.0]
)

# Fire surface with ramp
sim.surface(
    id='GROWING_FIRE',
    hrrpua=2000.0,
    ramp_q='FIRE_GROWTH'
)
```

See [RAMP Guide](ramps.md) for more time-varying options.

## Fire Geometries

### Square Burner
```python
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
```

### Circular Burner
```python
sim.vent(
    xb=(-1, 1, -1, 1, 0, 0),
    surf_id='FIRE',
    xyz=(0, 0, 0),
    radius=0.5
)
```

### Multiple Fires
```python
sim.obstruction(xb=(1, 2, 1, 2, 0, 0.1), surf_id='FIRE')
sim.obstruction(xb=(3, 4, 3, 4, 0, 0.1), surf_id='FIRE')
```

## Complete Example

```python
from pyfds import Simulation

sim = Simulation(chid='fire_demo')
sim.time(t_end=300.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Growing fire
sim.ramp(id='GROWTH', t=[0, 60, 180], f=[0, 0.3, 1.0])
sim.surface(id='FIRE', hrrpua=1500.0, ramp_q='GROWTH', color='ORANGE')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

sim.write('fire_demo.fds')
```

## Next Steps

- [Devices & Measurement](devices.md) - Monitor fire effects
- [RAMP Guide](ramps.md) - Time-varying properties
- [Examples](../examples/basic.md) - Complete scenarios

---

[Devices →](devices.md){ .md-button .md-button--primary }
