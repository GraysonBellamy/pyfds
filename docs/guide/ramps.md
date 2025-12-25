# Time-Varying Properties (RAMP)

Control how properties change over time using RAMP functions.

## Overview

The **RAMP** namelist defines functions that vary properties over time or with temperature. RAMPs are essential for:

- Growing/decaying fires
- Time-dependent boundary conditions
- Temperature-dependent material properties
- Controlled device activation

```python
from pyfds.core.namelists import Surface, Ramp

# Time-varying fire
sim.add(Ramp(id='FIRE_GROWTH', t=[0, 60, 300], f=[0, 0.5, 1.0]))
sim.add(Surface(id='FIRE', hrrpua=1500.0, ramp_q='FIRE_GROWTH'))
```

## RAMP Basics

### Time-Based RAMP

Define how a property changes with time:

```python
sim.add(Ramp(
    id='LINEAR_GROWTH',
    t=[0, 100, 200],      # Time points (seconds)
    f=[0.0, 0.5, 1.0]     # Function values (multipliers)
)
```

The property value at any time is: `value = base_value × f(t)`

### Temperature-Based RAMP

Define how a property changes with temperature:

```python
sim.add(Ramp(
    id='K_VS_T',
    t=[20, 200, 400, 600],    # Temperature (°C)
    f=[1.0, 1.2, 1.5, 2.0]    # Multipliers
)

sim.add(Material(
    id='TEMP_MATL',
    conductivity=0.5,
    conductivity_ramp='K_VS_T'
)
```

## Fire Growth Curves

### t-Squared Fire Growth

Standard fire growth rates:

```python
# Slow (600s to 1MW)
sim.add(Ramp(
    id='SLOW',
    t=[0, 150, 300, 450, 600],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)

# Medium (300s to 1MW)
sim.add(Ramp(
    id='MEDIUM',
    t=[0, 75, 150, 225, 300],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)

# Fast (150s to 1MW)
sim.add(Ramp(
    id='FAST',
    t=[0, 37.5, 75, 112.5, 150],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)

# Ultra-fast (75s to 1MW)
sim.add(Ramp(
    id='ULTRAFAST',
    t=[0, 18.75, 37.5, 56.25, 75],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)
```

Apply to fire surface:

```python
sim.add(Surface(id='FIRE', hrrpua=1000.0, ramp_q='MEDIUM'))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))
```

### Custom Fire Growth

```python
# Fire grows for 180s, steady for 300s, then decays
sim.add(Ramp(
    id='CUSTOM_FIRE',
    t=[0, 180, 480, 600],
    f=[0.0, 1.0, 1.0, 0.2]
)

sim.add(Surface(id='FIRE', hrrpua=2000.0, ramp_q='CUSTOM_FIRE'))
```

## Time-Dependent Velocities

### Pulsating Flow

```python
# Sinusoidal velocity variation
sim.add(Ramp(
    id='PULSE',
    t=[0, 30, 60, 90, 120],
    f=[0.0, 1.0, 0.0, -1.0, 0.0]
)

sim.add(Surface(
    id='PULSATING_VENT',
    vel=2.0,           # Base velocity
    vel_ramp='PULSE'   # Time variation
)
```

### HVAC Schedule

```python
from pyfds.core.namelists import Ramp, Vent
from pyfds.core.geometry import Bounds3D

# HVAC turns on at t=60s, off at t=300s
sim.add(Ramp(
    id='HVAC_SCHEDULE',
    t=[0, 60, 300, 301],
    f=[0.0, 1.0, 1.0, 0.0]
)

sim.add(Vent(
    xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3),
    surf_id='HVAC',
    volume_flow=0.5,
    volume_flow_ramp='HVAC_SCHEDULE'
)
```

## Temperature-Dependent Properties

### Conductivity vs Temperature

```python
# Concrete conductivity increases with temperature
sim.add(Ramp(
    id='K_CONCRETE',
    t=[20, 100, 200, 400, 600, 800],      # Temperature (°C)
    f=[1.0, 1.05, 1.15, 1.35, 1.60, 1.90] # Multiplier
)

sim.add(Material(
    id='CONCRETE',
    conductivity=1.8,              # Base value at 20°C
    specific_heat=0.88,
    density=2400.0,
    conductivity_ramp='K_CONCRETE'
)
```

### Specific Heat vs Temperature

```python
sim.add(Ramp(
    id='CP_STEEL',
    t=[20, 200, 400, 600, 800],
    f=[1.0, 1.1, 1.25, 1.45, 1.70]
)

sim.add(Material(
    id='STEEL',
    conductivity=45.8,
    specific_heat=0.46,
    density=7850.0,
    specific_heat_ramp='CP_STEEL'
)
```

## RAMP Interpolation

FDS uses **linear interpolation** between points:

```python
# Linear interpolation example
sim.add(Ramp(
    id='LINEAR',
    t=[0, 100, 200],
    f=[0.0, 0.5, 1.0]
)

# At t=50s:  f = 0.25 (halfway between 0.0 and 0.5)
# At t=150s: f = 0.75 (halfway between 0.5 and 1.0)
```

!!! tip "Smooth Curves"
    For smooth curves, use more points:
    ```python
    # Smooth exponential-like growth
    sim.add(Ramp(
        id='SMOOTH',
        t=[0, 30, 60, 90, 120, 150, 180],
        f=[0.0, 0.1, 0.25, 0.45, 0.70, 0.90, 1.0]
    )
    ```

## Complete Examples

### Growing Room Fire

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Ramp, Surface, Obstruction, Vent
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='growing_fire')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5)))

# Medium growth fire (300s to peak)
sim.add(Ramp(
    id='GROWTH',
    t=[0, 75, 150, 225, 300, 600],
    f=[0.0, 0.25, 0.5, 0.75, 1.0, 1.0]
)

sim.add(Surface(id='FIRE', hrrpua=1500.0, ramp_q='GROWTH', color='ORANGE'))
sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))

# Door
sim.add(Vent(xb=Bounds3D.of(6, 6, 2, 3, 0, 2.1), surf_id='OPEN'))

sim.write('growing_fire.fds')
```

### Fire with Decay

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Ramp, Surface, Obstruction
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='fire_decay')
sim.add(Time(t_end=900.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Fire grows to peak at 180s, steady until 600s, decays to 10% by 900s
sim.add(Ramp(
    id='FIRE_CURVE',
    t=[0, 180, 600, 900],
    f=[0.0, 1.0, 1.0, 0.1]
)

sim.add(Surface(id='FIRE', hrrpua=2000.0, ramp_q='FIRE_CURVE'))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))

sim.write('fire_decay.fds')
```

### Temperature-Dependent Wall

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Ramp, Material, Surface, Obstruction
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='temp_wall')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(60, 40, 25), xb=Bounds3D.of(0, 6, 0, 4, 0, 2.5)))

# Conductivity increases with temperature
sim.add(Ramp(
    id='K_RAMP',
    t=[20, 100, 200, 400, 600, 800],
    f=[1.0, 1.1, 1.2, 1.4, 1.7, 2.1]
)

# Gypsum with temperature-dependent conductivity
sim.add(Material(
    id='GYPSUM',
    conductivity=0.48,
    specific_heat=0.84,
    density=1440.0,
    conductivity_ramp='K_RAMP'
)

sim.add(Surface(
    id='GYPSUM_WALL',
    matl_id='GYPSUM',
    thickness=0.0127
)

# Fire
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 1.5, 2.5, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))

# Wall
sim.add(Obstruction(xb=Bounds3D.of(0, 0.1, 0, 4, 0, 2.5), surf_ids=('GYPSUM_WALL', 'INERT', 'INERT')))

sim.write('temp_wall.fds')
```

### Scheduled HVAC

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Ramp, Surface, Obstruction, Vent
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='hvac_schedule')
sim.add(Time(t_end=900.0))
sim.add(Mesh(ijk=Grid3D.of(60, 60, 30), xb=Bounds3D.of(0, 6, 0, 6, 0, 3)))

# HVAC operates: 0-300s off, 300-600s on, 600s+ off
sim.add(Ramp(
    id='HVAC_ON',
    t=[0, 300, 600, 601],
    f=[0.0, 0.0, 1.0, 0.0]
)

# Fire starts at t=0
sim.add(Surface(id='FIRE', hrrpua=800.0))
sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2.5, 3.5, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))

# HVAC vent with schedule
sim.add(Vent(
    xb=Bounds3D.of(1, 1.5, 1, 1.5, 3, 3),
    surf_id='HVAC',
    volume_flow=0.6,
    volume_flow_ramp='HVAC_ON'
)

sim.write('hvac_schedule.fds')
```

### Multiple Fire Sources with Staggered Start

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Ramp, Surface, Obstruction
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='staggered_fires')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(80, 60, 25), xb=Bounds3D.of(0, 8, 0, 6, 0, 2.5)))

# First fire: starts at t=0
sim.add(Ramp(id='FIRE1', t=[0, 120], f=[0.0, 1.0]))
sim.add(Surface(id='FIRE1', hrrpua=1000.0, ramp_q='FIRE1'))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_ids=('FIRE1', 'INERT', 'INERT')))

# Second fire: starts at t=180s
sim.add(Ramp(id='FIRE2', t=[0, 180, 300], f=[0.0, 0.0, 1.0]))
sim.add(Surface(id='FIRE2', hrrpua=1200.0, ramp_q='FIRE2'))
sim.add(Obstruction(xb=Bounds3D.of(5, 6, 3, 4, 0, 0.1), surf_ids=('FIRE2', 'INERT', 'INERT')))

sim.write('staggered_fires.fds')
```

## Advanced RAMP Techniques

### Piecewise Linear Function

```python
# Complex fire scenario with multiple phases
sim.add(Ramp(
    id='COMPLEX_FIRE',
    t=[0, 60, 120, 180, 300, 420, 600],
    f=[0.0, 0.3, 0.7, 1.0, 0.8, 0.5, 0.1]
)
```

This creates:
- 0-60s: Initial growth
- 60-120s: Accelerated growth
- 120-180s: Final growth to peak
- 180-300s: Slight decay
- 300-420s: Moderate decay
- 420-600s: Final decay

### Step Functions

```python
# Sudden activation/deactivation
sim.add(Ramp(
    id='STEP',
    t=[0, 99.9, 100, 299.9, 300],
    f=[0.0, 0.0, 1.0, 1.0, 0.0]
)
```

Creates step changes at t=100s (on) and t=300s (off).

## RAMP Applications Summary

| Application | RAMP Reference | Property Modified |
|-------------|----------------|-------------------|
| Fire growth | `ramp_q` | Heat release rate |
| Velocity variation | `vel_ramp` | Velocity |
| Volume flow | `volume_flow_ramp` | HVAC flow rate |
| Temperature | `tmp_ramp` | Surface temperature |
| Conductivity | `conductivity_ramp` | Thermal conductivity |
| Specific heat | `specific_heat_ramp` | Material specific heat |
| Mass flux | `mass_flux_ramp` | Species injection |
| TAU_Q | `tau_q_ramp` | Radiative fraction |

## Best Practices

### 1. Use Sufficient Points

```python
# Too coarse (might miss important behavior)
sim.add(Ramp(id='COARSE', t=[0, 600], f=[0, 1]))

# Better (captures growth curve)
sim.add(Ramp(id='BETTER', t=[0, 150, 300, 450, 600], f=[0, 0.25, 0.5, 0.75, 1]))
```

### 2. Start at t=0

```python
# Correct: Starts at t=0
sim.add(Ramp(id='GOOD', t=[0, 60, 120], f=[0, 0.5, 1]))

# Problematic: Doesn't define behavior before t=30
sim.add(Ramp(id='BAD', t=[30, 60, 120], f=[0, 0.5, 1]))
```

### 3. Match Simulation Duration

```python
sim.add(Time(t_end=600.0))

# RAMP extends to simulation end
sim.add(Ramp(
    id='FULL_DURATION',
    t=[0, 300, 600],
    f=[0, 1, 1]
)
```

### 4. Document Complex RAMPs

```python
# Clear documentation for complex behavior
# Fire growth: 0-180s growth, 180-420s steady, 420-600s decay
sim.add(Ramp(
    id='DOCUMENTED_FIRE',
    t=[0, 180, 420, 600],
    f=[0.0, 1.0, 1.0, 0.2]
)
```

## Common Issues

??? question "RAMP not working"
    **Cause**: RAMP ID not referenced correctly or wrong parameter

    **Solution**: Check RAMP reference matches the property
    ```python
    sim.add(Ramp(id='GROWTH', t=[0, 100], f=[0, 1]))

    # Correct: ramp_q for heat release rate
    sim.add(Surface(id='FIRE', hrrpua=1000.0, ramp_q='GROWTH'))

    # Wrong: Using wrong parameter name
    sim.add(Surface(id='FIRE', hrrpua=1000.0, ramp='GROWTH')  # Won't work
    ```

??? question "Property not changing over time"
    **Cause**: Base value is zero or RAMP not applied

    **Solution**: Ensure base value is non-zero
    ```python
    # Wrong: Base value is 0, multiplying by RAMP still gives 0
    sim.add(Surface(id='FIRE', hrrpua=0.0, ramp_q='GROWTH'))

    # Correct: Base value multiplied by RAMP function
    sim.add(Surface(id='FIRE', hrrpua=1500.0, ramp_q='GROWTH'))
    ```

??? question "Unexpected jumps in property"
    **Cause**: Not enough RAMP points for smooth transition

    **Solution**: Add more intermediate points
    ```python
    # Jumpy (only 2 points)
    sim.add(Ramp(id='JUMPY', t=[0, 300], f=[0, 1]))

    # Smooth (more points)
    sim.add(Ramp(id='SMOOTH', t=[0, 75, 150, 225, 300], f=[0, 0.25, 0.5, 0.75, 1]))
    ```

## Next Steps

- [Fire Sources](fire-sources.md) - Apply RAMPs to fires
- [Materials & Surfaces](materials-surfaces.md) - Temperature-dependent properties
- [Boundaries](boundaries.md) - Time-varying boundary conditions
- [Controls](controls.md) - Advanced control logic

---

[Controls →](controls.md){ .md-button .md-button--primary }
