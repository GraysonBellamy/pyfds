# Time-Varying Properties (RAMP)

Control how properties change over time using RAMP functions.

## Overview

The **RAMP** namelist defines functions that vary properties over time or with temperature. RAMPs are essential for:

- Growing/decaying fires
- Time-dependent boundary conditions
- Temperature-dependent material properties
- Controlled device activation

```python
# Time-varying fire
sim.ramp(id='FIRE_GROWTH', t=[0, 60, 300], f=[0, 0.5, 1.0])
sim.surface(id='FIRE', hrrpua=1500.0, ramp_q='FIRE_GROWTH')
```

## RAMP Basics

### Time-Based RAMP

Define how a property changes with time:

```python
sim.ramp(
    id='LINEAR_GROWTH',
    t=[0, 100, 200],      # Time points (seconds)
    f=[0.0, 0.5, 1.0]     # Function values (multipliers)
)
```

The property value at any time is: `value = base_value × f(t)`

### Temperature-Based RAMP

Define how a property changes with temperature:

```python
sim.ramp(
    id='K_VS_T',
    t=[20, 200, 400, 600],    # Temperature (°C)
    f=[1.0, 1.2, 1.5, 2.0]    # Multipliers
)

sim.material(
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
sim.ramp(
    id='SLOW',
    t=[0, 150, 300, 450, 600],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)

# Medium (300s to 1MW)
sim.ramp(
    id='MEDIUM',
    t=[0, 75, 150, 225, 300],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)

# Fast (150s to 1MW)
sim.ramp(
    id='FAST',
    t=[0, 37.5, 75, 112.5, 150],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)

# Ultra-fast (75s to 1MW)
sim.ramp(
    id='ULTRAFAST',
    t=[0, 18.75, 37.5, 56.25, 75],
    f=[0.0, 0.25, 0.5, 0.75, 1.0]
)
```

Apply to fire surface:

```python
sim.surface(id='FIRE', hrrpua=1000.0, ramp_q='MEDIUM')
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
```

### Custom Fire Growth

```python
# Fire grows for 180s, steady for 300s, then decays
sim.ramp(
    id='CUSTOM_FIRE',
    t=[0, 180, 480, 600],
    f=[0.0, 1.0, 1.0, 0.2]
)

sim.surface(id='FIRE', hrrpua=2000.0, ramp_q='CUSTOM_FIRE')
```

## Time-Dependent Velocities

### Pulsating Flow

```python
# Sinusoidal velocity variation
sim.ramp(
    id='PULSE',
    t=[0, 30, 60, 90, 120],
    f=[0.0, 1.0, 0.0, -1.0, 0.0]
)

sim.surface(
    id='PULSATING_VENT',
    vel=2.0,           # Base velocity
    vel_ramp='PULSE'   # Time variation
)
```

### HVAC Schedule

```python
# HVAC turns on at t=60s, off at t=300s
sim.ramp(
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
sim.ramp(
    id='K_CONCRETE',
    t=[20, 100, 200, 400, 600, 800],      # Temperature (°C)
    f=[1.0, 1.05, 1.15, 1.35, 1.60, 1.90] # Multiplier
)

sim.material(
    id='CONCRETE',
    conductivity=1.8,              # Base value at 20°C
    specific_heat=0.88,
    density=2400.0,
    conductivity_ramp='K_CONCRETE'
)
```

### Specific Heat vs Temperature

```python
sim.ramp(
    id='CP_STEEL',
    t=[20, 200, 400, 600, 800],
    f=[1.0, 1.1, 1.25, 1.45, 1.70]
)

sim.material(
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
sim.ramp(
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
    sim.ramp(
        id='SMOOTH',
        t=[0, 30, 60, 90, 120, 150, 180],
        f=[0.0, 0.1, 0.25, 0.45, 0.70, 0.90, 1.0]
    )
    ```

## Complete Examples

### Growing Room Fire

```python
from pyfds import Simulation

sim = Simulation(chid='growing_fire')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

# Medium growth fire (300s to peak)
sim.ramp(
    id='GROWTH',
    t=[0, 75, 150, 225, 300, 600],
    f=[0.0, 0.25, 0.5, 0.75, 1.0, 1.0]
)

sim.surface(id='FIRE', hrrpua=1500.0, ramp_q='GROWTH', color='ORANGE')
sim.add(Obstruction(ion(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

# Door
sim.add(Vent(xb=Bounds3D.of(6, 6, 2, 3, 0, 2.1), surf_id='OPEN')

sim.write('growing_fire.fds')
```

### Fire with Decay

```python
sim = Simulation(chid='fire_decay')
sim.add(Time(t_end=900.0)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

# Fire grows to peak at 180s, steady until 600s, decays to 10% by 900s
sim.ramp(
    id='FIRE_CURVE',
    t=[0, 180, 600, 900],
    f=[0.0, 1.0, 1.0, 0.1]
)

sim.surface(id='FIRE', hrrpua=2000.0, ramp_q='FIRE_CURVE')
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

sim.write('fire_decay.fds')
```

### Temperature-Dependent Wall

```python
sim = Simulation(chid='temp_wall')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(60, 40, 25), xb=Bounds3D.of(0, 6, 0, 4, 0, 2.5))

# Conductivity increases with temperature
sim.ramp(
    id='K_RAMP',
    t=[20, 100, 200, 400, 600, 800],
    f=[1.0, 1.1, 1.2, 1.4, 1.7, 2.1]
)

# Gypsum with temperature-dependent conductivity
sim.material(
    id='GYPSUM',
    conductivity=0.48,
    specific_heat=0.84,
    density=1440.0,
    conductivity_ramp='K_RAMP'
)

sim.surface(
    id='GYPSUM_WALL',
    matl_id='GYPSUM',
    thickness=0.0127
)

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.add(Obstruction(ion(xb=Bounds3D.of(2.5, 3.5, 1.5, 2.5, 0, 0.1), surf_id='FIRE')

# Wall
sim.add(Obstruction(ion(xb=Bounds3D.of(0, 0.1, 0, 4, 0, 2.5), surf_id='GYPSUM_WALL')

sim.write('temp_wall.fds')
```

### Scheduled HVAC

```python
sim = Simulation(chid='hvac_schedule')
sim.add(Time(t_end=900.0)
sim.add(Mesh(ijk=Grid3D.of(60, 60, 30), xb=Bounds3D.of(0, 6, 0, 6, 0, 3))

# HVAC operates: 0-300s off, 300-600s on, 600s+ off
sim.ramp(
    id='HVAC_ON',
    t=[0, 300, 600, 601],
    f=[0.0, 0.0, 1.0, 0.0]
)

# Fire starts at t=0
sim.surface(id='FIRE', hrrpua=800.0)
sim.add(Obstruction(ion(xb=Bounds3D.of(2.5, 3.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE')

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
sim = Simulation(chid='staggered_fires')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(80, 60, 25), xb=Bounds3D.of(0, 8, 0, 6, 0, 2.5))

# First fire: starts at t=0
sim.ramp(id='FIRE1', t=[0, 120], f=[0.0, 1.0])
sim.surface(id='FIRE1', hrrpua=1000.0, ramp_q='FIRE1')
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE1')

# Second fire: starts at t=180s
sim.ramp(id='FIRE2', t=[0, 180, 300], f=[0.0, 0.0, 1.0])
sim.surface(id='FIRE2', hrrpua=1200.0, ramp_q='FIRE2')
sim.add(Obstruction(ion(xb=Bounds3D.of(5, 6, 3, 4, 0, 0.1), surf_id='FIRE2')

sim.write('staggered_fires.fds')
```

## Advanced RAMP Techniques

### Piecewise Linear Function

```python
# Complex fire scenario with multiple phases
sim.ramp(
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
sim.ramp(
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
sim.ramp(id='COARSE', t=[0, 600], f=[0, 1])

# Better (captures growth curve)
sim.ramp(id='BETTER', t=[0, 150, 300, 450, 600], f=[0, 0.25, 0.5, 0.75, 1])
```

### 2. Start at t=0

```python
# Correct: Starts at t=0
sim.ramp(id='GOOD', t=[0, 60, 120], f=[0, 0.5, 1])

# Problematic: Doesn't define behavior before t=30
sim.ramp(id='BAD', t=[30, 60, 120], f=[0, 0.5, 1])
```

### 3. Match Simulation Duration

```python
sim.add(Time(t_end=600.0)

# RAMP extends to simulation end
sim.ramp(
    id='FULL_DURATION',
    t=[0, 300, 600],
    f=[0, 1, 1]
)
```

### 4. Document Complex RAMPs

```python
# Clear documentation for complex behavior
# Fire growth: 0-180s growth, 180-420s steady, 420-600s decay
sim.ramp(
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
    sim.ramp(id='GROWTH', t=[0, 100], f=[0, 1])

    # Correct: ramp_q for heat release rate
    sim.surface(id='FIRE', hrrpua=1000.0, ramp_q='GROWTH')

    # Wrong: Using wrong parameter name
    sim.surface(id='FIRE', hrrpua=1000.0, ramp='GROWTH')  # Won't work
    ```

??? question "Property not changing over time"
    **Cause**: Base value is zero or RAMP not applied

    **Solution**: Ensure base value is non-zero
    ```python
    # Wrong: Base value is 0, multiplying by RAMP still gives 0
    sim.surface(id='FIRE', hrrpua=0.0, ramp_q='GROWTH')

    # Correct: Base value multiplied by RAMP function
    sim.surface(id='FIRE', hrrpua=1500.0, ramp_q='GROWTH')
    ```

??? question "Unexpected jumps in property"
    **Cause**: Not enough RAMP points for smooth transition

    **Solution**: Add more intermediate points
    ```python
    # Jumpy (only 2 points)
    sim.ramp(id='JUMPY', t=[0, 300], f=[0, 1])

    # Smooth (more points)
    sim.ramp(id='SMOOTH', t=[0, 75, 150, 225, 300], f=[0, 0.25, 0.5, 0.75, 1])
    ```

## Next Steps

- [Fire Sources](fire-sources.md) - Apply RAMPs to fires
- [Materials & Surfaces](materials-surfaces.md) - Temperature-dependent properties
- [Boundaries](boundaries.md) - Time-varying boundary conditions
- [Controls](controls.md) - Advanced control logic

---

[Controls →](controls.md){ .md-button .md-button--primary }
