# Global Settings (MISC)

Configure global simulation parameters using the MISC namelist.

## Overview

The **MISC** namelist controls global settings that affect the entire simulation:

- Ambient conditions (temperature, pressure)
- Turbulence models
- Gravity and stratification
- Special simulation modes
- Output and logging options

```python
# Set ambient conditions
sim.set_misc(
    tmpa=20.0,        # Ambient temperature (°C)
    humidity=50.0,    # Relative humidity (%)
    p_inf=101325.0    # Ambient pressure (Pa)
)
```

## Ambient Conditions

### Temperature and Pressure

```python
# Standard conditions
sim.set_misc(
    tmpa=20.0,       # Ambient temperature (°C)
    p_inf=101325.0   # Ambient pressure (Pa, 1 atm)
)

# Hot day
sim.set_misc(
    tmpa=35.0,
    p_inf=101325.0
)

# High altitude (Denver, ~5000 ft)
sim.set_misc(
    tmpa=20.0,
    p_inf=84000.0    # Reduced pressure
)
```

### Humidity

```python
# Dry air
sim.set_misc(
    tmpa=20.0,
    humidity=10.0    # 10% relative humidity
)

# Humid conditions
sim.set_misc(
    tmpa=30.0,
    humidity=80.0    # 80% relative humidity
)
```

## Gravity and Orientation

### Standard Gravity

```python
# Default: gravity in -Z direction (downward)
sim.set_misc(gvec=(0.0, 0.0, -9.81))  # m/s²
```

### Modified Gravity

```python
# Reduced gravity (simulating microgravity effects)
sim.set_misc(gvec=(0.0, 0.0, -1.0))

# Zero gravity
sim.set_misc(gvec=(0.0, 0.0, 0.0))

# Tilted domain (for slope simulations)
import math
angle = 10.0  # degrees
g = 9.81
sim.set_misc(gvec=(
    0.0,
    g * math.sin(math.radians(angle)),
    -g * math.cos(math.radians(angle))
))
```

## Turbulence Models

### Default LES (Large Eddy Simulation)

FDS uses LES by default:

```python
# Default Deardorff model
sim.set_misc(turbulence_model='DEARDORFF')
```

### DNS (Direct Numerical Simulation)

For very fine mesh simulations:

```python
# Turn off turbulence model (DNS)
sim.set_misc(turbulence_model='DNS')
```

!!! warning "DNS Requirements"
    DNS requires extremely fine mesh resolution and is computationally expensive. Only use for small domains or academic studies.

### Constant Smagorinsky

```python
sim.set_misc(turbulence_model='CONSTANT SMAGORINSKY')
```

## Simulation Control

### Time Step Control

```python
# Let FDS automatically determine time step (default)
sim.set_misc()  # CFL-based adaptive time step

# Limit maximum time step
sim.set_misc(dt_max=0.1)  # Maximum 0.1s time step

# Force constant time step (not recommended)
sim.set_misc(dt=0.01, constant_dt=True)
```

### Restart Simulations

```python
# Enable restart capability (save state every 100s)
sim.set_misc(restart=True, dt_restart=100.0)

# Continue from previous simulation
sim.set_misc(restart=True, restart_chid='previous_run')
```

## Special Modes

### Check Run (Geometry Only)

Validate geometry without running simulation:

```python
sim.set_misc(check_geometry=True)
```

### No Combustion

Disable combustion for heat transfer studies:

```python
sim.set_misc(no_combustion=True)
```

### Freeze Velocity Field

Fix velocity field (for testing):

```python
sim.set_misc(freeze_velocity=True)
```

## Stratification

### Temperature Stratification

```python
# Linear temperature profile
# T(z) = TMPA + DTDZ * z
sim.set_misc(
    tmpa=20.0,           # Temperature at z=0 (°C)
    stratification='T',   # Temperature stratification
    dtdz=0.5             # Temperature gradient (°C/m)
)
```

At z=0m: T = 20°C
At z=10m: T = 20 + 0.5×10 = 25°C

### Density Stratification

```python
sim.set_misc(
    stratification='rho',  # Density stratification
    drhodz=-0.1            # Density gradient (kg/m³/m)
)
```

## Radiation

### Radiation Solution

```python
# Enable radiation (default)
sim.set_misc(radiation=True)

# Increase radiation solver frequency
sim.set_misc(number_radiation_angles=100)

# Wide-band radiation model
sim.set_misc(wide_band_model=True)
```

### Radiative Fraction

```python
# Default radiative fraction for all fires
sim.set_misc(radiative_fraction=0.35)  # 35% radiated
```

## Wind and Outdoor Simulations

### Wind Speed and Direction

```python
# 5 m/s wind in +X direction
sim.set_misc(wind_speed=5.0, wind_direction=0.0)

# Wind from the west (blowing toward +X)
sim.set_misc(wind_speed=3.0, wind_direction=270.0)
```

### Atmospheric Profile

```python
# Logarithmic wind profile
sim.set_misc(
    wind_speed=5.0,
    lapse_rate=0.01,      # Temperature lapse rate (K/m)
    roughness_length=0.1   # Surface roughness (m)
)
```

## Complete Examples

### Standard Indoor Fire

```python
from pyfds import Simulation

sim = Simulation(chid='indoor_fire')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

# Standard indoor conditions
sim.set_misc(
    tmpa=20.0,           # 20°C ambient
    humidity=50.0,       # 50% RH
    p_inf=101325.0,      # 1 atm
    radiation=True       # Include radiation
)

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

sim.write('indoor_fire.fds')
```

### Hot Environment

```python
sim = Simulation(chid='hot_environment')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

# Hot, dry conditions
sim.set_misc(
    tmpa=40.0,      # 40°C ambient (104°F)
    humidity=15.0,  # Low humidity
    p_inf=101325.0
)

# Fire
sim.surface(id='FIRE', hrrpua=800.0)
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

sim.write('hot_environment.fds')
```

### Outdoor Fire with Wind

```python
sim = Simulation(chid='outdoor_wind')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(100, 100, 40), xb=Bounds3D.of(0, 50, 0, 50, 0, 20))

# Outdoor conditions with wind
sim.set_misc(
    tmpa=25.0,
    humidity=60.0,
    wind_speed=5.0,        # 5 m/s wind
    wind_direction=270.0,  # From west
    roughness_length=0.1   # Grassland
)

# Open all boundaries
sim.add(Vent(mb='XMIN', surf_id='OPEN')
sim.add(Vent(mb='XMAX', surf_id='OPEN')
sim.add(Vent(mb='YMIN', surf_id='OPEN')
sim.add(Vent(mb='YMAX', surf_id='OPEN')
sim.add(Vent(mb='ZMAX', surf_id='OPEN')

# Fire
sim.surface(id='FIRE', hrrpua=500.0)
sim.add(Obstruction(xb=Bounds3D.of(22, 28, 22, 28, 0, 0.5), surf_id='FIRE')

sim.write('outdoor_wind.fds')
```

### Stratified Environment

```python
sim = Simulation(chid='stratified')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 50), xb=Bounds3D.of(0, 5, 0, 5, 0, 5))

# Temperature increases with height (stable stratification)
sim.set_misc(
    tmpa=20.0,            # 20°C at floor
    stratification='T',
    dtdz=2.0              # +2°C per meter
)
# At z=5m: T = 20 + 2*5 = 30°C

# Fire at floor level
sim.surface(id='FIRE', hrrpua=1000.0)
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

sim.write('stratified.fds')
```

### DNS Simulation (Small Domain)

```python
sim = Simulation(chid='dns_test')
sim.add(Time(t_end=10.0)  # Short duration for DNS

# Very fine mesh for DNS
sim.add(Mesh(ijk=Grid3D.of(100, 100, 100), xb=Bounds3D.of(0, 0.5, 0, 0.5, 0, 0.5))

# Direct numerical simulation (no turbulence model)
sim.set_misc(
    turbulence_model='DNS',
    tmpa=20.0
)

# Small fire
sim.surface(id='FIRE', hrrpua=500.0)
sim.add(Obstruction(xb=Bounds3D.of(0.2, 0.3, 0.2, 0.3, 0, 0.01), surf_id='FIRE')

sim.write('dns_test.fds')
```

### Geometry Check Only

```python
sim = Simulation(chid='geometry_check')
sim.add(Mesh(ijk=Grid3D.of(50, 40, 25), xb=Bounds3D.of(0, 5, 0, 4, 0, 2.5))

# Check geometry without running simulation
sim.set_misc(check_geometry=True)

# Add complex geometry
sim.add(Obstruction(xb=Bounds3D.of(0, 0.2, 0, 4, 0, 2.5), surf_id='INERT')
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 1.5, 2.5, 0, 0.5), surf_id='INERT')

sim.write('geometry_check.fds')
```

## Common MISC Parameters

| Parameter | Default | Units | Description |
|-----------|---------|-------|-------------|
| `tmpa` | 20.0 | °C | Ambient temperature |
| `p_inf` | 101325.0 | Pa | Ambient pressure |
| `humidity` | 40.0 | % | Relative humidity |
| `gvec` | (0, 0, -9.81) | m/s² | Gravity vector |
| `turbulence_model` | 'DEARDORFF' | - | LES turbulence model |
| `radiation` | True | - | Enable radiation |
| `radiative_fraction` | varies | - | Default χ_r for fires |
| `wind_speed` | 0.0 | m/s | Wind speed |
| `wind_direction` | 0.0 | degrees | Wind direction |
| `stratification` | None | - | 'T' or 'rho' |
| `dt_max` | varies | s | Maximum time step |

## Best Practices

### 1. Use Realistic Ambient Conditions

```python
# Good: Realistic conditions
sim.set_misc(tmpa=20.0, humidity=50.0, p_inf=101325.0)

# Questionable: Extreme conditions without justification
sim.set_misc(tmpa=100.0, humidity=0.0)
```

### 2. Let FDS Control Time Step

```python
# Preferred: Automatic adaptive time step
sim.set_misc()  # Uses CFL condition

# Avoid: Forcing constant time step
sim.set_misc(dt=0.01, constant_dt=True)  # Usually unnecessary
```

### 3. Document Special Modes

```python
# Clear documentation for non-standard settings
# Simulating reduced gravity environment for research
sim.set_misc(
    gvec=(0.0, 0.0, -3.71),  # Mars gravity
    tmpa=-50.0                # Mars temperature
)
```

### 4. Check Geometry First

```python
# For complex geometries, check before full run
sim.set_misc(check_geometry=True)
sim.write('check.fds')

# Then run full simulation
sim.set_misc(check_geometry=False)
sim.write('full_run.fds')
```

## Common Issues

??? question "Simulation unstable"
    **Cause**: Time step too large or inappropriate settings

    **Solution**: Limit maximum time step
    ```python
    sim.set_misc(dt_max=0.05)
    ```

??? question "Wind not affecting fire"
    **Cause**: Closed boundaries or insufficient domain size

    **Solution**: Open boundaries and larger domain
    ```python
    # Open boundaries for wind flow
    sim.add(Vent(mb='XMIN', surf_id='OPEN')
    sim.add(Vent(mb='XMAX', surf_id='OPEN')
    sim.set_misc(wind_speed=5.0, wind_direction=0.0)
    ```

??? question "Stratification not working"
    **Cause**: Wrong stratification parameter

    **Solution**: Use correct stratification type
    ```python
    # Correct: Temperature stratification
    sim.set_misc(stratification='T', dtdz=1.0)

    # Wrong: Missing stratification type
    sim.set_misc(dtdz=1.0)  # Won't work
    ```

## Next Steps

- [Fire Sources](fire-sources.md) - Configure fires with global radiative fraction
- [Boundaries](boundaries.md) - Wind boundary conditions
- [Initial Conditions](initial-conditions.md) - Set initial fields
- [Combustion](combustion.md) - Fuel and reaction parameters

---

[Initial Conditions →](initial-conditions.md){ .md-button .md-button--primary }
