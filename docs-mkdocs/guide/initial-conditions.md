# Initial Conditions (INIT)

Set initial temperature, velocity, and species distributions in your simulation domain.

## Overview

**Initial conditions** (INIT namelist) specify non-ambient starting states for regions of the domain. This is useful for:

- Pre-heated zones
- Initial velocity fields
- Pre-existing smoke layers
- Species concentrations
- Pressure distributions

```python
# Hot zone at start of simulation
sim.init(
    xb=(2, 4, 2, 4, 0, 2),
    temperature=200.0  # Start at 200°C
)
```

## Temperature Initialization

### Single Region

```python
# Hot region (industrial process area)
sim.init(
    xb=(1, 3, 1, 3, 0, 2),
    temperature=150.0  # °C
)

# Cold region (refrigerated space)
sim.init(
    xb=(5, 7, 2, 4, 0, 2),
    temperature=5.0  # °C
)
```

### Layered Temperature

```python
# Hot upper layer (post-flashover)
sim.init(
    xb=(0, 8, 0, 6, 2, 3),  # Upper region
    temperature=600.0
)

# Cooler lower layer
sim.init(
    xb=(0, 8, 0, 6, 0, 2),  # Lower region
    temperature=100.0
)
```

### Vertical Temperature Gradient

```python
# Create multiple layers with temperature gradient
# Bottom to top: 20°C to 200°C
temps = [20, 60, 100, 140, 180, 200]
z_levels = [(0, 0.5), (0.5, 1.0), (1.0, 1.5), (1.5, 2.0), (2.0, 2.4), (2.4, 3.0)]

for temp, (z_min, z_max) in zip(temps, z_levels):
    sim.init(
        xb=(0, 5, 0, 4, z_min, z_max),
        temperature=temp
    )
```

## Velocity Initialization

### Horizontal Flow

```python
# Eastward flow (e.g., prevailing wind)
sim.init(
    xb=(0, 50, 0, 50, 0, 30),
    u_velocity=5.0,  # 5 m/s in +X direction
    v_velocity=0.0,
    w_velocity=0.0
)

# Northward flow
sim.init(
    xb=(0, 50, 0, 50, 0, 30),
    u_velocity=0.0,
    v_velocity=3.0,  # 3 m/s in +Y direction
    w_velocity=0.0
)
```

### Vertical Flow

```python
# Updraft in chimney
sim.init(
    xb=(2, 3, 2, 3, 0, 10),
    u_velocity=0.0,
    v_velocity=0.0,
    w_velocity=2.0  # 2 m/s upward
)

# Downdraft
sim.init(
    xb=(5, 6, 5, 6, 0, 10),
    w_velocity=-1.5  # 1.5 m/s downward
)
```

### Rotating Flow

```python
# Vortex-like initial condition (simplified)
# Center at (5, 5), tangential velocity
import math

r = 2.0  # Radius
omega = 1.0  # Angular velocity (rad/s)

# Note: This is conceptual - FDS doesn't directly support rotating init
# Would need multiple INIT regions to approximate
sim.init(
    xb=(3, 7, 5, 5.2, 0, 3),
    u_velocity=0.0,
    v_velocity=omega * r  # Tangential velocity
)
```

## Species Initialization

### Pre-Existing Smoke

```python
# Smoke-filled upper layer
sim.init(
    xb=(0, 10, 0, 8, 2, 3),
    mass_fraction={'SOOT': 0.01}  # 1% soot by mass
)
```

### Oxygen-Depleted Zone

```python
# Low oxygen region (post-fire area)
sim.init(
    xb=(2, 4, 2, 4, 0, 2.5),
    mass_fraction={'OXYGEN': 0.15}  # 15% O2 (normal ~23%)
)
```

### CO2 Enriched Zone

```python
# High CO2 concentration
sim.init(
    xb=(1, 3, 1, 3, 0, 2),
    mass_fraction={'CARBON DIOXIDE': 0.05}  # 5% CO2
)
```

### Multiple Species

```python
# Complex initial composition
sim.init(
    xb=(2, 5, 2, 5, 1, 2),
    mass_fraction={
        'OXYGEN': 0.18,
        'CARBON DIOXIDE': 0.03,
        'CARBON MONOXIDE': 0.001,
        'SOOT': 0.005
    }
)
```

## Density and Pressure

### Density Initialization

```python
# Low-density hot zone
sim.init(
    xb=(2, 4, 2, 4, 2, 3),
    density=0.8  # kg/m³ (normal air ~1.2)
)

# High-density cold zone
sim.init(
    xb=(5, 7, 5, 7, 0, 1),
    density=1.4  # kg/m³
)
```

### Pressure Initialization

```python
# Elevated pressure zone
sim.init(
    xb=(3, 6, 3, 6, 0, 2.5),
    pressure=101825.0  # Pa (500 Pa above ambient)
)
```

## Complete Examples

### Post-Flashover Room

```python
from pyfds import Simulation

sim = Simulation(chid='post_flashover')
sim.time(t_end=300.0)
sim.mesh(ijk=(60, 50, 30), xb=(0, 6, 0, 5, 0, 3))

# Ambient conditions
sim.set_misc(tmpa=20.0)

# Hot upper layer (post-flashover conditions)
sim.init(
    xb=(0, 6, 0, 5, 2, 3),
    temperature=600.0  # Hot layer
)

# Warm lower layer
sim.init(
    xb=(0, 6, 0, 5, 0, 2),
    temperature=200.0  # Cooler but still hot
)

# Door for ventilation
sim.vent(xb=(6, 6, 2, 3, 0, 2.1), surf_id='OPEN')

# Temperature devices
for z in [0.5, 1.5, 2.5]:
    sim.device(
        id=f'TEMP_Z{int(z*10)}',
        quantity='TEMPERATURE',
        xyz=(3, 2.5, z)
    )

sim.write('post_flashover.fds')
```

### Wind Tunnel

```python
sim = Simulation(chid='wind_tunnel')
sim.time(t_end=120.0)
sim.mesh(ijk=(200, 40, 40), xb=(0, 20, 0, 4, 0, 4))

# Initial wind velocity throughout domain
sim.init(
    xb=(0, 20, 0, 4, 0, 4),
    u_velocity=10.0,  # 10 m/s wind in +X direction
    v_velocity=0.0,
    w_velocity=0.0
)

# Open boundaries for wind flow
sim.vent(mb='XMIN', surf_id='OPEN')
sim.vent(mb='XMAX', surf_id='OPEN')
sim.vent(mb='YMIN', surf_id='OPEN')
sim.vent(mb='YMAX', surf_id='OPEN')
sim.vent(mb='ZMAX', surf_id='OPEN')

# Obstacle in flow
sim.obstruction(xb=(8, 10, 1.5, 2.5, 0, 2), surf_id='INERT')

# Velocity devices downstream
for x in [12, 14, 16]:
    sim.device(
        id=f'VEL_X{x}',
        quantity='VELOCITY',
        xyz=(x, 2, 2)
    )

sim.write('wind_tunnel.fds')
```

### Smoke Layer Evolution

```python
sim = Simulation(chid='smoke_layer')
sim.time(t_end=600.0)
sim.mesh(ijk=(80, 60, 30), xb=(0, 8, 0, 6, 0, 3))

# Initial smoke layer at ceiling
sim.init(
    xb=(0, 8, 0, 6, 2.5, 3),
    temperature=100.0,
    mass_fraction={'SOOT': 0.005}  # Light smoke
)

# Small continued fire source
sim.surface(id='FIRE', hrrpua=200.0)
sim.obstruction(xb=(3.5, 4.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE')

# Door for ventilation
sim.vent(xb=(8, 8, 2.5, 3.5, 0, 2.1), surf_id='OPEN')

# Layer height tracking
sim.device(
    id='LAYER_HEIGHT',
    quantity='LAYER HEIGHT',
    xyz=(4, 3, 1.5)
)

# Visibility at multiple heights
for z in [0.5, 1.5, 2.5]:
    sim.device(
        id=f'VIS_Z{int(z*10)}',
        quantity='VISIBILITY',
        xyz=(4, 3, z)
    )

sim.write('smoke_layer.fds')
```

### Thermal Stratification

```python
sim = Simulation(chid='stratification')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 100), xb=(0, 5, 0, 5, 0, 10))

# Create stratified layers (warmer at top)
# Simulate stable atmospheric conditions
z_ranges = [(0, 2), (2, 4), (4, 6), (6, 8), (8, 10)]
temps = [15, 18, 21, 24, 27]  # Temperature increases with height

for (z_min, z_max), temp in zip(z_ranges, temps):
    sim.init(
        xb=(0, 5, 0, 5, z_min, z_max),
        temperature=temp
    )

# Fire at ground level
sim.surface(id='FIRE', hrrpua=500.0)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.2), surf_id='FIRE')

# Vertical temperature profile
for z in [1, 3, 5, 7, 9]:
    sim.device(
        id=f'TEMP_Z{z}',
        quantity='TEMPERATURE',
        xyz=(2.5, 2.5, z)
    )

sim.write('stratification.fds')
```

### Buoyant Plume in Crossflow

```python
sim = Simulation(chid='plume_crossflow')
sim.time(t_end=300.0)
sim.mesh(ijk=(120, 80, 60), xb=(0, 12, 0, 8, 0, 6))

# Crossflow wind
sim.init(
    xb=(0, 12, 0, 8, 0, 6),
    u_velocity=3.0,  # 3 m/s horizontal wind
    v_velocity=0.0,
    w_velocity=0.0
)

# Open boundaries
for mb in ['XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMAX']:
    sim.vent(mb=mb, surf_id='OPEN')

# Fire source
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 3.5, 4.5, 0, 0.1), surf_id='FIRE')

# Temperature measurements downwind
for x in [4, 6, 8, 10]:
    for z in [1, 2, 3, 4]:
        sim.device(
            id=f'TEMP_X{x}_Z{z}',
            quantity='TEMPERATURE',
            xyz=(x, 4, z)
        )

sim.write('plume_crossflow.fds')
```

## Best Practices

### 1. Consistent with Ambient Conditions

```python
# Set ambient first
sim.set_misc(tmpa=20.0, p_inf=101325.0)

# Then set initial conditions relative to ambient
sim.init(
    xb=(2, 4, 2, 4, 0, 2),
    temperature=100.0  # 100°C, not 80°C above ambient
)
```

### 2. Avoid Unrealistic Gradients

```python
# Good: Smooth transition
sim.init(xb=(0, 5, 0, 4, 0, 1), temperature=50.0)
sim.init(xb=(0, 5, 0, 4, 1, 2), temperature=100.0)
sim.init(xb=(0, 5, 0, 4, 2, 3), temperature=150.0)

# Avoid: Extreme jump (will cause numerical instability)
sim.init(xb=(0, 5, 0, 4, 0, 1.5), temperature=20.0)
sim.init(xb=(0, 5, 0, 4, 1.5, 3), temperature=800.0)  # Too steep!
```

### 3. Match Physics

```python
# For hot layer, reduce density appropriately
# FDS will compute density from ideal gas law
sim.init(
    xb=(0, 6, 0, 5, 2, 3),
    temperature=600.0  # Density automatically calculated
)
```

### 4. Document Initial Conditions

```python
# Clear comments explaining initial state
# Simulating conditions 10 minutes after ignition
# Upper layer at 600°C (post-flashover)
# Lower layer at 200°C (thermal penetration)
sim.init(xb=(0, 6, 0, 5, 2, 3), temperature=600.0)
sim.init(xb=(0, 6, 0, 5, 0, 2), temperature=200.0)
```

## Common Issues

??? question "Simulation unstable at start"
    **Cause**: Unrealistic initial conditions or steep gradients

    **Solution**: Use smoother transitions
    ```python
    # Instead of abrupt change
    sim.init(xb=(0, 5, 0, 4, 0, 1.5), temperature=20.0)
    sim.init(xb=(0, 5, 0, 4, 1.5, 3), temperature=600.0)

    # Use gradual layers
    for z, T in [(0, 20), (1, 100), (2, 300), (2.5, 600)]:
        sim.init(xb=(0, 5, 0, 4, z, z+0.5), temperature=T)
    ```

??? question "Initial velocity disappears quickly"
    **Cause**: No maintained boundary condition

    **Solution**: Use boundary conditions to sustain flow
    ```python
    # Initial velocity
    sim.init(xb=(0, 20, 0, 10, 0, 10), u_velocity=5.0)

    # Maintain with inflow boundary
    sim.vent(mb='XMIN', surf_id='OPEN')
    sim.set_misc(wind_speed=5.0, wind_direction=0.0)
    ```

??? question "Species concentration not conserved"
    **Cause**: Mass fractions don't sum correctly

    **Solution**: Ensure mass fractions are physically realistic
    ```python
    # FDS will normalize, but be explicit
    sim.init(
        xb=(2, 4, 2, 4, 0, 2),
        mass_fraction={
            'OXYGEN': 0.20,        # Reduced from 0.23
            'NITROGEN': 0.77,      # Normal
            'CARBON DIOXIDE': 0.03  # Added
        }
        # Total = 1.00
    )
    ```

## Use Cases

### Research Applications

```python
# Controlled initial conditions for parametric study
initial_temps = [100, 200, 300, 400, 500]

for T_init in initial_temps:
    sim = Simulation(chid=f'init_T{T_init}')
    sim.init(xb=(0, 5, 0, 4, 2, 3), temperature=T_init)
    # ... rest of setup
```

### Validation Studies

```python
# Match experimental initial conditions
# Experiment: Pre-heated ceiling to 200°C
sim.init(
    xb=(0, 3, 0, 3, 2.4, 2.5),  # Ceiling surface
    temperature=200.0
)
```

### Continuing Simulations

```python
# Approximate state from previous run
# Previous run ended with upper layer at 400°C
sim.init(
    xb=(0, 8, 0, 6, 2.5, 3),
    temperature=400.0,
    mass_fraction={'SOOT': 0.008}
)
```

## Combining with Other Features

### With RAMP

```python
# Initial condition + time-varying fire
sim.init(xb=(0, 5, 0, 4, 2, 3), temperature=200.0)

sim.ramp(id='GROWTH', t=[0, 120], f=[0, 1])
sim.surface(id='FIRE', hrrpua=1000.0, ramp_q='GROWTH')
```

### With CTRL

```python
# Pre-heated zone affects control activation
sim.init(xb=(2, 4, 2, 4, 0, 3), temperature=150.0)

sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(3, 3, 1.5))
sim.control(id='CTRL', input_id='TEMP', setpoint=200.0)  # Will activate sooner
```

## Next Steps

- [Global Settings](global-settings.md) - Ambient conditions (MISC)
- [RAMP](ramps.md) - Time-varying properties
- [Controls](controls.md) - Device-based automation
- [Examples](../examples/advanced.md) - Complex scenarios

---

[Combustion →](combustion.md){ .md-button .md-button--primary }
