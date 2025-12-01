# RampBuilder

::: pyfds.builders.ramp.RampBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`RampBuilder` creates time-dependent functions (`&RAMP` namelists) for fire growth, temperature-dependent properties, and control schedules.

## Key Features

- **Fire Growth Patterns**: T-squared curves (SLOW, MEDIUM, FAST, ULTRAFAST)
- **Temperature Tables**: For temperature-dependent material properties
- **Standard Patterns**: Linear, step, exponential, sine wave
- **Custom Points**: Full control over (T, F) pairs

## Quick Examples

### T-squared Fire Growth

```python
from pyfds.builders import RampBuilder

# Fast t-squared fire
fire_ramp = (
    RampBuilder('HRR_GROWTH')
    .t_squared('FAST', peak_hrr=2500, t_peak=300)
    .build()
)
```

Growth rates and α values:
- `'SLOW'`: α = 0.00293 kW/s²
- `'MEDIUM'`: α = 0.01172 kW/s²
- `'FAST'`: α = 0.04689 kW/s²
- `'ULTRAFAST'`: α = 0.1876 kW/s²

### Temperature-Dependent Properties

```python
# Thermal conductivity vs temperature
k_ramp = (
    RampBuilder('STEEL_K')
    .temperature_table({
        20: 45.8,   # 20°C → 45.8 W/(m·K)
        100: 43.3,
        200: 40.7,
        400: 36.4,
        600: 31.0
    })
    .build()
)

# Use in material
steel = (
    MaterialBuilder('STEEL')
    .thermal_conductivity_ramp('STEEL_K')
    .build()
)
```

### Linear Ramp

```python
# Linear increase from 0 to 1 over 100 seconds
linear = (
    RampBuilder('LINEAR')
    .linear(t_start=0, t_end=100, f_start=0, f_end=1)
    .build()
)
```

### Step Function

```python
# Step from 0 to 1 at t=60s
step = (
    RampBuilder('STEP')
    .step(t_step=60, f_before=0, f_after=1)
    .build()
)
```

### Exponential Growth

```python
# Exponential growth
exp_ramp = (
    RampBuilder('EXP')
    .exponential(t_start=0, t_end=100, f_start=1, f_end=100, rate=0.05)
    .build()
)
```

### Sine Wave

```python
# Sine wave oscillation
sine = (
    RampBuilder('SINE')
    .sine_wave(period=60, amplitude=1.0, offset=0.5, phase=0)
    .build()
)
```

### Custom Points

```python
# Custom time-value pairs
custom = (
    RampBuilder('CUSTOM')
    .add_point(0, 0)
    .add_point(10, 1)
    .add_point(20, 0.5)
    .add_point(30, 0)
    .build()
)
```

## Usage in Simulations

### Fire Growth

```python
from pyfds import Simulation
from pyfds.builders import RampBuilder

sim = Simulation('fire_growth')

# Create fire growth ramp
fire_ramp = RampBuilder('FIRE').t_squared('FAST', peak_hrr=2500, t_peak=300).build()
sim.add_ramp(fire_ramp)

# Use in surface
sim.add(Surface(id='BURNER', hrrpua=1000.0, ramp_q='FIRE'))
```

### Material Properties

```python
# Create temperature-dependent conductivity
k_ramp = RampBuilder('STEEL_K').temperature_table({
    20: 45.8, 100: 43.3, 200: 40.7, 400: 36.4, 600: 31.0
}).build()
sim.add_ramp(k_ramp)

# Create temperature-dependent specific heat
cp_ramp = RampBuilder('STEEL_CP').temperature_table({
    20: 0.46, 100: 0.49, 200: 0.52, 400: 0.58
}).build()
sim.add_ramp(cp_ramp)

# Use in material
steel = (
    MaterialBuilder('STEEL')
    .thermal_conductivity_ramp('STEEL_K')
    .specific_heat_ramp('STEEL_CP')
    .build()
)
sim.add_material(steel)
```

### Control Schedules

```python
# HVAC schedule
hvac_schedule = (
    RampBuilder('HVAC')
    .add_point(0, 0)        # Off initially
    .add_point(28800, 1)    # On at 8:00 AM (8*3600)
    .add_point(64800, 0)    # Off at 6:00 PM (18*3600)
    .add_point(86400, 0)    # Off at midnight
    .build()
)
sim.add_ramp(hvac_schedule)
```

## Predefined Library

```python
from pyfds.builders.libraries import CommonRamps

# T-squared fire growth (all rates)
slow = CommonRamps.t_squared_slow(peak_hrr=1000)
medium = CommonRamps.t_squared_medium(peak_hrr=2000)
fast = CommonRamps.t_squared_fast(peak_hrr=2500)
ultra = CommonRamps.t_squared_ultrafast(peak_hrr=5000)

# Step function
step = CommonRamps.step_at(t=60, value=1.0)

# Linear ramp
linear = CommonRamps.linear_ramp(t_start=0, t_end=100, f_start=0, f_end=1)

# HVAC schedule (24-hour)
hvac = CommonRamps.hvac_schedule_24h(on_time=8, off_time=18)
```

## Implementation Details

### T-squared Fire Growth

The t-squared fire growth model is:

$$
\text{HRR}(t) = \alpha (t - t_{\text{start}})^2
$$

where α is the growth rate constant:

| Rate | α (kW/s²) | Time to 1000 kW |
|------|-----------|-----------------|
| SLOW | 0.00293 | 585 s (9.8 min) |
| MEDIUM | 0.01172 | 292 s (4.9 min) |
| FAST | 0.04689 | 146 s (2.4 min) |
| ULTRAFAST | 0.1876 | 73 s (1.2 min) |

### Temperature Tables

Temperature tables use `DEVC_ID='TEMPERATURE'` to create temperature-dependent properties. The builder automatically sets this for you.

### Validation

The builder validates:
- No duplicate T values
- T values are sorted
- At least 2 points for valid ramp

## See Also

- [User Guide - Time-Varying Properties](../../guide/ramps.md)
- [User Guide - Builders](../../guide/builders.md)
- [MaterialBuilder](material.md) - Using ramps in materials
