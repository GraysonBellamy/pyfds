# PropBuilder

::: pyfds.builders.prop.PropBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`PropBuilder` creates device properties (`&PROP` namelists) using factory methods.

## Key Features

- **Sprinklers**: Activation temperature, RTI, flow rate, K-factor
- **Smoke Detectors**: Obscuration threshold
- **Heat Detectors**: Activation temperature, RTI
- **Predefined**: Quick-response and standard-response sprinklers

## Factory Methods Pattern

Unlike other builders, `PropBuilder` uses **class methods** (factory pattern) instead of fluent chaining:

```python
from pyfds.builders import PropBuilder

# Factory method (not fluent)
sprinkler = PropBuilder.quick_response_sprinkler(id='QR')

# Not: PropBuilder().sprinkler()...  # This doesn't exist
```

## Quick Examples

### Predefined Sprinklers

```python
# Quick-response sprinkler (RTI=50, T_act=68°C)
qr = PropBuilder.quick_response_sprinkler(id='SPRINKLER_QR')

# Standard-response sprinkler (RTI=100, T_act=68°C)
sr = PropBuilder.standard_response_sprinkler(id='SPRINKLER_SR')
```

### Custom Sprinkler

```python
# Custom sprinkler with specific parameters
sprinkler = PropBuilder.sprinkler(
    id='CUSTOM_SPRINKLER',
    activation_temp=68,     # °C
    rti=50,                 # (m·s)^0.5
    flow_rate=60,           # L/min
    k_factor=80             # L/(min·bar^0.5)
)
```

### Smoke Detector

```python
# Photoelectric smoke detector (UL 268 standard)
smoke_det = PropBuilder.smoke_detector(
    id='PHOTOELECTRIC',
    activation_obscuration=3.28  # %/ft
)

# More sensitive detector
sensitive = PropBuilder.smoke_detector(
    id='SENSITIVE',
    activation_obscuration=2.0  # Lower threshold
)
```

### Heat Detector

```python
# Heat detector
heat_det = PropBuilder.heat_detector(
    id='HEAT_DET',
    activation_temp=74,  # °C
    rti=5.0              # (m·s)^0.5
)

# Fast-response heat detector
fast_heat = PropBuilder.heat_detector(
    id='FAST_HEAT',
    activation_temp=57,
    rti=1.0  # Very low RTI = fast response
)
```

## Sprinkler Properties

### Parameters

- **`activation_temp`**: Activation temperature (°C)
- **`rti`**: Response Time Index ((m·s)^0.5) - thermal inertia
- **`flow_rate`**: Water flow rate (L/min)
- **`k_factor`**: Discharge coefficient (L/(min·bar^0.5))

### Response Time Index (RTI)

RTI characterizes thermal response:

| Category | RTI Range | Response |
|----------|-----------|----------|
| Quick-response | 30-50 | Fast |
| Special response | 50-80 | Moderate |
| Standard response | 80-350 | Slow |

```python
# Quick response (RTI=50)
qr = PropBuilder.sprinkler(id='QR', activation_temp=68, rti=50)

# Standard response (RTI=100)
sr = PropBuilder.sprinkler(id='SR', activation_temp=68, rti=100)
```

### Activation Temperature

Common ratings:

| Color | Temp (°C) | Temp (°F) | Application |
|-------|-----------|-----------|-------------|
| Orange | 57 | 135 | Very high ambient |
| Red | 68 | 155 | Normal (most common) |
| Yellow | 79 | 175 | High ambient |
| Green | 93 | 200 | Extra high ambient |
| Blue | 141 | 286 | Very high hazard |

```python
# Normal temperature rating
normal = PropBuilder.sprinkler(id='NORMAL', activation_temp=68, rti=50)

# High temperature rating
high_temp = PropBuilder.sprinkler(id='HIGH_TEMP', activation_temp=79, rti=50)
```

### K-Factor and Flow Rate

K-factor relates flow to pressure:

$$
Q = K \sqrt{P}
$$

where:
- $Q$ = flow rate (L/min)
- $K$ = K-factor (L/(min·bar^0.5))
- $P$ = pressure (bar)

Common K-factors:
- **K=80**: Standard residential/commercial
- **K=115**: Large orifice
- **K=160-360**: ESFR (Early Suppression Fast Response)

```python
# Standard K-factor
standard = PropBuilder.sprinkler(id='K80', k_factor=80, activation_temp=68, rti=50)

# Large orifice
large = PropBuilder.sprinkler(id='K115', k_factor=115, activation_temp=68, rti=50)
```

## Smoke Detector Properties

### Activation Obscuration

Optical density at which detector activates (%/ft or %/m):

```python
# UL 268 standard (3.28%/ft)
standard = PropBuilder.smoke_detector(id='UL268', activation_obscuration=3.28)

# More sensitive (2%/ft)
sensitive = PropBuilder.smoke_detector(id='SENSITIVE', activation_obscuration=2.0)

# Less sensitive (4%/ft)
less_sensitive = PropBuilder.smoke_detector(id='ROBUST', activation_obscuration=4.0)
```

Typical values:
- **1-2 %/ft**: Very sensitive (cleanrooms)
- **3.28 %/ft**: UL 268 standard (residential/commercial)
- **4-5 %/ft**: Less sensitive (industrial)

### Conversion

1 %/ft ≈ 3.28 %/m

```python
# Same sensitivity in different units
detector_ft = PropBuilder.smoke_detector(id='DET_FT', activation_obscuration=3.28)  # %/ft
detector_m = PropBuilder.smoke_detector(id='DET_M', activation_obscuration=10.76)   # %/m (3.28 * 3.28)
```

## Heat Detector Properties

### Activation Temperature

Temperature at which detector activates:

```python
# Standard 74°C (165°F)
standard = PropBuilder.heat_detector(id='STANDARD', activation_temp=74, rti=5.0)

# High temperature 88°C (190°F)
high_temp = PropBuilder.heat_detector(id='HIGH', activation_temp=88, rti=5.0)
```

Common ratings:
- **57°C (135°F)**: Low temperature
- **68°C (155°F)**: Ordinary
- **74°C (165°F)**: Standard
- **88°C (190°F)**: Intermediate
- **107°C (225°F)**: High

### Response Time Index (RTI)

For heat detectors, RTI typically:
- **1-5 (m·s)^0.5**: Fast response
- **5-20**: Moderate
- **20-50**: Slow

```python
# Fast response
fast = PropBuilder.heat_detector(id='FAST', activation_temp=74, rti=1.0)

# Standard response
standard = PropBuilder.heat_detector(id='STD', activation_temp=74, rti=5.0)

# Slow response
slow = PropBuilder.heat_detector(id='SLOW', activation_temp=74, rti=20.0)
```

## Usage in Simulations

### Sprinkler System

```python
from pyfds import Simulation
from pyfds.builders import PropBuilder

sim = Simulation('sprinkler_system')

# Add sprinkler property
sprinkler_prop = PropBuilder.quick_response_sprinkler(id='SPRINKLER_QR')
sim.add_prop(sprinkler_prop)

# Add sprinkler devices
for i in range(5):
    sim.device(
        id=f'SPRINKLER_{i}',
        prop_id='SPRINKLER_QR',
        xyz=(i*2, 5, 3),
        quantity='SPRINKLER_LINK_TEMPERATURE'
    )
```

### Smoke Detection System

```python
# Smoke detector property
smoke_prop = PropBuilder.smoke_detector(id='SMOKE_DET', activation_obscuration=3.28)
sim.add_prop(smoke_prop)

# Add detectors throughout building
locations = [(2, 5, 2.5), (5, 5, 2.5), (8, 5, 2.5)]
for i, xyz in enumerate(locations, 1):
    sim.device(
        id=f'SMOKE_DET_{i}',
        prop_id='SMOKE_DET',
        xyz=xyz
    )

# Add control logic (ANY detector triggers alarm)
from pyfds.builders import ControlBuilder
alarm = (
    ControlBuilder('ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2', 'SMOKE_DET_3'])
    .with_latch(True)
    .build()
)
sim.add_ctrl(alarm)
```

### Heat Detector Network

```python
# Heat detector property
heat_prop = PropBuilder.heat_detector(id='HEAT_DET', activation_temp=74, rti=5.0)
sim.add_prop(heat_prop)

# Add heat detectors
for i in range(1, 4):
    sim.device(
        id=f'HEAT_DET_{i}',
        prop_id='HEAT_DET',
        xyz=(i*3, 5, 2.5)
    )

# Delayed sprinkler activation on heat detection
delayed_ctrl = (
    ControlBuilder('DELAYED_SPRINKLER')
    .time_delay('HEAT_DET_1', delay=10.0)
    .build()
)
sim.add_ctrl(delayed_ctrl)
```

### Mixed System

```python
# Both smoke and heat detectors
smoke_prop = PropBuilder.smoke_detector(id='SMOKE')
heat_prop = PropBuilder.heat_detector(id='HEAT', activation_temp=74, rti=5.0)
sprinkler_prop = PropBuilder.quick_response_sprinkler(id='SPRINKLER')

sim.add_prop(smoke_prop)
sim.add_prop(heat_prop)
sim.add_prop(sprinkler_prop)

# Smoke detector for early warning
sim.device(id='SD_1', prop_id='SMOKE', xyz=(5, 5, 2.5))

# Heat detector for sprinkler activation
sim.device(id='HD_1', prop_id='HEAT', xyz=(5, 5, 2.5))

# Sprinkler activated by heat detector
sim.device(id='SPK_1', prop_id='SPRINKLER', xyz=(5, 5, 3), ctrl_id='HD_1')
```

## Predefined Properties

### Quick-Response Sprinkler

```python
qr = PropBuilder.quick_response_sprinkler(id='QR')
```

Default values:
- Activation temp: 68°C
- RTI: 50 (m·s)^0.5
- Flow rate: 60 L/min
- K-factor: 80 L/(min·bar^0.5)

### Standard-Response Sprinkler

```python
sr = PropBuilder.standard_response_sprinkler(id='SR')
```

Default values:
- Activation temp: 68°C
- RTI: 100 (m·s)^0.5
- Flow rate: 60 L/min
- K-factor: 80 L/(min·bar^0.5)

## Best Practices

### Match RTI to Application

```python
# Good: Fast response for high-hazard areas
high_hazard = PropBuilder.sprinkler(id='HIGH_HAZ', activation_temp=68, rti=30)

# Good: Standard for normal occupancy
normal = PropBuilder.quick_response_sprinkler(id='NORMAL')

# Avoid: Slow response for high hazard
# slow = PropBuilder.sprinkler(id='HIGH_HAZ', rti=200)  # Too slow
```

### Use UL 268 for Smoke Detectors

```python
# Good: Standard UL 268
detector = PropBuilder.smoke_detector(id='SMOKE', activation_obscuration=3.28)

# Custom only if needed
sensitive = PropBuilder.smoke_detector(id='SENSITIVE', activation_obscuration=2.0)
```

### Temperature Ratings

```python
# Good: Match rating to ambient temperature
# Normal ambient (20°C)
normal = PropBuilder.sprinkler(id='NORMAL', activation_temp=68, rti=50)

# High ambient (40°C)
high_ambient = PropBuilder.sprinkler(id='HIGH_AMB', activation_temp=79, rti=50)

# Avoid: Under-rated for ambient
# cold = PropBuilder.sprinkler(id='COLD', activation_temp=57, rti=50)  # Too low for 40°C ambient
```

## Thermal Response

### Temperature Rise Equation

Device temperature rise is governed by:

$$
\frac{dT_d}{dt} = \frac{1}{\tau} (T_g - T_d)
$$

where:
- $T_d$ = device temperature
- $T_g$ = gas temperature
- $\tau$ = time constant

Time constant relates to RTI:

$$
\tau = \frac{\text{RTI}}{\sqrt{u}}
$$

where $u$ is gas velocity.

## See Also

- [User Guide - Devices & Measurement](../../guide/devices.md)
- [User Guide - Builders](../../guide/builders.md)
- [ControlBuilder](control.md) - For device control logic
- [User Guide - Control Logic](../../guide/controls.md)
