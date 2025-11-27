# Suppression Systems

Model fire sprinklers, water mist, spray nozzles, and other suppression systems in FDS.

## Overview

PyFDS provides comprehensive support for modeling suppression systems including:
- Automatic sprinklers (quick-response, standard-response, ESFR)
- Water mist systems
- Spray nozzles
- Gaseous suppression
- Device activation and control logic

## Components

A complete sprinkler system requires three components:

1. **PART** - Water droplet particle definition
2. **PROP** - Sprinkler/detector activation properties
3. **SURF** - Spray pattern and droplet generation
4. **DEVC** - Device placement and control logic (optional)

## Quick-Response Sprinkler Example

Complete working example:

```python
from pyfds.builders import PartBuilder, PropBuilder, SurfBuilder, DevcBuilder
from pyfds.core.geometry import Point3D

# 1. Define water droplet particle
water = (
    PartBuilder("WATER_DROP")
    .as_water_droplet(diameter=0.001)
    .with_color("BLUE")
    .build()
)

# 2. Define sprinkler property (68°C, RTI=50)
sprinkler_prop = PropBuilder.quick_response_sprinkler(id="QR_SPRINKLER")

# 3. Create sprinkler spray surface
sprinkler_surf = (
    SurfBuilder("SPRINKLER_SPRAY")
    .as_sprinkler(
        part_id="WATER_DROP",
        mass_flux=0.02,          # kg/s/m²
        median_diameter=0.001,   # 1mm median droplet
        velocity=6.0             # 6 m/s downward
    )
    .build()
)

# 4. Create device with activation control
sprinkler_device = (
    DevcBuilder("SPRINK1")
    .with_quantity("SPRINKLER_LINK_TEMPERATURE")
    .with_control(setpoint=68.0, trip_direction=1, latch=True, delay=2.0)
    .at_point(Point3D(3.0, 2.0, 2.8))
    .with_prop("QR_SPRINKLER")
    .build()
)

# Add to simulation
sim.add_particle(water)
sim.add_prop(sprinkler_prop)
sim.add_surface(sprinkler_surf)
sim.add_device(sprinkler_device)
```

## Sprinkler Properties

### Predefined Sprinklers

```python
# Quick-response residential (68°C, RTI=50, 60 L/min)
qr = PropBuilder.quick_response_sprinkler()

# Standard-response commercial (74°C, RTI=100, K=80)
sr = PropBuilder.standard_response_sprinkler()
```

### Custom Sprinkler

```python
# Early Suppression Fast Response (ESFR)
esfr = PropBuilder.sprinkler(
    id="ESFR",
    activation_temp=74,      # °C
    rti=50,                  # (m·s)^0.5
    k_factor=200,            # (L/min)/bar^0.5
    spray_angle=(45, 90),    # degrees
    pressure=200000,         # Pa (2 bar)
    orifice_diameter=0.020   # 20mm orifice
)
```

### K-Factor vs Flow Rate

```python
# Using K-factor (pressure-dependent)
sprinkler = PropBuilder.sprinkler(
    id="K80",
    activation_temp=68,
    rti=50,
    k_factor=80,
    pressure=100000  # 1 bar → 80 L/min
)

# Using direct flow rate
sprinkler = PropBuilder.sprinkler(
    id="FIXED_FLOW",
    activation_temp=68,
    rti=50,
    flow_rate=60  # Always 60 L/min
)
```

## Spray Patterns

### Basic Spray

```python
# Simple downward spray
spray = (
    SurfBuilder("SIMPLE_SPRAY")
    .as_sprinkler(
        part_id="WATER_DROP",
        mass_flux=0.015,
        median_diameter=0.001,
        velocity=5.0
    )
    .build()
)
```

### Advanced Distribution

```python
# Spray with droplet size distribution
spray = (
    SurfBuilder("DISTRIBUTED_SPRAY")
    .with_particle_generation("WATER_DROP", mass_flux=0.02, nppc=5)
    .with_droplet_distribution(
        median_diameter=0.0008,  # 0.8mm median
        gamma_d=2.4,             # Log-normal shape parameter
        spray_pattern="GAUSSIAN" # Gaussian or UNIFORM
    )
    .with_particle_velocity(6.0)  # Magnitude
    .build()
)

# Vector velocity (angled spray)
angled_spray = (
    SurfBuilder("ANGLED")
    .with_particle_generation("WATER_DROP", mass_flux=0.01)
    .with_particle_velocity((1.0, 0.0, -5.0))  # Angled downward
    .build()
)
```

## Water Mist Systems

Smaller droplets, higher pressure:

```python
# Fine water mist droplet
mist_drop = (
    PartBuilder("MIST_DROP")
    .as_water_droplet(diameter=0.00005)  # 50 microns
    .with_breakup(enabled=True)
    .build()
)

# Mist nozzle with high pressure
mist_nozzle = PropBuilder.nozzle(
    id="MIST_NOZZLE",
    k_factor=20,
    pressure=5000000,  # 50 bar
    orifice_diameter=0.0005
)

# Mist spray pattern
mist_spray = (
    SurfBuilder("MIST_SPRAY")
    .with_particle_generation("MIST_DROP", mass_flux=0.005, nppc=10)
    .with_droplet_distribution(
        median_diameter=0.00005,
        gamma_d=2.0,
        spray_pattern="GAUSSIAN"
    )
    .with_particle_velocity(20.0)  # High velocity
    .build()
)
```

## Activation Control

### Temperature-Based Activation

```python
# Sprinkler activates at 68°C
device = (
    DevcBuilder("SPRINK1")
    .with_quantity("SPRINKLER_LINK_TEMPERATURE")
    .with_control(setpoint=68.0, trip_direction=1, latch=True)
    .at_point(Point3D(3.0, 2.0, 2.8))
    .with_prop("QR_SPRINKLER")
    .build()
)
```

### Delayed Activation

```python
# 2-second delay before activation
device = (
    DevcBuilder("SPRINK_DELAYED")
    .with_quantity("SPRINKLER_LINK_TEMPERATURE")
    .with_control(setpoint=68.0, trip_direction=1, latch=True, delay=2.0)
    .at_point(Point3D(3.0, 2.0, 2.8))
    .build()
)
```

### Manual Control

```python
# Activated by time
device = (
    DevcBuilder("TIMED_SPRAY")
    .with_quantity("TIME")
    .with_control(setpoint=10.0, trip_direction=1, latch=True)
    .build()
)
```

## Nozzle Systems

For fixed spray systems without thermal activation:

```python
# Define nozzle property
nozzle = PropBuilder.nozzle(
    id="SPRAY_NOZZLE",
    flow_rate=50,      # L/min
    pressure=300000,   # 3 bar
    orifice_diameter=0.01
)

# Nozzle surface (always on)
nozzle_surf = (
    SurfBuilder("NOZZLE_SPRAY")
    .with_particle_generation("WATER_DROP", mass_flux=0.01)
    .with_droplet_distribution(
        median_diameter=0.001,
        spray_pattern="UNIFORM"
    )
    .build()
)
```

## Array of Sprinklers

```python
# Create multiple sprinklers in a grid
sprinkler_locations = [
    (1.5, 1.5, 2.8),
    (1.5, 4.5, 2.8),
    (4.5, 1.5, 2.8),
    (4.5, 4.5, 2.8),
]

for i, (x, y, z) in enumerate(sprinkler_locations):
    device = (
        DevcBuilder(f"SPRINK_{i+1}")
        .with_quantity("SPRINKLER_LINK_TEMPERATURE")
        .with_control(setpoint=68.0, trip_direction=1, latch=True)
        .at_point(Point3D(x, y, z))
        .with_prop("QR_SPRINKLER")
        .build()
    )
    sim.add_device(device)
```

## Suppression Effectiveness

### Coverage Area

Typical sprinkler coverage:
- **Residential**: 9-12 m² per sprinkler
- **Commercial (light hazard)**: 12-21 m²
- **Commercial (ordinary hazard)**: 9-12 m²
- **High-challenge**: 6-9 m²

### Design Density

| Occupancy | Design Density | Flow Rate |
|-----------|----------------|-----------|
| Light Hazard | 2.5 mm/min | 30-50 L/min |
| Ordinary Hazard I | 6 mm/min | 60-80 L/min |
| Ordinary Hazard II | 10 mm/min | 80-100 L/min |
| Extra Hazard | 12+ mm/min | 100-200 L/min |

## Performance Tips

### Computational Efficiency

```python
# Start with coarse particles
water_coarse = (
    PartBuilder("WATER_COARSE")
    .as_water_droplet(diameter=0.002)  # Larger = fewer particles
    .build()
)

# Reduce particles per cell for testing
test_spray = (
    SurfBuilder("TEST_SPRAY")
    .with_particle_generation("WATER_COARSE", mass_flux=0.02, nppc=1)
    .build()
)

# Increase for production runs
production_spray = (
    SurfBuilder("PRODUCTION_SPRAY")
    .with_particle_generation("WATER_DROP", mass_flux=0.02, nppc=5)
    .build()
)
```

### Sampling

For very large particle populations:

```python
# Sample 1 in 10 particles
efficient_water = (
    PartBuilder("SAMPLED_WATER")
    .as_water_droplet(diameter=0.001)
    .with_sampling_factor(10)
    .build()
)
```

## Validation

Always validate suppression systems against:
- Required design density (mm/min)
- Activation time expectations
- Coverage area
- Pressure requirements
- Droplet size distribution

## Common Issues

### Sprinkler Not Activating

Check:
1. `SPRINKLER_LINK_TEMPERATURE` quantity is correct
2. Setpoint matches sprinkler activation temperature
3. `trip_direction=1` (activate when exceeding setpoint)
4. Sufficient heat reaching sprinkler location

### Insufficient Water

Increase:
- `mass_flux` value
- `nppc` (particles per cell)
- Number of active sprinklers
- Coverage area

### Computational Cost

Reduce:
- Particle diameter (fewer particles needed)
- `nppc` value
- Simulation duration
- Use `sampling_factor`

## See Also

- [Particles](particles.md) - Water droplet definition
- [Devices](devices.md) - Detector and control devices
- [Surfaces](materials-surfaces.md) - Surface particle generation
- [SFPE Handbook](https://www.sfpe.org/) - Suppression system design
