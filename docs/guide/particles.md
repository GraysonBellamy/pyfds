# Particles

Define particle classes for water droplets, smoke, fuel vapors, and other Lagrangian particles in your FDS simulations.

## Overview

Particles (PART) represent discrete entities that move through the flow field. Common uses include:
- Water droplets from sprinklers
- Smoke particles for visualization
- Fuel vapor droplets
- Tracer particles for flow visualization

```python
from pyfds.builders import PartBuilder

# Water droplet for sprinkler
water = (
    PartBuilder("WATER_DROP")
    .as_water_droplet(diameter=0.001)
    .with_color("BLUE")
    .build()
)

# Add to simulation
sim.add_particle(water)
```

## Particle Types

### Water Droplets

For sprinkler spray and water mist systems:

```python
# Standard water droplet
water = (
    PartBuilder("WATER_DROP")
    .as_water_droplet(diameter=0.001)  # 1mm diameter
    .with_color("BLUE")
    .build()
)

# Water with custom temperature
warm_water = (
    PartBuilder("WARM_WATER")
    .as_water_droplet(diameter=0.001, temp=40.0)
    .build()
)
```

**Key properties set by `as_water_droplet()`:**
- Species: Water vapor
- Liquid droplet: Yes
- Boiling temperature: 100°C
- Heat of vaporization: 2260 kJ/kg
- Drag law: SPHERE

### Smoke and Aerosol Particles

For smoke visualization and aerosol modeling:

```python
# Smoke particle
smoke = (
    PartBuilder("SMOKE")
    .as_aerosol(diameter=0.00001, spec_id="SOOT")
    .with_color("GRAY 50")
    .build()
)

# Fuel vapor
propane_vapor = (
    PartBuilder("PROPANE_VAPOR")
    .as_aerosol(diameter=0.0001, spec_id="PROPANE")
    .with_color("YELLOW")
    .build()
)
```

### Tracer Particles

For flow visualization (massless particles):

```python
tracer = (
    PartBuilder("TRACER")
    .as_tracer()
    .with_diameter(0.0001)
    .with_color("RED")
    .build()
)
```

## Particle Properties

### Size and Mass

```python
particle = (
    PartBuilder("CUSTOM")
    .with_diameter(0.002)      # 2mm diameter
    .with_density(1500)        # kg/m³
    .build()
)

# Or specify mass directly
particle = (
    PartBuilder("CUSTOM")
    .with_mass(5e-9)          # 5 nanograms
    .build()
)
```

### Motion Properties

```python
# Custom drag law
particle = (
    PartBuilder("DROP")
    .with_diameter(0.001)
    .with_drag_law("SPHERE")  # or other FDS drag laws
    .build()
)

# Static particle (doesn't move)
static_soot = (
    PartBuilder("DEPOSITED_SOOT")
    .with_diameter(0.00001)
    .as_static()
    .build()
)
```

### Particle Lifetime

Control how long particles exist:

```python
# Particle that disappears after 10 seconds
short_lived = (
    PartBuilder("TEMP_PARTICLE")
    .with_diameter(0.001)
    .with_lifetime(10.0)
    .build()
)

# Particle with initial age
aged = (
    PartBuilder("AGED")
    .with_diameter(0.001)
    .with_initial_age(5.0)
    .with_lifetime(10.0)  # Will disappear at t=15s
    .build()
)
```

### Breakup

For droplet breakup modeling:

```python
# Enable breakup
breakable = (
    PartBuilder("WATER_BREAKUP")
    .as_water_droplet(diameter=0.002)
    .with_breakup(enabled=True)
    .with_breakup_parameters(cns_min=0.5, cns_max=12.0)
    .build()
)
```

### Visualization

```python
# Named color
blue_particle = (
    PartBuilder("PART1")
    .with_diameter(0.001)
    .with_color("BLUE")
    .build()
)

# RGB color (0-255)
custom_color = (
    PartBuilder("PART2")
    .with_diameter(0.001)
    .with_rgb(255, 128, 0)  # Orange
    .build()
)
```

## Advanced Features

### Sampling Factor

Reduce computational cost by sampling particles:

```python
particle = (
    PartBuilder("SAMPLED")
    .with_diameter(0.001)
    .with_sampling_factor(10)  # 1 in 10 particles simulated
    .build()
)
```

### Linked Surfaces and Properties

```python
# Particle with custom surface
particle = (
    PartBuilder("CUSTOM")
    .with_diameter(0.001)
    .with_surface("CUSTOM_SURF")
    .build()
)

# Particle with device property
particle = (
    PartBuilder("DEVICE_PART")
    .with_diameter(0.001)
    .with_property("CUSTOM_PROP")
    .build()
)
```

### Orientation

For non-spherical particles:

```python
oriented = (
    PartBuilder("ORIENTED")
    .with_diameter(0.001)
    .with_orientation(axis=(0, 0, 1), rotation=45.0)
    .build()
)
```

## Typical Values

### Diameter Ranges

| Particle Type | Typical Diameter | Range |
|--------------|------------------|-------|
| Sprinkler droplets | 0.001 m (1 mm) | 0.0005 - 0.002 m |
| Mist/fog droplets | 0.00001 m (10 μm) | 0.000005 - 0.0001 m |
| Smoke particles | 0.00001 m (10 μm) | 0.000001 - 0.00005 m |
| Fuel vapor | 0.0001 m (100 μm) | 0.00005 - 0.0005 m |

### Density Values

| Material | Density (kg/m³) |
|----------|----------------|
| Water | 1000 |
| Fuel oils | 800-900 |
| Soot | 1800-2000 |

## Common Patterns

### Sprinkler System

```python
# Create water droplet
water = (
    PartBuilder("WATER_DROP")
    .as_water_droplet(diameter=0.001)
    .with_color("BLUE")
    .build()
)

# Create sprinkler surface (see Suppression Systems guide)
sprinkler = (
    SurfBuilder("SPRINKLER")
    .as_sprinkler(
        part_id="WATER_DROP",
        mass_flux=0.02,
        median_diameter=0.001,
        velocity=6.0
    )
    .build()
)
```

### Fire with Smoke

```python
# Create smoke particle
smoke = (
    PartBuilder("SMOKE")
    .as_aerosol(diameter=0.00001, spec_id="SOOT")
    .with_color("GRAY 50")
    .build()
)

# Create fire surface that generates smoke
fire = (
    SurfBuilder("FIRE")
    .with_heat_release(1000.0)
    .with_particle_generation("SMOKE", mass_flux=0.002)
    .with_color("ORANGE")
    .build()
)
```

## Best Practices

1. **Choose appropriate sizes**: Match particle diameter to physical reality
2. **Use predefined types**: `as_water_droplet()`, `as_aerosol()`, `as_tracer()` set physics automatically
3. **Set lifetimes for long simulations**: Prevent memory issues with particle accumulation
4. **Use static particles for deposition**: Model soot deposition on surfaces
5. **Sample large populations**: Use `sampling_factor` for millions of particles

## See Also

- [Suppression Systems](suppression-systems.md) - Sprinkler and spray systems
- [Surfaces](materials-surfaces.md) - Surface particle generation
- [Devices](devices.md) - Particle-based devices
- [FDS User Guide](https://pages.nist.gov/fds-smv/) - PART namelist reference
