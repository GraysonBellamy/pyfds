# PartBuilder

Builder for creating particle definitions with a fluent API.

::: pyfds.builders.part.PartBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`PartBuilder` creates particle definitions (`&PART` namelists) for sprinklers, droplets, tracers, and other particle-based phenomena.

## Quick Examples

### Water Droplets for Sprinklers

```python
from pyfds.builders import PartBuilder

# Water droplet particles
water = (
    PartBuilder("WATER")
    .as_water_droplet()
    .with_diameter(500)  # microns
    .build()
)
```

### Tracer Particles

```python
# Tracer particles for flow visualization
tracer = (
    PartBuilder("TRACER")
    .as_tracer()
    .with_color("BLUE")
    .build()
)
```

## See Also

- [Particles Guide](../../guide/particles.md) - Particle modeling overview
- [Suppression Systems](../../guide/suppression-systems.md) - Sprinkler systems
