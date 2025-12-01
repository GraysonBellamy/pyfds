# SurfaceBuilder

Builder for creating surface definitions with a fluent API.

::: pyfds.builders.surface.SurfaceBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`SurfaceBuilder` creates surface definitions (`&SURF` namelists) with support for burning surfaces, flow surfaces, and layered materials.

## Quick Examples

### Fire Surface

```python
from pyfds.builders import SurfaceBuilder

# Simple burner surface
burner = (
    SurfaceBuilder("BURNER")
    .with_hrrpua(1000.0)  # kW/m²
    .with_color("RED")
    .build()
)
```

### Wall with Material Layer

```python
# Concrete wall
wall = (
    SurfaceBuilder("CONCRETE_WALL")
    .with_material("CONCRETE")
    .with_thickness(0.2)  # 20 cm
    .with_color("GRAY")
    .build()
)
```

### Ventilation Surface

```python
# Supply vent
supply = (
    SurfaceBuilder("SUPPLY")
    .with_volume_flux(-1.0)  # m³/s/m² (negative = inflow)
    .with_temperature(20.0)
    .build()
)
```

## See Also

- [Materials & Surfaces Guide](../../guide/materials-surfaces.md) - Surface configuration
- [Fire Sources Guide](../../guide/fire-sources.md) - Fire surface setup
- [MaterialBuilder](material.md) - Material definitions
