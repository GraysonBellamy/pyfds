# Builders API Reference

The builder pattern API provides a fluent, chainable interface for constructing FDS simulation components.

## Overview

PyFDS builders offer an alternative to direct namelist construction with improved readability, discoverability, and validation. All builders follow a consistent pattern:

1. **Create** a builder instance
2. **Configure** using chainable methods
3. **Build** the final object with `.build()`

## Available Builders

| Builder | Purpose | Documentation |
|---------|---------|---------------|
| [RampBuilder](ramp.md) | Time-dependent functions | Patterns, fire growth, temperature tables |
| [MaterialBuilder](material.md) | Thermal and pyrolysis properties | Simple materials, temperature-dependent, pyrolysis |
| [ReactionBuilder](reaction.md) | Combustion reactions | Predefined fuels, custom composition |
| [ControlBuilder](control.md) | Control logic | Logic gates, time delays, special functions |
| [PropBuilder](prop.md) | Device properties | Sprinklers, detectors (factory methods) |
| [VentBuilder](vent.md) | Boundary conditions | Openings, HVAC, circular vents (factory methods) |

## Builder Pattern

All fluent builders inherit from a common base class that provides:

- **Type safety**: Full generic typing for build products
- **State tracking**: Prevents builder reuse after `.build()`
- **Validation**: Parameter validation before building
- **Method chaining**: All methods return `self`

### Example Usage

```python
from pyfds.builders import MaterialBuilder

# Create builder
builder = MaterialBuilder('WOOD')

# Configure using chainable methods
builder.density(500)
builder.thermal_conductivity(0.13)
builder.specific_heat(2.5)

# Build the final object
material = builder.build()

# Or chain everything (preferred)
material = (
    MaterialBuilder('WOOD')
    .density(500)
    .thermal_conductivity(0.13)
    .specific_heat(2.5)
    .build()
)
```

## Factory Methods Pattern

Some builders (`PropBuilder`, `VentBuilder`) use factory methods instead of fluent chaining:

```python
from pyfds.builders import PropBuilder

# Factory method (not fluent)
sprinkler = PropBuilder.quick_response_sprinkler(id='QR_SPRINKLER')

# Factory method with parameters
custom = PropBuilder.sprinkler(
    id='CUSTOM',
    activation_temp=68,
    rti=50
)
```

## Predefined Libraries

Pre-configured objects for common scenarios:

```python
from pyfds.builders.libraries import CommonMaterials, CommonRamps

# Predefined materials
concrete = CommonMaterials.concrete()
steel = CommonMaterials.steel()

# Predefined ramps
fast_fire = CommonRamps.t_squared_fast(peak_hrr=2500)
hvac = CommonRamps.hvac_schedule_24h(on_time=8, off_time=18)
```

## Quick Reference

### RampBuilder

```python
from pyfds.builders import RampBuilder

# Fire growth
ramp = RampBuilder('FIRE').t_squared('FAST', peak_hrr=2500, t_peak=300).build()

# Temperature table
ramp = RampBuilder('K').temperature_table({20: 45.8, 100: 43.3}).build()

# Linear
ramp = RampBuilder('LINEAR').linear(t_start=0, t_end=100, f_start=0, f_end=1).build()
```

[Full RampBuilder Documentation →](ramp.md)

### MaterialBuilder

```python
from pyfds.builders import MaterialBuilder

# Simple material
mat = MaterialBuilder('WOOD').density(500).thermal_conductivity(0.13).specific_heat(2.5).build()

# With pyrolysis
mat = (
    MaterialBuilder('FOAM')
    .density(40)
    .add_pyrolysis_reaction(a=1e10, e=80000, product_species='FUEL')
    .build()
)

# Predefined
concrete = MaterialBuilder.concrete()
```

[Full MaterialBuilder Documentation →](material.md)

### ReactionBuilder

```python
from pyfds.builders import ReactionBuilder

# Predefined fuel
reac = ReactionBuilder().fuel('PROPANE').yields(soot=0.015, co=0.02).build()

# Custom fuel
reac = ReactionBuilder().custom_fuel(c=7, h=16, heat_of_combustion=44600).build()

# List fuels
fuels = ReactionBuilder.list_fuels()
```

[Full ReactionBuilder Documentation →](reaction.md)

### ControlBuilder

```python
from pyfds.builders import ControlBuilder

# ANY logic
ctrl = ControlBuilder('ALARM').any(['DET_1', 'DET_2', 'DET_3']).build()

# Time delay
ctrl = ControlBuilder('DELAYED').time_delay('HEAT_DET', delay=10.0).build()

# With modifiers
ctrl = ControlBuilder('CTRL').any(['DET_1', 'DET_2']).with_latch(True).build()
```

[Full ControlBuilder Documentation →](control.md)

### PropBuilder

```python
from pyfds.builders import PropBuilder

# Predefined sprinklers
qr = PropBuilder.quick_response_sprinkler(id='QR')
sr = PropBuilder.standard_response_sprinkler(id='SR')

# Custom sprinkler
custom = PropBuilder.sprinkler(id='CUSTOM', activation_temp=68, rti=50)

# Smoke detector
smoke = PropBuilder.smoke_detector(id='SMOKE', activation_obscuration=3.28)
```

[Full PropBuilder Documentation →](prop.md)

### VentBuilder

```python
from pyfds.builders import VentBuilder

# Opening
opening = VentBuilder.opening(xb=(5, 5, 2, 4, 0, 2.1), id='DOOR')

# Convenience methods
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id='DOOR')
window = VentBuilder.window(x=0.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5)

# HVAC
supply = VentBuilder.hvac_supply(xb=(5, 6, 5, 6, 3, 3), volume_flow=0.5)

# Circular
burner = VentBuilder.circular_burner(center=(0, 0, 0), radius=0.5, surf_id='FIRE')
```

[Full VentBuilder Documentation →](vent.md)

## Design Principles

### 1. Fluent Interface

Methods return `self` for chaining:

```python
material = (
    MaterialBuilder('WOOD')
    .density(500)          # Returns self
    .thermal_conductivity(0.13)  # Returns self
    .specific_heat(2.5)    # Returns self
    .build()               # Returns Material
)
```

### 2. Smart Defaults

Sensible defaults reduce boilerplate:

```python
# Emissivity defaults to 0.9
material = MaterialBuilder('WOOD').density(500).thermal_conductivity(0.13).build()
assert material.emissivity == 0.9
```

### 3. Progressive Disclosure

Simple cases are simple, complex cases are possible:

```python
# Simple
wood = MaterialBuilder('WOOD').density(500).thermal_conductivity(0.13).build()

# Complex
polymer = (
    MaterialBuilder('POLYMER')
    .density(40)
    .thermal_conductivity(0.04)
    .add_pyrolysis_reaction(a=1e10, e=80000, product_species='FUEL')
    .add_pyrolysis_reaction(a=5e8, e=120000, residue_material='CHAR')
    .build()
)
```

### 4. Type Safety

Full type hints enable IDE support:

```python
from pyfds.builders import MaterialBuilder
from pyfds.core.namelists import Material

builder: MaterialBuilder = MaterialBuilder('WOOD')
material: Material = builder.density(500).build()
```

### 5. Fail Fast

Validation happens at build time:

```python
# Raises ValueError immediately
material = MaterialBuilder('TEST').thermal_conductivity(1.0).build()
# ValueError: density is required
```

## See Also

- [User Guide - Builders](../../guide/builders.md) - Usage guide and examples
