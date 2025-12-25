# Builder Libraries

::: pyfds.builders.libraries

## Overview

Builder libraries provide pre-configured objects for common scenarios. Instead of manually configuring every parameter, you can use factory functions to create standard materials, props, ramps, and more with sensible defaults.

## Importing Libraries

```python
from pyfds.builders.libraries import (
    CommonMaterials,
    CommonProps,
    CommonHoles,
    CommonRamps,
    CommonSpecies,
    CommonFuels,
)
```

## Common Materials

Pre-configured material definitions.

::: pyfds.builders.libraries.materials

### Usage Examples

```python
from pyfds.builders.libraries import CommonMaterials
from pyfds import Simulation

sim = Simulation(chid="test")

# Add standard materials
sim.add(CommonMaterials.concrete())
sim.add(CommonMaterials.steel())
sim.add(CommonMaterials.gypsum())
sim.add(CommonMaterials.wood())

# Use in surfaces
sim.add(Surface(
    id="CONCRETE_WALL",
    matl_id="CONCRETE",
    thickness=0.2
))
```

### Available Materials

- **`concrete()`**: Standard concrete
  - Density: 2400 kg/m³
  - Conductivity: 1.8 W/(m·K)
  - Specific heat: 0.88 kJ/(kg·K)

- **`steel()`**: Structural steel
  - Density: 7850 kg/m³
  - Conductivity: 45 W/(m·K)
  - Specific heat: 0.46 kJ/(kg·K)

- **`gypsum()`**: Gypsum board
  - Density: 790 kg/m³
  - Conductivity: 0.48 W/(m·K)
  - Specific heat: 1.09 kJ/(kg·K)

- **`wood()`**: Generic wood
  - Density: 500 kg/m³
  - Conductivity: 0.13 W/(m·K)
  - Specific heat: 2.5 kJ/(kg·K)

- **`glass()`**: Standard glass
  - Density: 2500 kg/m³
  - Conductivity: 0.76 W/(m·K)
  - Specific heat: 0.84 kJ/(kg·K)

- **`insulation()`**: Thermal insulation
  - Density: 65 kg/m³
  - Conductivity: 0.04 W/(m·K)
  - Specific heat: 1.0 kJ/(kg·K)

## Common Properties

Pre-configured device properties (sprinklers, detectors, etc.).

::: pyfds.builders.libraries.props

### Usage Examples

```python
from pyfds.builders.libraries import CommonProps
from pyfds import Simulation

sim = Simulation(chid="test")

# Add standard sprinkler
sprinkler = CommonProps.quick_response_sprinkler()
sim.add(sprinkler)

# Add smoke detector
detector = CommonProps.smoke_detector()
sim.add(detector)

# Use in devices
sim.add(Device(
    id="SPK_1",
    prop_id="SPRINKLER_QR",
    xyz=(5, 5, 2.5)
))
```

### Available Properties

- **`quick_response_sprinkler()`**: Quick-response sprinkler
  - RTI: 50 (m·s)^0.5
  - Activation temp: 68°C
  - Spray angle: 60°

- **`standard_sprinkler()`**: Standard sprinkler
  - RTI: 165 (m·s)^0.5
  - Activation temp: 74°C
  - Spray angle: 60°

- **`smoke_detector()`**: Photoelectric smoke detector
  - Activation obscuration: 3.28%/m
  - Beta: 1.8 (m/s)^-1

- **`heat_detector()`**: Fixed-temperature heat detector
  - RTI: 100 (m·s)^0.5
  - Activation temp: 57°C

- **`aspirating_detector()`**: Aspirating smoke detection
  - Activation obscuration: 0.5%/m
  - Sampling rate: 0.05 m³/s

## Common Holes

Pre-configured hole geometries (doors, windows, etc.).

::: pyfds.builders.libraries.holes

### Usage Examples

```python
from pyfds.builders.libraries import CommonHoles
from pyfds import Simulation

sim = Simulation(chid="test")

# Single door at wall location
door = CommonHoles.door(location=(5, 0, 0), wall_normal="y")
sim.add(door)

# Window
window = CommonHoles.window(location=(3, 10, 1.2), wall_normal="y", width=1.2, height=1.0)
sim.add(window)

# Double door
double_door = CommonHoles.double_door(location=(8, 0, 0), wall_normal="y")
sim.add(double_door)
```

### Available Holes

- **`door(location, wall_normal, width=0.9, height=2.1)`**: Standard door
  - Default: 0.9m × 2.1m

- **`double_door(location, wall_normal, width=1.8, height=2.1)`**: Double door
  - Default: 1.8m × 2.1m

- **`window(location, wall_normal, width, height)`**: Window opening
  - Custom dimensions

- **`vent_opening(location, wall_normal, width, height)`**: Vent opening
  - Custom dimensions

## Common Ramps

Pre-configured time-varying functions.

::: pyfds.builders.libraries.ramps

### Usage Examples

```python
from pyfds.builders.libraries import CommonRamps
from pyfds import Simulation

sim = Simulation(chid="test")

# Fast t² fire growth to 2500 kW at 300s
fire_ramp = CommonRamps.t_squared_fast(peak_hrr=2500, t_peak=300)
sim.add(fire_ramp)

# HVAC schedule: on 8AM-6PM
hvac_ramp = CommonRamps.hvac_schedule_24h(on_time=8, off_time=18)
sim.add(hvac_ramp)

# Linear ramp
linear_ramp = CommonRamps.linear(t_start=0, t_end=100, f_start=0, f_end=1)
sim.add(linear_ramp)
```

### Available Ramps

- **`t_squared_fast(peak_hrr, t_peak)`**: Fast t² fire growth
  - Growth rate: α = 0.047 kW/s²

- **`t_squared_medium(peak_hrr, t_peak)`**: Medium t² fire growth
  - Growth rate: α = 0.012 kW/s²

- **`t_squared_slow(peak_hrr, t_peak)`**: Slow t² fire growth
  - Growth rate: α = 0.003 kW/s²

- **`t_squared_ultra_fast(peak_hrr, t_peak)`**: Ultra-fast t² fire growth
  - Growth rate: α = 0.188 kW/s²

- **`linear(t_start, t_end, f_start, f_end)`**: Linear ramp
  - Straight line between two points

- **`step(t_step, f_before, f_after)`**: Step function
  - Instantaneous change at time t

- **`hvac_schedule_24h(on_time, off_time)`**: 24-hour HVAC schedule
  - On/off times in hours (0-24)

- **`exponential(tau, t_end)`**: Exponential growth/decay
  - Time constant: τ

## Common Species

Pre-configured chemical species.

::: pyfds.builders.libraries.species

### Usage Examples

```python
from pyfds.builders.libraries import CommonSpecies
from pyfds import Simulation

sim = Simulation(chid="test")

# Add tracer species
tracer = CommonSpecies.tracer(id="SF6")
sim.add(tracer)

# Add water vapor tracking
water = CommonSpecies.water_vapor()
sim.add(water)
```

### Available Species

- **`tracer(id="TRACER")`**: Massless tracer
  - Zero diffusivity
  - No reactions

- **`water_vapor()`**: Water vapor species
  - Molecular weight: 18 g/mol

- **`carbon_dioxide()`**: CO₂ species
  - Molecular weight: 44 g/mol

- **`carbon_monoxide()`**: CO species
  - Molecular weight: 28 g/mol

## Common Fuels

Pre-configured fuel definitions for reactions.

::: pyfds.builders.libraries.fuels

### Usage Examples

```python
from pyfds.builders.libraries import CommonFuels

# Get fuel properties
propane = CommonFuels.get_fuel("PROPANE")
print(f"Heat of combustion: {propane['heat_of_combustion']} kJ/kg")
print(f"Soot yield: {propane['soot_yield']}")

# List available fuels
all_fuels = CommonFuels.list_fuels()
print(f"Available fuels: {all_fuels}")
```

### Available Fuels

The library includes properties for common fuels:

- **Hydrocarbons**: METHANE, PROPANE, ETHANE, BUTANE, HEPTANE, OCTANE
- **Polymers**: POLYSTYRENE, POLYETHYLENE, POLYPROPYLENE, PMMA
- **Natural materials**: WOOD, CELLULOSE
- **Liquids**: METHANOL, ETHANOL, ACETONE, GASOLINE, DIESEL

Each fuel includes:
- Chemical formula (C, H, O, N atoms)
- Heat of combustion (kJ/kg)
- Soot yield
- CO yield
- Radiative fraction

## Customization

All library functions return standard PyFDS objects that can be modified:

```python
from pyfds.builders.libraries import CommonMaterials

# Get base material
concrete = CommonMaterials.concrete()

# Modify as needed
concrete.emissivity = 0.95
concrete.absorption = 50000  # J/m³

sim.add(concrete)
```

## Creating Custom Libraries

You can create your own library following the same pattern:

```python
from pyfds import Material

class MyMaterials:
    """Custom material library."""

    @staticmethod
    def my_custom_material() -> Material:
        """Create custom material."""
        return Material(
            id="MY_MATERIAL",
            density=1500,
            conductivity=1.2,
            specific_heat=1.8,
            emissivity=0.9
        )

# Use it
from my_library import MyMaterials
sim.add(MyMaterials.my_custom_material())
```

## See Also

- [Material Builder](material.md) - Building custom materials
- [Ramp Builder](ramp.md) - Building custom ramps
- [Builders Index](index.md) - All builder documentation
- [Materials Guide](../../guide/materials-surfaces.md) - Material modeling guide
