# Builder Pattern API

PyFDS provides a fluent builder pattern API for creating FDS simulations with improved readability and discoverability. Builders offer an alternative to direct namelist construction with chainable methods, smart defaults, and predefined configurations.

## Why Use Builders?

### Before (Direct Construction)

```python
material = Material(
    id='GYPSUM',
    density=930,
    conductivity=None,
    conductivity_ramp='GYPSUM_K',
    specific_heat=None,
    specific_heat_ramp='GYPSUM_CP',
    emissivity=0.9,
    absorption_coefficient=50000.0,
    n_reactions=2,
    a=[1e10, 5e8],
    e=[80000, 120000],
    heat_of_reaction=[1000, 1500],
    nu_spec=['FUEL_VAPOR', ''],
    nu_matl=['', 'CHAR']
)
```

### After (Builder Pattern)

```python
material = (
    MaterialBuilder('GYPSUM')
    .density(930)
    .thermal_conductivity_ramp('GYPSUM_K')
    .specific_heat_ramp('GYPSUM_CP')
    .add_pyrolysis_reaction(
        a=1e10, e=80000, heat_of_reaction=1000,
        product_species='FUEL_VAPOR'
    )
    .add_pyrolysis_reaction(
        a=5e8, e=120000, heat_of_reaction=1500,
        residue_material='CHAR'
    )
    .build()
)
```

### Key Advantages

- **Readable**: Clear parameter names and intent
- **Discoverable**: IDE autocomplete guides you through available methods
- **Flexible**: Set parameters in any order
- **Validated**: Each step can perform validation
- **Smart Defaults**: Sensible defaults reduce boilerplate
- **Helpers**: Predefined configurations for common scenarios

## Available Builders

PyFDS provides six builder classes:

| Builder | Purpose | Usage Pattern |
|---------|---------|---------------|
| [RampBuilder](#rampbuilder) | Time-dependent functions | Fluent API |
| [MaterialBuilder](#materialbuilder) | Thermal and pyrolysis properties | Fluent API |
| [ReactionBuilder](#reactionbuilder) | Combustion reactions | Fluent API |
| [ControlBuilder](#controlbuilder) | Control logic | Fluent API |
| [PropBuilder](#propbuilder) | Device properties | Factory methods |
| [VentBuilder](#ventbuilder) | Boundary conditions | Factory methods |

## RampBuilder

Create time-dependent functions for fire growth, temperature-dependent properties, and control schedules.

### Fire Growth Patterns

```python
from pyfds.builders import RampBuilder

# T-squared fire growth
fire_ramp = (
    RampBuilder('HRR_GROWTH')
    .t_squared('FAST', peak_hrr=2500, t_peak=300)
    .build()
)
sim.add_ramp(fire_ramp)
```

Available growth rates: `'SLOW'`, `'MEDIUM'`, `'FAST'`, `'ULTRAFAST'`

### Temperature Tables

For temperature-dependent material properties:

```python
# Temperature-dependent thermal conductivity
k_ramp = (
    RampBuilder('STEEL_K')
    .temperature_table({
        20: 45.8,   # 20°C: 45.8 W/(m·K)
        100: 43.3,
        200: 40.7,
        400: 36.4,
        600: 31.0
    })
    .build()
)
sim.add_ramp(k_ramp)

# Use in material
steel = (
    MaterialBuilder('STEEL')
    .density(7850)
    .thermal_conductivity_ramp('STEEL_K')
    .specific_heat(0.46)
    .build()
)
```

### Other Patterns

```python
# Linear ramp
linear = RampBuilder('LINEAR').linear(t_start=0, t_end=100, f_start=0, f_end=1).build()

# Step function
step = RampBuilder('STEP').step(t_step=60, f_before=0, f_after=1).build()

# Exponential growth
exp = RampBuilder('EXP').exponential(t_start=0, t_end=100, f_start=1, f_end=100, rate=0.05).build()

# Sine wave
sine = RampBuilder('SINE').sine_wave(period=60, amplitude=1.0, offset=0.5, phase=0).build()

# Custom points
custom = RampBuilder('CUSTOM').add_point(0, 0).add_point(10, 1).add_point(20, 0.5).build()
```

## MaterialBuilder

Build materials with thermal properties and pyrolysis reactions.

### Simple Materials

```python
from pyfds.builders import MaterialBuilder

# Constant thermal properties
wood = (
    MaterialBuilder('PINE')
    .density(500)
    .thermal_conductivity(0.13)
    .specific_heat(2.5)
    .emissivity(0.9)
    .build()
)
sim.add_material(wood)
```

### Temperature-Dependent Properties

```python
# Using RAMP for temperature-dependent conductivity
steel = (
    MaterialBuilder('STEEL')
    .density(7850)
    .thermal_conductivity_ramp('STEEL_K')  # Reference a RAMP
    .specific_heat(0.46)
    .emissivity(0.7)
    .build()
)
```

### Pyrolysis Materials

```python
# Single-reaction pyrolysis
foam = (
    MaterialBuilder('FOAM')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(
        a=1e10,                      # Pre-exponential factor
        e=80000,                     # Activation energy (J/mol)
        heat_of_reaction=1000,       # Heat of reaction (kJ/kg)
        product_species='FUEL_VAPOR' # Product species
    )
    .build()
)

# Multi-reaction pyrolysis
polymer = (
    MaterialBuilder('POLYURETHANE')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(
        a=1e10, e=80000, heat_of_reaction=1000,
        product_species='FUEL_VAPOR'
    )
    .add_pyrolysis_reaction(
        a=5e8, e=120000, heat_of_reaction=1500,
        residue_material='CHAR'
    )
    .build()
)
```

### Predefined Materials

```python
# Use predefined common materials
concrete = MaterialBuilder.concrete()
gypsum = MaterialBuilder.gypsum()
steel = MaterialBuilder.steel()
aluminum = MaterialBuilder.aluminum()
brick = MaterialBuilder.brick()
wood = MaterialBuilder.wood()

sim.add_material(concrete)
sim.add_material(gypsum)
```

Available predefined materials:
- `concrete()` - Concrete (ρ=2400 kg/m³, k=1.6 W/(m·K))
- `gypsum()` - Gypsum board (ρ=930 kg/m³, k=0.48 W/(m·K))
- `steel()` - Steel (ρ=7850 kg/m³, k=45.8 W/(m·K))
- `aluminum()` - Aluminum (ρ=2700 kg/m³, k=237 W/(m·K))
- `brick()` - Red brick (ρ=1920 kg/m³, k=0.69 W/(m·K))
- `wood()` - Generic wood (ρ=500 kg/m³, k=0.13 W/(m·K))

## ReactionBuilder

Define combustion reactions using predefined fuels or custom compositions.

### Using Predefined Fuels

```python
from pyfds.builders import ReactionBuilder

# Use predefined fuel
reac = (
    ReactionBuilder()
    .fuel('PROPANE')
    .yields(soot=0.015, co=0.02)
    .radiative_fraction(0.33)
    .build()
)
sim.add_reaction(reac)
```

### Fuel Database

The builder includes 17 predefined fuels:

**Gases**: METHANE, ETHANE, PROPANE, BUTANE, HYDROGEN

**Liquids**: N-HEPTANE, N-HEXANE, GASOLINE, ACETONE, METHANOL, ETHANOL

**Solids**: POLYURETHANE, WOOD, PMMA, POLYSTYRENE, POLYETHYLENE, POLYPROPYLENE

```python
# List available fuels
fuels = ReactionBuilder.list_fuels()
print(fuels)  # ['ACETONE', 'BUTANE', 'ETHANE', ...]

# Get fuel information
info = ReactionBuilder.get_fuel_info('PROPANE')
print(info)
# {
#   'c': 3, 'h': 8, 'o': 0, 'n': 0,
#   'hoc': 46000,  # kJ/kg
#   'soot_yield': 0.01,
#   'co_yield': 0.004,
#   'description': 'Propane gas'
# }
```

### Custom Fuel Composition

```python
# Define custom fuel
reac = (
    ReactionBuilder()
    .custom_fuel(
        c=7,                        # Carbon atoms
        h=16,                       # Hydrogen atoms
        o=0,                        # Oxygen atoms (optional)
        n=0,                        # Nitrogen atoms (optional)
        heat_of_combustion=44600    # kJ/kg
    )
    .yields(soot=0.01, co=0.02)
    .radiative_fraction(0.30)
    .auto_ignition_temperature(350)
    .build()
)
```

### Configuring Product Yields

```python
reac = (
    ReactionBuilder()
    .fuel('POLYURETHANE')
    .soot_yield(0.10)          # Soot yield (kg soot/kg fuel)
    .co_yield(0.03)            # CO yield (kg CO/kg fuel)
    .radiative_fraction(0.30)  # Radiative fraction (0-1)
    .build()
)
```

## ControlBuilder

Create control logic for device interactions and system automation.

### Logic Gates

```python
from pyfds.builders import ControlBuilder

# ANY logic (activates if ANY detector triggers)
alarm = (
    ControlBuilder('SMOKE_ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2', 'SMOKE_DET_3'])
    .build()
)
sim.add_ctrl(alarm)

# ALL logic (activates if ALL detectors trigger)
multi_zone = (
    ControlBuilder('MULTI_ZONE')
    .all(['ZONE_1_DET', 'ZONE_2_DET'])
    .build()
)

# ONLY logic (passthrough single device)
passthrough = (
    ControlBuilder('PASSTHROUGH')
    .only('HEAT_DET_1')
    .build()
)
```

### Time Delays

```python
# Delayed activation
delayed_sprinkler = (
    ControlBuilder('DELAYED_SPRINKLER')
    .time_delay('HEAT_DETECTOR', delay=10.0)  # 10 second delay
    .build()
)
```

### Modifiers

```python
# Complex control with modifiers
ctrl = (
    ControlBuilder('COMPLEX_CTRL')
    .any(['SD_1', 'SD_2'])
    .with_delay(3.0)              # 3 second delay
    .with_initial_state(False)    # Start deactivated
    .with_latch(True)             # Stay on once activated
    .build()
)
```

### Special Functions

```python
# KILL function (stops simulation)
kill_ctrl = (
    ControlBuilder('KILL')
    .kill(on_t=600)  # Kill at t=600s
    .build()
)

# RESTART function
restart_ctrl = (
    ControlBuilder('RESTART')
    .restart(on_t=300)  # Restart at t=300s
    .build()
)

# CUSTOM function
custom_ctrl = (
    ControlBuilder('CUSTOM')
    .custom(ramp_id='MY_RAMP')
    .build()
)
```

## PropBuilder

Create device properties using factory methods.

### Sprinklers

```python
from pyfds.builders import PropBuilder

# Custom sprinkler
sprinkler = PropBuilder.sprinkler(
    id='CUSTOM_SPRINKLER',
    activation_temp=68,     # °C
    rti=50,                 # Response time index (m·s)^0.5
    flow_rate=60,           # L/min
    k_factor=80             # K-factor (L/min/bar^0.5)
)
sim.add_prop(sprinkler)

# Predefined sprinklers
qr = PropBuilder.quick_response_sprinkler(id='QR_SPRINKLER')
sr = PropBuilder.standard_response_sprinkler(id='SR_SPRINKLER')
```

### Smoke Detectors

```python
# Photoelectric smoke detector
smoke_det = PropBuilder.smoke_detector(
    id='PHOTOELECTRIC',
    activation_obscuration=3.28  # %/ft (UL 268 standard)
)
```

### Heat Detectors

```python
# Heat detector
heat_det = PropBuilder.heat_detector(
    id='HEAT_DET',
    activation_temp=74,  # °C
    rti=5.0              # (m·s)^0.5
)
```

## VentBuilder

Create boundary conditions and openings using factory methods.

### Openings

```python
from pyfds.builders import VentBuilder

# Generic opening to ambient
opening = VentBuilder.opening(
    xb=Bounds3D.of(5, 5, 2, 4, 0, 2.1),
    id='DOOR'
)
sim.add_vent(opening)

# Convenience methods for common openings
door = VentBuilder.door(
    x=5.0,
    y_min=2.0,
    y_max=3.0,
    z_min=0.0,    # Optional, default 0.0
    z_max=2.1,    # Optional, default 2.1
    id='DOOR_1'
)

window = VentBuilder.window(
    x=0.0,
    y_min=1.0,
    y_max=2.0,
    z_min=1.0,
    z_max=1.5,
    id='WINDOW_1'
)
```

### HVAC

```python
# Supply vent (positive flow)
supply = VentBuilder.hvac_supply(
    xb=Bounds3D.of(5, 6, 5, 6, 3, 3),
    volume_flow=0.5,  # m³/s
    id='SUPPLY_1'
)

# Exhaust vent (negative flow)
exhaust = VentBuilder.hvac_exhaust(
    xb=Bounds3D.of(0, 1, 0, 1, 3, 3),
    volume_flow=0.3,  # m³/s (automatically negated)
    id='EXHAUST_1'
)
```

### Circular Vents

```python
# Circular burner
burner = VentBuilder.circular_burner(
    center=(0, 0, 0),
    radius=0.5,
    surf_id='FIRE',
    id='BURNER'
)

# Annular (ring) burner
ring = VentBuilder.annular_burner(
    center=(0, 0, 0),
    radius=0.5,
    radius_inner=0.3,
    surf_id='FIRE',
    id='RING_BURNER'
)
```

### Mesh Boundaries

```python
# Open mesh boundary
open_boundary = VentBuilder.mesh_boundary(
    mb='XMIN',
    surf_id='OPEN'
)

# Periodic boundary
periodic = VentBuilder.mesh_boundary(
    mb='XMAX',
    surf_id='PERIODIC',
    mesh_id='MESH_1'  # Optional
)
```

## Predefined Libraries

PyFDS includes predefined libraries for common configurations.

### Common Materials

```python
from pyfds.builders.libraries import CommonMaterials

# Get predefined materials
concrete = CommonMaterials.concrete()
steel = CommonMaterials.steel()
gypsum = CommonMaterials.gypsum()
aluminum = CommonMaterials.aluminum()
brick = CommonMaterials.brick()
wood = CommonMaterials.wood()
fiberglass = CommonMaterials.fiberglass_insulation()
ceramic = CommonMaterials.ceramic()
glass = CommonMaterials.glass()
copper = CommonMaterials.copper()

# Add to simulation
sim.add_material(concrete)
sim.add_material(steel)
```

### Common Ramps

```python
from pyfds.builders.libraries import CommonRamps

# T-squared fire growth
slow_fire = CommonRamps.t_squared_slow(peak_hrr=1000)
medium_fire = CommonRamps.t_squared_medium(peak_hrr=2000)
fast_fire = CommonRamps.t_squared_fast(peak_hrr=2500)
ultra_fire = CommonRamps.t_squared_ultrafast(peak_hrr=5000)

# Step function
step = CommonRamps.step_at(t=60, value=1.0)

# Linear ramp
linear = CommonRamps.linear_ramp(t_start=0, t_end=100, f_start=0, f_end=1)

# HVAC schedule (24-hour)
hvac = CommonRamps.hvac_schedule_24h(on_time=8, off_time=18)
```

## Complete Example

Here's a complete apartment fire simulation using builders:

```python
from pyfds import Simulation
from pyfds.builders import (
    RampBuilder, MaterialBuilder, ReactionBuilder,
    ControlBuilder, PropBuilder, VentBuilder
)

# Create simulation
sim = Simulation('apartment_fire', title='Apartment Fire Demo')
sim.add(Time(t_end=600)
sim.add(Mesh(ijk=Grid3D.of(100, 100, 50), xb=Bounds3D.of(0, 10, 0, 10, 0, 5))

# Add combustion reaction
reac = (
    ReactionBuilder()
    .fuel('POLYURETHANE')
    .yields(soot=0.10, co=0.02)
    .radiative_fraction(0.30)
    .build()
)
sim.add_reaction(reac)

# Add fire growth ramp
fire_ramp = (
    RampBuilder('HRR_GROWTH')
    .t_squared('FAST', peak_hrr=2500, t_peak=300)
    .build()
)
sim.add_ramp(fire_ramp)

# Add temperature-dependent steel
k_ramp = (
    RampBuilder('STEEL_K')
    .temperature_table({20: 45.8, 100: 43.3, 400: 36.4})
    .build()
)
sim.add_ramp(k_ramp)

steel = (
    MaterialBuilder('STEEL')
    .density(7850)
    .thermal_conductivity_ramp('STEEL_K')
    .specific_heat(0.46)
    .emissivity(0.7)
    .build()
)
sim.add_material(steel)

# Add vents
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id='DOOR')
supply = VentBuilder.hvac_supply(xb=Bounds3D.of(5, 6, 5, 6, 3, 3), volume_flow=0.5, id='SUPPLY')

sim.add_vent(door)
sim.add_vent(supply)

# Add devices
sprinkler = PropBuilder.quick_response_sprinkler(id='SPRINKLER')
smoke_det = PropBuilder.smoke_detector(id='SMOKE_DET')

sim.add_prop(sprinkler)
sim.add_prop(smoke_det)

# Add control
alarm = (
    ControlBuilder('ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2'])
    .with_latch(True)
    .build()
)
sim.add_ctrl(alarm)

# Add surfaces
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
sim.surface(id='WALL', matl_id='STEEL', thickness=0.012)

# Add geometry
sim.add(Obstruction(xb=Bounds3D.of(4.5, 5.5, 4.5, 5.5, 0, 0.1), surf_id='FIRE')

# Write FDS file
sim.write('apartment_fire.fds')
```

## Best Practices

### 1. Use Builders for Readability

Builders make code more readable, especially for complex configurations:

```python
# Good: Clear and readable
material = (
    MaterialBuilder('FOAM')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(a=1e10, e=80000, product_species='FUEL')
    .build()
)

# Avoid: All on one line
material = MaterialBuilder('FOAM').density(40).thermal_conductivity(0.04).specific_heat(1.5).build()
```

### 2. Leverage Predefined Configurations

Use predefined materials and fuels when available:

```python
# Good: Use predefined
concrete = MaterialBuilder.concrete()
reac = ReactionBuilder().fuel('PROPANE').build()

# Avoid: Manually specifying common materials
concrete = MaterialBuilder('CONCRETE').density(2400).thermal_conductivity(1.6)...
```

### 3. Validate Early

Build objects as soon as you're done configuring them to catch errors early:

```python
# Good: Build immediately
steel = MaterialBuilder('STEEL').density(7850).thermal_conductivity(45.8).build()
sim.add_material(steel)

# Avoid: Long chains of operations before building
builder = MaterialBuilder('STEEL')
# ... many lines later ...
steel = builder.build()  # Error discovered late
```

### 4. Use Type Hints

Enable IDE autocomplete by using type hints:

```python
from pyfds.builders import MaterialBuilder
from pyfds.core.namelists import Material

material: Material = (
    MaterialBuilder('TEST')
    .density(1000)
    .thermal_conductivity(1.0)
    .build()
)
```

## Migration from Direct Construction

If you have existing code using direct namelist construction, you can migrate gradually:

### Step 1: Identify Complex Constructions

Look for namelists with many parameters or complex configurations:

```python
# Complex material with pyrolysis
material = Material(
    id='FOAM',
    density=40,
    conductivity=0.04,
    specific_heat=1.5,
    n_reactions=2,
    a=[1e10, 5e8],
    e=[80000, 120000],
    # ... many more parameters
)
```

### Step 2: Convert to Builder

Replace with builder pattern:

```python
material = (
    MaterialBuilder('FOAM')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(a=1e10, e=80000, ...)
    .add_pyrolysis_reaction(a=5e8, e=120000, ...)
    .build()
)
```

### Step 3: Use Predefined Where Possible

Replace common materials/fuels with predefined versions:

```python
# Before
concrete = Material(id='CONCRETE', density=2400, conductivity=1.6, specific_heat=0.88)

# After
concrete = MaterialBuilder.concrete()
```

## See Also

- [API Reference - Builders](../api/builders/index.md) - Detailed API documentation
