# Namelist Reference

Complete reference for all FDS namelists supported by PyFDS.

## Overview

This reference provides comprehensive parameter listings for all FDS namelists. For usage examples and explanations, see the [User Guide](../guide/index.md).

!!! info "PyFDS Method Names"
    PyFDS uses lowercase method names (e.g., `sim.mesh()`) to create namelists. The actual FDS namelist names (e.g., `&MESH`) are used in the generated `.fds` files.

## Quick Index

| Namelist | Purpose | PyFDS Method | Guide |
|----------|---------|--------------|-------|
| [HEAD](#head) | Simulation title | `head()` | [Metadata](../guide/building-simulations.md) |
| [TIME](#time) | Time control | `time()` | [Metadata](../guide/building-simulations.md) |
| [MISC](#misc) | Global settings | `set_misc()` | [Global Settings](../guide/global-settings.md) |
| [MESH](#mesh) | Computational domain | `mesh()` | [Domain](../guide/domain.md) |
| [OBST](#obst) | Obstructions | `obstruction()` | [Geometry](../guide/geometry.md) |
| [VENT](#vent) | Vents/boundaries | `vent()` | [Boundaries](../guide/boundaries.md) |
| [SURF](#surf) | Surface properties | `surface()` | [Materials](../guide/materials-surfaces.md) |
| [MATL](#matl) | Material properties | `material()` | [Materials](../guide/materials-surfaces.md) |
| [DEVC](#devc) | Devices | `device()` | [Devices](../guide/devices.md) |
| [PROP](#prop) | Device properties | `prop()` | [Devices](../guide/devices.md) |
| [RAMP](#ramp) | Time ramps | `ramp()` | [Ramps](../guide/ramps.md) |
| [CTRL](#ctrl) | Control logic | `control()` | [Controls](../guide/controls.md) |
| [INIT](#init) | Initial conditions | `init()` | [Initial Conditions](../guide/initial-conditions.md) |
| [REAC](#reac) | Combustion | `reaction()` | [Combustion](../guide/combustion.md) |

## Metadata Namelists

### HEAD

Simulation title and description.

**PyFDS Method**: `sim.head()`

**Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `chid` | str | Yes | Case identifier (set at Simulation creation) |
| `title` | str | No | Descriptive title |

**Example**

```python
sim = Simulation(chid='room_fire_01')
sim.head(title='Single room fire with sprinkler')
```

**FDS Output**

```fortran
&HEAD CHID='room_fire_01', TITLE='Single room fire with sprinkler' /
```

---

### TIME

Time control parameters.

**PyFDS Method**: `sim.time()`

**Parameters**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `t_end` | float | Yes | - | End time (s) |
| `t_begin` | float | No | 0.0 | Start time (s) |
| `dt` | float | No | Auto | Initial time step (s) |
| `wall_clock_limit` | int | No | - | Max wall clock time (s) |

**Example**

```python
sim.time(t_end=600.0, dt=0.1)
```

**FDS Output**

```fortran
&TIME T_END=600.0, DT=0.1 /
```

---

### MISC

Global simulation settings.

**PyFDS Method**: `sim.set_misc()`

**Common Parameters**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `tmpa` | float | 20.0 | Ambient temperature (°C) |
| `humidity` | float | 40.0 | Relative humidity (%) |
| `p_inf` | float | 101325 | Ambient pressure (Pa) |
| `noise` | bool | True | Enable velocity perturbations |
| `turbulence_model` | str | 'DEARDORFF' | LES turbulence model |
| `radiation` | bool | True | Enable radiation |
| `stratification` | bool | True | Enable gravity |

**Wind Parameters**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `wind_speed` | float | 0.0 | Wind speed (m/s) |
| `wind_direction` | float | 0.0 | Wind direction (degrees) |

**Output Control**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `flush_file_buffers` | bool | False | Flush output frequently |
| `verbose` | bool | False | Extra output messages |

**Example**

```python
sim.set_misc(
    tmpa=25.0,
    humidity=50.0,
    radiation=True,
    turbulence_model='DEARDORFF'
)
```

---

## Domain Namelists

### MESH

Computational mesh definition.

**PyFDS Method**: `sim.mesh()`

**Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `ijk` | tuple[int, int, int] | Yes | Cell counts (I, J, K) |
| `xb` | tuple[float, ...] | Yes | Bounds (x0, x1, y0, y1, z0, z1) |
| `id` | str | No | Mesh identifier |
| `rgb` | tuple[int, int, int] | No | Color for visualization |

**Example**

```python
sim.mesh(
    id='MESH1',
    ijk=(50, 50, 25),
    xb=(0, 5, 0, 5, 0, 2.5)
)
```

**FDS Output**

```fortran
&MESH ID='MESH1', IJK=50,50,25, XB=0.0,5.0,0.0,5.0,0.0,2.5 /
```

---

## Geometry Namelists

### OBST

Solid obstructions.

**PyFDS Method**: `sim.obstruction()`

**Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `xb` | tuple[float, ...] | Yes | Bounds (x0, x1, y0, y1, z0, z1) |
| `surf_id` | str | No | Surface ID |
| `surf_ids` | list[str] | No | Per-face surface IDs [x0, x1, y0, y1, z0, z1] |
| `color` | str | No | Color name |
| `rgb` | tuple[int, int, int] | No | RGB color (0-255) |
| `transparency` | float | No | 0.0-1.0 (0=opaque) |
| `removable` | bool | No | Can be removed by CTRL |
| `id` | str | No | Obstruction identifier |

**Example**

```python
# Fire source
sim.obstruction(
    xb=(2, 3, 2, 3, 0, 0.1),
    surf_id='FIRE',
    color='RED'
)

# Wall with different faces
sim.obstruction(
    xb=(0, 0.2, 0, 5, 0, 2.5),
    surf_ids=['BRICK', 'BRICK', 'INERT', 'INERT', 'INERT', 'INERT']
)
```

---

### VENT

Boundary conditions and openings.

**PyFDS Method**: `sim.vent()`

**Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `xb` | tuple[float, ...] | Yes | Bounds (x0, x1, y0, y1, z0, z1) |
| `surf_id` | str | Yes | Surface ID |
| `ior` | int | No | Face orientation (±1, ±2, ±3) |
| `id` | str | No | Vent identifier |
| `color` | str | No | Color name |
| `rgb` | tuple[int, int, int] | No | RGB color |
| `transparency` | float | No | Transparency (0-1) |

**Special SURF_IDs**

| SURF_ID | Purpose |
|---------|---------|
| `'OPEN'` | Open boundary (pressure BC) |
| `'MIRROR'` | Symmetry plane |
| `'INERT'` | Adiabatic wall |

**Example**

```python
# Open door
sim.vent(
    xb=(5, 5, 2, 3, 0, 2.1),
    surf_id='OPEN',
    ior=1
)

# Supply vent
sim.vent(
    xb=(0, 0.3, 0, 0.3, 2.5, 2.5),
    surf_id='SUPPLY',
    ior=3
)
```

---

## Material Namelists

### SURF

Surface boundary conditions.

**PyFDS Method**: `sim.surface()`

**Common Parameters**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `id` | str | Required | Surface identifier |
| `color` | str | - | Color name |
| `rgb` | tuple[int, int, int] | - | RGB color |

**Fire Properties**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `hrrpua` | float | kW/m² | Heat release rate per unit area |
| `mlrpua` | float | kg/s/m² | Mass loss rate per unit area |
| `ramp_q` | str | - | Time ramp ID for HRR |
| `tau_q` | float | s | HRR ramp-up time |

**Thermal Properties**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `tmp_front` | float | °C | Front surface temperature |
| `tmp_back` | float | °C | Back surface temperature |
| `backing` | str | - | 'INSULATED' or 'EXPOSED' |
| `thickness` | float | m | Material thickness |
| `matl_id` | str/list | - | Material ID(s) |

**Velocity Boundary**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `vel` | float | m/s | Normal velocity |
| `volume_flux` | float | m³/s | Volume flow rate |
| `mass_flux` | float | kg/s/m² | Mass flux |

**Example**

```python
# Fire source
sim.surface(
    id='FIRE',
    hrrpua=1000.0,
    ramp_q='FIRE_RAMP',
    color='RED'
)

# Thermally thick material
sim.surface(
    id='CONCRETE_WALL',
    matl_id='CONCRETE',
    thickness=0.2,
    backing='INSULATED'
)

# Supply vent
sim.surface(
    id='SUPPLY',
    vel=-1.5,  # Negative = into domain
    tmp_front=20.0
)
```

---

### MATL

Material thermal properties.

**PyFDS Method**: `sim.material()`

**Parameters**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `id` | str | - | Material identifier |
| `conductivity` | float | W/m/K | Thermal conductivity |
| `specific_heat` | float | kJ/kg/K | Specific heat |
| `density` | float | kg/m³ | Density |
| `emissivity` | float | - | Surface emissivity (0-1) |
| `absorption_coefficient` | float | 1/m | Radiation absorption |

**Pyrolysis (Advanced)**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `heat_of_combustion` | float | kJ/kg | Heat of combustion |
| `heat_of_reaction` | float | kJ/kg | Pyrolysis endotherm |
| `reference_temperature` | float | °C | Reference temp |
| `pyrolysis_range` | tuple | °C | [T_min, T_max] |

**Example**

```python
# Concrete
sim.material(
    id='CONCRETE',
    conductivity=1.0,
    specific_heat=0.88,
    density=2280,
    emissivity=0.9
)

# Gypsum board
sim.material(
    id='GYPSUM',
    conductivity=0.48,
    specific_heat=0.84,
    density=1440,
    emissivity=0.9
)
```

---

## Device Namelists

### DEVC

Measurement devices and outputs.

**PyFDS Method**: `sim.device()`

**Common Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `id` | str | Yes | Device identifier |
| `quantity` | str | Yes | Measured quantity |
| `xyz` | tuple[float, float, float] | Sometimes | Point location |
| `xb` | tuple[float, ...] | Sometimes | Area/volume bounds |

**Point Device Parameters**

| Parameter | Type | Description |
|-----------|------|-------------|
| `xyz` | tuple | Location (x, y, z) |
| `ior` | int | Surface orientation (±1, ±2, ±3) |

**Area/Volume Device Parameters**

| Parameter | Type | Description |
|-----------|------|-------------|
| `xb` | tuple | Bounds (x0, x1, y0, y1, z0, z1) |
| `spatial_statistic` | str | 'MEAN', 'MAX', 'MIN', 'VOLUME INTEGRAL' |

**Output Control**

| Parameter | Type | Description |
|-----------|------|-------------|
| `output_file` | bool | Write to .csv |
| `statistics` | str | 'MEAN', 'RMS', etc. |

**Common Quantities**

| Quantity | Units | Type |
|----------|-------|------|
| `'TEMPERATURE'` | °C | Gas or surface |
| `'VELOCITY'` | m/s | Gas |
| `'PRESSURE'` | Pa | Gas |
| `'DENSITY'` | kg/m³ | Gas |
| `'HEAT FLUX'` | kW/m² | Surface |
| `'GAUGE HEAT FLUX'` | kW/m² | Surface |
| `'RADIATIVE HEAT FLUX'` | kW/m² | Surface |
| `'WALL TEMPERATURE'` | °C | Surface |
| `'VISIBILITY'` | m | Gas |
| `'EXTINCTION COEFFICIENT'` | 1/m | Gas |

**Example**

```python
# Point temperature
sim.device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=(2.5, 2.5, 2.4)
)

# Surface heat flux
sim.device(
    id='HF_WALL',
    quantity='GAUGE HEAT FLUX',
    xyz=(0.1, 2.5, 1.5),
    ior=1
)

# Volume average
sim.device(
    id='TEMP_AVG',
    quantity='TEMPERATURE',
    xb=(0, 5, 0, 5, 2, 2.5),
    spatial_statistic='MEAN'
)
```

---

### PROP

Device property definitions.

**PyFDS Method**: `sim.prop()`

**Common Parameters**

| Parameter | Type | Description |
|-----------|------|-------------|
| `id` | str | Property identifier |
| `quantity` | str | Measured quantity |

**Sprinkler Properties**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `rti` | float | (m·s)^0.5 | Response Time Index |
| `activation_temperature` | float | °C | Activation temperature |
| `spray_angle` | tuple | degrees | [min, max] spray cone |
| `k_factor` | float | LPM/bar^0.5 | K-factor |
| `operating_pressure` | float | bar | Operating pressure |

**Detector Properties**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `rti` | float | (m·s)^0.5 | Response Time Index |
| `activation_temperature` | float | °C | Activation temperature |
| `alpha_e` | float | kW/m² | Extinction coefficient |
| `beta_e` | float | 1/(m·K) | Thermal expansion |

**Example**

```python
# Sprinkler
sim.prop(
    id='STANDARD_SPRINKLER',
    quantity='SPRINKLER LINK TEMPERATURE',
    rti=50.0,
    activation_temperature=68.0,
    spray_angle=(60, 80),
    k_factor=80.0,
    operating_pressure=1.0
)

# Smoke detector
sim.prop(
    id='SMOKE_DETECTOR',
    quantity='CHAMBER OBSCURATION',
    alpha_e=1.8,
    beta_e=0.7
)
```

---

## Advanced Namelists

### RAMP

Time-varying functions.

**PyFDS Method**: `sim.ramp()`

**Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `id` | str | Yes | Ramp identifier |
| `t` | float | Yes | Time (s) |
| `f` | float | Yes | Function value |

**Example**

```python
# t² fire growth
sim.ramp(id='FIRE_GROWTH', t=0, f=0.0)
sim.ramp(id='FIRE_GROWTH', t=60, f=0.25)
sim.ramp(id='FIRE_GROWTH', t=120, f=1.0)
sim.ramp(id='FIRE_GROWTH', t=300, f=1.0)
sim.ramp(id='FIRE_GROWTH', t=360, f=0.5)
sim.ramp(id='FIRE_GROWTH', t=600, f=0.0)
```

---

### CTRL

Control functions.

**PyFDS Method**: `sim.control()`

**Parameters**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `id` | str | Yes | Control identifier |
| `input_id` | str/list | Yes | Input DEVC ID(s) |
| `function_type` | str | No | 'ANY', 'ALL', 'ONLY', etc. |
| `setpoint` | float | No | Activation threshold |
| `latch` | bool | No | Maintain state after activation |
| `initial_state` | bool | No | Initial on/off state |
| `on_bound` | str | No | Activation comparison |
| `delay` | float | No | Activation delay (s) |

**Function Types**

| Type | Logic | Description |
|------|-------|-------------|
| `'ANY'` | OR | True if any input is true |
| `'ALL'` | AND | True if all inputs are true |
| `'ONLY'` | XOR | True if exactly one input is true |
| `'NOT'` | NOT | Invert input |

**Example**

```python
# Temperature threshold
sim.control(
    id='SPRINKLER_CTRL',
    input_id='TEMP_LINK',
    setpoint=68.0,
    latch=True
)

# Logical AND
sim.control(
    id='HVAC_SHUTDOWN',
    function_type='ALL',
    input_id=['SMOKE_1', 'SMOKE_2'],
    delay=5.0
)
```

---

### INIT

Initial conditions.

**PyFDS Method**: `sim.init()`

**Parameters**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `xb` | tuple | m | Region bounds |
| `temperature` | float | °C | Initial temperature |
| `density` | float | kg/m³ | Initial density |

**Velocity Components**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `uu` | float | m/s | X-velocity |
| `vv` | float | m/s | Y-velocity |
| `ww` | float | m/s | Z-velocity |

**Species**

| Parameter | Type | Description |
|-----------|------|-------------|
| `mass_fraction` | dict | Species mass fractions |
| `volume_fraction` | dict | Species volume fractions |

**Example**

```python
# Hot upper layer
sim.init(
    xb=(0, 10, 0, 10, 2.5, 3),
    temperature=400.0
)

# Velocity field
sim.init(
    xb=(0, 10, 0, 10, 0, 3),
    uu=0.5,  # Wind in +X
    ww=0.0
)
```

---

### REAC

Combustion reaction.

**PyFDS Method**: `sim.reaction()`

**Parameters**

| Parameter | Type | Units | Description |
|-----------|------|-------|-------------|
| `id` | str | - | Reaction identifier |
| `fuel` | str | - | Fuel species |
| `formula` | str | - | Chemical formula |
| `heat_of_combustion` | float | kJ/kg | Heat of combustion |
| `soot_yield` | float | kg/kg | Soot production |
| `co_yield` | float | kg/kg | CO production |
| `radiative_fraction` | float | - | Radiative fraction (0-1) |

**Example**

```python
# Custom fuel
sim.reaction(
    id='WOOD',
    fuel='CELLULOSE',
    heat_of_combustion=15000.0,
    soot_yield=0.015,
    co_yield=0.004,
    radiative_fraction=0.35
)
```

---

## Best Practices

### Parameter Validation

PyFDS validates parameters before writing:

```python
try:
    sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
except ValueError as e:
    print(f"Validation error: {e}")
```

### Default Values

Many parameters have sensible defaults:

```python
# These are equivalent
sim.time(t_end=600.0, t_begin=0.0)
sim.time(t_end=600.0)  # t_begin defaults to 0.0
```

### Method Chaining

Most methods return `self` for chaining:

```python
sim = (Simulation(chid='test')
    .time(t_end=600.0)
    .mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
    .surface(id='FIRE', hrrpua=1000.0))
```

## See Also

- [User Guide](../guide/index.md) - Detailed explanations and examples
- [API Reference](../api/index.md) - Python API documentation
- [FDS User Guide](https://pages.nist.gov/fds-smv/) - Official FDS documentation
- [Validation](validation.md) - Validation rules

---

[Validation →](validation.md){ .md-button .md-button--primary }
[Glossary →](glossary.md){ .md-button }
