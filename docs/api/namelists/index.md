# Namelist Classes

PyFDS provides Pydantic model classes for all major FDS namelists. All namelist classes inherit from `NamelistBase` and can be added to a simulation using `sim.add()`.

## Quick Reference

```python
from pyfds import (
    Simulation, Time, Mesh, Surface, Obstruction, Vent, Device,
    Ramp, Material, Reaction, Control, Property, Initialization,
    Particle, Hole, Species, Combustion, Multiplier, Hvac
)
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D

sim = Simulation(chid='example')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE'))
```

## Simulation Control

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Head`](#head) | HEAD | Simulation metadata (CHID, TITLE) |
| [`Time`](#time) | TIME | Time control (t_end, t_begin, dt) |
| [`Misc`](#misc) | MISC | Miscellaneous parameters |

## Computational Domain

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Mesh`](#mesh) | MESH | Computational grid definition |
| [`Multiplier`](#multiplier) | MULT | Geometry replication |
| [`Initialization`](#initialization) | INIT | Initial conditions |

## Geometry

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Obstruction`](#obstruction) | OBST | Solid objects and walls |
| [`Hole`](#hole) | HOLE | Cutouts in obstructions |
| [`Vent`](#vent) | VENT | Openings, vents, and surfaces |
| [`Geometry`](#geometry) | GEOM | Complex geometry |
| [`Move`](#move) | MOVE | Moving geometry |

## Materials & Surfaces

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Surface`](#surface) | SURF | Surface properties and fire sources |
| [`Material`](#material) | MATL | Material thermal properties |

## Devices & Control

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Device`](#device) | DEVC | Measurement devices |
| [`Property`](#property) | PROP | Device properties |
| [`Control`](#control) | CTRL | Control logic |
| [`Ramp`](#ramp) | RAMP | Time-varying functions |

## Combustion & Species

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Reaction`](#reaction) | REAC | Combustion reactions |
| [`Combustion`](#combustion) | COMB | Combustion parameters |
| [`Species`](#species) | SPEC | Chemical species |

## Particles & HVAC

| Class | FDS Namelist | Description |
|-------|--------------|-------------|
| [`Particle`](#particle) | PART | Particle definitions |
| [`Hvac`](#hvac) | HVAC | HVAC systems |

---

## Class Details

### Head

Simulation header information. Usually set automatically via `Simulation()`.

```python
# Head is typically set via Simulation constructor
sim = Simulation(chid='my_fire', title='Room Fire Test')
```

### Time

Time control parameters.

```python
sim.add(Time(
    t_end=600.0,     # End time (seconds)
    t_begin=0.0,     # Start time (optional)
    dt=None          # Time step (optional, auto-calculated)
))
```

### Misc

Miscellaneous simulation parameters.

```python
sim.set_misc(
    tmpa=20.0,          # Ambient temperature (°C)
    radiation=True,     # Enable radiation
    humidity=40.0       # Relative humidity (%)
)
```

### Mesh

Computational mesh definition.

```python
from pyfds.core.geometry import Bounds3D, Grid3D

sim.add(Mesh(
    id='MAIN',
    ijk=Grid3D.of(50, 50, 25),                    # Cell count
    xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)           # Domain bounds
))
```

### Surface

Surface properties for fire sources, walls, and boundaries.

```python
# Fire source surface
sim.add(Surface(
    id='BURNER',
    hrrpua=1000.0,     # Heat release rate per unit area (kW/m²)
    color='RED'
))

# Material surface
sim.add(Surface(
    id='CONCRETE_WALL',
    matl_id='CONCRETE',
    thickness=0.2,
    color='GRAY'
))
```

### Obstruction

Solid objects and walls.

```python
from pyfds.core.geometry import Bounds3D

sim.add(Obstruction(
    xb=Bounds3D.of(0, 5, 0, 0.2, 0, 2.5),
    surf_id='WALL'
))
```

### Vent

Openings, vents, and surface patches.

```python
sim.add(Vent(
    xb=Bounds3D.of(4.8, 4.8, 1, 2, 0, 2.1),
    surf_id='OPEN'
))
```

### Geometry

Complex geometry.

```python
sim.add(Geometry(
    id='SPHERE',
    surfaces=[(1, 2, 3, 1)],
    vertices=[(0,0,0), (1,0,0), (0,1,0)]
))
```

### Move

Moving geometry.

```python
sim.add(Move(
    id='SHIFT',
    dx=5.0
))
```

### Device

Measurement devices.

```python
from pyfds.core.geometry import Point3D

sim.add(Device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=Point3D.of(2.5, 2.5, 2.4)
))
```

### Ramp

Time-varying functions.

```python
sim.add(Ramp(
    id='FIRE_GROWTH',
    t=[0, 60, 120, 180],
    f=[0, 0.25, 0.5, 1.0]
))
```

### Material

Material thermal properties.

```python
sim.add(Material(
    id='CONCRETE',
    conductivity=1.8,
    specific_heat=0.88,
    density=2400.0
))
```

### Control

Control logic for devices and actions.

```python
sim.add(Control(
    id='SPRK_ACTIVATE',
    function_type='ANY',
    input_id=['TEMP_1', 'TEMP_2'],
    setpoint=68.0
))
```

### Reaction

Combustion reaction definitions.

```python
sim.add(Reaction(
    fuel='PROPANE',
    soot_yield=0.01,
    co_yield=0.006
))
```

### Initialization

Initial conditions.

```python
sim.add(Initialization(
    xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5),
    temperature=25.0
))
```

### Particle

Particle definitions for sprinklers and tracers.

```python
sim.add(Particle(
    id='WATER',
    diameter=1000.0  # microns
))
```

### Hole

Cutouts in obstructions.

```python
sim.add(Hole(
    xb=Bounds3D.of(2, 3, 0, 0.2, 0.5, 2.0)
))
```

### Property

Device properties (e.g., sprinkler characteristics).

```python
sim.add(Property(
    id='SPRINKLER',
    part_id='WATER',
    flow_rate=189.0,
    operating_pressure=1.0
))
```

### Multiplier

Geometry replication.

```python
sim.add(Multiplier(
    id='MULT_X',
    dx=5.0,
    n_copies=3
))
```

### Species

Chemical species definitions.

```python
sim.add(Species(
    id='TRACER',
    mass_fraction_0=0.0
))
```

### Combustion

Combustion model parameters.

```python
sim.add(Combustion(
    extinction_model='EXTINCTION 2'
))
```

### Hvac

HVAC system definitions.

```python
sim.add(Hvac(
    id='SUPPLY',
    type_id='NODE',
    duct_id='DUCT_1'
))
```

---

## See Also

- [Building Simulations](../../guide/building-simulations.md) - How to use namelist classes
- [Examples](../../examples/basic.md) - Working code examples
- [API Reference](../index.md) - Full API documentation
