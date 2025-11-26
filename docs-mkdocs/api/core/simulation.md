# Simulation

The `Simulation` class is the main entry point for creating FDS simulations with PyFDS.

## Overview

`Simulation` provides a Pythonic interface for building FDS input files. It manages all namelist groups and provides methods for adding simulation components.

```python
from pyfds import Simulation

# Create a new simulation
sim = Simulation(chid='my_fire', title='My Fire Simulation')

# Add components
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# Write FDS file
sim.write('my_fire.fds')
```

## Class Reference

::: pyfds.core.simulation.Simulation
    options:
      show_root_heading: true
      show_source: true
      heading_level: 3
      members:
        - __init__
        - time
        - set_misc
        - mesh
        - surface
        - material
        - ramp
        - reaction
        - prop
        - obstruction
        - vent
        - device
        - ctrl
        - init
        - validate
        - to_fds
        - write
        - run

## Quick Reference

### Creating a Simulation

```python
# Basic creation
sim = Simulation(chid='test')

# With title
sim = Simulation(chid='test', title='My Test Simulation')
```

### Time Configuration

```python
# Simple
sim.time(t_end=600.0)

# With start time and time step
sim.time(t_end=600.0, t_begin=0.0, dt=0.1)
```

### Domain Setup

```python
# Single mesh
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Multiple meshes
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5), id='MESH1')
sim.mesh(ijk=(30, 30, 15), xb=(5, 8, 0, 3, 0, 1.5), id='MESH2')
```

### Surfaces

```python
# Fire surface
sim.surface(id='BURNER', hrrpua=1000.0, color='RED')

# Material surface
sim.surface(id='WALL', matl_id='CONCRETE', thickness=0.2)

# Boundary condition
sim.surface(id='INLET', vel=1.0, tmp_front=25.0)
```

### Geometry

```python
# Obstruction
sim.obstruction(
    xb=(0, 0.2, 0, 5, 0, 2.5),
    surf_id='WALL'
)

# Vent
sim.vent(
    xb=(4, 4, 0, 2, 0, 2),
    surf_id='OPEN'
)

# Circular vent
sim.vent(
    xb=(-2, 2, -2, 2, 0, 0),
    surf_id='BURNER',
    xyz=(0, 0, 0),
    radius=1.0
)
```

### Devices

```python
# Point measurement
sim.device(
    id='TEMP_1',
    quantity='TEMPERATURE',
    xyz=(2.5, 2.5, 2.4)
)

# Area measurement
sim.device(
    id='HF_FLOOR',
    quantity='HEAT FLUX',
    xb=(0, 5, 0, 5, 0, 0)
)
```

### Advanced Features

```python
# Material definition
sim.material(
    id='WOOD',
    conductivity=0.12,
    specific_heat=1.0,
    density=500.0
)

# Time-varying property
sim.ramp(
    id='FIRE_GROWTH',
    t=[0, 100, 200, 300],
    f=[0, 0.5, 1.0, 1.0]
)

# Control logic
sim.ctrl(
    id='ACTIVATION',
    input_id='TEMP_1',
    setpoint=100.0
)

# Initial condition
sim.init(
    xb=(0, 5, 0, 5, 2.0, 2.5),
    temperature=200.0
)
```

### Validation and Output

```python
# Validate before writing
warnings = sim.validate()
for w in warnings:
    print(f"Warning: {w}")

# Write FDS file
sim.write('simulation.fds')

# Get FDS content as string
fds_content = sim.to_fds()
print(fds_content)
```

### Execution

```python
# Blocking execution
results = sim.run(n_threads=4)

# Non-blocking execution
job = sim.run(wait=False, monitor=True)

# With MPI
results = sim.run(n_mpi=4, n_threads=2)
```

## Method Chaining

All configuration methods return `self` for chaining:

```python
sim = (Simulation(chid='test')
       .time(t_end=600.0)
       .mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
       .surface(id='FIRE', hrrpua=1000.0)
       .obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE'))

sim.write('test.fds')
```

## See Also

- [Validator](validator.md) - Validation functionality
- [Namelist Classes](../namelists/index.md) - Individual namelist documentation
- [User Guide](../../guide/building-simulations.md) - Comprehensive usage guide
