# Building Simulations

Learn how to create and configure FDS simulations using PyFDS.

## Overview

The `Simulation` class is the main entry point for creating FDS simulations. It provides a high-level API for adding components and generating FDS input files.

```python
from pyfds import Simulation

# Create a new simulation
sim = Simulation(chid='my_simulation', title='My Fire Test')
```

## Creating a Simulation

### Basic Creation

Every simulation requires a **Case ID** (CHID):

```python
sim = Simulation(chid='room_fire')
```

The CHID is used as the filename prefix for all output files:

- `room_fire.fds` - Input file
- `room_fire.out` - Output log
- `room_fire_devc.csv` - Device data
- `room_fire_hrr.csv` - Heat release rate data

### Adding a Title

Add a descriptive title for documentation:

```python
sim = Simulation(
    chid='room_fire',
    title='5m x 5m Room Fire with 1kW/m² Burner'
)
```

The title appears in the FDS output and helps identify simulations.

### CHID Naming Rules

!!! tip "CHID Best Practices"
    - Use lowercase with underscores: `room_fire`, `corridor_test`
    - Keep it short but descriptive: `office_fire_2kw`
    - No spaces or special characters
    - Avoid starting with numbers

```python
# Good
sim = Simulation(chid='warehouse_fire_1000kw')

# Bad
sim = Simulation(chid='Warehouse Fire 1000kW')  # Spaces and capitals
sim = Simulation(chid='123_test')  # Starts with number
```

## Building a Simulation

### Step-by-Step Approach

Build simulations by adding components in logical order:

```python
from pyfds import Simulation

# 1. Create simulation
sim = Simulation(chid='example', title='Example Fire')

# 2. Set time parameters
sim.add(Time(t_end=600.0)

# 3. Define computational domain
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

# 4. Create surfaces
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
sim.surface(id='WALL', matl_id='CONCRETE', thickness=0.2)

# 5. Add geometry
sim.add(Obstruction(ion(xb=Bounds3D.of(0, 0.2, 0, 5, 0, 2.5), surf_id='WALL')
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# 6. Add measurement devices
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 2.4))

# 7. Write FDS file
sim.write('example.fds')
```

### Method Chaining

For more concise code, use method chaining:

```python
sim = (Simulation(chid='fire_test', title='Fire Test')
       .time(t_end=300.0)
       .mesh(ijk=Grid3D.of(30, 30, 15), xb=Bounds3D.of(0, 3, 0, 3, 0, 1.5))
       .surface(id='FIRE', hrrpua=500.0)
       .obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE'))

sim.write('fire_test.fds')
```

!!! note "Method Chaining"
    All configuration methods return `self`, allowing you to chain method calls. This is optional - use whichever style you prefer.

## Component Organization

### Recommended Structure

Organize your simulation code for clarity:

```python
from pyfds import Simulation

# ============================================================
# SIMULATION SETUP
# ============================================================
sim = Simulation(chid='organized_fire', title='Well Organized Simulation')

# ============================================================
# TIME AND DOMAIN
# ============================================================
sim.add(Time(t_end=600.0, dt=0.1)
sim.set_misc(tmpa=20.0, radiation=True)

# ============================================================
# COMPUTATIONAL MESH
# ============================================================
sim.add(Mesh(ijk=Grid3D.of(60, 40, 30), xb=Bounds3D.of(0, 6, 0, 4, 0, 3), id='MAIN_MESH')

# ============================================================
# MATERIALS AND SURFACES
# ============================================================
# Materials
sim.material(id='CONCRETE', conductivity=1.8, specific_heat=0.88, density=2400.0)
sim.material(id='STEEL', conductivity=45.8, specific_heat=0.46, density=7850.0)

# Surfaces
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
sim.surface(id='CONCRETE_WALL', matl_id='CONCRETE', thickness=0.2)
sim.surface(id='STEEL_DOOR', matl_id='STEEL', thickness=0.05)

# ============================================================
# GEOMETRY
# ============================================================
# Walls
sim.add(Obstruction(ion(xb=Bounds3D.of(0, 0.2, 0, 4, 0, 3), surf_id='CONCRETE_WALL')  # West wall
sim.add(Obstruction(ion(xb=Bounds3D.of(5.8, 6, 0, 4, 0, 3), surf_id='CONCRETE_WALL')  # East wall
sim.add(Obstruction(ion(xb=Bounds3D.of(0, 6, 0, 0.2, 0, 3), surf_id='CONCRETE_WALL')  # South wall
sim.add(Obstruction(ion(xb=Bounds3D.of(0, 6, 3.8, 4, 0, 3), surf_id='CONCRETE_WALL')  # North wall

# Door
sim.add(Vent(xb=Bounds3D.of(5.8, 5.8, 1, 2, 0, 2.1), surf_id='OPEN')  # Door opening

# Fire source
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 1.5, 2.5, 0, 0.1), surf_id='FIRE')

# ============================================================
# MEASUREMENT DEVICES
# ============================================================
# Temperature measurements
for i, x in enumerate([1.5, 3.0, 4.5]):
    sim.device(
        id=f'TEMP_{i+1}',
        quantity='TEMPERATURE',
        xyz=Point3D.of(x, 2.0, 2.7)
    )

# Heat flux at floor
sim.device(
    id='HF_FLOOR',
    quantity='HEAT FLUX',
    xb=Bounds3D.of(0, 6, 0, 4, 0, 0)
)

# ============================================================
# WRITE OUTPUT
# ============================================================
sim.write('organized_fire.fds')
```

### Using Functions for Reusability

Create helper functions for repeated patterns:

```python
def add_room(sim, origin, size, wall_surf='WALL'):
    """Add a rectangular room to the simulation."""
    x0, y0, z0 = origin
    lx, ly, lz = size

    # Four walls
    sim.add(Obstruction(ion(xb=Bounds3D.of(x0, x0+0.2, y0, y0+ly, z0, z0+lz), surf_id=wall_surf)
    sim.add(Obstruction(ion(xb=Bounds3D.of(x0+lx-0.2, x0+lx, y0, y0+ly, z0, z0+lz), surf_id=wall_surf)
    sim.add(Obstruction(ion(xb=Bounds3D.of(x0, x0+lx, y0, y0+0.2, z0, z0+lz), surf_id=wall_surf)
    sim.add(Obstruction(ion(xb=Bounds3D.of(x0, x0+lx, y0+ly-0.2, y0+ly, z0, z0+lz), surf_id=wall_surf)

    return sim

# Use the function
sim = Simulation(chid='multi_room')
sim.add(Mesh(ijk=Grid3D.of(100, 50, 30), xb=Bounds3D.of(0, 10, 0, 5, 0, 3))
sim.surface(id='WALL', matl_id='CONCRETE', thickness=0.2)

add_room(sim, origin=(0, 0, 0), size=(5, 5, 3))
add_room(sim, origin=(5, 0, 0), size=(5, 5, 3))
```

## Validation

### Automatic Validation

PyFDS automatically validates when you write files:

```python
sim.write('test.fds')  # Validates automatically
```

If there are errors, you'll see informative messages:

```
ValidationError: Obstruction (2, 3, 2, 3, 0, 0.1) is outside mesh bounds (0, 5, 0, 5, 0, 2.5)
```

### Explicit Validation

Validate before writing to catch issues early:

```python
warnings = sim.validate()

if warnings:
    print(f"Found {len(warnings)} warnings:")
    for warning in warnings:
        print(f"  - {warning}")
else:
    print("No validation warnings!")
    sim.write('test.fds')
```

### Common Validation Checks

PyFDS validates:

- ✅ Geometry is within mesh bounds
- ✅ Surface IDs exist before use
- ✅ Coordinate bounds are properly ordered
- ✅ Required parameters are present
- ✅ Parameter values are physically reasonable
- ✅ Material properties are consistent

## Generating FDS Files

### Writing to File

```python
sim.write('simulation.fds')
```

This creates a properly formatted FDS input file.

### Getting FDS Content

Get the FDS file content as a string:

```python
fds_content = sim.to_fds()
print(fds_content)
```

Output:
```fortran
&HEAD CHID='simulation', TITLE='My Simulation' /
&TIME T_END=600.0 /
&MESH IJK=50,50,25, XB=0,5,0,5,0,2.5 /
...
```

### Custom Output Locations

Specify a directory for output files:

```python
from pathlib import Path

# Create output directory
output_dir = Path('simulations/room_fires')
output_dir.mkdir(parents=True, exist_ok=True)

# Write to that directory
sim.write(output_dir / 'fire_1000kw.fds')
```

## Complete Examples

### Minimal Simulation

The absolute minimum for a valid FDS simulation:

```python
from pyfds import Simulation

sim = Simulation(chid='minimal')
sim.add(Time(t_end=10.0)
sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
sim.write('minimal.fds')
```

### Simple Room Fire

A complete, runnable room fire:

```python
from pyfds import Simulation

# Create simulation
sim = Simulation(chid='room_fire', title='5x5x2.5m Room Fire')

# Time: 10 minutes
sim.add(Time(t_end=600.0)

# Domain: 5m x 5m x 2.5m room, 0.1m cells
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

# Fire: 1m x 1m burner, 1000 kW/m²
sim.surface(id='BURNER', hrrpua=1000.0, color='RED')
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='BURNER')

# Measurements
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 2.4))
sim.device(id='TEMP_FLOOR', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 0.1))
sim.device(id='VEL_CEILING', quantity='VELOCITY', xyz=Point3D.of(2.5, 2.5, 2.4))

# Write
sim.write('room_fire.fds')
print("Simulation created successfully!")
```

### Room with Ventilation

Room with door opening:

```python
from pyfds import Simulation

sim = Simulation(chid='room_vent', title='Room with Door')

sim.add(Time(t_end=300.0)
sim.add(Mesh(ijk=Grid3D.of(60, 40, 30), xb=Bounds3D.of(0, 6, 0, 4, 0, 3))

# Ambient conditions
sim.set_misc(tmpa=20.0)

# Surfaces
sim.surface(id='FIRE', hrrpua=500.0)
sim.surface(id='WALL', matl_id='GYPSUM', thickness=0.013)

# Walls (with door opening)
sim.add(Obstruction(ion(xb=Bounds3D.of(0, 0.2, 0, 4, 0, 3), surf_id='WALL')
sim.add(Obstruction(ion(xb=Bounds3D.of(5.8, 6, 0, 1.5, 0, 3), surf_id='WALL')  # Wall beside door
sim.add(Obstruction(ion(xb=Bounds3D.of(5.8, 6, 2.5, 4, 0, 3), surf_id='WALL')  # Wall above door
sim.add(Obstruction(ion(xb=Bounds3D.of(5.8, 6, 1.5, 2.5, 2.1, 3), surf_id='WALL')  # Wall above door

# Door opening
sim.add(Vent(xb=Bounds3D.of(5.8, 5.8, 1.5, 2.5, 0, 2.1), surf_id='OPEN')

# Fire
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 1.5, 2.5, 0, 0.1), surf_id='FIRE')

# Devices
sim.device(id='TEMP_DOOR', quantity='TEMPERATURE', xyz=Point3D.of(5.8, 2.0, 1.0))
sim.device(id='VEL_DOOR', quantity='VELOCITY', xyz=Point3D.of(5.8, 2.0, 1.0))

sim.write('room_vent.fds')
```

## Best Practices

### 1. Start Simple

Begin with coarse meshes and short times for testing:

```python
# Quick test (runs in seconds)
sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 1))
sim.add(Time(t_end=10.0)
```

Then refine for production:

```python
# Production run
sim.add(Mesh(ijk=Grid3D.of(40, 40, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 1))
sim.add(Time(t_end=300.0)
```

### 2. Use Descriptive IDs

```python
# Good - clear and specific
sim.surface(id='WOOD_WALL_12MM', ...)
sim.device(id='TEMP_CEILING_CENTER', ...)

# Bad - unclear
sim.surface(id='S1', ...)
sim.device(id='D1', ...)
```

### 3. Comment Your Code

```python
# Heat release rate per unit area for medium intensity fire
sim.surface(id='FIRE', hrrpua=1000.0)  # 1 MW/m²

# Cell size: 0.1m x 0.1m x 0.1m
# Total cells: 50 * 50 * 25 = 62,500
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))
```

### 4. Keep Related Items Together

```python
# Material and its surface together
sim.material(id='CONCRETE', conductivity=1.8, density=2400.0)
sim.surface(id='CONCRETE_WALL', matl_id='CONCRETE', thickness=0.2)

# Fire source and measurement together
sim.add(Obstruction(ion(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
sim.device(id='TEMP_ABOVE_FIRE', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 0.5))
```

### 5. Validate Early and Often

```python
# Validate after each major addition
sim.add(Mesh(...)
assert len(sim.validate()) == 0, "Mesh validation failed"

sim.add(Obstruction(ion(...)
assert len(sim.validate()) == 0, "Geometry validation failed"
```

## Troubleshooting

### Common Issues

??? question "Simulation won't write"

    **Cause**: Validation errors

    **Solution**: Check validation output
    ```python
    warnings = sim.validate()
    for w in warnings:
        print(w)
    ```

??? question "Missing required parameters"

    **Cause**: Forgot TIME or MESH

    **Solution**: Every simulation needs time and mesh
    ```python
    sim.add(Time(t_end=100.0)  # Required
    sim.add(Mesh(...)  # Required
    ```

??? question "Geometry outside mesh"

    **Cause**: Obstruction coordinates outside mesh bounds

    **Solution**: Check your coordinates
    ```python
    # Mesh: 0 to 5 in X
    sim.add(Mesh(xb=(0, 5, ...))

    # Bad: X goes to 6
    sim.add(Obstruction(ion(xb=(4, 6, ...))  # ERROR

    # Good: X stays within 0-5
    sim.add(Obstruction(ion(xb=(4, 5, ...))  # OK
    ```

## Next Steps

Now that you know how to build simulations, learn about:

- [Computational Domain](domain.md) - Setting up meshes
- [Geometry](geometry.md) - Creating walls and obstructions
- [Fire Sources](fire-sources.md) - Defining fires
- [Devices](devices.md) - Adding measurements

---

[Computational Domain →](domain.md){ .md-button .md-button--primary }
[View Examples →](../examples/basic.md){ .md-button }
