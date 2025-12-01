# Boundary Conditions

Configure vents, openings, and boundary conditions for your FDS simulations.

## Overview

Boundary conditions are specified using the **VENT** namelist. Vents define openings, HVAC systems, external boundaries, and pressure zones.

```python
from pyfds import Vent
from pyfds.core.geometry import Bounds3D

# Open door
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))

# HVAC supply vent
sim.add(Vent(xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3), surf_id='HVAC', volume_flow=0.5))
```

## Open Boundaries

### Doors and Windows

Open boundaries to ambient conditions:

```python
# Door: 1m wide × 2.1m tall at X=5
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))

# Window: 1.5m wide × 1m tall
sim.add(Vent(xb=Bounds3D.of(0, 0, 1, 2.5, 1.5, 2.5), surf_id='OPEN'))
```

!!! note "Zero-Thickness Vents"
    Vents have zero thickness - one coordinate dimension has equal min/max values.

### Open Mesh Boundaries

Open the edges of the computational domain:

```python
# Open all horizontal boundaries
sim.add(Vent(mb='XMIN', surf_id='OPEN')  # -X face
sim.add(Vent(mb='XMAX', surf_id='OPEN')  # +X face
sim.add(Vent(mb='YMIN', surf_id='OPEN')  # -Y face
sim.add(Vent(mb='YMAX', surf_id='OPEN')  # +Y face

# Top open to atmosphere
sim.add(Vent(mb='ZMAX', surf_id='OPEN'))
```

Mesh boundary options: `XMIN`, `XMAX`, `YMIN`, `YMAX`, `ZMIN`, `ZMAX`

## HVAC Systems

### Supply and Exhaust Vents

```python
# Set ambient conditions
sim.set_misc(tmpa=22.0, humidity=50.0)

# Supply vent: 0.5 m³/s inflow
sim.add(Vent(
    xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3),
    surf_id='HVAC',
    volume_flow=0.5  # Positive = supply
)

# Exhaust vent: -0.4 m³/s outflow
sim.add(Vent(
    xb=Bounds3D.of(4, 4.5, 4, 4.5, 3, 3),
    surf_id='HVAC',
    volume_flow=-0.4  # Negative = exhaust
)
```

### Velocity-Specified Vents

```python
# Supply vent with specific velocity
sim.add(Surface(id='SUPPLY', vel=2.0, tmp_front=15.0)  # 2 m/s, 15°C
sim.add(Vent(xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3), surf_id='SUPPLY'))
```

## Circular and Annular Vents

### Circular Burner or Vent

```python
# 1m diameter circular burner
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Vent(
    xb=Bounds3D.of(-2, 2, -2, 2, 0, 0),    # Bounding box
    surf_id='FIRE',
    xyz=Point3D.of(0, 0, 0),               # Center point
    radius=0.5                   # Radius in meters
)
```

The `xb` defines the bounding box, but only the circular area defined by `xyz` and `radius` is used.

### Annular (Ring-Shaped) Vent

```python
# Ring burner: outer radius 1.5m, inner radius 0.5m
sim.add(Vent(
    xb=Bounds3D.of(-2, 2, -2, 2, 0, 0),
    surf_id='FIRE',
    xyz=Point3D.of(0, 0, 0),
    radius=1.5,
    radius_inner=0.5  # Creates a ring
)
```

## Special Boundary Types

### Mirror Boundaries (Symmetry)

Use symmetry to model half or quarter domains:

```python
# Mirror at X=0 plane (model half domain)
sim.add(Vent(mb='XMIN', surf_id='MIRROR'))

# Quarter domain
sim.add(Vent(mb='XMIN', surf_id='MIRROR'))
sim.add(Vent(mb='YMIN', surf_id='MIRROR'))
```

### Periodic Boundaries

For repeating patterns:

```python
# Periodic in X direction
sim.add(Vent(mb='XMIN', surf_id='PERIODIC'))
sim.add(Vent(mb='XMAX', surf_id='PERIODIC'))
```

### Solid Walls

Default boundaries are solid walls. Make explicit if needed:

```python
sim.add(Vent(mb='ZMIN', surf_id='INERT')  # Solid floor
```

## Pressure Zones

### Leak Areas

Model pressure zones with leak areas:

```python
# Compartment with leak area
sim.add(Vent(
    xb=Bounds3D.of(2, 3, 2, 3, 2.5, 2.5),
    surf_id='OPEN',
    leak_area=0.05  # m²
)
```

### Pressure Zones

Define pressure zones for compartments:

```python
# Create pressure zone
sim.add(Vent(
    xb=Bounds3D.of(0, 5, 0, 5, 2.5, 2.5),
    surf_id='OPEN',
    pressure_zone='ROOM_1'
)
```

## Temperature Boundary Conditions

### Fixed Temperature Surface

```python
# Hot wall at 200°C
sim.add(Surface(id='HOT_WALL', tmp_front=200.0))
sim.add(Vent(xb=Bounds3D.of(0, 0, 0, 5, 0, 2.5), surf_id='HOT_WALL'))

# Cold surface at 5°C
sim.add(Surface(id='COLD', tmp_front=5.0))
sim.add(Vent(xb=Bounds3D.of(5, 5, 0, 5, 0, 2.5), surf_id='COLD'))
```

## Complete Examples

### Room with Door

```python
from pyfds import Simulation

sim = Simulation(chid='room_door')
sim.add(Time(ime(t_end=300.0)))
sim.add(Mesh(ijk=Grid3D.of(50, 40, 25), xb=Bounds3D.of(0, 5, 0, 4, 0, 2.5)))

# Fire
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 1.5, 2.5, 0, 0.1), surf_id='FIRE'))

# Door opening (1m wide × 2.1m tall)
sim.add(Vent(xb=Bounds3D.of(5, 5, 1.5, 2.5, 0, 2.1), surf_id='OPEN'))

sim.write('room_door.fds')
```

### HVAC Room

```python
sim = Simulation(chid='hvac_room')
sim.add(Time(ime(t_end=600.0)))
sim.add(Mesh(ijk=Grid3D.of(60, 60, 30), xb=Bounds3D.of(0, 6, 0, 6, 0, 3)))

# Ambient conditions
sim.set_misc(tmpa=22.0, humidity=50.0)

# Fire
sim.add(Surface(id='FIRE', hrrpua=800.0))
sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE'))

# Supply vent at ceiling (0.5 m³/s)
sim.add(Vent(xb=Bounds3D.of(1, 1.5, 1, 1.5, 3, 3), surf_id='HVAC', volume_flow=0.5))

# Exhaust vent at ceiling (-0.4 m³/s)
sim.add(Vent(xb=Bounds3D.of(4.5, 5, 4.5, 5, 3, 3), surf_id='HVAC', volume_flow=-0.4))

# Door to outside
sim.add(Vent(xb=Bounds3D.of(6, 6, 2.5, 3.5, 0, 2.1), surf_id='OPEN'))

sim.write('hvac_room.fds')
```

### Outdoor Fire with Open Boundaries

```python
sim = Simulation(chid='outdoor_fire')
sim.add(Time(ime(t_end=600.0)))
sim.add(Mesh(ijk=Grid3D.of(100, 100, 40), xb=Bounds3D.of(0, 50, 0, 50, 0, 20)))

# Open all sides to atmosphere
sim.add(Vent(mb='XMIN', surf_id='OPEN'))
sim.add(Vent(mb='XMAX', surf_id='OPEN'))
sim.add(Vent(mb='YMIN', surf_id='OPEN'))
sim.add(Vent(mb='YMAX', surf_id='OPEN'))
sim.add(Vent(mb='ZMAX', surf_id='OPEN'))

# Fire source
sim.add(Surface(id='FIRE', hrrpua=500.0))
sim.add(Obstruction(xb=Bounds3D.of(22, 28, 22, 28, 0, 0.5), surf_id='FIRE'))

sim.write('outdoor_fire.fds')
```

### Circular Burner

```python
sim = Simulation(chid='circular_burner')
sim.add(Time(ime(t_end=300.0)))
sim.add(Mesh(ijk=Grid3D.of(60, 60, 30), xb=Bounds3D.of(-3, 3, -3, 3, 0, 3)))

# 1m diameter circular fire
sim.add(Surface(id='BURNER', hrrpua=1500.0))
sim.add(Vent(
    xb=Bounds3D.of(-2, 2, -2, 2, 0, 0),
    surf_id='BURNER',
    xyz=Point3D.of(0, 0, 0),
    radius=0.5  # 0.5m radius = 1m diameter
)

sim.write('circular_burner.fds')
```

## Best Practices

### 1. Zero-Thickness Requirement

Vents must be zero-thickness (one dimension has min=max):

```python
# Correct: X dimension has equal values (5, 5)
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))

# Incorrect: All dimensions have different min/max
sim.add(Vent(xb=Bounds3D.of(5, 5.1, 2, 3, 0, 2.1), surf_id='OPEN')  # Wrong!
```

### 2. Align with Mesh Faces

Align vents with mesh cell faces when possible:

```python
# Mesh with 0.1m cells
sim.add(Mesh(xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Good: Aligned with mesh edge
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))

# Less ideal: Not aligned
sim.add(Vent(xb=Bounds3D.of(4.87, 4.87, 2, 3, 0, 2.1), surf_id='OPEN'))
```

### 3. HVAC Volume Flow Conservation

Total inflow should approximately equal total outflow:

```python
# Supply: 0.5 m³/s in
sim.add(Vent(xb=Bounds3D.of(1, 1.5, 1, 1.5, 3, 3), surf_id='HVAC', volume_flow=0.5))

# Exhaust: -0.4 m³/s out
sim.add(Vent(xb=Bounds3D.of(4, 4.5, 4, 4.5, 3, 3), surf_id='HVAC', volume_flow=-0.4))

# Door: makes up difference
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))
```

### 4. Document Boundary Conditions

```python
# Clear documentation
# Door: 1m wide × 2.1m tall at east wall
# Provides primary ventilation path
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))
```

## Common Issues

??? question "Vent not opening"
    **Cause**: Wrong SURF_ID or geometry issue

    **Solution**: Check surface and coordinates
    ```python
    # Ensure OPEN surface or define custom
    sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))
    ```

??? question "HVAC not working"
    **Cause**: Missing HVAC surface or wrong volume_flow

    **Solution**: Verify HVAC surface and flow direction
    ```python
    # Positive = inflow, Negative = outflow
    sim.add(Vent(xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3), surf_id='HVAC', volume_flow=0.5))
    ```

??? question "Circular vent not circular"
    **Cause**: Missing xyz or radius parameters

    **Solution**: Include center and radius
    ```python
    sim.add(Vent(
        xb=Bounds3D.of(-1, 1, -1, 1, 0, 0),
        surf_id='FIRE',
        xyz=Point3D.of(0, 0, 0),      # Required for circular
        radius=0.5          # Required for circular
    )
    ```

## Next Steps

- [Materials & Surfaces](materials-surfaces.md) - Surface properties
- [Global Settings](global-settings.md) - MISC parameters
- [Examples](../examples/advanced.md) - HVAC and ventilation examples

---

[Materials & Surfaces →](materials-surfaces.md){ .md-button .md-button--primary }
