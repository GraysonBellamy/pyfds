# Geometry

Learn how to create walls, obstructions, and solid objects in your FDS simulations.

## Overview

Geometry in FDS is created using **obstructions** - solid rectangular blocks that can represent walls, furniture, equipment, and other objects.

```python
from pyfds import Obstruction
from pyfds.core.geometry import Bounds3D

sim.add(Obstruction(
    xb=Bounds3D.of(0, 0.2, 0, 5, 0, 2.5),  # Coordinates
    surf_id='WALL'               # Surface properties
))
```

## Creating Obstructions

### Basic Obstruction

```python
from pyfds import Simulation, Mesh, Obstruction
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='geometry_test')
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Create a wall (0.2m thick)
sim.add(Obstruction(
    xb=Bounds3D.of(0, 0.2, 0, 5, 0, 2.5),
    surf_id='CONCRETE_WALL'
)
```

### Coordinate Specification

Coordinates define a rectangular box:

```python
xb = (xmin, xmax, ymin, ymax, zmin, zmax)
```

Example - 1m × 1m × 0.5m box:
```python
sim.add(Obstruction(xb=Bounds3D.of(1.0, 2.0, 1.0, 2.0, 0.0, 0.5)))
```

## Common Geometry Patterns

### Walls

#### Four Room Walls

```python
# 5m × 5m × 2.5m room with 0.2m thick walls
wall_thickness = 0.2

# West wall (low X)
sim.add(Obstruction(xb=Bounds3D.of(0, wall_thickness, 0, 5, 0, 2.5), surf_id='WALL'))

# East wall (high X)
sim.add(Obstruction(xb=Bounds3D.of(5-wall_thickness, 5, 0, 5, 0, 2.5), surf_id='WALL'))

# South wall (low Y)
sim.add(Obstruction(xb=Bounds3D.of(0, 5, 0, wall_thickness, 0, 2.5), surf_id='WALL'))

# North wall (high Y)
sim.add(Obstruction(xb=Bounds3D.of(0, 5, 5-wall_thickness, 5, 0, 2.5), surf_id='WALL'))
```

#### Wall with Door Opening

```python
# Wall with 1m door centered at Y=2.5, 2.1m tall
wall_thickness = 0.2
door_width = 1.0
door_height = 2.1
door_center_y = 2.5

# Left part of wall (below door center - half width)
sim.add(Obstruction(
    xb=Bounds3D.of(5-wall_thickness, 5, 0, door_center_y - door_width/2, 0, 2.5),
    surf_id='WALL'
)

# Right part of wall (above door center + half width)
sim.add(Obstruction(
    xb=Bounds3D.of(5-wall_thickness, 5, door_center_y + door_width/2, 5, 0, 2.5),
    surf_id='WALL'
)

# Top part of wall (above door)
sim.add(Obstruction(
    xb=Bounds3D.of(5-wall_thickness, 5, door_center_y - door_width/2, door_center_y + door_width/2, door_height, 2.5),
    surf_id='WALL'
)
```

### Floors and Ceilings

```python
# Floor (thin obstruction at Z=0)
sim.add(Obstruction(xb=Bounds3D.of(0, 5, 0, 5, 0, 0.01), surf_id='FLOOR'))

# Ceiling (at top of domain)
sim.add(Obstruction(xb=Bounds3D.of(0, 5, 0, 5, 2.49, 2.5), surf_id='CEILING'))
```

### Furniture and Objects

```python
# Table: 1.5m × 0.8m × 0.8m tall
sim.add(Obstruction(xb=Bounds3D.of(2.0, 3.5, 1.5, 2.3, 0.0, 0.8), surf_id='WOOD'))

# Chair: 0.5m × 0.5m × 0.5m
sim.add(Obstruction(xb=Bounds3D.of(1.5, 2.0, 1.5, 2.0, 0.0, 0.5), surf_id='PLASTIC'))

# Cabinet: 1m × 0.5m × 2m tall
sim.add(Obstruction(xb=Bounds3D.of(0.2, 1.2, 4.5, 5.0, 0.0, 2.0), surf_id='METAL'))
```

### Columns

```python
# Square column: 0.3m × 0.3m, full height
sim.add(Obstruction(xb=Bounds3D.of(2.35, 2.65, 2.35, 2.65, 0.0, 2.5), surf_id='CONCRETE'))

# Multiple columns in a row
for x in [1.0, 2.0, 3.0, 4.0]:
    sim.add(Obstruction(xb=Bounds3D.of(x-0.15, x+0.15, 2.35, 2.65, 0.0, 2.5), surf_id='CONCRETE'))
```

## Advanced Geometry

### Parametric Geometry

Use loops and functions for complex geometry:

```python
def create_bookshelf(sim, x, y, z, width, depth, height, shelves):
    """Create a bookshelf with multiple shelves."""
    shelf_thickness = 0.02

    # Back and sides
    sim.add(Obstruction(xb=Bounds3D.of(x, x+width, y+depth-0.02, y+depth, z, z+height), surf_id='WOOD'))
    sim.add(Obstruction(xb=Bounds3D.of(x, x+0.02, y, y+depth, z, z+height), surf_id='WOOD'))
    sim.add(Obstruction(xb=Bounds3D.of(x+width-0.02, x+width, y, y+depth, z, z+height), surf_id='WOOD'))

    # Shelves
    shelf_spacing = height / shelves
    for i in range(shelves + 1):
        shelf_z = z + i * shelf_spacing
        sim.add(Obstruction(
            xb=Bounds3D.of(x, x+width, y, y+depth, shelf_z, shelf_z+shelf_thickness),
            surf_id='WOOD'
        )

# Create bookshelves
create_bookshelf(sim, x=0.5, y=4.5, z=0.0, width=1.0, depth=0.3, height=2.0, shelves=4)
create_bookshelf(sim, x=2.0, y=4.5, z=0.0, width=1.0, depth=0.3, height=2.0, shelves=4)
```

### Arrays of Objects

```python
# Grid of cubes
cube_size = 0.3
spacing = 0.5

for i in range(5):
    for j in range(5):
        x0 = 0.5 + i * spacing
        y0 = 0.5 + j * spacing
        sim.add(Obstruction(
            xb=Bounds3D.of(x0, x0+cube_size, y0, y0+cube_size, 0, cube_size),
            surf_id='BOX'
        )
```

### Stepped Geometry

```python
# Stairs: 10 steps, 0.3m wide, 0.2m rise
step_width = 0.3
step_rise = 0.2

for i in range(10):
    x0 = 1.0 + i * step_width
    z0 = i * step_rise
    sim.add(Obstruction(
        xb=Bounds3D.of(x0, x0+step_width, 1.0, 2.0, 0, z0+step_rise),
        surf_id='CONCRETE'
    )
```

## Surface Assignment

### Default Surface

All obstructions have a surface. If not specified, FDS uses 'INERT':

```python
# Uses default 'INERT' surface
sim.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

# Explicit surface
sim.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id='CONCRETE'))
```

### Different Surfaces on Each Face

Assign different surfaces to each face:

```python
# Box with fire on top, concrete on other faces
sim.add(Obstruction(
    xb=Bounds3D.of(2, 3, 2, 3, 0, 0.5),
    surf_id='CONCRETE',      # Default for all faces
    surf_ids=['CONCRETE', 'CONCRETE', 'CONCRETE',
              'CONCRETE', 'CONCRETE', 'FIRE']  # Override top face
)
```

Face order: -X, +X, -Y, +Y, -Z, +Z (west, east, south, north, bottom, top)

## Holes and Cutouts

### Creating Holes

Use HOLE namelist (via obstruction with `hole=True`):

```python
# Create wall
sim.add(Obstruction(xb=Bounds3D.of(5, 5.2, 0, 5, 0, 3), surf_id='WALL'))

# Cut window in wall
sim.add(Obstruction(xb=Bounds3D.of(5, 5.2, 1.5, 2.5, 1.5, 2.5), hole=True))
```

!!! note "Holes"
    Holes remove geometry. They must overlap existing obstructions exactly.

## Best Practices

### 1. Align with Mesh

Align geometry with cell boundaries when possible:

```python
# Mesh has 0.1m cells
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Good - aligns with cells
sim.add(Obstruction(xb=Bounds3D.of(1.0, 2.0, 1.0, 2.0, 0.0, 0.5)))

# Less ideal - doesn't align
sim.add(Obstruction(xb=Bounds3D.of(1.07, 2.13, 1.05, 1.98, 0.02, 0.47)))
```

### 2. Avoid Tiny Gaps

Don't leave tiny gaps between obstructions:

```python
# Bad - tiny 0.01m gap
sim.add(Obstruction(xb=Bounds3D.of(0, 2.49, 0, 5, 0, 2.5)))
sim.add(Obstruction(xb=Bounds3D.of(2.51, 5, 0, 5, 0, 2.5)))

# Good - touching or overlapping
sim.add(Obstruction(xb=Bounds3D.of(0, 2.5, 0, 5, 0, 2.5)))
sim.add(Obstruction(xb=Bounds3D.of(2.5, 5, 0, 5, 0, 2.5)))
```

### 3. Organize by Type

Group related geometry:

```python
# ============================================================
# WALLS
# ============================================================
sim.add(Obstruction(xb=Bounds3D.of(0, 0.2, 0, 5, 0, 2.5), surf_id='WALL'))
sim.add(Obstruction(xb=Bounds3D.of(4.8, 5, 0, 5, 0, 2.5), surf_id='WALL'))

# ============================================================
# FURNITURE
# ============================================================
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.8), surf_id='TABLE'))
sim.add(Obstruction(xb=Bounds3D.of(1, 1.5, 1, 1.5, 0, 0.5), surf_id='CHAIR'))

# ============================================================
# FIRE SOURCES
# ============================================================
sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2.5, 3.5, 0, 0.1), surf_id='BURNER'))
```

### 4. Use Helper Functions

Create reusable functions for common patterns:

```python
def add_rectangular_room(sim, xb, wall_thickness, surf_id='WALL'):
    """Add walls for a rectangular room."""
    xmin, xmax, ymin, ymax, zmin, zmax = xb
    t = wall_thickness

    # Four walls
    sim.add(Obstruction(xb=Bounds3D.of(xmin, xmin+t, ymin, ymax, zmin, zmax), surf_id=surf_id))
    sim.add(Obstruction(xb=Bounds3D.of(xmax-t, xmax, ymin, ymax, zmin, zmax), surf_id=surf_id))
    sim.add(Obstruction(xb=Bounds3D.of(xmin, xmax, ymin, ymin+t, zmin, zmax), surf_id=surf_id))
    sim.add(Obstruction(xb=Bounds3D.of(xmin, xmax, ymax-t, ymax, zmin, zmax), surf_id=surf_id))

# Use it
add_rectangular_room(sim, (0, 5, 0, 5, 0, 2.5), wall_thickness=0.2)
```

## Common Issues

??? question "Obstruction outside mesh"
    **Cause**: Coordinates extend beyond mesh bounds

    **Solution**: Check mesh and obstruction bounds
    ```python
    # Mesh: 0 to 5
    sim.add(Mesh(xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    # Bad: extends to 5.5
    sim.add(Obstruction(xb=Bounds3D.of(4, 5.5, 0, 5, 0, 2.5)))

    # Good: within bounds
    sim.add(Obstruction(xb=Bounds3D.of(4, 5, 0, 5, 0, 2.5)))
    ```

??? question "Surface ID not found"
    **Cause**: Referenced surface before defining it

    **Solution**: Define surfaces before use
    ```python
    # Bad - surface not defined
    sim.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id='WOOD'))

    # Good - define first
    sim.add(Surface(id='WOOD', matl_id='PINE'))
    sim.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id='WOOD'))
    ```

??? question "Overlapping geometry"
    **Cause**: Multiple obstructions in same space

    **Solution**: FDS handles overlaps, but avoid if unintentional
    ```python
    # Overlapping (may be intentional or error)
    sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 1)))
    sim.add(Obstruction(xb=Bounds3D.of(1.5, 2.5, 1, 2, 0, 1))  # Overlaps first
    ```

## Complete Examples

### Simple Room

```python
sim = Simulation(chid='simple_room')
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Walls
add_rectangular_room(sim, (0, 5, 0, 5, 0, 2.5), wall_thickness=0.2)

# Door opening (use vent, covered in next guide)
sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2.1), surf_id='OPEN'))
```

### Office Space

```python
sim = Simulation(chid='office')
sim.add(Mesh(ijk=Grid3D.of(80, 60, 30), xb=Bounds3D.of(0, 8, 0, 6, 0, 3)))

# Walls
add_rectangular_room(sim, (0, 8, 0, 6, 0, 3), wall_thickness=0.2)

# Desk
sim.add(Obstruction(xb=Bounds3D.of(1, 2.5, 1, 2, 0, 0.75), surf_id='WOOD'))

# Filing cabinet
sim.add(Obstruction(xb=Bounds3D.of(0.5, 1, 5, 5.5, 0, 1.2), surf_id='METAL'))

# Bookshelf
create_bookshelf(sim, x=7, y=5.5, z=0, width=0.8, depth=0.3, height=2, shelves=5)
```

## Next Steps

- [Boundary Conditions](boundaries.md) - Vents and openings
- [Materials & Surfaces](materials-surfaces.md) - Surface properties
- [Fire Sources](fire-sources.md) - Create fires

---

[Boundary Conditions →](boundaries.md){ .md-button .md-button--primary }
[Back to Guide →](index.md){ .md-button }
