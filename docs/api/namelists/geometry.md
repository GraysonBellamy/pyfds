# Geometry Namelists

FDS geometry namelists define the physical layout and boundaries of the simulation domain.

## Overview

Geometry in FDS is defined through several namelists that work together:

- **MESH**: Computational domain discretization
- **OBST**: Solid obstructions and structures
- **VENT**: Boundary conditions and openings
- **HOLE**: Openings carved through obstructions
- **MULT**: Array replication of objects
- **GEOM**: Unstructured geometry (beta)
- **MOVE**: Geometry transformations

## MESH - Computational Domain

Defines the computational grid and domain boundaries.

```python
from pyfds import Mesh

# Simple rectangular domain
mesh = Mesh(
    xb=Bounds3D.of(0, 10, 0, 8, 0, 6),  # domain bounds
    ijk=Grid3D.of(50, 40, 30)           # grid resolution
)

# Multiple meshes for parallel computation
mesh1 = Mesh(id='MESH1', xb=Bounds3D.of(0, 5, 0, 8, 0, 6), ijk=Grid3D.of(25, 40, 30))
mesh2 = Mesh(id='MESH2', xb=Bounds3D.of(5, 10, 0, 8, 0, 6), ijk=Grid3D.of(25, 40, 30))
```

## OBST - Solid Obstructions

Creates solid objects that block flow and affect heat transfer.

```python
from pyfds import Obstruction

# Simple wall
wall = Obstruction(
    xb=Bounds3D.of(5, 5.2, 0, 8, 0, 6),  # thick wall at x=5
    surf_id='CONCRETE'
)

# Floor slab
floor = Obstruction(
    xb=Bounds3D.of(0, 10, 0, 8, 0, 0.3),  # 30cm thick floor
    surf_id='CONCRETE'
)

# With thermal properties
thermal_wall = Obstruction(
    xb=Bounds3D.of(0, 0.2, 0, 8, 0, 6),
    surf_id='INSULATED_WALL',
    ht3d=True  # Enable 3D heat conduction
)
```

## VENT - Boundary Conditions

Defines openings, vents, and boundary conditions.

```python
from pyfds import Vent

# Open boundary (ambient)
open_wall = Vent(
    mb='XMIN',  # mesh boundary
    surf_id='OPEN'
)

# Supply vent
supply = Vent(
    xb=Bounds3D.of(2, 3, 2, 3, 6, 6),  # ceiling vent
    surf_id='SUPPLY',
    volume_flow=0.5  # m³/s
)

# Fire source
fire = Vent(
    xb=Bounds3D.of(4.5, 5.5, 4.5, 5.5, 0, 0),  # floor burner
    surf_id='FIRE'
)
```

## HOLE - Carved Openings

Creates openings through existing obstructions.

```python
from pyfds import Hole

# Door opening
door = Hole(
    xb=Bounds3D.of(4.9, 5.1, 2, 4, 0, 2.1),  # door bounds
    id='MAIN_DOOR'
)

# Controlled window
window = Hole(
    xb=Bounds3D.of(0, 0.1, 1, 2, 1.5, 2.5),
    id='WINDOW',
    ctrl_id='WINDOW_CONTROL',
    color='GRAY'  # appearance when closed
)
```

## MULT - Array Replication

Creates regular arrays of repeated objects.

```python
from pyfds import Mult

# 3x3 grid of objects
grid = Mult(
    id='GRID_3X3',
    dx=2.0, dy=2.0,  # 2m spacing
    i_lower=0, i_upper=2,
    j_lower=0, j_upper=2
)

# Linear array
row = Mult(
    id='ROW_10',
    dx=1.0,  # 1m spacing
    n_lower=0, n_upper=9  # 10 objects
)
```

## GEOM - Unstructured Geometry (Beta)

Defines complex geometry using triangulated surfaces.

!!! warning "Beta Feature"
    GEOM is a beta feature in FDS. Use with caution.

```python
from pyfds import Geom

# Simple triangle
triangle = Geom(
    id='TRIANGLE',
    surf_id='STEEL',
    verts=[0,0,0, 1,0,0, 0.5,1,0],  # vertices
    faces=[1,2,3,1]  # face definition
)

# Sphere
sphere = Geom(
    id='BALL',
    surf_id='RUBBER',
    sphere_origin=(5, 5, 1),
    sphere_radius=0.5,
    n_levels=3
)

# Terrain
terrain = Geom(
    id='GROUND',
    surf_id='DIRT',
    zvals=[1.0, 1.2, 1.1, 0.9],  # elevation data
    is_terrain=True
)
```

## MOVE - Geometry Transformations

Applies transformations to GEOM objects.

```python
from pyfds import Move

# Translation
shift = Move(
    id='SHIFT_RIGHT',
    dx=5.0, dy=0.0, dz=0.0
)

# Rotation
rotate = Move(
    id='ROTATE_90',
    axis=(0, 0, 1),
    rotation_angle=90.0
)

# Combined
transform = Move(
    id='COMPLEX',
    dx=1.0, dy=1.0, dz=0.0,
    axis=(0, 0, 1),
    rotation_angle=45.0,
    scale=(2.0, 2.0, 1.0)
)
```

## Integration Examples

### Complete Room with Door

```python
from pyfds import Simulation, Mesh, Obstruction, Hole

sim = Simulation('room_with_door')

# Domain
sim.add(Mesh(xb=Bounds3D.of(0, 6, 0, 4, 0, 3), ijk=Grid3D.of(30, 20, 15))

# Walls
sim.add(Obstruction(xb=Bounds3D.of(0, 0.2, 0, 4, 0, 3), surf_id='WALL')  # Left wall
sim.add(Obstruction(xb=Bounds3D.of(5.8, 6, 0, 4, 0, 3), surf_id='WALL')  # Right wall
sim.add(Obstruction(xb=Bounds3D.of(0, 6, 0, 0.2, 0, 3), surf_id='WALL')  # Back wall
sim.add(Obstruction(xb=Bounds3D.of(0, 6, 3.8, 4, 0, 3), surf_id='WALL')  # Front wall

# Floor and ceiling
sim.add(Obstruction(xb=Bounds3D.of(0, 6, 0, 4, 0, 0.1), surf_id='FLOOR')
sim.add(Obstruction(xb=Bounds3D.of(0, 6, 0, 4, 2.9, 3), surf_id='CEILING')

# Door
sim.add(Hole(xb=Bounds3D.of(2.9, 3.1, 0, 0.2, 0, 2.1), id='DOOR')
```

### Storage Warehouse with Arrays

```python
from pyfds import Simulation, Mult, Obstruction

sim = Simulation('warehouse')

# Rack array multiplier
sim.mult(
    id='RACK_ARRAY',
    dx=3.0, dy=5.0,  # spacing
    i_lower=0, i_upper=9,  # 10 aisles
    j_lower=0, j_upper=3   # 4 rows
)

# Single rack definition (replicated by MULT)
sim.add(Obstruction(
    xb=Bounds3D.of(0, 1, 0, 2, 0, 4),  # rack dimensions
    surf_id='STEEL',
    mult_id='RACK_ARRAY'     # creates 10×4 = 40 racks
)
```

### Complex Geometry with Transformations

```python
from pyfds import Simulation, Geom, Move

sim = Simulation('complex_geometry')

# Create a sphere
sphere = Geom(
    id='BALL',
    surf_id='RUBBER',
    sphere_origin=(0, 0, 0),
    sphere_radius=0.5
)

# Transform it
transform = Move(
    id='BALL_POSITION',
    dx=5.0, dy=5.0, dz=1.0,  # position
    axis=(0, 0, 1),
    rotation_angle=45.0       # rotation
)

sim.add_geometry(sphere)
sim.add_geometry(transform)

# Reference the transformation
sphere.move_id = 'BALL_POSITION'
```

## Namelist Reference

### Common Parameters

| Parameter | Description | Units |
|-----------|-------------|-------|
| `id` | Unique identifier | - |
| `xb` | Bounds (xmin, xmax, ymin, ymax, zmin, zmax) | m |
| `surf_id` | Surface properties ID | - |
| `color` | Visualization color | - |
| `rgb` | RGB color values (0-255) | - |

### MESH Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `ijk` | Grid resolution (nx, ny, nz) | - |
| `trnx`, `trny`, `trnz` | Mesh stretching factors | - |

### OBST Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `ht3d` | Enable 3D heat conduction | - |
| `thick` | Custom thickness | m |
| `mult_id` | MULT reference for arrays | - |

### VENT Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `mb` | Mesh boundary location | - |
| `volume_flow` | HVAC volume flow rate | m³/s |
| `mass_flow` | HVAC mass flow rate | kg/s |
| `tmp` | Supply temperature | °C |

### HOLE Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `ctrl_id` | Control ID for activation | - |
| `devc_id` | Device ID for activation | - |
| `transparency` | Transparency when closed | - |
| `mult_id` | MULT reference for arrays | - |

### MULT Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `dx`, `dy`, `dz` | Spacing in each direction | m |
| `i_lower/upper` | X-direction array bounds | - |
| `j_lower/upper` | Y-direction array bounds | - |
| `k_lower/upper` | Z-direction array bounds | - |
| `n_lower/upper` | Sequential bounds | - |

### GEOM Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `verts` | Vertex coordinates | m |
| `faces` | Face definitions | - |
| `sphere_origin` | Sphere center | m |
| `sphere_radius` | Sphere radius | m |
| `move_id` | MOVE transformation reference | - |

### MOVE Specific

| Parameter | Description | Units |
|-----------|-------------|-------|
| `dx`, `dy`, `dz` | Translation offsets | m |
| `axis` | Rotation axis vector | - |
| `rotation_angle` | Rotation angle | degrees |
| `scale` | Scaling factors | - |

## See Also

- [Geometry Guide](../../guide/geometry.md) - Complete usage guide
- [Builder API](../builders/index.md) - Fluent interfaces for geometry
- [Examples](../../examples/index.md) - Complete geometry examples
