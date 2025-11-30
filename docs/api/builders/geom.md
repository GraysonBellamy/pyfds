# GeomBuilder API Reference

The `GeomBuilder` provides a fluent interface for creating unstructured geometry using triangulated surfaces, predefined shapes, and terrain.

## Overview

The GEOM namelist enables complex 3D geometry beyond simple rectangular obstructions. This includes:

- **Triangulated surfaces**: Custom meshes defined by vertices and faces
- **Predefined shapes**: Spheres, cylinders, and blocks
- **Terrain**: Elevation-based ground surfaces
- **Texture mapping**: Visual enhancements

!!! warning "Beta Feature"
    GEOM is a beta feature in FDS. Use with caution and validate results.

## Basic Usage

```python
from pyfds.builders import GeomBuilder

# Create a simple triangle
geom = (
    GeomBuilder('TRIANGLE')
    .with_vertices([(0,0,0), (1,0,0), (0.5,1,0)])
    .with_faces([(1,2,3,1)])  # vertex indices, surface index
    .build()
)
```

## Triangulated Surfaces

Define custom geometry using vertices and triangular faces:

```python
# Cube vertices (8 corners)
vertices = [
    (0,0,0), (1,0,0), (1,1,0), (0,1,0),  # bottom
    (0,0,1), (1,0,1), (1,1,1), (0,1,1)   # top
]

# Triangle faces (12 triangles for a cube)
faces = [
    (1,2,3,1), (1,3,4,1),  # bottom
    (5,8,7,1), (5,7,6,1),  # top
    (1,4,8,1), (1,8,5,1),  # front
    (2,6,7,1), (2,7,3,1),  # back
    (1,5,6,1), (1,6,2,1),  # left
    (4,3,7,1), (4,7,8,1),  # right
]

geom = (
    GeomBuilder('CUBE')
    .with_vertices(vertices)
    .with_faces(faces)
    .build()
)
```

## Predefined Shapes

### Sphere

```python
sphere = (
    GeomBuilder('BALL')
    .sphere(center=(5, 5, 1), radius=0.5, levels=3)
    .build()
)

# With custom subdivision
sphere = (
    GeomBuilder('BALL')
    .sphere(
        center=(5, 5, 1),
        radius=0.5,
        levels=4,      # radial subdivisions
        n_lat=8,       # latitude divisions
        n_long=12      # longitude divisions
    )
    .build()
)
```

### Cylinder

```python
cylinder = (
    GeomBuilder('PIPE')
    .cylinder(
        origin=(0, 0, 0),      # bottom center
        axis=(0, 0, 1),        # direction vector
        length=2.0,
        radius=0.5,
        nseg_axis=4,           # axial segments
        nseg_theta=8           # angular segments
    )
    .build()
)
```

### Block

```python
block = (
    GeomBuilder('BOX')
    .block(
        xb=Bounds3D.of(0, 2, 0, 1, 0, 3),  # bounds
        ijk=Grid3D.of(4, 2, 6)             # subdivisions
    )
    .build()
)
```

## Terrain

Create elevation-based surfaces:

```python
# Simple terrain from elevation data
terrain = (
    GeomBuilder('LANDSCAPE')
    .terrain(elevation_data=[1.0, 1.2, 1.1, 0.9, 1.3, 0.8])
    .build()
)

# With terrain options
terrain = (
    GeomBuilder('MOUNTAIN')
    .terrain(
        elevation_data=elevation_grid,
        zmin=0.0,                    # minimum elevation
        extend_terrain=True,          # extend to domain boundaries
        zval_horizon=100.0            # horizon elevation
    )
    .build()
)
```

## Extrusion

Create 3D geometry by extruding 2D polygons:

```python
# Define polygon vertices (in XY plane)
polygon_indices = [1, 2, 3, 4]  # indices into vertex list

geom = (
    GeomBuilder('EXTRUDED')
    .with_vertices([(0,0,0), (1,0,0), (1,1,0), (0,1,0)])
    .extrude(polygon_indices, distance=2.0)
    .build()
)
```

## Surface Assignment

Assign different surfaces to different parts:

```python
geom = (
    GeomBuilder('COMPLEX')
    .with_vertices(vertices)
    .with_faces(faces)
    .surface_ids(['STEEL', 'WOOD', 'GLASS'])  # multiple surface types
    .build()
)
```

## Transformations

Apply transformations using MOVE references:

```python
geom = (
    GeomBuilder('SHIFTED_SPHERE')
    .sphere(center=(0, 0, 0), radius=1.0)
    .move_id('CENTER_OFFSET')  # reference to MOVE namelist
    .build()
)
```

## Visualization

Control appearance in smokeview:

```python
geom = (
    GeomBuilder('VISIBLE')
    .with_vertices(vertices)
    .with_faces(faces)
    .color('RED')                    # named color
    .rgb((255, 0, 0))               # RGB color
    .transparency(0.8)              # semi-transparent
    .build()
)
```

## Texture Mapping

Add visual textures:

```python
geom = (
    GeomBuilder('TEXTURED')
    .with_vertices(vertices)
    .with_faces(faces)
    .texture_mapping('RECTANGULAR')    # or 'SPHERICAL'
    .texture_origin((0, 0, 0))         # texture origin
    .texture_scale(2.0)                # scale factor
    .texture_file('brick.png')         # texture image
    .build()
)
```

## Thin Geometry

Create infinitely thin surfaces:

```python
# Vertical wall
wall = (
    GeomBuilder('WALL')
    .with_vertices([(0,0,0), (0,0,3), (5,0,3), (5,0,0)])
    .with_faces([(1,2,3,1), (1,3,4,1)])
    .thin_geometry(cell_block_ior=1)  # X-normal direction
    .build()
)
```

## Complete Example

```python
from pyfds.builders import GeomBuilder, MoveBuilder

# Create a transformed sphere
sphere = (
    GeomBuilder('BALL')
    .sphere(center=(0, 0, 0), radius=0.5)
    .move_id('BALL_POSITION')
    .color('BLUE')
    .build()
)

# Define the transformation
move = (
    MoveBuilder('BALL_POSITION')
    .translate(dx=5.0, dy=5.0, dz=1.0)
    .rotate(axis=(0, 0, 1), angle=45.0)
    .build()
)

# Add to simulation
sim = Simulation('geometry_demo')
sim.add_geometry(sphere)
sim.add_geometry(move)
```

## Method Reference

### Geometry Definition
- `with_vertices(vertices)`: Set vertex coordinates
- `with_faces(faces)`: Set triangular faces
- `sphere(center, radius, levels, n_lat, n_long)`: Create sphere
- `cylinder(origin, axis, length, radius, nseg_axis, nseg_theta)`: Create cylinder
- `block(xb, ijk)`: Create subdivided block
- `terrain(elevation_data, zmin, extend_terrain, zval_horizon)`: Create terrain
- `extrude(poly, extrude_dist)`: Extrude 2D polygon

### Surface Properties
- `surface_id(surf_id)`: Set default surface
- `surface_ids(surf_ids)`: Set multiple surface types

### Visualization
- `color(color)`: Set named color
- `rgb(rgb)`: Set RGB color tuple
- `transparency(alpha)`: Set transparency (0-1)

### Texture Mapping
- `texture_mapping(mapping)`: Set mapping type
- `texture_origin(origin)`: Set texture origin
- `texture_scale(scale)`: Set texture scale
- `texture_file(filename)`: Set texture image file

### Transformations
- `move_id(move_id)`: Reference MOVE transformation

### Special Geometry
- `thin_geometry(cell_block_ior)`: Create thin surface

## See Also

- [Geometry Documentation](../../guide/geometry.md) - Usage guide
- [MoveBuilder](move.md) - Geometry transformations
- [HoleBuilder](hole.md) - Carving holes in geometry
- [MultBuilder](mult.md) - Array replication
