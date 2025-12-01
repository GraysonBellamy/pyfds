# Complex Geometry Examples

This directory contains examples demonstrating complex geometry features
including GEOM primitives, obstruction arrays, and holes.

## Examples

### 1. sphere_geometry.py
Demonstrates spherical geometry using GEOM.
- **Concepts**: GEOM sphere primitive
- **Key Features**:
  - SPHERE_ORIGIN, SPHERE_RADIUS parameters
  - N_LEVELS for mesh refinement
  - Curved surface with material properties

### 2. cylinder_geometry.py
Demonstrates cylindrical geometry using GEOM.
- **Concepts**: GEOM cylinder primitive
- **Key Features**:
  - CYLINDER_ORIGIN, CYLINDER_AXIS, CYLINDER_RADIUS, CYLINDER_LENGTH
  - Horizontal pipe geometry
  - Flow around cylinder

### 3. obst_array.py
Creates an array of obstructions programmatically.
- **Concepts**: Multiple OBST namelists
- **Key Features**:
  - Nested loops for rack placement
  - Warehouse/storage configuration
  - Multiple temperature sensors

### 4. hole_geometry.py
Demonstrates HOLE namelist for openings.
- **Concepts**: HOLE for doors/windows
- **Key Features**:
  - Door opening in wall
  - Window opening at height
  - Flow through openings

## Key PyFDS Features Demonstrated

```python
# Sphere geometry
geom = Geometry(
    id="BALL",
    surf_id="STEEL",
    sphere_origin=(1.5, 1.5, 1.5),
    sphere_radius=0.5,
    n_levels=3,
)

# Cylinder geometry
geom = Geometry(
    id="PIPE",
    surf_id="STEEL",
    cylinder_origin=(0.5, 1.0, 1.0),
    cylinder_axis=(1.0, 0.0, 0.0),
    cylinder_radius=0.2,
    cylinder_length=3.0,
)

# Hole in obstruction
hole = Hole(
    id="DOOR",
    xb=Bounds3D.of(3.8, 4.2, 1.5, 2.5, 0.0, 2.2),
)

# Programmatic obstruction array
for i in range(3):
    for j in range(2):
        sim.add(Obstruction(id=f"RACK_{i}_{j}", xb=...))
```

## FDS Reference Cases

These examples are inspired by:
- **geom_sphere_***: Sphere geometry verification
- **geom_cylinder_***: Cylinder geometry tests
- **hole_***: Hole namelist tests

## Running the Examples

```bash
# Run individual examples
python examples/complex_geometry/sphere_geometry.py
python examples/complex_geometry/cylinder_geometry.py
python examples/complex_geometry/obst_array.py
python examples/complex_geometry/hole_geometry.py
```

## Notes

- GEOM is a Beta feature in FDS - use with caution
- N_LEVELS controls sphere/cylinder mesh refinement
- HOLE extends slightly beyond OBST to ensure complete opening
- Programmatic arrays are useful for parametric studies
