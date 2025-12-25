# Computational Domain

Learn how to define meshes and set up the computational domain for your FDS simulations.

## Overview

The **computational domain** is the 3D space where FDS performs calculations. It's divided into a grid of cells called a **mesh**.

```python
from pyfds.core.namelists import Mesh
from pyfds.core.geometry import Bounds3D, Grid3D

sim.add(Mesh(
    ijk=Grid3D.of(50, 50, 25),              # Number of cells in each direction
    xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)        # Physical bounds in meters
))
```

## Mesh Basics

### Creating a Mesh

Every simulation requires at least one mesh:

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid='test')
sim.add(Time(t_end=100.0))

# Define a 5m x 5m x 2.5m domain
sim.add(Mesh(
    ijk=Grid3D.of(50, 50, 25),
    xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)
)
```

### Coordinate Bounds (XB)

The `xb` parameter defines the physical extents:

```python
xb = (xmin, xmax, ymin, ymax, zmin, zmax)
#     ↓     ↓     ↓     ↓     ↓     ↓
xb = (0,    5,    0,    5,    0,    2.5)
```

!!! note "Coordinate System"
    - **X**: Usually the longest horizontal direction
    - **Y**: Perpendicular horizontal direction
    - **Z**: Vertical direction (height)
    - **Units**: Always meters

### Cell Counts (IJK)

The `ijk` parameter defines the grid resolution:

```python
ijk = (I, J, K)
#      ↓  ↓  ↓
ijk = (50, 50, 25)
```

- **I**: Number of cells in X direction
- **J**: Number of cells in Y direction
- **K**: Number of cells in Z direction

## Cell Size Calculation

Cell size determines resolution and accuracy:

```python
# For xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5) and ijk=Grid3D.of(50, 50, 25):
dx = (xmax - xmin) / I = (5 - 0) / 50 = 0.1 m
dy = (ymax - ymin) / J = (5 - 0) / 50 = 0.1 m
dz = (zmax - zmin) / K = (2.5 - 0) / 25 = 0.1 m
```

!!! tip "Uniform Cells"
    For best results, keep cell sizes equal in all directions (dx = dy = dz).

### Calculating IJK from Target Cell Size

Given a desired cell size, calculate IJK:

```python
# Target: 0.1m cells for a 6m x 4m x 3m room
domain = (6, 4, 3)
cell_size = 0.1

ijk = tuple(int(d / cell_size) for d in domain)
# Result: (60, 40, 30)

sim.add(Mesh(ijk=ijk, xb=Bounds3D.of(0, 6, 0, 4, 0, 3)))
```

## Resolution Guidelines

### By Application

| Application | Cell Size | Example |
|-------------|-----------|---------|
| **Quick test** | 0.2 - 0.3 m | `ijk=Grid3D.of(25, 25, 12)` for 5×5×2.5m |
| **Standard room fire** | 0.1 - 0.2 m | `ijk=Grid3D.of(50, 50, 25)` for 5×5×2.5m |
| **Detailed analysis** | 0.05 - 0.1 m | `ijk=Grid3D.of(100, 100, 50)` for 5×5×2.5m |
| **Research validation** | < 0.05 m | `ijk=Grid3D.of(200, 200, 100)` for 5×5×2.5m |

### By Fire Diameter

FDS recommends 10-16 cells across the fire diameter:

```python
# 1m diameter fire, want 12 cells across
cell_size = 1.0 / 12  # = 0.083 m

# For 5m x 5m x 2.5m room
ijk = (int(5/0.083), int(5/0.083), int(2.5/0.083))
# ijk = (60, 60, 30)
```

## Multiple Meshes

### When to Use Multiple Meshes

Use multiple meshes for:

- ✅ Large domains with areas of interest
- ✅ Different resolutions in different regions
- ✅ Parallel processing with MPI
- ✅ Complex geometries

### Creating Multiple Meshes

```python
# Fine mesh around fire (0.05m cells)
sim.add(Mesh(
    ijk=Grid3D.of(40, 40, 40),
    xb=Bounds3D.of(1, 3, 1, 3, 0, 2),
    id='FINE_MESH'
)

# Coarse mesh for surrounding room (0.1m cells)
sim.add(Mesh(
    ijk=Grid3D.of(50, 50, 25),
    xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5),
    id='COARSE_MESH'
)
```

### Mesh Alignment

Meshes should align at boundaries:

```python
# Good - meshes share boundary at X=3
sim.add(Mesh(ijk=Grid3D.of(30, 20, 20), xb=Bounds3D.of(0, 3, 0, 2, 0, 2), id='MESH1'))
sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(3, 5, 0, 2, 0, 2), id='MESH2'))

# Bad - gap between meshes
sim.add(Mesh(ijk=Grid3D.of(30, 20, 20), xb=Bounds3D.of(0, 2.9, 0, 2, 0, 2), id='MESH1'))
sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(3.1, 5, 0, 2, 0, 2), id='MESH2'))
```

## Mesh Examples

### Small Room

```python
# 3m x 3m x 2.4m room, 0.1m cells
sim.add(Mesh(ijk=Grid3D.of(30, 30, 24), xb=Bounds3D.of(0, 3, 0, 3, 0, 2.4)))
```

### Corridor

```python
# 20m x 2m x 2.4m corridor, 0.2m cells
sim.add(Mesh(ijk=Grid3D.of(100, 10, 12), xb=Bounds3D.of(0, 20, 0, 2, 0, 2.4)))
```

### Multi-Room Building

```python
# Three connected rooms with consistent resolution
sim.add(Mesh(ijk=Grid3D.of(50, 30, 25), xb=Bounds3D.of(0, 5, 0, 3, 0, 2.5), id='ROOM1'))
sim.add(Mesh(ijk=Grid3D.of(50, 30, 25), xb=Bounds3D.of(5, 10, 0, 3, 0, 2.5), id='ROOM2'))
sim.add(Mesh(ijk=Grid3D.of(50, 30, 25), xb=Bounds3D.of(10, 15, 0, 3, 0, 2.5), id='ROOM3'))
```

### Outdoor Fire

```python
# Large outdoor domain, 0.5m cells
sim.add(Mesh(ijk=Grid3D.of(100, 100, 40), xb=Bounds3D.of(0, 50, 0, 50, 0, 20)))
```

## Performance Considerations

### Cell Count Impact

Total cells = I × J × K

```python
# Coarse: 15,000 cells (fast)
sim.add(Mesh(ijk=Grid3D.of(30, 25, 20), xb=Bounds3D.of(0, 3, 0, 2.5, 0, 2)))

# Medium: 60,000 cells (moderate)
sim.add(Mesh(ijk=Grid3D.of(60, 50, 40), xb=Bounds3D.of(0, 3, 0, 2.5, 0, 2)))

# Fine: 240,000 cells (slow)
sim.add(Mesh(ijk=Grid3D.of(120, 100, 80), xb=Bounds3D.of(0, 3, 0, 2.5, 0, 2)))
```

!!! warning "Computational Cost"
    Halving the cell size multiplies the cell count by 8 and increases runtime by roughly 16× (8× more cells, 2× smaller time step).

### Optimization Tips

1. **Start coarse, refine later**
   ```python
   # Initial test
   sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 1)))

   # Production
   sim.add(Mesh(ijk=Grid3D.of(80, 80, 40), xb=Bounds3D.of(0, 2, 0, 2, 0, 1)))
   ```

2. **Use variable resolution**
   ```python
   # Fine near fire
   sim.add(Mesh(ijk=Grid3D.of(40, 40, 30), xb=Bounds3D.of(1, 3, 1, 3, 0, 1.5)))

   # Coarse elsewhere
   sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
   ```

3. **Parallelize with MPI**
   ```python
   # Split domain into 4 meshes for 4 MPI processes
   for i in range(4):
       x0 = i * 2.5
       x1 = (i + 1) * 2.5
       sim.add(Mesh(ijk=Grid3D.of(25, 50, 25), xb=Bounds3D.of(x0, x1, 0, 5, 0, 2.5), id=f'MESH{i}'))
   ```

## Validation

### Common Issues

??? question "Mesh is too coarse"
    **Symptom**: Poor resolution of fire or features

    **Solution**: Increase IJK or decrease domain size
    ```python
    # Before: 0.5m cells
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 5), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    # After: 0.1m cells
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    ```

??? question "Aspect ratio warning"
    **Symptom**: Cells are not cubic (dx ≠ dy ≠ dz)

    **Solution**: Adjust IJK for uniform cells
    ```python
    # Bad: Non-uniform cells
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 10), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    # dx=0.1, dy=0.1, dz=0.25

    # Good: Uniform cells
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    # dx=dy=dz=0.1
    ```

??? question "Too many cells"
    **Symptom**: Simulation takes too long

    **Solution**: Reduce resolution or use multiple meshes
    ```python
    # Too fine: 1,000,000 cells
    sim.add(Mesh(ijk=Grid3D.of(100, 100, 100), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    # Better: 62,500 cells
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    ```

## Best Practices

### 1. Cubic Cells

Always aim for cubic cells (dx = dy = dz):

```python
# Calculate IJK for cubic cells
def calculate_ijk(xb, cell_size):
    """Calculate IJK for cubic cells."""
    xmin, xmax, ymin, ymax, zmin, zmax = xb
    i = int((xmax - xmin) / cell_size)
    j = int((ymax - ymin) / cell_size)
    k = int((zmax - zmin) / cell_size)
    return (i, j, k)

# Use it
ijk = calculate_ijk((0, 6, 0, 4, 0, 3), cell_size=0.1)
sim.add(Mesh(ijk=ijk, xb=Bounds3D.of(0, 6, 0, 4, 0, 3)))
```

### 2. Align with Geometry

Align mesh boundaries with walls and openings:

```python
# Wall at X=5, align mesh boundary
sim.add(Mesh(xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))  # Good
sim.add(Mesh(xb=Bounds3D.of(0, 5.3, 0, 5, 0, 2.5))  # Bad - wall at 5, mesh at 5.3
```

### 3. Document Your Choices

```python
# Document cell size and justification
# Cell size: 0.1m (D*/dx = 12 for 1.2m fire)
# Based on FDS User Guide recommendation
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
```

### 4. Test Resolution

Run grid convergence studies:

```python
for cell_size in [0.2, 0.1, 0.05]:
    ijk = calculate_ijk((0, 5, 0, 5, 0, 2.5), cell_size)
    sim = Simulation(chid=f'grid_{cell_size}')
    sim.add(Mesh(ijk=ijk, xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    # ... rest of simulation ...
```

## Next Steps

- [Geometry](geometry.md) - Add walls and obstructions
- [Boundary Conditions](boundaries.md) - Set up vents and openings
- [Fire Sources](fire-sources.md) - Create fires

---

[Geometry →](geometry.md){ .md-button .md-button--primary }
[Back to Guide →](index.md){ .md-button }
