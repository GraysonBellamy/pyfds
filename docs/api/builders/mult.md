# MultBuilder API Reference

The `MultBuilder` provides a fluent interface for creating MULT namelists that replicate objects in regular arrays.

## Overview

The MULT namelist creates arrays of repeated objects without specifying each one individually. This is useful for creating regular patterns like rack storage, window arrays, or ventilation grids.

MULT works with: MESH, OBST, VENT, HOLE, and INIT namelists.

## Basic Usage

```python
from pyfds.builders import MultBuilder

# 3x3 array with 2m spacing
mult = (
    MultBuilder('ARRAY_3X3')
    .spacing(dx=2.0, dy=2.0)
    .count(i_lower=0, i_upper=2, j_lower=0, j_upper=2)
    .build()
)

# Linear array of 10 objects
mult = (
    MultBuilder('ROW_10')
    .spacing(dx=1.0)
    .count(n_lower=0, n_upper=9)
    .build()
)
```

## Spacing Definition

Set spacing between array elements:

```python
# Uniform spacing in X and Y
mult = MultBuilder('GRID').spacing(dx=3.0, dy=4.0).build()

# Different spacing per direction
mult = MultBuilder('RECTANGLE').spacing(dx=2.0, dy=5.0, dz=1.0).build()

# Only X direction
mult = MultBuilder('ROW').spacing(dx=1.5).build()
```

## Array Bounds

Define the range of the array:

```python
# 3D array: 3x4x2 = 24 objects
mult = (
    MultBuilder('RACK_ARRAY')
    .spacing(dx=1.0, dy=1.5, dz=2.0)
    .count(i_lower=0, i_upper=2, j_lower=0, j_upper=3, k_lower=0, k_upper=1)
    .build()
)

# 2D array: 5x3 = 15 objects
mult = (
    MultBuilder('WINDOW_GRID')
    .spacing(dx=2.0, dy=1.5)
    .count(i_lower=0, i_upper=4, j_lower=0, j_upper=2)
    .build()
)

# Linear array using sequential numbering
mult = (
    MultBuilder('LINEAR_ARRAY')
    .spacing(dx=0.5)
    .count(n_lower=1, n_upper=10)  # 10 objects
    .build()
)
```

## Initial Offset

Set starting position offset:

```python
# Start array at specific position
mult = (
    MultBuilder('OFFSET_ARRAY')
    .spacing(dx=2.0, dy=2.0)
    .offset(dx0=1.0, dy0=1.0, dz0=0.0)  # Start at (1,1,0)
    .count(i_lower=0, i_upper=2, j_lower=0, j_upper=2)
    .build()
)
```

## Skip Ranges

Create gaps in arrays:

```python
# Array with missing elements
mult = (
    MultBuilder('SPARSE_ARRAY')
    .spacing(dx=1.0, dy=1.0)
    .count(i_lower=0, i_upper=9, j_lower=0, j_upper=9)
    .skip(i_lower_skip=3, i_upper_skip=3)  # Skip column 3
    .skip(j_lower_skip=5, j_upper_skip=7)  # Skip rows 5-7
    .build()
)
```

## Incremental XB Offsets

For complex geometric arrays:

```python
# Custom spacing pattern
mult = (
    MultBuilder('CUSTOM_SPACING')
    .xb_offsets((0.5, 1.0, 0.0, 0.8, 0.0, 0.0))  # dx_min, dx_max, dy_min, dy_max, dz_min, dz_max
    .count(n_lower=0, n_upper=4)
    .build()
)
```

## Usage Examples

### Storage Racks

```python
# Warehouse storage racks
rack_array = (
    MultBuilder('STORAGE_RACKS')
    .spacing(dx=3.0, dy=5.0)  # 3m between aisles, 5m between rows
    .count(i_lower=0, i_upper=9, j_lower=0, j_upper=3)  # 10x4 = 40 racks
    .build()
)

# Individual rack definition
single_rack = Obstruction(xb=Bounds3D.of(0, 1, 0, 2, 0, 4), surf_id='STEEL', mult_id='STORAGE_RACKS')
```

### Window Arrays

```python
# Building facade windows
window_grid = (
    MultBuilder('FACADE_WINDOWS')
    .spacing(dx=2.5, dy=2.0)  # Window spacing
    .offset(dx0=0.5, dy0=0.0, dz0=1.0)  # Position on wall
    .count(i_lower=0, i_upper=7, j_lower=0, j_upper=4)  # 8x5 windows
    .build()
)

# Window definition
window = HoleBuilder('WINDOW').bounds(xb=Bounds3D.of(0, 0.1, 0, 1.5, 0, 1.2)).mult_id('FACADE_WINDOWS').build()
```

### Sprinkler Systems

```python
# Ceiling sprinkler grid
sprinkler_array = (
    MultBuilder('SPRINKLERS')
    .spacing(dx=3.0, dy=3.0)  # 3m x 3m grid
    .offset(dx0=1.5, dy0=1.5, dz0=2.9)  # Center of tiles
    .count(i_lower=0, i_upper=9, j_lower=0, j_upper=9)  # 10x10 grid
    .build()
)

# Sprinkler definition
sprinkler = Device(xb=Bounds3D.of(0, 0, 0, 0, 0, 0), prop_id='SPRINKLER', mult_id='SPRINKLERS')
```

### Ventilation Grids

```python
# Ceiling vents
vent_array = (
    MultBuilder('CEILING_VENTS')
    .spacing(dx=2.0, dy=2.0)
    .offset(dx0=1.0, dy0=1.0, dz0=2.95)  # Just below ceiling
    .count(i_lower=0, i_upper=4, j_lower=0, j_upper=4)
    .build()
)

# Vent definition
vent = Vent(xb=Bounds3D.of(0, 0.1, 0, 0.1, 0, 0.05), surf_id='EXHAUST', mult_id='CEILING_VENTS')
```

## Complex Arrays

### Non-rectangular Arrays

```python
# Staggered pattern
staggered = (
    MultBuilder('STAGGERED')
    .spacing(dx=2.0, dy=1.5)
    .count(i_lower=0, i_upper=4, j_lower=0, j_upper=5)
    .skip(i_lower_skip=1, i_upper_skip=1, j_lower_skip=2, j_upper_skip=2)
    .skip(i_lower_skip=3, i_upper_skip=3, j_lower_skip=4, j_upper_skip=4)
    .build()
)
```

### Multi-level Arrays

```python
# Multi-floor array
multi_level = (
    MultBuilder('MULTI_FLOOR')
    .spacing(dx=3.0, dy=4.0, dz=3.0)  # Include floor height
    .count(i_lower=0, i_upper=2, j_lower=0, j_upper=3, k_lower=0, k_upper=2)  # 3 floors
    .build()
)
```

## Method Reference

### Spacing
- `spacing(dx=None, dy=None, dz=None)`: Set spacing between elements

### Positioning
- `offset(dx0=0.0, dy0=0.0, dz0=0.0)`: Set initial offset

### Array Size
- `count(i_lower=0, i_upper=0, j_lower=0, j_upper=0, k_lower=0, k_upper=0)`: 3D array bounds
- `count(n_lower=None, n_upper=None)`: Sequential array bounds

### Gaps
- `skip(i_lower_skip=None, i_upper_skip=None, j_lower_skip=None, j_upper_skip=None, k_lower_skip=None, k_upper_skip=None)`: Skip ranges

### Advanced
- `xb_offsets(dxb)`: Custom XB offset pattern

## Validation

The builder validates array parameters:

- At least one spacing direction must be specified (or dxb)
- Array bounds must be valid (upper >= lower)
- Skip ranges must be within array bounds
- XB offsets must be 6-tuple if specified

## Performance Considerations

- Large arrays (1000+ elements) may impact FDS performance
- Consider using multiple smaller MULT namelists for complex patterns
- Skip ranges can help optimize large sparse arrays

## See Also

- [HoleBuilder](hole.md) - Creating arrays of holes
- [VentBuilder](vent.md) - Creating arrays of vents
- [Geometry Guide](../../guide/geometry.md) - Complete geometry usage
