# Move Builder (Planned)

!!! warning "Not Yet Implemented"
    `MoveBuilder` is planned for a future release. For now, use the `Move` namelist class directly.

## Current Usage

```python
from pyfds import Simulation, Move

sim = Simulation(chid="move_example")

# Define movement for geometry
sim.add(Move(
    id="SHIFT",
    x0=5.0,
    y0=0.0,
    z0=2.0
))
```

## Overview

The MOVE namelist defines transformations (translation, rotation, scaling) that can be applied to unstructured geometry created with GEOM. Transformations are referenced by GEOM objects via the `move_id` parameter.

## Basic Usage

```python
from pyfds import Move

# Simple translation
move = Move(
    id='SHIFT',
    x0=5.0,
    y0=0.0,
    z0=2.0
)

# Rotation around Z-axis
move = (
    MoveBuilder('ROTATE_90')
    .rotate(axis=(0, 0, 1), angle=90.0)
    .build()
)

# Combined transformation
move = (
    MoveBuilder('COMPLEX_TRANSFORM')
    .translate(dx=1.0, dy=1.0, dz=0.0)
    .rotate(axis=(0, 0, 1), angle=45.0)
    .scale(sx=2.0, sy=2.0, sz=1.0)
    .build()
)
```

## Translation

Move geometry to a new position:

```python
# Absolute offset
move = MoveBuilder('POSITION').translate(dx=10.0, dy=5.0, dz=2.0).build()

# Negative values for left/down
move = MoveBuilder('LEFT').translate(dx=-3.0).build()

# Only in one direction
move = MoveBuilder('UP').translate(dz=1.5).build()
```

## Rotation

Rotate geometry around an axis:

```python
# 90 degrees around Z-axis (counter-clockwise)
move = MoveBuilder('ROT90').rotate(axis=(0, 0, 1), angle=90.0).build()

# 180 degrees around Y-axis
move = MoveBuilder('FLIP').rotate(axis=(0, 1, 0), angle=180.0).build()

# 45 degrees around arbitrary axis
move = MoveBuilder('TILT').rotate(axis=(1, 1, 0), angle=45.0).build()

# Negative angle for clockwise
move = MoveBuilder('CW_ROTATE').rotate(axis=(0, 0, 1), angle=-90.0).build()
```

## Scaling

Change the size of geometry:

```python
# Uniform scaling
move = MoveBuilder('DOUBLE_SIZE').scale(sx=2.0, sy=2.0, sz=2.0).build()

# Scale in one direction
move = MoveBuilder('TALL').scale(sz=3.0).build()

# Shrink
move = MoveBuilder('HALF').scale(sx=0.5, sy=0.5, sz=0.5).build()

# Non-uniform scaling
move = MoveBuilder('FLATTEN').scale(sx=2.0, sy=2.0, sz=0.1).build()
```

## Combined Transformations

Transformations are applied in order: translation, then rotation, then scaling:

```python
# Move to position, then rotate, then scale
complex_move = (
    MoveBuilder('COMPLEX')
    .translate(dx=5.0, dy=5.0, dz=0.0)    # Move to (5,5,0)
    .rotate(axis=(0, 0, 1), angle=45.0)   # Rotate 45° around Z
    .scale(sx=1.5, sy=1.5, sz=2.0)        # Scale up
    .build()
)
```

## Usage with Geometry

Transformations are applied to GEOM objects by reference:

```python
from pyfds.builders import GeomBuilder, MoveBuilder

# Create a sphere
sphere = (
    GeomBuilder('BALL')
    .sphere(center=(0, 0, 0), radius=1.0)
    .move_id('BALL_TRANSFORM')  # Reference the transformation
    .build()
)

# Define the transformation
transform = (
    MoveBuilder('BALL_TRANSFORM')
    .translate(dx=10.0, dy=5.0, dz=2.0)
    .rotate(axis=(0, 1, 0), angle=30.0)
    .build()
)

# Add to simulation
sim = Simulation('transformed_geometry')
sim.add_geometry(sphere)
sim.add_geometry(transform)
```

## Multiple Objects

The same transformation can be applied to multiple GEOM objects:

```python
# Define transformation once
position = MoveBuilder('CENTER').translate(dx=5.0, dy=5.0, dz=1.0).build()

# Apply to multiple objects
sphere1 = GeomBuilder('BALL1').sphere(center=(0,0,0), radius=0.5).move_id('CENTER').build()
sphere2 = GeomBuilder('BALL2').sphere(center=(1,0,0), radius=0.5).move_id('CENTER').build()
cylinder = GeomBuilder('POLE').cylinder(origin=(0,0,0), axis=(0,0,1), length=2, radius=0.1).move_id('CENTER').build()
```

## Coordinate System

Transformations follow FDS coordinate conventions:

- **X**: Left-right (increasing to the right)
- **Y**: Front-back (increasing forward)
- **Z**: Bottom-top (increasing upward)

Rotation angles are in degrees, positive counter-clockwise when viewed along the positive axis direction.

## Method Reference

### Translation
- `translate(dx=0.0, dy=0.0, dz=0.0)`: Set translation offsets in meters

### Rotation
- `rotate(axis, angle)`: Set rotation axis vector and angle in degrees

### Scaling
- `scale(sx=1.0, sy=1.0, sz=1.0)`: Set scaling factors (1.0 = no change)

## Validation

The builder validates transformation parameters:

- Axis vectors should be normalized (but will be auto-normalized)
- Angles are in degrees (-360 to 360)
- Scaling factors must be positive
- At least one transformation must be specified

## Examples

### Positioning Objects

```python
# Place objects at specific locations
table = MoveBuilder('TABLE_POS').translate(dx=2.0, dy=3.0, dz=0.0).build()
chair1 = MoveBuilder('CHAIR1_POS').translate(dx=1.5, dy=2.0, dz=0.0).build()
chair2 = MoveBuilder('CHAIR2_POS').translate(dx=2.5, dy=2.0, dz=0.0).build()
```

### Creating Arrays

```python
# Use with MULT for complex arrays
base_transform = MoveBuilder('ARRAY_BASE').translate(dx=1.0, dy=1.0, dz=0.0).build()
# MULT namelist handles the array replication
```

### Animation Setup

```python
# Transformations can be combined with CTRL for moving objects
moving_object = (
    MoveBuilder('MOVING')
    .translate(dx=5.0, dy=5.0, dz=0.0)
    # Position updated by control logic in FDS
)
```

## See Also

- [GeomBuilder](geom.md) - Creating geometry to transform
- [Geometry Guide](../../guide/geometry.md) - Complete geometry usage
- [ControlBuilder](control.md) - Dynamic transformations
