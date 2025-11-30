# HoleBuilder API Reference

The `HoleBuilder` provides a fluent interface for creating holes that carve openings through obstructions.

## Overview

The HOLE namelist creates openings in solid obstructions (OBST namelists). Holes can represent doors, windows, vents, or other openings. Holes can be static or controlled to open/close dynamically.

## Basic Usage

```python
from pyfds.builders import HoleBuilder

# Simple door opening
door = (
    HoleBuilder('DOOR')
    .bounds(xb=Bounds3D.of(5, 5.1, 2, 4, 0, 2.1))
    .build()
)

# Window with control
window = (
    HoleBuilder('WINDOW')
    .bounds(xb=Bounds3D.of(0, 0.1, 1, 2, 1.5, 2.5))
    .controlled_by('WINDOW_CTRL')
    .color_when_closed('GRAY')
    .build()
)
```

## Bounds Definition

Define the 3D volume of the hole:

```python
# Rectangular hole
hole = HoleBuilder('OPENING').bounds(xb=Bounds3D.of(xmin, xmax, ymin, ymax, zmin, zmax)).build()

# Door dimensions
door = HoleBuilder('DOOR').bounds(xb=Bounds3D.of(5.0, 5.1, 2.0, 4.0, 0.0, 2.1)).build()

# Window
window = HoleBuilder('WINDOW').bounds(xb=Bounds3D.of(0.0, 0.1, 1.0, 2.0, 1.5, 2.5)).build()
```

## Control Logic

Make holes open/close based on conditions:

```python
# Controlled by device
hole = (
    HoleBuilder('VENT')
    .bounds(xb=Bounds3D.of(2, 3, 2, 3, 3, 3))
    .controlled_by('VENT_CTRL')
    .build()
)

# Controlled by multiple devices
hole = (
    HoleBuilder('EMERGENCY_EXIT')
    .bounds(xb=Bounds3D.of(8, 8.1, 0, 3, 0, 2.2))
    .controlled_by('SMOKE_DETECTOR')
    .also_controlled_by('HEAT_DETECTOR')
    .build()
)
```

## Visualization

Control appearance when hole is closed:

```python
# Colored when closed
hole = (
    HoleBuilder('DOOR')
    .bounds(xb=Bounds3D.of(5, 5.1, 2, 4, 0, 2.1))
    .color_when_closed('WOOD')
    .rgb_when_closed((139, 69, 19))  # Brown
    .transparency_when_closed(1.0)    # Opaque
    .build()
)

# Semi-transparent
hole = (
    HoleBuilder('GLASS_DOOR')
    .bounds(xb=Bounds3D.of(5, 5.1, 2, 4, 0, 2.1))
    .color_when_closed('BLUE')
    .transparency_when_closed(0.7)
    .build()
)
```

## Array Replication

Use MULT to create arrays of holes:

```python
# Single hole definition
window = (
    HoleBuilder('WINDOW_TEMPLATE')
    .bounds(xb=Bounds3D.of(0, 0.1, 1, 2, 1.5, 2.5))
    .mult_id('WINDOW_ARRAY')  # Reference to MULT namelist
    .build()
)

# MULT creates the array
from pyfds.builders import MultBuilder
window_array = (
    MultBuilder('WINDOW_ARRAY')
    .spacing(dx=3.0)  # 3m spacing
    .count(n_lower=0, n_upper=4)  # 5 windows
    .build()
)
```

## Complete Examples

### Residential House

```python
from pyfds.builders import HoleBuilder

# Front door
front_door = (
    HoleBuilder('FRONT_DOOR')
    .bounds(xb=Bounds3D.of(4.9, 5.1, 2.0, 3.0, 0.0, 2.1))
    .color_when_closed('WOOD')
    .build()
)

# Windows
living_room_window = (
    HoleBuilder('LIVING_WINDOW')
    .bounds(xb=Bounds3D.of(1.0, 1.1, 1.0, 2.5, 1.0, 1.8))
    .color_when_closed('GLASS')
    .transparency_when_closed(0.8)
    .build()
)

bedroom_window = (
    HoleBuilder('BEDROOM_WINDOW')
    .bounds(xb=Bounds3D.of(6.0, 6.1, 1.0, 2.0, 1.2, 1.8))
    .color_when_closed('GLASS')
    .transparency_when_closed(0.8)
    .build()
)
```

### Warehouse with Roll-up Doors

```python
# Large roll-up door
roll_up_door = (
    HoleBuilder('ROLL_UP_DOOR')
    .bounds(xb=Bounds3D.of(9.9, 10.1, 2.0, 8.0, 0.0, 4.0))
    .controlled_by('DOOR_CONTROL')
    .color_when_closed('METAL')
    .build()
)

# Small personnel door
personnel_door = (
    HoleBuilder('PERSONNEL_DOOR')
    .bounds(xb=Bounds3D.of(4.9, 5.1, 4.5, 5.5, 0.0, 2.1))
    .color_when_closed('METAL')
    .build()
)
```

### Controlled Ventilation

```python
# Smoke vent that opens automatically
smoke_vent = (
    HoleBuilder('SMOKE_VENT')
    .bounds(xb=Bounds3D.of(2.0, 8.0, 2.0, 8.0, 9.9, 10.1))
    .controlled_by('SMOKE_CONTROL')
    .color_when_closed('BLACK')
    .transparency_when_closed(0.9)
    .build()
)
```

## Method Reference

### Geometry
- `bounds(xb)`: Set hole volume bounds (xmin, xmax, ymin, ymax, zmin, zmax)

### Control
- `controlled_by(ctrl_id)`: Set control ID for activation
- `also_controlled_by(devc_id)`: Additional device control

### Visualization
- `color_when_closed(color)`: Named color when closed
- `rgb_when_closed(rgb)`: RGB color tuple when closed
- `transparency_when_closed(alpha)`: Transparency when closed (0-1)

### Arrays
- `mult_id(mult_id)`: Reference MULT namelist for array replication

## Validation

The builder validates hole parameters:

- Bounds must be valid 3D box (xmax > xmin, etc.)
- RGB values must be 0-255
- Transparency must be 0-1
- At least bounds must be specified

## Integration with Obstructions

Holes only affect OBST namelists that intersect the hole volume:

```python
# Obstruction with hole
wall = Obstruction(xb=Bounds3D.of(0, 10, 0, 0.2, 0, 3), surf_id='WALL')
hole = HoleBuilder('DOOR').bounds(xb=Bounds3D.of(4.9, 5.1, 0, 0.2, 0, 2.1)).build()

# Result: wall with door opening
```

## See Also

- [Obstruction Documentation](../../guide/geometry.md) - Creating solid geometry
- [ControlBuilder](control.md) - Control logic for dynamic holes
- [MultBuilder](mult.md) - Creating arrays of holes
- [VentBuilder](vent.md) - Alternative for boundary vents
