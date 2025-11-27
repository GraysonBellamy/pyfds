# VentBuilder

::: pyfds.builders.vent.VentBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`VentBuilder` creates boundary conditions and openings (`&VENT` namelists) using factory methods.

## Key Features

- **Openings**: Doors, windows, ambient boundaries
- **HVAC**: Supply and exhaust vents
- **Circular/Annular**: Burner geometries
- **Mesh Boundaries**: Domain boundaries

## Factory Methods Pattern

Like `PropBuilder`, `VentBuilder` uses **class methods** (factory pattern):

```python
from pyfds.builders import VentBuilder

# Factory methods (not fluent)
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0)
supply = VentBuilder.hvac_supply(xb=(5, 6, 5, 6, 3, 3), volume_flow=0.5)

# Not: VentBuilder().door()...  # This doesn't exist
```

## Quick Examples

### Openings

```python
# Generic opening to ambient
opening = VentBuilder.opening(
    xb=(5, 5, 2, 4, 0, 2.1),
    id='DOOR'
)

# Door (convenience method)
door = VentBuilder.door(
    x=5.0,
    y_min=2.0,
    y_max=3.0,
    z_min=0.0,    # Optional, default 0.0
    z_max=2.1,    # Optional, default 2.1
    id='DOOR_1'
)

# Window
window = VentBuilder.window(
    x=0.0,
    y_min=1.0,
    y_max=2.0,
    z_min=1.0,
    z_max=1.5,
    id='WINDOW_1'
)
```

### HVAC

```python
# Supply vent (positive flow)
supply = VentBuilder.hvac_supply(
    xb=(5, 6, 5, 6, 3, 3),
    volume_flow=0.5,  # m³/s
    id='SUPPLY_1'
)

# Exhaust vent (negative flow)
exhaust = VentBuilder.hvac_exhaust(
    xb=(0, 1, 0, 1, 3, 3),
    volume_flow=0.3,  # m³/s (automatically negated)
    id='EXHAUST_1'
)
```

### Circular Vents

```python
# Circular burner
burner = VentBuilder.circular_burner(
    center=(0, 0, 0),
    radius=0.5,
    surf_id='FIRE',
    id='BURNER'
)

# Annular (ring) burner
ring = VentBuilder.annular_burner(
    center=(0, 0, 0),
    radius=0.5,
    radius_inner=0.3,
    surf_id='FIRE',
    id='RING_BURNER'
)
```

### Mesh Boundaries

```python
# Open boundary
open_boundary = VentBuilder.mesh_boundary(
    mb='XMIN',
    surf_id='OPEN'
)

# Periodic boundary
periodic = VentBuilder.mesh_boundary(
    mb='XMAX',
    surf_id='PERIODIC',
    mesh_id='MESH_1'  # Optional
)
```

## Opening Types

### Generic Opening

Most flexible - specify exact bounding box:

```python
# Rectangular opening
opening = VentBuilder.opening(
    xb=(5, 5, 2, 4, 0, 2.1),  # (x1, x2, y1, y2, z1, z2)
    id='OPENING_1'
)

# Opening with custom surface
opening = VentBuilder.opening(
    xb=(5, 5, 2, 4, 0, 2.1),
    surf_id='CUSTOM_SURF',
    id='CUSTOM_OPENING'
)
```

### Door

Convenience method for vertical openings:

```python
# Standard door (2.1m height)
door = VentBuilder.door(
    x=5.0,         # X position
    y_min=2.0,     # Y start
    y_max=3.0,     # Y end (1m wide)
    id='DOOR_1'
)

# Custom height door
tall_door = VentBuilder.door(
    x=5.0,
    y_min=2.0,
    y_max=4.0,     # 2m wide
    z_min=0.0,
    z_max=2.5,     # 2.5m tall
    id='TALL_DOOR'
)

# Door on Y face
door_y = VentBuilder.door(
    x=None,        # Not used
    y=10.0,        # Y position
    x_min=2.0,     # Use x_min/x_max for Y-face
    x_max=3.0,
    id='DOOR_Y'
)
```

### Window

Convenience method for elevated openings:

```python
# Window
window = VentBuilder.window(
    x=0.0,         # X position (wall location)
    y_min=1.0,     # Y start
    y_max=2.0,     # Y end (1m wide)
    z_min=1.0,     # Bottom of window (1m high)
    z_max=1.5,     # Top of window (0.5m tall)
    id='WINDOW_1'
)

# Large window
large_window = VentBuilder.window(
    x=0.0,
    y_min=0.0,
    y_max=5.0,     # 5m wide
    z_min=1.0,
    z_max=2.5,     # 1.5m tall
    id='LARGE_WINDOW'
)
```

## HVAC Vents

### Supply Vent

Adds air to the domain:

```python
# Supply vent (positive flow)
supply = VentBuilder.hvac_supply(
    xb=(5, 6, 5, 6, 3, 3),  # 1m x 1m at ceiling
    volume_flow=0.5,        # 0.5 m³/s
    id='SUPPLY_1'
)

# Supply with temperature
supply_temp = VentBuilder.hvac_supply(
    xb=(5, 6, 5, 6, 3, 3),
    volume_flow=0.5,
    id='SUPPLY_TEMP'
)
# Note: Set temperature via surface
sim.surface(id='HVAC', tmp_front=20.0)  # 20°C supply air
```

### Exhaust Vent

Removes air from domain:

```python
# Exhaust vent (negative flow)
exhaust = VentBuilder.hvac_exhaust(
    xb=(0, 1, 0, 1, 3, 3),
    volume_flow=0.3,  # Automatically negated to -0.3
    id='EXHAUST_1'
)

# Already negative input
exhaust = VentBuilder.hvac_exhaust(
    xb=(0, 1, 0, 1, 3, 3),
    volume_flow=-0.3,  # Stays -0.3
    id='EXHAUST_2'
)
```

### Flow Rates

Typical HVAC flow rates:

| Application | Flow Rate (m³/s) | Flow Rate (CFM) |
|-------------|------------------|-----------------|
| Small room | 0.05-0.1 | 100-200 |
| Office | 0.1-0.3 | 200-600 |
| Large room | 0.3-1.0 | 600-2000 |
| Laboratory | 1.0-5.0 | 2000-10000 |

Conversion: 1 m³/s = 2118.88 CFM

```python
# Small room ventilation (200 CFM ≈ 0.094 m³/s)
small = VentBuilder.hvac_supply(xb=(5, 6, 5, 6, 3, 3), volume_flow=0.094, id='SMALL')

# Large room (2000 CFM ≈ 0.944 m³/s)
large = VentBuilder.hvac_supply(xb=(5, 7, 5, 7, 3, 3), volume_flow=0.944, id='LARGE')
```

## Circular and Annular Vents

### Circular Vent

For circular burners or vents:

```python
# Circular burner at origin
burner = VentBuilder.circular_burner(
    center=(0, 0, 0),
    radius=0.5,        # 0.5m radius (1m diameter)
    surf_id='FIRE',
    id='BURNER'
)

# Elevated circular vent
circular = VentBuilder.circular_burner(
    center=(5, 5, 2),
    radius=0.3,
    surf_id='FIRE',
    id='ELEVATED'
)
```

Area calculation:

$$
A = \pi r^2
$$

```python
# 0.5m radius → 0.785 m² area
burner = VentBuilder.circular_burner(center=(0, 0, 0), radius=0.5, surf_id='FIRE')
```

### Annular Vent

Ring-shaped vent (washer):

```python
# Annular burner
ring = VentBuilder.annular_burner(
    center=(0, 0, 0),
    radius=0.5,         # Outer radius
    radius_inner=0.3,   # Inner radius
    surf_id='FIRE',
    id='RING'
)
```

Area calculation:

$$
A = \pi (r_{outer}^2 - r_{inner}^2)
$$

```python
# Outer r=0.5m, inner r=0.3m → 0.503 m² area
ring = VentBuilder.annular_burner(
    center=(0, 0, 0),
    radius=0.5,
    radius_inner=0.3,
    surf_id='FIRE'
)
```

## Mesh Boundaries

### Boundary Types

Control mesh boundaries with surfaces:

```python
# Open boundary (XMIN face)
open_x = VentBuilder.mesh_boundary(mb='XMIN', surf_id='OPEN')

# Periodic boundary (XMAX face)
periodic_x = VentBuilder.mesh_boundary(mb='XMAX', surf_id='PERIODIC')

# Mirror boundary
mirror_y = VentBuilder.mesh_boundary(mb='YMIN', surf_id='MIRROR')

# Solid wall
wall_z = VentBuilder.mesh_boundary(mb='ZMAX', surf_id='WALL')
```

### Boundary Positions

| MB Value | Position | Description |
|----------|----------|-------------|
| XMIN | x = x₁ | Lower X boundary |
| XMAX | x = x₂ | Upper X boundary |
| YMIN | y = y₁ | Lower Y boundary |
| YMAX | y = y₂ | Upper Y boundary |
| ZMIN | z = z₁ | Lower Z boundary (ground) |
| ZMAX | z = z₂ | Upper Z boundary (ceiling) |

### Multi-Mesh Boundaries

For multi-mesh simulations:

```python
# Boundary between meshes
interface = VentBuilder.mesh_boundary(
    mb='XMAX',
    surf_id='OPEN',
    mesh_id='MESH_1'  # Apply to specific mesh
)
```

## Usage in Simulations

### Natural Ventilation

```python
from pyfds import Simulation
from pyfds.builders import VentBuilder

sim = Simulation('natural_vent')
sim.mesh(ijk=(100, 100, 50), xb=(0, 10, 0, 10, 0, 5))

# Openings for natural ventilation
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id='DOOR')
window_1 = VentBuilder.window(x=0.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5, id='WIN_1')
window_2 = VentBuilder.window(x=10.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5, id='WIN_2')

sim.add_vent(door)
sim.add_vent(window_1)
sim.add_vent(window_2)
```

### Mechanical Ventilation

```python
# HVAC system
supply = VentBuilder.hvac_supply(
    xb=(2, 3, 2, 3, 5, 5),
    volume_flow=0.5,
    id='SUPPLY'
)
exhaust = VentBuilder.hvac_exhaust(
    xb=(8, 9, 8, 9, 5, 5),
    volume_flow=0.3,
    id='EXHAUST'
)

sim.add_vent(supply)
sim.add_vent(exhaust)

# Define HVAC surface
sim.surface(id='HVAC', tmp_front=20.0, color='BLUE')
```

### Fire Source

```python
# Circular burner
burner = VentBuilder.circular_burner(
    center=(5, 5, 0),
    radius=0.5,
    surf_id='FIRE',
    id='BURNER'
)
sim.add_vent(burner)

# Fire surface
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
```

### Controlled Opening

```python
# Door with control
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id='AUTO_DOOR')
door.ctrl_id = 'DOOR_CONTROL'  # Reference to &CTRL
sim.add_vent(door)

# Control logic
from pyfds.builders import ControlBuilder
door_ctrl = (
    ControlBuilder('DOOR_CONTROL')
    .time_delay('SMOKE_DET', delay=5.0)
    .build()
)
sim.add_ctrl(door_ctrl)
```

### Periodic Boundaries

```python
# Periodic in X direction
sim.add_vent(VentBuilder.mesh_boundary(mb='XMIN', surf_id='PERIODIC'))
sim.add_vent(VentBuilder.mesh_boundary(mb='XMAX', surf_id='PERIODIC'))

# Open in Y and Z
sim.add_vent(VentBuilder.mesh_boundary(mb='YMIN', surf_id='OPEN'))
sim.add_vent(VentBuilder.mesh_boundary(mb='YMAX', surf_id='OPEN'))
sim.add_vent(VentBuilder.mesh_boundary(mb='ZMIN', surf_id='OPEN'))
sim.add_vent(VentBuilder.mesh_boundary(mb='ZMAX', surf_id='OPEN'))
```

## Best Practices

### Use Convenience Methods

```python
# Good: Use door() for standard doors
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id='DOOR')

# Avoid: Manual XB for standard geometries
opening = VentBuilder.opening(xb=(5, 5, 2, 3, 0, 2.1), id='DOOR')  # Less clear
```

### Match HVAC Flow Rates

```python
# Good: Balanced system
supply = VentBuilder.hvac_supply(xb=(2, 3, 2, 3, 5, 5), volume_flow=0.5, id='SUP')
exhaust = VentBuilder.hvac_exhaust(xb=(8, 9, 8, 9, 5, 5), volume_flow=0.5, id='EXH')

# Avoid: Large imbalance (causes pressure issues)
# supply = VentBuilder.hvac_supply(..., volume_flow=1.0)
# exhaust = VentBuilder.hvac_exhaust(..., volume_flow=0.1)  # 10x imbalance
```

### Appropriate Circular Vent Sizing

```python
# Good: Reasonable burner size
burner = VentBuilder.circular_burner(center=(5, 5, 0), radius=0.5, surf_id='FIRE')

# Avoid: Very small (< 1 cell) or very large
# tiny = VentBuilder.circular_burner(center=(5, 5, 0), radius=0.01, ...)  # Too small
# huge = VentBuilder.circular_burner(center=(5, 5, 0), radius=10, ...)    # Too large
```

### Clear Boundary Conditions

```python
# Good: All boundaries specified
for mb in ['XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX']:
    sim.add_vent(VentBuilder.mesh_boundary(mb=mb, surf_id='OPEN'))

# Avoid: Unspecified boundaries (defaults may not be what you want)
```

## Surface ID Reference

Common surface IDs for vents:

| Surface ID | Purpose | Description |
|------------|---------|-------------|
| `'OPEN'` | Ambient opening | Open to atmosphere |
| `'HVAC'` | HVAC vent | Supply/exhaust |
| `'PERIODIC'` | Periodic boundary | Repeating domain |
| `'MIRROR'` | Symmetry | Mirror boundary |
| Custom | Fire/material | User-defined surface |

## Geometry Calculations

### Rectangular Opening Area

$$
A = (x_2 - x_1) \times (y_2 - y_1) \quad \text{or} \quad (y_2 - y_1) \times (z_2 - z_1)
$$

```python
# 1m wide, 2.1m tall door
door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0)  # Area = 1 × 2.1 = 2.1 m²

# 1m wide, 0.5m tall window
window = VentBuilder.window(x=0.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5)
# Area = 1 × 0.5 = 0.5 m²
```

### Circular Area

$$
A = \pi r^2
$$

```python
# r=0.5m → A=0.785 m²
burner = VentBuilder.circular_burner(center=(0, 0, 0), radius=0.5, surf_id='FIRE')
```

### Annular Area

$$
A = \pi (r_{outer}^2 - r_{inner}^2)
$$

```python
# r_outer=0.5m, r_inner=0.3m → A=0.503 m²
ring = VentBuilder.annular_burner(center=(0, 0, 0), radius=0.5, radius_inner=0.3, surf_id='FIRE')
```

## See Also

- [User Guide - Boundary Conditions](../../guide/boundaries.md)
- [User Guide - Builders](../../guide/builders.md)
- [User Guide - Fire Sources](../../guide/fire-sources.md)
- [ControlBuilder](control.md) - For controlled vents
