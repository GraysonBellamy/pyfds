# Burn-Away Guide

This guide covers burn-away modeling for combustible obstructions and furniture, including BULK_DENSITY usage, interior surface exposure, and burn-away physics.

## Overview

Burn-away modeling allows solid materials to be consumed by fire, exposing interior surfaces or removing obstructions entirely. This is crucial for:

- Furniture fire modeling
- Compartment burn-through
- Structural element failure
- Realistic fire spread

## Basic Burn-Away Setup

### Simple Burn-Away Obstruction

```python
from pyfds import Simulation
from pyfds.core.namelists import Obstacle, Surface, Material

# Create simulation
sim = Simulation(chid="burn_away", title="Burn-Away Example")

# Define combustible material
wood = Material(
    id="WOOD",
    density=500.0,
    conductivity=0.13,
    specific_heat=2.5,
    n_reactions=1,
    a=[1e10], e=[100000], heat_of_reaction=[1800],
    spec_id=["WOOD_GAS"], nu_spec=[0.75],
    matl_id=["CHAR"], nu_matl=[0.25]
)

# Burn-away surface
burning_surface = Surface(
    id="BURNING_WOOD",
    matl_id="WOOD",
    thickness=0.02,  # Surface thickness (m)
    burn_away=True   # Enable burn-away
)

# Combustible obstruction
furniture = Obstacle(
    id="CHAIR",
    xb=[1.0, 2.0, 1.0, 2.0, 0.0, 0.8],  # Chair dimensions
    surf_id="BURNING_WOOD",
    bulk_density=200.0  # Mass per unit volume (kg/m³)
)

sim.material_mgr.add_material(wood)
sim.material_mgr.add_surface(burning_surface)
sim.geometry.add_obstacle(furniture)
```

## BULK_DENSITY Parameter

### Purpose

`BULK_DENSITY` controls how much mass is available for burning:

- **Without BULK_DENSITY**: Only surface material burns
- **With BULK_DENSITY**: Interior mass burns as surface is consumed

```python
# Surface-only burning (default)
surface_only = Obstacle(
    id="THIN_PANEL",
    xb=[0, 1, 0, 1, 0, 0.01],  # 1cm thick
    surf_id="BURNING_SURFACE"
    # No bulk_density - only surface burns
)

# Bulk burning
thick_furniture = Obstacle(
    id="THICK_COUCH",
    xb=[0, 2, 0, 1, 0, 0.3],   # 30cm thick
    surf_id="BURNING_SURFACE",
    bulk_density=100.0  # Burns interior mass too
)
```

### Calculating BULK_DENSITY

```python
# For uniform density materials
material_density = 500.0  # kg/m³ (from MATL)
bulk_density = material_density  # Same as material density

# For porous/fibrous materials
solid_density = 1000.0  # kg/m³
porosity = 0.8         # 80% air
bulk_density = solid_density * (1 - porosity)  # 200 kg/m³
```

## Interior Surface Exposure

### Multi-Layer Surfaces

Expose different materials as outer layers burn away:

```python
# Multi-layer surface
wall = Surface(
    id="COMPOSITE_WALL",
    matl_id=["GYPSUM", "WOOD", "INSULATION"],
    thickness=[0.013, 0.02, 0.1],  # Layer thicknesses (m)
    burn_away=True
)

# Wall obstruction
wall_obst = Obstacle(
    id="WALL",
    xb=[0, 0.2, 0, 5, 0, 2.5],  # Thin wall
    surf_id="COMPOSITE_WALL",
    bulk_density=300.0
)
```

### SURF_ID_Interior

Specify different surface properties for interior:

```python
# Exterior surface (burns)
exterior = Surface(
    id="WOOD_EXTERIOR",
    matl_id="WOOD",
    thickness=0.02,
    burn_away=True
)

# Interior surface (exposed after burning)
interior = Surface(
    id="WOOD_INTERIOR",
    matl_id="WOOD",
    thickness=0.02,
    burn_away=False  # Interior doesn't burn away
)

# Obstruction with different interior
panel = Obstacle(
    id="WOOD_PANEL",
    xb=[0, 1, 0, 1, 0, 0.05],
    surf_id="WOOD_EXTERIOR",
    surf_id_interior="WOOD_INTERIOR",  # Different interior surface
    bulk_density=400.0
)
```

## Advanced Burn-Away Features

### Temperature-Based Delamination

Layers can separate at specific temperatures:

```python
# Delaminating surface
plywood = Surface(
    id="PLYWOOD",
    matl_id=["FACE_VENEER", "CORE", "BACK_VENEER"],
    thickness=[0.003, 0.014, 0.003],
    delamination_tmp=[None, 200.0, None],  # Core delaminates at 200°C
    burn_away=True
)
```

### Density-Based Delamination

Layers can separate based on density changes:

```python
# Density-based delamination
composite = Surface(
    id="COMPOSITE",
    matl_id=["RESIN", "FIBER", "RESIN"],
    thickness=[0.002, 0.016, 0.002],
    delamination_density=[None, 150.0, None],  # Fiber layer delaminates
    burn_away=True
)
```

## Burn-Away Physics

### Burning Rate Control

The burning rate depends on:

1. **Heat Flux**: Higher flux → faster burning
2. **Material Properties**: Pyrolysis kinetics
3. **Bulk Density**: Available fuel mass
4. **Geometry**: Surface area and thickness

### Mass Loss Calculation

```python
# FDS calculates mass loss rate as:
# m_dot = A * rho_b * pyrolysis_rate
#
# Where:
# A = surface area (m²)
# rho_b = bulk density (kg/m³)
# pyrolysis_rate = reaction rate (1/s)
```

### Surface Regression

As material burns:

1. Surface thickness decreases
2. Interior surfaces become exposed
3. Geometry changes affect heat transfer
4. New surfaces ignite if hot enough

## Validation and Best Practices

### Required Parameters

For burn-away surfaces:

- `BURN_AWAY=True` in SURF
- `BULK_DENSITY` in OBST (recommended)
- Valid MATL with pyrolysis parameters

### Parameter Validation

- Thickness must be positive
- BULK_DENSITY should match material density
- Cross-references must be valid

### Common Issues

1. **No burning**: Check pyrolysis parameters in MATL
2. **Too fast burning**: Adjust BULK_DENSITY or kinetics
3. **Unstable burning**: Use appropriate grid resolution
4. **Interior exposure**: Verify SURF_ID_INTERIOR setup

## Examples

### Burning Furniture

```python
# Couch fire
foam = Material(
    id="FOAM",
    density=30.0,
    conductivity=0.04,
    specific_heat=1.5,
    n_reactions=1,
    a=[1e8], e=[80000], heat_of_reaction=[1000],
    spec_id=["FOAM_GAS"], nu_spec=[0.85],
    matl_id=["CHAR"], nu_matl=[0.15]
)

couch_surface = Surface(
    id="COUCH_SURFACE",
    matl_id="FOAM",
    thickness=0.05,
    burn_away=True
)

couch = Obstacle(
    id="COUCH",
    xb=[1, 3, 1, 2, 0, 0.5],  # Couch dimensions
    surf_id="COUCH_SURFACE",
    bulk_density=25.0  # Slightly less than solid density
)
```

### Compartment Burn-Through

```python
# Wall burn-through
drywall = Material(
    id="DRYWALL",
    density=800.0,
    conductivity=0.16,
    specific_heat=0.84,
    n_reactions=1,
    a=[2e10], e=[100000], heat_of_reaction=[500],
    spec_id=["GYPSUM_GAS"], nu_spec=[0.2],
    matl_id=["CALCINED_GYPSUM"], nu_matl=[0.8]
)

wall_surface = Surface(
    id="WALL_SURFACE",
    matl_id="DRYWALL",
    thickness=0.013,  # 1/2 inch drywall
    burn_away=True
)

wall = Obstacle(
    id="GYPSUM_WALL",
    xb=[0, 0.013, 0, 4, 0, 2.5],  # Thin wall
    surf_id="WALL_SURFACE",
    bulk_density=800.0
)
```

### Multi-Layer Panel

```python
# Composite panel with burn-through
panel = Surface(
    id="COMPOSITE_PANEL",
    matl_id=["ALUMINUM", "INSULATION", "STEEL"],
    thickness=[0.001, 0.05, 0.002],  # Metal-insulation-metal
    burn_away=True
)

composite_obst = Obstacle(
    id="COMPOSITE_WALL",
    xb=[0, 0.053, 0, 3, 0, 2],
    surf_id="COMPOSITE_PANEL",
    bulk_density=200.0  # Effective density
)
```

See the examples directory for complete burn-away simulations.
