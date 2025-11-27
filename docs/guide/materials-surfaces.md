# Materials & Surfaces

Define material properties and surface characteristics for your FDS simulations.

## Overview

Surfaces (SURF) define boundary conditions and material properties. Materials (MATL) define the physical properties of solids.

```python
# Simple fire surface
sim.surface(id='FIRE', hrrpua=1000.0)

# Material surface
sim.material(id='WOOD', conductivity=0.12, specific_heat=1.0, density=500.0)
sim.surface(id='WOOD_WALL', matl_id='WOOD', thickness=0.02)
```

## Surface Types

### Fire Surfaces

For combustion sources:

```python
sim.surface(
    id='BURNER',
    hrrpua=1000.0,      # Heat release rate per unit area (kW/m²)
    color='RED'          # Visualization color
)
```

See [Fire Sources](fire-sources.md) for detailed fire surface information.

### Material Surfaces

For walls, floors, objects:

```python
# Define material
sim.material(
    id='CONCRETE',
    conductivity=1.8,    # Thermal conductivity (W/m·K)
    specific_heat=0.88,  # Specific heat (kJ/kg·K)
    density=2400.0       # Density (kg/m³)
)

# Create surface with material
sim.surface(
    id='CONCRETE_WALL',
    matl_id='CONCRETE',
    thickness=0.2  # meters
)
```

### Predefined Surfaces

FDS includes predefined surfaces:

- `INERT` - Adiabatic, non-reactive (default)
- `OPEN` - Open to ambient
- `MIRROR` - Symmetry plane
- `PERIODIC` - Periodic boundary
- `HVAC` - HVAC vent

```python
# Use predefined
sim.obstruction(xb=(0, 1, 0, 1, 0, 1), surf_id='INERT')
```

## Material Properties

### Basic Material

```python
sim.material(
    id='MATERIAL_NAME',
    conductivity=0.5,    # W/m·K
    specific_heat=1.0,   # kJ/kg·K
    density=500.0        # kg/m³
)
```

### Common Materials

```python
# Gypsum board
sim.material(
    id='GYPSUM',
    conductivity=0.48,
    specific_heat=0.84,
    density=1440.0
)

# Wood (generic)
sim.material(
    id='WOOD',
    conductivity=0.12,
    specific_heat=1.0,
    density=500.0
)

# Steel
sim.material(
    id='STEEL',
    conductivity=45.8,
    specific_heat=0.46,
    density=7850.0
)

# Concrete
sim.material(
    id='CONCRETE',
    conductivity=1.8,
    specific_heat=0.88,
    density=2400.0
)

# Insulation
sim.material(
    id='INSULATION',
    conductivity=0.04,
    specific_heat=0.84,
    density=100.0
)
```

## Multi-Layer Surfaces

### Composite Wall

```python
# Define each material layer
sim.material(id='GYPSUM', conductivity=0.48, specific_heat=0.84, density=1440.0)
sim.material(id='INSULATION', conductivity=0.04, specific_heat=0.84, density=100.0)

# Create surface with multiple layers
sim.surface(
    id='COMPOSITE_WALL',
    matl_id=['GYPSUM', 'INSULATION', 'GYPSUM'],  # Inside to outside
    thickness=[0.0127, 0.10, 0.0127]             # Thickness of each layer (m)
)
```

## Temperature-Dependent Properties

### Conductivity vs Temperature

Using RAMP for temperature-dependent conductivity:

```python
# Conductivity ramp (temperature in °C, conductivity factor)
sim.ramp(
    id='K_RAMP',
    t=[20, 100, 200, 400, 600],          # Temperature (°C)
    f=[1.0, 1.1, 1.3, 1.6, 2.0]          # Conductivity multiplier
)

sim.material(
    id='TEMP_DEP_MATL',
    conductivity=0.5,
    specific_heat=1.0,
    density=1000.0,
    conductivity_ramp='K_RAMP'
)
```

## Surface Colors

For visualization in Smokeview:

```python
# Named colors
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
sim.surface(id='WALL', matl_id='CONCRETE', thickness=0.2, color='GRAY')

# RGB values (0-255)
sim.surface(id='CUSTOM', matl_id='WOOD', thickness=0.02, rgb=(139, 69, 19))
```

Common colors: `RED`, `ORANGE`, `YELLOW`, `GREEN`, `BLUE`, `GRAY`, `BLACK`, `WHITE`

## Heat Transfer Properties

### Emissivity

```python
sim.surface(
    id='BLACK_SURFACE',
    matl_id='STEEL',
    thickness=0.005,
    emissivity=0.95  # 0.0 to 1.0
)

sim.surface(
    id='SHINY_SURFACE',
    matl_id='ALUMINUM',
    thickness=0.003,
    emissivity=0.09  # Polished aluminum
)
```

### Backing Condition

```python
# Insulated backing
sim.surface(
    id='INSULATED_WALL',
    matl_id='CONCRETE',
    thickness=0.2,
    backing='INSULATED'  # No heat loss through back
)

# Exposed backing (default)
sim.surface(
    id='EXPOSED_WALL',
    matl_id='CONCRETE',
    thickness=0.2,
    backing='EXPOSED'  # Heat loss to ambient
)
```

## Complete Examples

### Basic Wall Material

```python
from pyfds import Simulation

sim = Simulation(chid='wall_material')
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Define gypsum material
sim.material(
    id='GYPSUM',
    conductivity=0.48,
    specific_heat=0.84,
    density=1440.0
)

# Create surface with gypsum
sim.surface(
    id='GYPSUM_WALL',
    matl_id='GYPSUM',
    thickness=0.0127,  # 1/2 inch
    color='WHITE'
)

# Apply to walls
sim.obstruction(xb=(0, 0.2, 0, 5, 0, 2.5), surf_id='GYPSUM_WALL')

sim.write('wall_material.fds')
```

### Multi-Layer Wall

```python
sim = Simulation(chid='composite_wall')
sim.mesh(ijk=(50, 40, 25), xb=(0, 5, 0, 4, 0, 2.5))

# Define materials
sim.material(id='GYPSUM', conductivity=0.48, specific_heat=0.84, density=1440.0)
sim.material(id='INSULATION', conductivity=0.04, specific_heat=0.84, density=100.0)
sim.material(id='WOOD_STUD', conductivity=0.12, specific_heat=1.0, density=500.0)

# Composite wall: gypsum-insulation-gypsum
sim.surface(
    id='EXTERIOR_WALL',
    matl_id=['GYPSUM', 'INSULATION', 'GYPSUM'],
    thickness=[0.0127, 0.089, 0.0127],  # Total: ~0.115m
    color='TAN'
)

# Apply to wall
sim.obstruction(xb=(0, 0.2, 0, 4, 0, 2.5), surf_id='EXTERIOR_WALL')

sim.write('composite_wall.fds')
```

### Temperature-Dependent Material

```python
sim = Simulation(chid='temp_dependent')
sim.mesh(ijk=(30, 30, 15), xb=(0, 3, 0, 3, 0, 1.5))

# Conductivity increases with temperature
sim.ramp(
    id='K_VS_T',
    t=[20, 200, 400, 600],
    f=[1.0, 1.2, 1.5, 2.0]
)

# Material with temperature-dependent conductivity
sim.material(
    id='TEMP_MATL',
    conductivity=0.5,
    specific_heat=1.0,
    density=1000.0,
    conductivity_ramp='K_VS_T'
)

sim.surface(
    id='TEMP_SURF',
    matl_id='TEMP_MATL',
    thickness=0.05
)

# Fire and wall
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(1, 2, 1, 2, 0, 0.1), surf_id='FIRE')
sim.obstruction(xb=(0, 0.1, 0, 3, 0, 1.5), surf_id='TEMP_SURF')

sim.write('temp_dependent.fds')
```

## Best Practices

### 1. Use Realistic Material Properties

Reference material property databases:
- SFPE Handbook
- Engineering Toolbox
- NIST material database

### 2. Match Physical Thickness

```python
# Actual 1/2" gypsum board
sim.surface(
    id='GYPSUM_WALL',
    matl_id='GYPSUM',
    thickness=0.0127  # 0.5 inches = 0.0127 m
)
```

### 3. Consider Thermal Penetration

Thickness should be greater than thermal penetration depth:
```python
# δ ≈ √(α × t) where α = k/(ρ×c)
# For 600s simulation and concrete:
# α = 1.8 / (2400 × 0.88) ≈ 8.5×10⁻⁷ m²/s
# δ ≈ √(8.5×10⁻⁷ × 600) ≈ 0.023 m

# Use thickness > δ
sim.surface(
    id='CONCRETE_WALL',
    matl_id='CONCRETE',
    thickness=0.05  # > 0.023 m
)
```

### 4. Document Sources

```python
# Clear documentation
# Material properties from: SFPE Handbook, 5th Ed., Table X.Y
sim.material(
    id='CONCRETE',
    conductivity=1.8,     # W/m·K
    specific_heat=0.88,   # kJ/kg·K
    density=2400.0        # kg/m³
)
```

## Common Material Properties

| Material | k (W/m·K) | c (kJ/kg·K) | ρ (kg/m³) |
|----------|-----------|-------------|-----------|
| Gypsum | 0.48 | 0.84 | 1440 |
| Concrete | 1.8 | 0.88 | 2400 |
| Wood (oak) | 0.17 | 1.7 | 700 |
| Wood (pine) | 0.12 | 1.0 | 500 |
| Steel | 45.8 | 0.46 | 7850 |
| Aluminum | 237 | 0.90 | 2700 |
| Glass | 0.76 | 0.84 | 2500 |
| Brick | 0.69 | 0.84 | 1920 |
| Insulation | 0.04 | 0.84 | 100 |

## Pyrolysis Materials

Materials can thermally decompose (pyrolyze) producing gaseous fuel vapors and solid residues.

### Single-Reaction Pyrolysis

Simple material that decomposes into one product:

```python
from pyfds.builders import MaterialBuilder

# PMMA pyrolysis
pmma = (
    MaterialBuilder("PMMA")
    .density(1200)
    .thermal_conductivity(0.19)
    .specific_heat(1.4)
    .with_pyrolysis_product("MMA_VAPOR", yield_fraction=1.0)
    .with_heat_of_combustion(25000)
    .build()
)

sim.add_material(pmma)
```

### Multi-Reaction Pyrolysis

Materials with complex decomposition (e.g., wood → vapor + char):

```python
# Wood decomposition
wood = (
    MaterialBuilder("WOOD")
    .density(500)
    .thermal_conductivity(0.13)
    .specific_heat(2.5)
    .add_pyrolysis_reaction(
        a=1e10,                      # Pre-exponential factor (1/s)
        e=100000,                    # Activation energy (kJ/kmol)
        heat_of_reaction=1800,       # Heat absorbed (kJ/kg)
        product_species="WOOD_VAPOR" # Gas product
    )
    .add_pyrolysis_reaction(
        a=5e8,
        e=120000,
        heat_of_reaction=500,
        residue_material="CHAR"      # Solid residue
    )
    .build()
)

# Char material (solid residue)
char = (
    MaterialBuilder("CHAR")
    .density(200)
    .thermal_conductivity(0.08)
    .specific_heat(1.0)
    .build()
)

sim.add_material(wood)
sim.add_material(char)
```

### Predefined Materials

Use common materials with validated properties:

```python
# Pre-configured materials
concrete = MaterialBuilder.concrete()
steel = MaterialBuilder.steel()
wood = MaterialBuilder.wood()
gypsum = MaterialBuilder.gypsum()
aluminum = MaterialBuilder.aluminum()
brick = MaterialBuilder.brick()

# Add to simulation
sim.add_material(concrete)
sim.add_material(steel)
```

### Pyrolysis Parameters

**Kinetics:**
- `a`: Pre-exponential factor (1/s) - typical range: 1e8 to 1e12
- `e`: Activation energy (kJ/kmol) - typical range: 80,000 to 200,000
- `heat_of_reaction`: Energy absorbed during pyrolysis (kJ/kg)

**Products:**
- `product_species`: Gas species ID produced
- `residue_material`: Solid material ID remaining
- `yield_fraction`: Fraction of original material yielded (0-1)
- `reaction_order`: Reaction order (default: 1.0)

## Surface Particle Generation

Surfaces can generate particles (e.g., smoke from fires, water from sprinklers):

```python
from pyfds.builders import SurfBuilder

# Fire generating smoke
fire_surf = (
    SurfBuilder("FIRE")
    .with_heat_release(1000.0, ramp_id="fire_growth")
    .with_particle_generation("SMOKE", mass_flux=0.002, nppc=2)
    .with_color("ORANGE")
    .build()
)

# Sprinkler generating water droplets
sprinkler_surf = (
    SurfBuilder("SPRINKLER")
    .as_sprinkler(
        part_id="WATER_DROP",
        mass_flux=0.02,
        median_diameter=0.001,
        velocity=6.0
    )
    .build()
)

sim.add_surface(fire_surf)
sim.add_surface(sprinkler_surf)
```

See [Particles](particles.md) and [Suppression Systems](suppression-systems.md) for details.

## Next Steps

- [Fire Sources](fire-sources.md) - Creating fires
- [Particles](particles.md) - Particle systems
- [Suppression Systems](suppression-systems.md) - Sprinkler systems
- [RAMP Guide](ramps.md) - Temperature-dependent properties
- [Combustion](combustion.md) - Fuel properties

---

[Particles Guide →](particles.md){ .md-button .md-button--primary }
