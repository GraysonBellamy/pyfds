# MaterialBuilder

::: pyfds.builders.material.MaterialBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`MaterialBuilder` creates material definitions (`&MATL` namelists) with thermal properties and pyrolysis reactions.

## Key Features

- **Simple Materials**: Constant thermal properties
- **Temperature-Dependent**: Properties via RAMP references
- **Multi-Reaction Pyrolysis**: Complex material decomposition
- **Predefined Materials**: Common building materials

## Quick Examples

### Simple Material

```python
from pyfds.builders import MaterialBuilder

# Material with constant properties
wood = (
    MaterialBuilder('PINE')
    .density(500)                    # kg/m³
    .thermal_conductivity(0.13)      # W/(m·K)
    .specific_heat(2.5)              # kJ/(kg·K)
    .emissivity(0.9)                 # 0-1
    .build()
)
```

### Temperature-Dependent Properties

```python
# Create ramp first
k_ramp = RampBuilder('STEEL_K').temperature_table({
    20: 45.8, 100: 43.3, 200: 40.7, 400: 36.4
}).build()
sim.add_ramp(k_ramp)

# Use ramp in material
steel = (
    MaterialBuilder('STEEL')
    .density(7850)
    .thermal_conductivity_ramp('STEEL_K')  # Reference ramp
    .specific_heat(0.46)
    .emissivity(0.7)
    .build()
)
```

### Pyrolysis Material (Single Reaction)

```python
# Simple decomposition
foam = (
    MaterialBuilder('FOAM')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(
        a=1e10,                      # Pre-exponential factor (1/s)
        e=80000,                     # Activation energy (J/mol)
        heat_of_reaction=1000,       # Heat of reaction (kJ/kg)
        product_species='FUEL_VAPOR' # Product species ID
    )
    .build()
)
```

### Multi-Reaction Pyrolysis

```python
# Complex decomposition with multiple pathways
polymer = (
    MaterialBuilder('POLYURETHANE')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    # First reaction: produces fuel vapor
    .add_pyrolysis_reaction(
        a=1e10,
        e=80000,
        heat_of_reaction=1000,
        product_species='FUEL_VAPOR'
    )
    # Second reaction: produces char residue
    .add_pyrolysis_reaction(
        a=5e8,
        e=120000,
        heat_of_reaction=1500,
        residue_material='CHAR'
    )
    .build()
)
```

### Predefined Materials

```python
# Use predefined common materials
concrete = MaterialBuilder.concrete()
gypsum = MaterialBuilder.gypsum()
steel = MaterialBuilder.steel()
aluminum = MaterialBuilder.aluminum()
brick = MaterialBuilder.brick()
wood = MaterialBuilder.wood()

sim.add_material(concrete)
sim.add_material(steel)
```

## Available Predefined Materials

| Material | ρ (kg/m³) | k (W/(m·K)) | c (kJ/(kg·K)) | ε |
|----------|-----------|-------------|---------------|---|
| Concrete | 2400 | 1.6 | 0.88 | 0.9 |
| Gypsum | 930 | 0.48 | 1.09 | 0.9 |
| Steel | 7850 | 45.8 | 0.46 | 0.7 |
| Aluminum | 2700 | 237 | 0.90 | 0.2 |
| Brick | 1920 | 0.69 | 0.84 | 0.9 |
| Wood | 500 | 0.13 | 2.5 | 0.9 |
| Fiberglass | 12 | 0.04 | 0.84 | 0.9 |
| Ceramic | 2600 | 1.4 | 0.88 | 0.9 |
| Glass | 2500 | 0.8 | 0.84 | 0.9 |
| Copper | 8900 | 387 | 0.38 | 0.05 |

## Usage in Simulations

### Simple Material in Surface

```python
from pyfds import Simulation

sim = Simulation('wall_heating')

# Add material
wood = MaterialBuilder('WOOD').density(500).thermal_conductivity(0.13).build()
sim.add_material(wood)

# Use in surface
sim.surface(id='WOOD_WALL', matl_id='WOOD', thickness=0.012)

# Apply to geometry
sim.obstruction(xb=(0, 0, 0, 5, 0, 3), surf_id='WOOD_WALL')
```

### Layered Materials

```python
# Create materials for layered assembly
gypsum = MaterialBuilder.gypsum()
insulation = MaterialBuilder.fiberglass_insulation()

sim.add_material(gypsum)
sim.add_material(insulation)

# Layered surface
sim.surface(
    id='WALL',
    matl_id=['GYPSUM', 'INSULATION', 'GYPSUM'],
    thickness=[0.012, 0.10, 0.012]  # Gypsum-insulation-gypsum sandwich
)
```

### Pyrolysis with Combustion

```python
# Pyrolyzing material
polymer = (
    MaterialBuilder('POLYMER')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(
        a=1e10,
        e=80000,
        heat_of_reaction=1000,
        product_species='FUEL_VAPOR'
    )
    .build()
)
sim.add_material(polymer)

# Combustion reaction for fuel vapor
reac = ReactionBuilder().fuel('PROPANE').build()
sim.add_reaction(reac)

# Surface with pyrolysis
sim.surface(id='FOAM', matl_id='POLYMER', thickness=0.05)
```

### Structured Pyrolysis API

The `MaterialBuilder` provides a cleaner, more maintainable API for defining pyrolysis reactions using structured data classes.

#### Single-Step Pyrolysis

```python
from pyfds.core.namelists.pyrolysis import PyrolysisReaction, PyrolysisProduct

# Method 1: Arrhenius kinetics (explicit A and E)
wood = (
    MaterialBuilder('WOOD')
    .density(500)
    .thermal_conductivity(0.13)
    .specific_heat(2.5)
    .add_reaction(
        PyrolysisReaction(
            a=1e10,                    # Pre-exponential factor [1/s]
            e=100000,                  # Activation energy [kJ/kmol]
            heat_of_reaction=500,      # Optional, defaults to 0.0 [kJ/kg]
            products=[
                PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
            ]
        )
    )
    .build()
)

# Method 2: Simplified with REFERENCE_TEMPERATURE and PYROLYSIS_RANGE
wood_simple = (
    MaterialBuilder('WOOD_SIMPLE')
    .density(500)
    .thermal_conductivity(0.13)
    .specific_heat(2.5)
    .add_reaction(
        PyrolysisReaction(
            reference_temperature=300,  # Peak reaction temperature [°C]
            pyrolysis_range=80,         # Temperature width [°C]
            heat_of_reaction=500,
            products=[
                PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
            ]
        )
    )
    .build()
)

# Method 3: From TGA with known mass loss rate
tga_wood = (
    MaterialBuilder('TGA_WOOD')
    .density(500)
    .thermal_conductivity(0.13)
    .specific_heat(2.5)
    .add_reaction(
        PyrolysisReaction(
            reference_temperature=300,  # Peak temperature [°C]
            reference_rate=0.002,       # Normalized mass loss rate [1/s]
            heating_rate=5.0,           # TGA heating rate [K/min]
            heat_of_reaction=500,
            products=[
                PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
            ]
        )
    )
    .build()
)
```

#### Multi-Step Pyrolysis

```python
# Complex material with multiple decomposition pathways
polyurethane = (
    MaterialBuilder('POLYURETHANE')
    .density(40)
    .thermal_conductivity(0.04)
    .specific_heat(1.5)
    .add_reaction(
        PyrolysisReaction(
            reference_temperature=100,  # Moisture evaporation
            pyrolysis_range=20,
            heat_of_reaction=2260,      # Heat of vaporization
            products=[PyrolysisProduct(spec_id="WATER_VAPOR", nu_spec=0.05)]
        )
    )
    .add_reaction(
        PyrolysisReaction(
            a=1e12, e=150000,          # Primary pyrolysis
            heat_of_reaction=800,
            products=[
                PyrolysisProduct(spec_id="FUEL_GAS", nu_spec=0.70),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.15),
            ]
        )
    )
    .add_reaction(
        PyrolysisReaction(
            a=1e8, e=120000,           # Char oxidation
            n_o2=1.0,                  # Heterogeneous oxidation
            heat_of_reaction=-30000,   # Exothermic
            products=[
                PyrolysisProduct(spec_id="CARBON_DIOXIDE", nu_spec=0.08),
                PyrolysisProduct(matl_id="ASH", nu_matl=0.02),
            ]
        )
    )
    .build()
)
```

#### Advanced Reaction Parameters

```python
# Reaction with custom kinetics and advanced parameters
plywood = (
    MaterialBuilder('PLYWOOD')
    .density(545)
    .thermal_conductivity(0.12)
    .specific_heat(1.2)
    .add_reaction(
        a=2.5e11,
        e=148000,
        n_s=0.9,                    # Reaction order (slightly less than 1)
        n_t=0.5,                    # Temperature exponent
        gas_diffusion_depth=0.001,  # Gas diffusion length scale [m]
        max_reaction_rate=100.0,    # Maximum reaction rate [kg/(m³·s)]
        heat_of_reaction=420,
        products=[
            {"spec_id": "CELLULOSE", "nu_spec": 0.82},
            {"matl_id": "PLYWOOD_CHAR", "nu_matl": 0.18},
        ]
    )
    .build()
)
```

## Pyrolysis Kinetics

### Arrhenius Equation

Pyrolysis rate is governed by the Arrhenius equation:

$$
k = A e^{-E_a / RT}
$$

where:
- $A$ = pre-exponential factor (1/s)
- $E_a$ = activation energy (J/mol)
- $R$ = gas constant (8.314 J/(mol·K))
- $T$ = temperature (K)

### Parameters

When adding pyrolysis reactions with `PyrolysisReaction`:

#### Kinetic Parameters (Mutually Exclusive)

**Method 1: Arrhenius Kinetics**
- **`a`**: Pre-exponential factor [1/s] - typically 10⁸ to 10¹² for polymers
- **`e`**: Activation energy [kJ/kmol] - typically 80,000 to 200,000 for polymers

**Method 2: Simplified with Reference Rate**
- **`reference_temperature`**: Peak reaction temperature [°C]
- **`reference_rate`**: Normalized mass loss rate at peak [1/s]
- **`heating_rate`**: TGA heating rate [K/min] (default: 5.0)

**Method 3: Simplified with Temperature Range**
- **`reference_temperature`**: Peak reaction temperature [°C]
- **`pyrolysis_range`**: Temperature width of reaction [°C] (default: 80.0)
- **`heating_rate`**: TGA heating rate [K/min] (default: 5.0)

**Method 4: Auto-Derive**
- **`reference_temperature`**: Peak reaction temperature [°C] only

#### Other Parameters

- **`heat_of_reaction`**: Heat absorbed (+) or released (-) [kJ/kg] (optional, default: 0.0)
- **`n_s`**: Reaction order (default: 1.0)
- **`n_t`**: Temperature exponent (default: 0.0)
- **`n_o2`**: Oxygen reaction order (default: 0.0)
- **`gas_diffusion_depth`**: Gas diffusion length scale [m] (optional)
- **`max_reaction_rate`**: Maximum reaction rate limit [kg/(m³·s)] (optional)
- **`products`**: List of `PyrolysisProduct` objects

### Typical Values

| Material | A (1/s) | E (J/mol) | ΔH (kJ/kg) |
|----------|---------|-----------|------------|
| Wood | 1×10¹⁰ | 80,000 | 1000 |
| Polyurethane | 1×10¹⁰ | 80,000 | 1000 |
| PMMA | 5×10⁸ | 120,000 | 1500 |
| Polystyrene | 3×10⁹ | 100,000 | 1200 |

## Property Methods

### Constant Properties

```python
material = (
    MaterialBuilder('TEST')
    .density(1000)                      # Required
    .thermal_conductivity(1.0)          # W/(m·K)
    .specific_heat(1.0)                 # kJ/(kg·K)
    .emissivity(0.9)                    # 0-1, default: 0.9
    .absorption_coefficient(50000)      # 1/m, default: 50000
    .reference_temperature(20)          # °C
    .build()
)
```

### Temperature-Dependent Properties

```python
material = (
    MaterialBuilder('TEST')
    .density(1000)
    .thermal_conductivity_ramp('K_RAMP')    # Reference to &RAMP
    .specific_heat_ramp('CP_RAMP')          # Reference to &RAMP
    .build()
)
```

### Overriding

Setting a ramp overrides a constant value:

```python
material = (
    MaterialBuilder('TEST')
    .density(1000)
    .thermal_conductivity(1.0)         # Set constant
    .thermal_conductivity_ramp('K')    # Override with ramp
    .build()
)
# Result: conductivity=None, conductivity_ramp='K'
```

## Validation

The builder validates:

- **Density is required** - All materials must have density
- **Either constant or ramp** - Can't have both for same property
- **Pyrolysis reactions** - Valid parameters for each reaction

## Predefined Library

```python
from pyfds.builders.libraries import CommonMaterials

# All predefined materials
concrete = CommonMaterials.concrete()
gypsum = CommonMaterials.gypsum()
steel = CommonMaterials.steel()
aluminum = CommonMaterials.aluminum()
brick = CommonMaterials.brick()
wood = CommonMaterials.wood()
fiberglass = CommonMaterials.fiberglass_insulation()
ceramic = CommonMaterials.ceramic()
glass = CommonMaterials.glass()
copper = CommonMaterials.copper()
```

## Best Practices

### Use Predefined When Available

```python
# Good
concrete = MaterialBuilder.concrete()

# Avoid
concrete = MaterialBuilder('CONCRETE').density(2400).thermal_conductivity(1.6)...
```

### Layer Materials Appropriately

```python
# Good: Thin surface materials
sim.surface(id='WALL', matl_id='GYPSUM', thickness=0.012)

# Avoid: Very thick single layer (use multiple cells instead)
sim.surface(id='WALL', matl_id='GYPSUM', thickness=1.0)  # Too thick
```

### Validate Pyrolysis Parameters

```python
# Good: Reasonable Arrhenius parameters
.add_pyrolysis_reaction(a=1e10, e=80000, heat_of_reaction=1000, ...)

# Avoid: Unrealistic values
.add_pyrolysis_reaction(a=1e50, e=10, heat_of_reaction=1e6, ...)
```

## See Also

- [User Guide - Materials & Surfaces](../../guide/materials-surfaces.md)
- [User Guide - Builders](../../guide/builders.md)
- [RampBuilder](ramp.md) - For temperature-dependent properties
