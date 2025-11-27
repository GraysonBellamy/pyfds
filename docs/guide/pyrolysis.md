# Pyrolysis Modeling Guide

This guide covers the comprehensive pyrolysis modeling capabilities in pyfds, including single-reaction and multi-reaction kinetics, charring materials, and advanced pyrolysis parameters.

## Overview

Pyrolysis is the thermal decomposition of materials in the absence of oxygen, producing gaseous, liquid, and solid products. FDS supports sophisticated pyrolysis modeling through the `MATL` namelist, allowing you to specify:

- Reaction kinetics (Arrhenius parameters)
- Product yields and species
- Multi-reaction pathways
- Temperature-dependent properties
- Advanced parameters for complex behavior

## Basic Pyrolysis Setup

### Single-Reaction Pyrolysis

For simple materials that decompose in a single reaction:

```python
from pyfds import Simulation
from pyfds.core.namelists import Material

# Create simulation
sim = Simulation(chid="simple_pyrolysis", title="Simple Pyrolysis Example")

# Define pyrolyzing material
wood = Material(
    id="WOOD",
    density=500.0,           # kg/m³
    conductivity=0.13,       # W/(m·K)
    specific_heat=2.5,       # kJ/(kg·K)
    # Pyrolysis parameters
    n_reactions=1,
    a=[1e10],                # Pre-exponential factor (1/s)
    e=[100000],              # Activation energy (kJ/kmol)
    heat_of_reaction=[1800], # Heat of pyrolysis (kJ/kg)
    spec_id=["WOOD_GAS"],    # Gaseous products
    nu_spec=[0.75],          # Gas yield
    matl_id=["CHAR"],        # Solid residue
    nu_matl=[0.25]           # Residue yield
)

# Add to simulation
sim.material_mgr.add_material(wood)
```

### Multi-Reaction Pyrolysis

Complex materials may have multiple decomposition pathways:

```python
# Multi-reaction composite material
composite = Material(
    id="COMPOSITE",
    density=800.0,
    conductivity=0.2,
    specific_heat=2.0,
    n_reactions=2,
    # Reaction 1: Primary decomposition
    a=[1e12, 5e8],
    e=[120000, 140000],
    heat_of_reaction=[2000, 500],
    spec_id=[["VOLATILE_1"], ["VOLATILE_2"]],
    nu_spec=[[0.4], [0.15]],
    matl_id=[["CHAR"], ["ASH"]],
    nu_matl=[[0.3], [0.05]]
)

# Each reaction's yields sum to ≤ 1.0
# Reaction 1: 0.4 + 0.3 = 0.7
# Reaction 2: 0.15 + 0.05 = 0.2
```

## Using the MaterialBuilder

The `MaterialBuilder` provides a fluent API for creating materials:

```python
from pyfds.builders import MaterialBuilder

wood = MaterialBuilder("WOOD") \
    .density(500) \
    .thermal_conductivity(0.13) \
    .specific_heat(2.5) \
    .add_pyrolysis_reaction(
        a=1e10, e=100000, heat_of_reaction=1800,
        product_species="WOOD_GAS"
    ) \
    .build()
```

## Advanced Pyrolysis Parameters

### Temperature-Dependent Properties

Use RAMPs for temperature-dependent thermal properties:

```python
from pyfds.core.namelists import Ramp

# Temperature-dependent conductivity
k_ramp = Ramp(
    id="WOOD_K",
    t=[20, 100, 200, 300, 400],
    f=[0.13, 0.15, 0.18, 0.22, 0.25]
)

wood = Material(
    id="WOOD",
    density=500.0,
    conductivity_ramp="WOOD_K",  # Reference the ramp
    specific_heat=2.5,
    # ... pyrolysis parameters
)
```

### Kinetic Parameter Estimation

For TGA-derived parameters:

```python
material = Material(
    id="SAMPLE",
    density=600.0,
    conductivity=0.18,
    specific_heat=2.3,
    # TGA parameters
    pyrolysis_range=50.0,      # Width of mass loss curve (°C)
    heating_rate=10.0,         # TGA heating rate (K/min)
    reference_temperature=300.0,  # Reference temperature (°C)
    # ... other parameters
)
```

### Advanced Reaction Kinetics

```python
advanced_material = Material(
    id="ADVANCED",
    density=1000.0,
    conductivity=0.5,
    specific_heat=1.5,
    n_reactions=1,
    a=[1e8],
    e=[80000],
    heat_of_reaction=[1500],
    # Advanced kinetics
    n_t=[1.5],                 # Temperature exponent
    n_o2=[0.5],                # Oxygen order
    gas_diffusion_depth=[1e-4], # Gas diffusion length (m)
    max_reaction_rate=[0.01],   # Maximum rate (kg/m³/s)
    # Environmental effects
    x_o2_pyro=0.1,             # Oxygen concentration for kinetics
)
```

## Material Behavior Control

### Shrinking and Swelling

Control material volume changes during pyrolysis:

```python
material = Material(
    id="SAMPLE",
    # ... basic parameters
    allow_shrinking=True,      # Allow volume reduction
    allow_swelling=False,      # Prevent volume increase
)
```

### Energy Conservation

Adjust enthalpy calculations for energy conservation:

```python
material = Material(
    id="SAMPLE",
    # ... basic parameters
    adjust_h=True,             # Adjust enthalpies
    reference_enthalpy=100.0,  # Reference enthalpy (kJ/kg)
    reference_enthalpy_temperature=25.0,  # Reference temperature (°C)
)
```

## Validation and Best Practices

### Yield Validation

Pyfds automatically validates that yields sum correctly:

- For single reactions: `nu_spec + nu_matl ≤ 1.0`
- For multi-reactions: Each reaction's yields sum ≤ 1.0
- Cross-references between materials are validated

### Parameter Ranges

Ensure parameters are within FDS limits:

- Density: 0.1 - 25,000 kg/m³
- Conductivity: 0.001 - 2,000 W/(m·K)
- Specific heat: 0.1 - 50 kJ/(kg·K)
- Activation energy: Typically 50,000 - 200,000 kJ/kmol

### Common Issues

1. **Missing thermal properties**: Always specify conductivity and specific heat
2. **Invalid yields**: Ensure yields sum ≤ 1.0 per reaction
3. **Cross-reference errors**: Define all referenced materials and species
4. **Parameter units**: Double-check units match FDS requirements

## Examples

### Charring Wood

```python
# Char-forming wood
char = Material(id="CHAR", density=150, conductivity=0.1, specific_heat=1.0)

wood = Material(
    id="WOOD",
    density=500,
    conductivity=0.13,
    specific_heat=2.5,
    n_reactions=1,
    a=[1e10], e=[100000], heat_of_reaction=[1800],
    spec_id=["WOOD_GAS"], nu_spec=[0.75],
    matl_id=["CHAR"], nu_matl=[0.25]
)
```

### Non-Charring Polymer

```python
# Pure pyrolysis without residue
polymer = Material(
    id="POLYMER",
    density=1000,
    conductivity=0.2,
    specific_heat=2.0,
    n_reactions=1,
    a=[5e9], e=[120000], heat_of_reaction=[1000],
    spec_id=["FUEL_GAS"], nu_spec=[1.0]  # Complete conversion to gas
)
```

See the examples directory for complete working simulations.
