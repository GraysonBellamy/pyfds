# Data Models

::: pyfds.core.models

## Overview

Data models are non-namelist Pydantic classes used to represent complex domain concepts. Unlike namelist classes that map directly to FDS input, models provide structured data for specific use cases like pyrolysis reactions.

## Pyrolysis Models

### PyrolysisReaction

Represents a single pyrolysis reaction within a material.

```python
from pyfds.core.models import PyrolysisReaction

# Define a pyrolysis reaction
reaction = PyrolysisReaction(
    a=1e10,              # Pre-exponential factor (1/s)
    e=80000,             # Activation energy (J/mol)
    n_s=1.0,             # Reaction order for solid
    nu_spec={"FUEL": 1.0},  # Product species with yields
)
```

**Parameters:**

- `a` (float): Pre-exponential factor in Arrhenius equation (1/s)
- `e` (float): Activation energy (J/mol)
- `n_s` (float, optional): Reaction order for solid reactant (default: 1.0)
- `n_o2` (float, optional): Reaction order for oxygen (default: 0.0)
- `n_h2o` (float, optional): Reaction order for water (default: 0.0)
- `nu_spec` (dict[str, float], optional): Product species and their stoichiometric coefficients
- `nu_matl` (dict[str, float], optional): Residue materials and their yields
- `heat_of_reaction` (float, optional): Heat of reaction (J/kg)

### PyrolysisProduct

Represents a product from a pyrolysis reaction.

```python
from pyfds.core.models import PyrolysisProduct

# Gas product
gas_product = PyrolysisProduct(
    species_id="FUEL",
    yield_fraction=0.8
)

# Solid residue
residue = PyrolysisProduct(
    material_id="CHAR",
    yield_fraction=0.2
)
```

**Parameters:**

- `species_id` (str, optional): ID of gas/vapor species produced
- `material_id` (str, optional): ID of solid residue material
- `yield_fraction` (float): Mass fraction yield (0.0 to 1.0)

**Note:** Either `species_id` OR `material_id` must be specified, not both.

## Usage with MaterialBuilder

The models are primarily used with `MaterialBuilder` to define complex pyrolysis behavior:

```python
from pyfds.builders import MaterialBuilder
from pyfds.core.models import PyrolysisReaction

# Create material with pyrolysis
material = (
    MaterialBuilder("FOAM")
    .density(40)
    .conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(
        PyrolysisReaction(
            a=1e10,
            e=80000,
            nu_spec={"FUEL": 0.8},
            nu_matl={"CHAR": 0.2}
        )
    )
    .build()
)
```

Or using the fluent API methods:

```python
material = (
    MaterialBuilder("FOAM")
    .density(40)
    .conductivity(0.04)
    .specific_heat(1.5)
    .add_pyrolysis_reaction(
        a=1e10,
        e=80000,
        product_species="FUEL",
        product_yield=0.8,
        residue_material="CHAR",
        residue_yield=0.2
    )
    .build()
)
```

## Pyrolysis Reaction Examples

### Simple Single-Step Pyrolysis

Complete decomposition to gas:

```python
from pyfds.core.models import PyrolysisReaction

reaction = PyrolysisReaction(
    a=1e10,
    e=80000,
    nu_spec={"FUEL": 1.0}  # 100% gas
)
```

### Two-Step Pyrolysis

Initial decomposition followed by char oxidation:

```python
# Step 1: Decomposition to fuel + char
step1 = PyrolysisReaction(
    a=1e10,
    e=80000,
    nu_spec={"FUEL": 0.7},
    nu_matl={"CHAR": 0.3}
)

# Step 2: Char oxidation (oxygen-dependent)
step2 = PyrolysisReaction(
    a=5e8,
    e=120000,
    n_o2=1.0,  # Requires oxygen
    nu_spec={"PRODUCTS": 1.0}
)

material = (
    MaterialBuilder("WOOD")
    .density(500)
    .add_pyrolysis_reaction(step1)
    .add_pyrolysis_reaction(step2)
    .build()
)
```

### Temperature-Dependent Decomposition

Multiple reactions at different activation energies:

```python
# Low-temperature volatiles
low_temp = PyrolysisReaction(
    a=1e8,
    e=60000,
    nu_spec={"VOLATILES": 0.3}
)

# High-temperature decomposition
high_temp = PyrolysisReaction(
    a=1e12,
    e=150000,
    nu_spec={"FUEL": 0.5},
    nu_matl={"CHAR": 0.2}
)

material = (
    MaterialBuilder("POLYMER")
    .density(1200)
    .add_pyrolysis_reaction(low_temp)
    .add_pyrolysis_reaction(high_temp)
    .build()
)
```

### Endothermic Pyrolysis

Reaction that absorbs heat:

```python
reaction = PyrolysisReaction(
    a=1e10,
    e=80000,
    heat_of_reaction=-500000,  # Negative = endothermic (J/kg)
    nu_spec={"FUEL": 0.8},
    nu_matl={"CHAR": 0.2}
)
```

## Validation

Models are validated using Pydantic:

```python
from pyfds.core.models import PyrolysisReaction

# This will raise a validation error
try:
    invalid = PyrolysisReaction(
        a=-1e10,  # Must be positive
        e=80000
    )
except ValueError as e:
    print(f"Validation error: {e}")

# Yield fractions must sum appropriately
try:
    invalid = PyrolysisReaction(
        a=1e10,
        e=80000,
        nu_spec={"FUEL": 0.5, "OTHER": 0.7}  # Sums to 1.2 > 1.0
    )
except ValueError as e:
    print(f"Yields exceed 100%: {e}")
```

## Type Safety

Full type hints enable IDE support:

```python
from pyfds.core.models import PyrolysisReaction, PyrolysisProduct

# Type-safe construction
reaction: PyrolysisReaction = PyrolysisReaction(
    a=1e10,
    e=80000,
    nu_spec={"FUEL": 1.0}
)

# IDE provides autocomplete
print(reaction.a)  # IDE knows this is float
print(reaction.e)  # IDE knows this is float
```

## See Also

- [MaterialBuilder](../builders/material.md) - Building materials with pyrolysis
- [Materials & Surfaces Guide](../../guide/materials-surfaces.md) - Material modeling guide
- [Pyrolysis Modeling Guide](../../guide/pyrolysis.md) - Detailed pyrolysis guide
- [Material Namelist](../namelists/materials.md) - MATL namelist reference
