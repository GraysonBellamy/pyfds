# Advanced Chemistry Guide

This guide covers advanced combustion chemistry modeling in pyfds, including two-step chemistry, product yields, extinction models, and detailed reaction mechanisms.

## Overview

FDS supports sophisticated combustion chemistry through the `REAC` namelist, allowing you to model:

- Multi-step reaction mechanisms
- Detailed product yields
- Extinction and reignition
- Nitrogen and carbon chemistry
- Oxygen-limited combustion

## Basic Reaction Setup

### Single-Step Combustion

```python
from pyfds import Simulation
from pyfds.core.namelists import Reaction

# Create simulation
sim = Simulation(chid="combustion", title="Combustion Chemistry Example")

# Define combustion reaction
methane_combustion = Reaction(
    fuel="METHANE",
    soot_yield=0.015,        # Soot yield (kg/kg)
    co_yield=0.001,          # CO yield (kg/kg)
    radiative_fraction=0.35  # Radiative fraction
)

sim.physics.add_reaction(methane_combustion)
```

### Using ReactionBuilder

The `ReactionBuilder` provides a fluent API:

```python
from pyfds.builders import ReactionBuilder

reaction = ReactionBuilder() \
    .fuel("PROPANE") \
    .soot_yield(0.015) \
    .co_yield(0.001) \
    .radiative_fraction(0.35) \
    .build()
```

## Two-Step Chemistry

### Basic Two-Step Model

```python
# Step 1: Fuel → CO + other products
step1 = Reaction(
    fuel="FUEL",
    co_yield=0.5,           # High CO yield
    soot_yield=0.01,
    radiative_fraction=0.2
)

# Step 2: CO → CO2
step2 = Reaction(
    fuel="CO",
    co_yield=0.0,           # Complete oxidation
    soot_yield=0.005,
    radiative_fraction=0.3
)

sim.physics.add_reaction(step1)
sim.physics.add_reaction(step2)
```

### Advanced Two-Step Setup

```python
# Detailed two-step chemistry
fuel_pyrolysis = Reaction(
    id="FUEL_BREAKDOWN",
    fuel="HYDROCARBON",
    formula="C3 H8",         # Chemical formula
    co_yield=0.3,
    soot_yield=0.02,
    hcn_yield=0.001,         # HCN yield for nitrogenous fuels
    radiative_fraction=0.25
)

co_oxidation = Reaction(
    id="CO_OXIDATION",
    fuel="CO",
    co_yield=0.0,
    soot_yield=0.0,
    radiative_fraction=0.4,
    # Advanced parameters
    epumo2=1.0,              # Oxygen exponent
    lower_oxygen_limit=0.01  # Extinction limit
)

sim.physics.add_reaction(fuel_pyrolysis)
sim.physics.add_reaction(co_oxidation)
```

## Detailed Product Yields

### Carbon and Nitrogen Chemistry

```python
# Fuel with nitrogen content
coal = Reaction(
    fuel="COAL_VOLATILES",
    # Carbon fractions
    carbon_fraction=0.8,     # Carbon mass fraction
    hydrogen_fraction=0.05,  # Hydrogen mass fraction
    oxygen_fraction=0.05,    # Oxygen mass fraction
    nitrogen_fraction=0.015, # Nitrogen mass fraction
    # Yields
    soot_yield=0.08,
    co_yield=0.02,
    hcn_yield=0.005,
    # Combustion properties
    heat_of_combustion_complete=32000.0,  # kJ/kg
    radiative_fraction=0.25
)
```

### Atom Balance Checking

```python
# FDS can check atom balance
reaction = Reaction(
    fuel="C2H6",
    formula="C2 H6",         # Explicit formula
    check_atom_balance=True, # Enable balance checking
    soot_yield=0.01,
    co_yield=0.005
)
```

## Extinction and Reignition

### Oxygen-Limited Combustion

```python
# Extinction model
reaction = Reaction(
    fuel="FUEL",
    lower_oxygen_limit=0.05,  # Extinction when O2 < 5%
    epumo2=1.0,              # Oxygen exponent
    # Reignition parameters
    ait_exclusion_zone=0.1   # Auto-ignition exclusion zone
)
```

### Ventilation-Limited Burning

```python
# Ventilation-controlled fire
reaction = Reaction(
    fuel="FUEL",
    # Extinction limits
    lower_oxygen_limit=0.06,
    critical_flame_temperature=1300.0,  # K
    # Heat release
    heat_of_combustion_complete=40000.0
)
```

## Advanced Reaction Parameters

### Reaction Kinetics

```python
# Detailed kinetics
reaction = Reaction(
    fuel="FUEL",
    # Kinetic parameters
    n_simple_chemistry_reactions=2,  # Number of reactions
    # Heat release parameters
    hoc_complete=44000.0,     # Complete heat of combustion
    # Advanced yields
    so2_yield=0.001,          # SO2 yield
    hcl_yield=0.0005,         # HCl yield
    # Radiation
    radiative_fraction=0.33
)
```

### Fuel-Specific Properties

```python
# Polymer combustion
plastic = Reaction(
    fuel="PLASTIC_VAPOR",
    # Composition
    carbon_fraction=0.6,
    hydrogen_fraction=0.08,
    oxygen_fraction=0.22,
    chlorine_fraction=0.1,    # PVC-like
    # Yields
    soot_yield=0.05,
    co_yield=0.01,
    hcl_yield=0.08,           # HCl from chlorine
    # Combustion
    heat_of_combustion_complete=18000.0,
    radiative_fraction=0.3
)
```

## Chemistry Validation

### Consistency Checks

Pyfds validates reaction parameters:

- Fuel species must be defined in SPEC
- Yields must be physically reasonable (0-1)
- Atom balance when requested
- Cross-references to other reactions

### Parameter Ranges

- Soot yield: 0.0 - 0.2 (kg/kg)
- CO yield: 0.0 - 0.5 (kg/kg)
- Radiative fraction: 0.2 - 0.5
- Heat of combustion: 10,000 - 50,000 kJ/kg

## Best Practices

### Choosing Chemistry Models

1. **Simple fuels**: Single-step chemistry
2. **Complex fuels**: Two-step chemistry
3. **Nitrogenous fuels**: Include HCN yields
4. **Halogenated fuels**: Include acid gas yields

### Parameter Estimation

1. **Yields**: Measure experimentally or use literature
2. **Heat of combustion**: Use bomb calorimeter data
3. **Radiative fraction**: Measure or correlate with soot
4. **Extinction limits**: Use standard values

### Common Issues

1. **Incorrect yields**: Sum of yields should be reasonable
2. **Missing species**: Define all referenced SPEC
3. **Atom imbalance**: Check formulas and yields
4. **Convergence issues**: Adjust extinction parameters

## Examples

### Methane Combustion

```python
# Natural gas fire
methane = Reaction(
    fuel="METHANE",
    soot_yield=0.005,
    co_yield=0.001,
    radiative_fraction=0.34,
    heat_of_combustion_complete=50000.0
)
```

### Wood Pyrolysis Products

```python
# Wood combustion
wood_gas = Reaction(
    fuel="WOOD_GAS",
    carbon_fraction=0.5,
    hydrogen_fraction=0.06,
    oxygen_fraction=0.44,
    soot_yield=0.02,
    co_yield=0.03,
    radiative_fraction=0.25,
    heat_of_combustion_complete=18000.0
)
```

### Two-Step Diesel Combustion

```python
# Diesel surrogate
diesel1 = Reaction(
    id="DIESEL_PYROLYSIS",
    fuel="DIESEL",
    co_yield=0.4,
    soot_yield=0.03,
    radiative_fraction=0.2
)

diesel2 = Reaction(
    id="SOOT_OXIDATION",
    fuel="SOOT",
    co_yield=0.0,
    soot_yield=0.0,
    radiative_fraction=0.4
)
```

### Flame Extinction

```python
# Extinction-capable reaction
reaction = Reaction(
    fuel="FUEL",
    lower_oxygen_limit=0.055,  # 5.5% O2
    epumo2=1.0,
    critical_flame_temperature=1420.0,  # K
    ait_exclusion_zone=0.05
)
```

See the examples directory for complete combustion chemistry simulations.
