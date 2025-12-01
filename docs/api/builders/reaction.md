# ReactionBuilder

::: pyfds.builders.reaction.ReactionBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`ReactionBuilder` creates combustion reaction definitions (`&REAC` namelists) using predefined fuels or custom compositions.

## Key Features

- **Fuel Database**: 17 predefined fuels (gases, liquids, solids)
- **Custom Fuels**: Define C/H/O/N composition
- **Product Yields**: Soot, CO, radiative fraction
- **Auto-Ignition**: Temperature specification

## Quick Examples

### Predefined Fuel

```python
from pyfds.builders import ReactionBuilder

# Use predefined fuel from database
reac = (
    ReactionBuilder()
    .fuel('PROPANE')
    .yields(soot=0.015, co=0.02)
    .radiative_fraction(0.33)
    .build()
)
```

### Custom Fuel Composition

```python
# Define custom fuel
reac = (
    ReactionBuilder()
    .custom_fuel(
        c=7,                         # Carbon atoms
        h=16,                        # Hydrogen atoms
        heat_of_combustion=44600     # kJ/kg
    )
    .yields(soot=0.01, co=0.02)
    .build()
)

# With oxygen and nitrogen
reac = (
    ReactionBuilder()
    .custom_fuel(
        c=3.52,
        h=5.48,
        o=0.88,
        n=0.32,
        heat_of_combustion=23200
    )
    .build()
)
```

### Complete Configuration

```python
reac = (
    ReactionBuilder()
    .fuel('POLYURETHANE')
    .soot_yield(0.10)
    .co_yield(0.03)
    .radiative_fraction(0.30)
    .auto_ignition_temperature(350)
    .build()
)
```

## Fuel Database

PyFDS includes 17 predefined fuels:

### Gases

| Fuel | Formula | HOC (kJ/kg) | Soot Yield | CO Yield |
|------|---------|-------------|------------|----------|
| METHANE | CH₄ | 50000 | 0.001 | 0.001 |
| ETHANE | C₂H₆ | 47500 | 0.005 | 0.003 |
| PROPANE | C₃H₈ | 46000 | 0.010 | 0.004 |
| BUTANE | C₄H₁₀ | 45700 | 0.015 | 0.005 |
| HYDROGEN | H₂ | 120000 | 0.000 | 0.000 |

### Liquids

| Fuel | Formula | HOC (kJ/kg) | Soot Yield | CO Yield |
|------|---------|-------------|------------|----------|
| N-HEPTANE | C₇H₁₆ | 44600 | 0.037 | 0.010 |
| N-HEXANE | C₆H₁₄ | 44750 | 0.033 | 0.009 |
| GASOLINE | ~C₇H₁₄ | 43700 | 0.059 | 0.014 |
| ACETONE | C₃H₆O | 25800 | 0.014 | 0.004 |
| METHANOL | CH₃OH | 19900 | 0.001 | 0.003 |
| ETHANOL | C₂H₅OH | 26800 | 0.005 | 0.003 |

### Solids

| Fuel | Approx. Formula | HOC (kJ/kg) | Soot Yield | CO Yield |
|------|-----------------|-------------|------------|----------|
| POLYURETHANE | C₃.₅₂H₅.₄₈O₀.₈₈N₀.₃₂ | 23200 | 0.131 | 0.042 |
| WOOD | C₃.₄H₆.₂O₂.₅ | 12400 | 0.015 | 0.004 |
| PMMA | C₅H₈O₂ | 24900 | 0.022 | 0.004 |
| POLYSTYRENE | C₈H₈ | 39200 | 0.060 | 0.010 |
| POLYETHYLENE | C₂H₄ | 43600 | 0.060 | 0.024 |
| POLYPROPYLENE | C₃H₆ | 43300 | 0.059 | 0.024 |

### Querying the Database

```python
# List all available fuels
fuels = ReactionBuilder.list_fuels()
print(fuels)
# ['ACETONE', 'BUTANE', 'ETHANE', 'ETHANOL', 'GASOLINE', ...]

# Get detailed info about a fuel
info = ReactionBuilder.get_fuel_info('PROPANE')
print(info)
# {
#     'c': 3,
#     'h': 8,
#     'o': 0,
#     'n': 0,
#     'hoc': 46000,
#     'soot_yield': 0.010,
#     'co_yield': 0.004,
#     'description': 'Propane gas'
# }

# Case-insensitive lookup
info = ReactionBuilder.get_fuel_info('propane')  # Works
```

## Usage in Simulations

### Simple Fire

```python
from pyfds import Simulation
from pyfds.builders import ReactionBuilder

sim = Simulation('propane_fire')

# Add reaction
reac = ReactionBuilder().fuel('PROPANE').build()
sim.add_reaction(reac)

# Create fire surface
sim.add(Surface(id='FIRE', hrrpua=1000.0, color='RED'))
sim.add(Obstruction(xb=Bounds3D.of(4.5, 5.5, 4.5, 5.5, 0, 0.1), surf_id='FIRE'))
```

### With Pyrolysis

```python
# Pyrolyzing material
polymer = (
    MaterialBuilder('POLYMER')
    .density(40)
    .add_pyrolysis_reaction(
        a=1e10,
        e=80000,
        product_species='FUEL_VAPOR'
    )
    .build()
)
sim.add_material(polymer)

# Reaction for fuel vapor
reac = ReactionBuilder().fuel('POLYURETHANE').build()
sim.add_reaction(reac)

# Surface with pyrolysis
sim.add(Surface(id='FOAM', matl_id='POLYMER', thickness=0.05))
```

### Custom Fuel with Species

```python
# Define custom fuel species
sim.spec(id='MY_FUEL', mw=114)  # C₇H₁₆ molecular weight

# Custom fuel for that species
reac = (
    ReactionBuilder()
    .custom_fuel(c=7, h=16, heat_of_combustion=44600)
    .fuel_id('MY_FUEL')
    .yields(soot=0.037, co=0.010)
    .build()
)
sim.add_reaction(reac)
```

## Product Yields

### Soot Yield

Fraction of fuel mass converted to soot:

```python
reac = ReactionBuilder().fuel('PROPANE').soot_yield(0.015).build()
```

Typical values:
- **Clean burning gases** (methane, hydrogen): 0.000-0.005
- **Gasoline/alkanes**: 0.010-0.040
- **Aromatics/plastics**: 0.040-0.130

### CO Yield

Fraction of fuel mass converted to CO:

```python
reac = ReactionBuilder().fuel('PROPANE').co_yield(0.02).build()
```

Typical values:
- **Complete combustion**: 0.001-0.005
- **Well-ventilated**: 0.004-0.010
- **Under-ventilated**: 0.010-0.050

### Setting Both

```python
reac = (
    ReactionBuilder()
    .fuel('PROPANE')
    .yields(soot=0.015, co=0.02)
    .build()
)
```

### Radiative Fraction

Fraction of heat release radiated:

```python
reac = (
    ReactionBuilder()
    .fuel('PROPANE')
    .radiative_fraction(0.33)  # 33% radiated
    .build()
)
```

Typical values:
- **Small/clean fires**: 0.30-0.35
- **Sooty fires**: 0.25-0.30
- **Large pool fires**: 0.20-0.30

## Auto-Ignition

Set auto-ignition temperature:

```python
reac = (
    ReactionBuilder()
    .fuel('PROPANE')
    .auto_ignition_temperature(450)  # °C
    .build()
)
```

Common auto-ignition temperatures:
- **Propane**: 450-500°C
- **Methane**: 540°C
- **Gasoline**: 280-450°C
- **Wood**: 300-400°C

## Overriding Database Values

Database values are defaults that can be overridden:

```python
# Database default: soot_yield=0.010
reac = ReactionBuilder().fuel('PROPANE').build()
assert reac.soot_yield == 0.010

# Override with custom value
reac = ReactionBuilder().fuel('PROPANE').soot_yield(0.020).build()
assert reac.soot_yield == 0.020
```

## Validation

The builder validates:

- **Fuel specified**: Either `.fuel()` or `.custom_fuel()` required
- **Valid fuel name**: Fuel must exist in database
- **Positive values**: HOC, yields must be non-negative

## Chemical Background

### Combustion Equation

General hydrocarbon combustion:

$$
\text{C}_n\text{H}_m + \left(n + \frac{m}{4}\right) \text{O}_2 \rightarrow n \text{CO}_2 + \frac{m}{2} \text{H}_2\text{O}
$$

With nitrogen and oxygen in fuel:

$$
\text{C}_a\text{H}_b\text{O}_c\text{N}_d + x \text{O}_2 \rightarrow a \text{CO}_2 + \frac{b}{2} \text{H}_2\text{O} + \frac{d}{2} \text{N}_2
$$

where:

$$
x = a + \frac{b}{4} - \frac{c}{2}
$$

### Heat of Combustion

Lower heating value (LHV) ranges:

- **Gases**: 45,000-50,000 kJ/kg
- **Liquids**: 40,000-45,000 kJ/kg
- **Solid fuels**: 15,000-45,000 kJ/kg

## Best Practices

### Use Database Fuels

```python
# Good: Use predefined fuel
reac = ReactionBuilder().fuel('PROPANE').build()

# Avoid: Manually entering known fuel
reac = ReactionBuilder().custom_fuel(c=3, h=8, heat_of_combustion=46000).build()
```

### Realistic Yields

```python
# Good: Reasonable yields based on literature
reac = ReactionBuilder().fuel('GASOLINE').yields(soot=0.059, co=0.014).build()

# Avoid: Unrealistic yields
reac = ReactionBuilder().fuel('METHANE').yields(soot=0.5, co=0.9).build()
```

### Override When Needed

```python
# Good: Override for specific scenario
reac = (
    ReactionBuilder()
    .fuel('PROPANE')
    .soot_yield(0.030)  # Higher soot for under-ventilated
    .build()
)
```

## See Also

- [User Guide - Combustion](../../guide/combustion.md)
- [User Guide - Builders](../../guide/builders.md)
- [MaterialBuilder](material.md) - For pyrolysis materials
