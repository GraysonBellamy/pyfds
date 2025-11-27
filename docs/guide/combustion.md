# Combustion & Reactions (REAC)

Define fuel properties and combustion reactions for fire simulations.

## Overview

The **REAC** (reaction) namelist specifies fuel properties and combustion chemistry. FDS includes default fuels, but custom fuels can be defined for specific scenarios.

```python
# Use default propane
sim.reaction(fuel='PROPANE')

# Custom fuel with specific properties
sim.reaction(
    id='WOOD',
    fuel='CELLULOSE',
    soot_yield=0.015,
    co_yield=0.004
)
```

## Default Fuels

FDS includes predefined fuels with realistic properties:

### Common Default Fuels

```python
# Hydrocarbon fuels
sim.reaction(fuel='METHANE')      # Natural gas
sim.reaction(fuel='PROPANE')      # LPG
sim.reaction(fuel='ETHYLENE')     # Polymer precursor
sim.reaction(fuel='ACETYLENE')    # Welding gas

# Liquid fuels
sim.reaction(fuel='N-HEPTANE')    # Gasoline surrogate
sim.reaction(fuel='ETHANOL')      # Alcohol
sim.reaction(fuel='METHANOL')     # Wood alcohol

# Solid fuels
sim.reaction(fuel='CELLULOSE')    # Wood, paper
sim.reaction(fuel='POLYURETHANE') # Foam
sim.reaction(fuel='POLYSTYRENE')  # Plastic
```

## Fuel Properties

### Heat of Combustion

```python
# Heat released per kg of fuel burned
sim.reaction(
    id='CUSTOM_FUEL',
    heat_of_combustion=25000.0  # kJ/kg
)
```

### Soot Yield

```python
# Mass of soot per mass of fuel
sim.reaction(
    id='SOOTY_FUEL',
    fuel='N-HEPTANE',
    soot_yield=0.037  # 3.7% of fuel mass becomes soot
)

# Clean-burning fuel
sim.reaction(
    id='CLEAN_FUEL',
    fuel='METHANE',
    soot_yield=0.001  # Very little soot
)
```

### CO Yield

```python
# Carbon monoxide production
sim.reaction(
    id='FUEL_WITH_CO',
    fuel='PROPANE',
    co_yield=0.010  # 1% of fuel mass becomes CO
)
```

### Radiative Fraction

```python
# Fraction of energy radiated (not convected)
sim.reaction(
    id='FUEL_RAD',
    fuel='PROPANE',
    radiative_fraction=0.35  # 35% radiated, 65% convected
)

# Sooty fire (more radiation)
sim.reaction(
    id='SOOTY_FIRE',
    fuel='N-HEPTANE',
    radiative_fraction=0.45  # Higher radiation
)
```

## Custom Fuel Composition

### Stoichiometry

Define fuel chemical formula:

```python
# Wood (approximated as cellulose C6H10O5)
sim.reaction(
    id='WOOD',
    formula='C6H10O5',
    heat_of_combustion=15000.0,  # kJ/kg
    soot_yield=0.015,
    co_yield=0.004,
    radiative_fraction=0.35
)

# Polyethylene (C2H4)n
sim.reaction(
    id='POLYETHYLENE',
    formula='C2H4',
    heat_of_combustion=43000.0,
    soot_yield=0.060,
    radiative_fraction=0.40
)
```

### Fuel Composition Table

| Material | Formula | ΔH_c (kJ/kg) | Soot Yield | CO Yield |
|----------|---------|--------------|------------|----------|
| Methane | CH4 | 50,000 | 0.001 | 0.001 |
| Propane | C3H8 | 46,300 | 0.024 | 0.010 |
| N-Heptane | C7H16 | 44,600 | 0.037 | 0.010 |
| Ethanol | C2H6O | 26,800 | 0.008 | 0.007 |
| Cellulose | C6H10O5 | 15,000 | 0.015 | 0.004 |
| Polystyrene | C8H8 | 39,800 | 0.164 | 0.060 |
| PVC | C2H3Cl | 16,400 | 0.172 | 0.063 |

## Complete Examples

### Pool Fire with Custom Fuel

```python
from pyfds import Simulation

sim = Simulation(chid='pool_fire')
sim.time(t_end=300.0)
sim.mesh(ijk=(60, 60, 40), xb=(0, 6, 0, 6, 0, 4))

# Custom gasoline surrogate (n-heptane)
sim.reaction(
    id='GASOLINE',
    fuel='N-HEPTANE',
    heat_of_combustion=44600.0,  # kJ/kg
    soot_yield=0.037,
    co_yield=0.010,
    radiative_fraction=0.33
)

# Pool fire surface
sim.surface(
    id='POOL',
    hrrpua=1500.0,  # kW/m²
    color='ORANGE'
)

# Circular pool (1m radius)
sim.vent(
    xb=(-2, 2, -2, 2, 0, 0),
    surf_id='POOL',
    xyz=(3, 3, 0),
    radius=1.0
)

# Soot and CO monitoring
sim.device(
    id='SOOT',
    quantity='SOOT DENSITY',
    xyz=(3, 3, 2)
)

sim.device(
    id='CO',
    quantity='VOLUME FRACTION',
    spec_id='CARBON MONOXIDE',
    xyz=(3, 3, 2)
)

sim.write('pool_fire.fds')
```

### Wood Fire

```python
sim = Simulation(chid='wood_fire')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 30), xb=(0, 5, 0, 5, 0, 3))

# Wood fuel properties
sim.reaction(
    id='WOOD',
    fuel='CELLULOSE',
    heat_of_combustion=15000.0,
    soot_yield=0.015,
    co_yield=0.004,
    radiative_fraction=0.35
)

# Wood crib surface
sim.surface(
    id='WOOD_CRIB',
    hrrpua=600.0,
    color='BROWN'
)

sim.obstruction(
    xb=(2, 3, 2, 3, 0, 0.5),
    surf_id='WOOD_CRIB'
)

# Visibility (affected by soot)
sim.device(
    id='VISIBILITY',
    quantity='VISIBILITY',
    xyz=(2.5, 2.5, 1.5)
)

sim.write('wood_fire.fds')
```

### Plastic Fire (High Soot)

```python
sim = Simulation(chid='plastic_fire')
sim.time(t_end=600.0)
sim.mesh(ijk=(60, 50, 30), xb=(0, 6, 0, 5, 0, 3))

# Polystyrene (very sooty)
sim.reaction(
    id='POLYSTYRENE',
    fuel='POLYSTYRENE',
    heat_of_combustion=39800.0,
    soot_yield=0.164,  # High soot production
    co_yield=0.060,
    radiative_fraction=0.42  # High radiation due to soot
)

# Burning plastic items
sim.surface(
    id='PLASTIC',
    hrrpua=800.0,
    color='BLACK'
)

sim.obstruction(
    xb=(2.5, 3.5, 2, 3, 0, 1),
    surf_id='PLASTIC'
)

# Dense smoke monitoring
sim.device(
    id='SOOT_CEILING',
    quantity='SOOT DENSITY',
    xyz=(3, 2.5, 2.9)
)

sim.device(
    id='OPTICAL_DENSITY',
    quantity='OPTICAL DENSITY',
    xyz=(3, 2.5, 2.9)
)

sim.device(
    id='VISIBILITY',
    quantity='VISIBILITY',
    xyz=(3, 2.5, 1.5)
)

sim.write('plastic_fire.fds')
```

### Multi-Fuel Fire

```python
sim = Simulation(chid='multi_fuel')
sim.time(t_end=600.0)
sim.mesh(ijk=(80, 60, 30), xb=(0, 8, 0, 6, 0, 3))

# Define multiple fuels
sim.reaction(
    id='GASOLINE',
    fuel='N-HEPTANE',
    soot_yield=0.037,
    co_yield=0.010
)

sim.reaction(
    id='WOOD',
    fuel='CELLULOSE',
    soot_yield=0.015,
    co_yield=0.004
)

# Gasoline pool fire
sim.surface(id='GAS_POOL', hrrpua=2000.0, reac_id='GASOLINE')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='GAS_POOL')

# Wood pallet fire
sim.surface(id='WOOD_PALLET', hrrpua=600.0, reac_id='WOOD')
sim.obstruction(xb=(5, 6, 3, 4, 0, 1), surf_id='WOOD_PALLET')

# Monitor species from both fires
sim.device(
    id='SOOT',
    quantity='SOOT DENSITY',
    xyz=(4, 3, 2)
)

sim.write('multi_fuel.fds')
```

## Radiation and Soot

### Soot-Radiation Coupling

Soot increases radiative heat transfer:

```python
# Clean fuel (low soot, low radiation)
sim.reaction(
    id='METHANE',
    fuel='METHANE',
    soot_yield=0.001,
    radiative_fraction=0.15  # Low radiation
)

# Sooty fuel (high soot, high radiation)
sim.reaction(
    id='HEPTANE',
    fuel='N-HEPTANE',
    soot_yield=0.037,
    radiative_fraction=0.33  # Higher radiation
)
```

### Measuring Radiative Heat Flux

```python
# Define sooty fuel
sim.reaction(
    id='FUEL',
    fuel='N-HEPTANE',
    soot_yield=0.040,
    radiative_fraction=0.35
)

# Fire
sim.surface(id='FIRE', hrrpua=1500.0, reac_id='FUEL')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# Radiative heat flux at distance
sim.device(
    id='RAD_HF_1M',
    quantity='RADIATIVE HEAT FLUX',
    xyz=(4, 2.5, 1),
    ior=1  # Facing fire
)

sim.device(
    id='GAUGE_HF_1M',
    quantity='GAUGE HEAT FLUX',
    xyz=(4, 2.5, 1),
    ior=1  # Total (rad + conv)
)
```

## Species Tracking

### Oxygen Depletion

```python
sim.reaction(fuel='PROPANE')

# Monitor oxygen concentration
sim.device(
    id='O2_UPPER',
    quantity='VOLUME FRACTION',
    spec_id='OXYGEN',
    xyz=(3, 2.5, 2.5)
)

sim.device(
    id='O2_LOWER',
    quantity='VOLUME FRACTION',
    spec_id='OXYGEN',
    xyz=(3, 2.5, 0.5)
)
```

### CO and CO2 Production

```python
sim.reaction(
    id='FUEL',
    fuel='PROPANE',
    co_yield=0.010,  # CO production
    soot_yield=0.024
)

# Track CO (toxic)
sim.device(
    id='CO_CONC',
    quantity='VOLUME FRACTION',
    spec_id='CARBON MONOXIDE',
    xyz=(3, 2.5, 1.5)
)

# Track CO2 (asphyxiant)
sim.device(
    id='CO2_CONC',
    quantity='VOLUME FRACTION',
    spec_id='CARBON DIOXIDE',
    xyz=(3, 2.5, 1.5)
)
```

## Combustion Efficiency

### Fuel Lean vs Fuel Rich

```python
# Well-ventilated fire (complete combustion)
sim.reaction(
    id='COMPLETE',
    fuel='PROPANE',
    co_yield=0.005,  # Low CO
    soot_yield=0.015  # Low soot
)

# Under-ventilated fire (incomplete combustion)
sim.reaction(
    id='INCOMPLETE',
    fuel='PROPANE',
    co_yield=0.020,  # Higher CO
    soot_yield=0.040  # More soot
)
```

## Best Practices

### 1. Use Realistic Fuel Properties

```python
# Good: Properties from literature/experiments
sim.reaction(
    id='WOOD',
    fuel='CELLULOSE',
    heat_of_combustion=15000.0,  # From SFPE Handbook
    soot_yield=0.015,
    co_yield=0.004
)

# Avoid: Made-up values
sim.reaction(
    id='MYSTERY_FUEL',
    heat_of_combustion=100000.0,  # Unrealistic!
    soot_yield=0.5
)
```

### 2. Match Fuel to Application

```python
# Gasoline spill
sim.reaction(fuel='N-HEPTANE')  # Good surrogate

# Natural gas leak
sim.reaction(fuel='METHANE')  # Appropriate

# Wood structure
sim.reaction(fuel='CELLULOSE')  # Reasonable approximation
```

### 3. Consider Soot Impact

```python
# For visibility/radiation studies, soot is critical
sim.reaction(
    id='FUEL',
    soot_yield=0.037,  # Don't neglect!
    radiative_fraction=0.33
)

# Monitor soot effects
sim.device(id='VISIBILITY', quantity='VISIBILITY', xyz=(3, 2.5, 1.5))
sim.device(id='SOOT', quantity='SOOT DENSITY', xyz=(3, 2.5, 2))
```

### 4. Document Fuel Sources

```python
# Clear documentation
# Fuel properties from:
# - Heat of combustion: SFPE Handbook, 5th Ed.
# - Soot yield: Mulholland & Croarkin (2000)
# - CO yield: Tewarson (2008)
sim.reaction(
    id='POLYURETHANE',
    fuel='POLYURETHANE',
    heat_of_combustion=25000.0,
    soot_yield=0.131,
    co_yield=0.042
)
```

## Common Issues

??? question "Too much/too little soot"
    **Cause**: Incorrect soot yield

    **Solution**: Check literature values for fuel type
    ```python
    # Typical soot yields:
    # - Methane: 0.001
    # - Propane: 0.024
    # - Gasoline: 0.037
    # - Wood: 0.015
    # - Polystyrene: 0.164
    sim.reaction(fuel='PROPANE', soot_yield=0.024)  # Realistic
    ```

??? question "Wrong radiative fraction"
    **Cause**: Inconsistent with soot production

    **Solution**: Higher soot → higher radiation
    ```python
    # Clean fuel
    sim.reaction(fuel='METHANE', soot_yield=0.001, radiative_fraction=0.15)

    # Sooty fuel
    sim.reaction(fuel='N-HEPTANE', soot_yield=0.037, radiative_fraction=0.33)
    ```

??? question "Unrealistic CO levels"
    **Cause**: Incorrect CO yield

    **Solution**: Use measured yields (typically 0.001-0.060)
    ```python
    # Well-ventilated
    sim.reaction(co_yield=0.004)

    # Under-ventilated
    sim.reaction(co_yield=0.020)
    ```

## Advanced Topics

### Simplified vs Detailed Chemistry

FDS uses simplified combustion (mixture fraction approach):

```python
# FDS default: Simple, fast
sim.reaction(fuel='PROPANE')  # One-step chemistry

# For most fire simulations, this is sufficient
# Detailed chemistry (not in FDS) needed only for:
# - Flame chemistry research
# - Pollutant formation mechanisms
# - Ignition transients
```

### Heat Release Rate vs Fuel Mass

```python
# HRR specified directly (most common)
sim.surface(id='FIRE', hrrpua=1000.0)  # kW/m²

# Mass flux (advanced)
sim.surface(
    id='FIRE',
    mass_flux=0.020,  # kg/m²/s
    reac_id='PROPANE'  # Uses fuel heat of combustion
)
```

## Fuel Property Reference

### Gaseous Fuels

| Fuel | Formula | ΔH_c (kJ/kg) | Soot Yield | Radiative Fraction |
|------|---------|--------------|------------|--------------------|
| Methane | CH4 | 50,000 | 0.001 | 0.15 |
| Propane | C3H8 | 46,300 | 0.024 | 0.30 |
| Ethylene | C2H4 | 47,200 | 0.059 | 0.35 |

### Liquid Fuels

| Fuel | Formula | ΔH_c (kJ/kg) | Soot Yield | Radiative Fraction |
|------|---------|--------------|------------|--------------------|
| Methanol | CH4O | 20,000 | 0.001 | 0.15 |
| Ethanol | C2H6O | 26,800 | 0.008 | 0.20 |
| N-Heptane | C7H16 | 44,600 | 0.037 | 0.33 |

### Solid Fuels

| Fuel | Approx. Formula | ΔH_c (kJ/kg) | Soot Yield | Radiative Fraction |
|------|-----------------|--------------|------------|--------------------|
| Cellulose | C6H10O5 | 15,000 | 0.015 | 0.35 |
| Polystyrene | C8H8 | 39,800 | 0.164 | 0.42 |
| Polyurethane | C7H12O2N2 | 25,000 | 0.131 | 0.40 |

## Next Steps

- [Fire Sources](fire-sources.md) - Apply fuel properties to fires
- [Materials & Surfaces](materials-surfaces.md) - Combustible surfaces
- [Devices](devices.md) - Monitor combustion products
- [Examples](../examples/basic.md) - Complete fire scenarios

---

[Examples →](../examples/index.md){ .md-button .md-button--primary }
