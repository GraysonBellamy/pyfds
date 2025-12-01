# Combustion & Reactions (REAC)

Define fuel properties and combustion reactions for fire simulations.

## Overview

The **REAC** (reaction) namelist specifies fuel properties and combustion chemistry. FDS includes default fuels, but custom fuels can be defined for specific scenarios.

```python
# Use default propane
sim.add(Reaction(fuel='PROPANE'))

# Custom fuel with specific properties
sim.add(Reaction(
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
sim.add(Reaction(fuel='METHANE')      # Natural gas
sim.add(Reaction(fuel='PROPANE')      # LPG
sim.add(Reaction(fuel='ETHYLENE')     # Polymer precursor
sim.add(Reaction(fuel='ACETYLENE')    # Welding gas

# Liquid fuels
sim.add(Reaction(fuel='N-HEPTANE')    # Gasoline surrogate
sim.add(Reaction(fuel='ETHANOL')      # Alcohol
sim.add(Reaction(fuel='METHANOL')     # Wood alcohol

# Solid fuels
sim.add(Reaction(fuel='CELLULOSE')    # Wood, paper
sim.add(Reaction(fuel='POLYURETHANE') # Foam
sim.add(Reaction(fuel='POLYSTYRENE')  # Plastic
```

## Fuel Properties

### Heat of Combustion

```python
# Heat released per kg of fuel burned
sim.add(Reaction(
    id='CUSTOM_FUEL',
    heat_of_combustion=25000.0  # kJ/kg
)
```

### Soot Yield

```python
# Mass of soot per mass of fuel
sim.add(Reaction(
    id='SOOTY_FUEL',
    fuel='N-HEPTANE',
    soot_yield=0.037  # 3.7% of fuel mass becomes soot
)

# Clean-burning fuel
sim.add(Reaction(
    id='CLEAN_FUEL',
    fuel='METHANE',
    soot_yield=0.001  # Very little soot
)
```

### CO Yield

```python
# Carbon monoxide production
sim.add(Reaction(
    id='FUEL_WITH_CO',
    fuel='PROPANE',
    co_yield=0.010  # 1% of fuel mass becomes CO
)
```

### Radiative Fraction

```python
# Fraction of energy radiated (not convected)
sim.add(Reaction(
    id='FUEL_RAD',
    fuel='PROPANE',
    radiative_fraction=0.35  # 35% radiated, 65% convected
)

# Sooty fire (more radiation)
sim.add(Reaction(
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
sim.add(Reaction(
    id='WOOD',
    formula='C6H10O5',
    heat_of_combustion=15000.0,  # kJ/kg
    soot_yield=0.015,
    co_yield=0.004,
    radiative_fraction=0.35
)

# Polyethylene (C2H4)n
sim.add(Reaction(
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
sim.add(Time(t_end=300.0))
sim.add(Mesh(ijk=Grid3D.of(60, 60, 40), xb=Bounds3D.of(0, 6, 0, 6, 0, 4)))

# Custom gasoline surrogate (n-heptane)
sim.add(Reaction(
    id='GASOLINE',
    fuel='N-HEPTANE',
    heat_of_combustion=44600.0,  # kJ/kg
    soot_yield=0.037,
    co_yield=0.010,
    radiative_fraction=0.33
)

# Pool fire surface
sim.add(Surface(
    id='POOL',
    hrrpua=1500.0,  # kW/m²
    color='ORANGE'
)

# Circular pool (1m radius)
sim.add(Vent(
    xb=Bounds3D.of(-2, 2, -2, 2, 0, 0),
    surf_id='POOL',
    xyz=Point3D.of(3, 3, 0),
    radius=1.0
)

# Soot and CO monitoring
sim.add(Device(
    id='SOOT',
    quantity='SOOT DENSITY',
    xyz=Point3D.of(3, 3, 2)
)

sim.add(Device(
    id='CO',
    quantity='VOLUME FRACTION',
    spec_id='CARBON MONOXIDE',
    xyz=Point3D.of(3, 3, 2)
)

sim.write('pool_fire.fds')
```

### Wood Fire

```python
sim = Simulation(chid='wood_fire')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 30), xb=Bounds3D.of(0, 5, 0, 5, 0, 3)))

# Wood fuel properties
sim.add(Reaction(
    id='WOOD',
    fuel='CELLULOSE',
    heat_of_combustion=15000.0,
    soot_yield=0.015,
    co_yield=0.004,
    radiative_fraction=0.35
)

# Wood crib surface
sim.add(Surface(
    id='WOOD_CRIB',
    hrrpua=600.0,
    color='BROWN'
)

sim.add(Obstruction(
    xb=Bounds3D.of(2, 3, 2, 3, 0, 0.5),
    surf_id='WOOD_CRIB'
)

# Visibility (affected by soot)
sim.add(Device(
    id='VISIBILITY',
    quantity='VISIBILITY',
    xyz=Point3D.of(2.5, 2.5, 1.5)
)

sim.write('wood_fire.fds')
```

### Plastic Fire (High Soot)

```python
sim = Simulation(chid='plastic_fire')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(60, 50, 30), xb=Bounds3D.of(0, 6, 0, 5, 0, 3)))

# Polystyrene (very sooty)
sim.add(Reaction(
    id='POLYSTYRENE',
    fuel='POLYSTYRENE',
    heat_of_combustion=39800.0,
    soot_yield=0.164,  # High soot production
    co_yield=0.060,
    radiative_fraction=0.42  # High radiation due to soot
)

# Burning plastic items
sim.add(Surface(
    id='PLASTIC',
    hrrpua=800.0,
    color='BLACK'
)

sim.add(Obstruction(
    xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 1),
    surf_id='PLASTIC'
)

# Dense smoke monitoring
sim.add(Device(
    id='SOOT_CEILING',
    quantity='SOOT DENSITY',
    xyz=Point3D.of(3, 2.5, 2.9)
)

sim.add(Device(
    id='OPTICAL_DENSITY',
    quantity='OPTICAL DENSITY',
    xyz=Point3D.of(3, 2.5, 2.9)
)

sim.add(Device(
    id='VISIBILITY',
    quantity='VISIBILITY',
    xyz=Point3D.of(3, 2.5, 1.5)
)

sim.write('plastic_fire.fds')
```

### Multi-Fuel Fire

```python
sim = Simulation(chid='multi_fuel')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(80, 60, 30), xb=Bounds3D.of(0, 8, 0, 6, 0, 3)))

# Define multiple fuels
sim.add(Reaction(
    id='GASOLINE',
    fuel='N-HEPTANE',
    soot_yield=0.037,
    co_yield=0.010
)

sim.add(Reaction(
    id='WOOD',
    fuel='CELLULOSE',
    soot_yield=0.015,
    co_yield=0.004
)

# Gasoline pool fire
sim.add(Surface(id='GAS_POOL', hrrpua=2000.0, reac_id='GASOLINE'))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='GAS_POOL'))

# Wood pallet fire
sim.add(Surface(id='WOOD_PALLET', hrrpua=600.0, reac_id='WOOD'))
sim.add(Obstruction(xb=Bounds3D.of(5, 6, 3, 4, 0, 1), surf_id='WOOD_PALLET'))

# Monitor species from both fires
sim.add(Device(
    id='SOOT',
    quantity='SOOT DENSITY',
    xyz=Point3D.of(4, 3, 2)
)

sim.write('multi_fuel.fds')
```

## Radiation and Soot

### Soot-Radiation Coupling

Soot increases radiative heat transfer:

```python
# Clean fuel (low soot, low radiation)
sim.add(Reaction(
    id='METHANE',
    fuel='METHANE',
    soot_yield=0.001,
    radiative_fraction=0.15  # Low radiation
)

# Sooty fuel (high soot, high radiation)
sim.add(Reaction(
    id='HEPTANE',
    fuel='N-HEPTANE',
    soot_yield=0.037,
    radiative_fraction=0.33  # Higher radiation
)
```

### Measuring Radiative Heat Flux

```python
# Define sooty fuel
sim.add(Reaction(
    id='FUEL',
    fuel='N-HEPTANE',
    soot_yield=0.040,
    radiative_fraction=0.35
)

# Fire
sim.add(Surface(id='FIRE', hrrpua=1500.0, reac_id='FUEL'))
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE'))

# Radiative heat flux at distance
sim.add(Device(
    id='RAD_HF_1M',
    quantity='RADIATIVE HEAT FLUX',
    xyz=Point3D.of(4, 2.5, 1),
    ior=1  # Facing fire
)

sim.add(Device(
    id='GAUGE_HF_1M',
    quantity='GAUGE HEAT FLUX',
    xyz=Point3D.of(4, 2.5, 1),
    ior=1  # Total (rad + conv)
)
```

## Species Tracking

### Oxygen Depletion

```python
sim.add(Reaction(fuel='PROPANE'))

# Monitor oxygen concentration
sim.add(Device(
    id='O2_UPPER',
    quantity='VOLUME FRACTION',
    spec_id='OXYGEN',
    xyz=Point3D.of(3, 2.5, 2.5)
)

sim.add(Device(
    id='O2_LOWER',
    quantity='VOLUME FRACTION',
    spec_id='OXYGEN',
    xyz=Point3D.of(3, 2.5, 0.5)
)
```

### CO and CO2 Production

```python
sim.add(Reaction(
    id='FUEL',
    fuel='PROPANE',
    co_yield=0.010,  # CO production
    soot_yield=0.024
)

# Track CO (toxic)
sim.add(Device(
    id='CO_CONC',
    quantity='VOLUME FRACTION',
    spec_id='CARBON MONOXIDE',
    xyz=Point3D.of(3, 2.5, 1.5)
)

# Track CO2 (asphyxiant)
sim.add(Device(
    id='CO2_CONC',
    quantity='VOLUME FRACTION',
    spec_id='CARBON DIOXIDE',
    xyz=Point3D.of(3, 2.5, 1.5)
)
```

## Combustion Efficiency

### Fuel Lean vs Fuel Rich

```python
# Well-ventilated fire (complete combustion)
sim.add(Reaction(
    id='COMPLETE',
    fuel='PROPANE',
    co_yield=0.005,  # Low CO
    soot_yield=0.015  # Low soot
)

# Under-ventilated fire (incomplete combustion)
sim.add(Reaction(
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
sim.add(Reaction(
    id='WOOD',
    fuel='CELLULOSE',
    heat_of_combustion=15000.0,  # From SFPE Handbook
    soot_yield=0.015,
    co_yield=0.004
)

# Avoid: Made-up values
sim.add(Reaction(
    id='MYSTERY_FUEL',
    heat_of_combustion=100000.0,  # Unrealistic!
    soot_yield=0.5
)
```

### 2. Match Fuel to Application

```python
# Gasoline spill
sim.add(Reaction(fuel='N-HEPTANE')  # Good surrogate

# Natural gas leak
sim.add(Reaction(fuel='METHANE')  # Appropriate

# Wood structure
sim.add(Reaction(fuel='CELLULOSE')  # Reasonable approximation
```

### 3. Consider Soot Impact

```python
# For visibility/radiation studies, soot is critical
sim.add(Reaction(
    id='FUEL',
    soot_yield=0.037,  # Don't neglect!
    radiative_fraction=0.33
)

# Monitor soot effects
sim.add(Device(id='VISIBILITY', quantity='VISIBILITY', xyz=Point3D.of(3, 2.5, 1.5)))
sim.add(Device(id='SOOT', quantity='SOOT DENSITY', xyz=Point3D.of(3, 2.5, 2)))
```

### 4. Document Fuel Sources

```python
# Clear documentation
# Fuel properties from:
# - Heat of combustion: SFPE Handbook, 5th Ed.
# - Soot yield: Mulholland & Croarkin (2000)
# - CO yield: Tewarson (2008)
sim.add(Reaction(
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
    sim.add(Reaction(fuel='PROPANE', soot_yield=0.024)  # Realistic
    ```

??? question "Wrong radiative fraction"
    **Cause**: Inconsistent with soot production

    **Solution**: Higher soot → higher radiation
    ```python
    # Clean fuel
    sim.add(Reaction(fuel='METHANE', soot_yield=0.001, radiative_fraction=0.15))

    # Sooty fuel
    sim.add(Reaction(fuel='N-HEPTANE', soot_yield=0.037, radiative_fraction=0.33))
    ```

??? question "Unrealistic CO levels"
    **Cause**: Incorrect CO yield

    **Solution**: Use measured yields (typically 0.001-0.060)
    ```python
    # Well-ventilated
    sim.add(Reaction(co_yield=0.004))

    # Under-ventilated
    sim.add(Reaction(co_yield=0.020))
    ```

## Advanced Topics

### Simplified vs Detailed Chemistry

FDS uses simplified combustion (mixture fraction approach):

```python
# FDS default: Simple, fast
sim.add(Reaction(fuel='PROPANE')  # One-step chemistry

# For most fire simulations, this is sufficient
# Detailed chemistry (not in FDS) needed only for:
# - Flame chemistry research
# - Pollutant formation mechanisms
# - Ignition transients
```

### Heat Release Rate vs Fuel Mass

```python
# HRR specified directly (most common)
sim.add(Surface(id='FIRE', hrrpua=1000.0)  # kW/m²

# Mass flux (advanced)
sim.add(Surface(
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
