# Liquid Fuel Guide

This guide covers liquid fuel evaporation and combustion modeling in pyfds, including pool fires, liquid mixtures, and evaporation physics.

## Overview

Liquid fuels require special treatment in FDS due to their evaporation behavior. When `BOILING_TEMPERATURE` is specified in a `MATL` namelist, FDS automatically switches to the liquid fuel pyrolysis model, which simulates:

- Temperature-dependent evaporation
- Heat of vaporization effects
- Liquid surface regression
- Coupling with gas-phase combustion

## Basic Liquid Fuel Setup

### Simple Liquid Fuel

```python
from pyfds import Simulation
from pyfds.core.namelists import Material

# Create simulation
sim = Simulation(chid="liquid_fuel", title="Liquid Fuel Example")

# Define liquid fuel
methanol = Material(
    id="METHANOL_LIQUID",
    density=792.0,              # Liquid density (kg/m³)
    conductivity=0.2,           # Thermal conductivity (W/(m·K))
    specific_heat=2.51,         # Specific heat (kJ/(kg·K))
    # Liquid fuel parameters
    boiling_temperature=64.7,   # Boiling point (°C)
    spec_id="METHANOL",         # Vapor species
    heat_of_reaction=837.0,     # Heat of combustion (kJ/kg)
    absorption_coefficient=140.0  # Absorption coefficient (1/m)
)

sim.material_mgr.add_material(methanol)
```

### Complete Pool Fire Setup

For a pool fire, you need both the liquid fuel and a surface to contain it:

```python
from pyfds.core.namelists import Surface

# Liquid fuel material
methanol = Material(
    id="METHANOL_LIQUID",
    density=792.0,
    conductivity=0.2,
    specific_heat=2.51,
    boiling_temperature=64.7,
    spec_id="METHANOL",
    heat_of_reaction=837.0,
    absorption_coefficient=140.0
)

# Pool surface (liquid layer)
pool_surface = Surface(
    id="METHANOL_POOL",
    matl_id="METHANOL_LIQUID",
    thickness=0.05,  # Liquid depth (m)
    tmp_front=25.0   # Initial temperature (°C)
)

sim.material_mgr.add_material(methanol)
sim.material_mgr.add_surface(pool_surface)
```

## Advanced Liquid Fuel Parameters

### Heat of Vaporization

For accurate evaporation modeling:

```python
fuel = Material(
    id="FUEL",
    density=800.0,
    conductivity=0.15,
    specific_heat=2.0,
    boiling_temperature=100.0,
    spec_id="FUEL_VAPOR",
    heat_of_reaction=1500.0,
    # Advanced evaporation
    heat_of_vaporization=500.0,  # Heat of vaporization (kJ/kg)
    mw=46.0,                     # Molecular weight (g/mol)
)
```

### Liquid Mixtures

For multi-component liquids:

```python
# Define individual components
ethanol = Material(
    id="ETHANOL_LIQUID",
    density=789.0,
    conductivity=0.17,
    specific_heat=2.44,
    boiling_temperature=78.5,
    spec_id="ETHANOL",
    heat_of_reaction=1367.0,
    absorption_coefficient=1140.0
)

water = Material(
    id="WATER_LIQUID",
    density=1000.0,
    conductivity=0.6,
    specific_heat=4.18,
    boiling_temperature=100.0,
    spec_id="WATER_VAPOR",
    heat_of_reaction=0.0,  # Non-combustible
    absorption_coefficient=1000.0
)

# Mixture surface (volume fractions)
mixture = Surface(
    id="ETHANOL_WATER_MIX",
    matl_id=["ETHANOL_LIQUID", "WATER_LIQUID"],
    thickness=0.05,
    matl_mass_fraction=[0.8, 0.2],  # 80% ethanol, 20% water by mass
    tmp_front=25.0
)
```

## Validation Rules

### Required Parameters

When `BOILING_TEMPERATURE` is specified:

- `SPEC_ID` must be provided (vapor species)
- `HEAT_OF_REACTION` should be set (combustion energy)
- Cannot use `N_REACTIONS > 1`
- Cannot use multi-reaction arrays

### Parameter Ranges

- Boiling temperature: Typically 20-300°C
- Heat of vaporization: 100-2000 kJ/kg
- Molecular weight: 10-200 g/mol
- Absorption coefficient: 100-5000 1/m

## Surface Configuration

### Pool Geometry

```python
from pyfds.core.namelists import Obstacle

# Define pool boundaries
pool = Obstacle(
    id="POOL",
    xb=[2.0, 3.0, 2.0, 3.0, 0.0, 0.05],  # 1m x 1m x 5cm deep
    surf_id="METHANOL_POOL"
)

sim.geometry.add_obstacle(pool)
```

### Ignition Sources

```python
# Ignition surface
ignition = Surface(
    id="IGNITER",
    hrrpua=1000.0,  # Heat release rate (kW/m²)
    ignition_temperature=400.0
)

# Small ignition patch
igniter = Obstacle(
    id="IGNITER_PATCH",
    xb=[2.45, 2.55, 2.45, 2.55, 0.0, 0.01],  # Small patch
    surf_id="IGNITER"
)

sim.geometry.add_obstacle(igniter)
```

## Combustion Chemistry

### Gas-Phase Reactions

Define the combustion reaction for the vapor:

```python
from pyfds.core.namelists import Reaction

combustion = Reaction(
    id="METHANOL_COMBUSTION",
    fuel="METHANOL",
    soot_yield=0.015,
    co_yield=0.001,
    radiative_fraction=0.35
)

sim.physics.add_reaction(combustion)
```

### Multi-Step Chemistry

For more detailed combustion:

```python
# Two-step combustion
step1 = Reaction(
    fuel="METHANOL",
    formula="C H4 O",
    co_yield=0.5,
    soot_yield=0.01
)

step2 = Reaction(
    fuel="CO",
    co_yield=0.0,
    soot_yield=0.005
)

sim.physics.add_reaction(step1)
sim.physics.add_reaction(step2)
```

## Best Practices

### Pool Fire Experiments

1. **Grid Resolution**: Use fine grids near the pool surface (≤1cm)
2. **Boundary Conditions**: Ensure proper vent boundaries
3. **Ignition**: Use small ignition patches to avoid unrealistic burning
4. **Monitoring**: Add thermocouples and flux gauges

### Parameter Estimation

1. **Boiling Temperature**: Use literature values or measure experimentally
2. **Heat of Vaporization**: Calculate from vapor pressure data
3. **Absorption Coefficient**: Measure or use correlations
4. **Combustion Properties**: Use standard values or measure in burners

### Common Issues

1. **No evaporation**: Check boiling temperature is below flame temperature
2. **Unstable burning**: Adjust absorption coefficient
3. **Incorrect burning rate**: Verify heat of vaporization
4. **Cross-reference errors**: Ensure SPEC_ID matches REAC fuel

## Examples

### Methanol Pool Fire

```python
# Complete methanol pool fire setup
methanol = Material(
    id="METHANOL_LIQUID",
    density=792.0,
    conductivity=0.2,
    specific_heat=2.51,
    boiling_temperature=64.7,
    spec_id="METHANOL",
    heat_of_reaction=837.0,
    absorption_coefficient=140.0
)

pool_surf = Surface(
    id="METHANOL_POOL",
    matl_id="METHANOL_LIQUID",
    thickness=0.05,
    tmp_front=25.0
)

combustion = Reaction(fuel="METHANOL", soot_yield=0.015)
```

### Gasoline Pool Fire

```python
# Gasoline surrogate
gasoline = Material(
    id="GASOLINE_LIQUID",
    density=750.0,
    conductivity=0.12,
    specific_heat=2.0,
    boiling_temperature=80.0,  # Representative boiling point
    spec_id="GASOLINE_VAPOR",
    heat_of_reaction=44000.0,  # MJ/kg
    absorption_coefficient=1000.0,
    heat_of_vaporization=350.0
)
```

See the examples directory for complete working pool fire simulations.
