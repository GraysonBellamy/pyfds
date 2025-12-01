# Chemical Species

PyFDS provides comprehensive support for defining chemical species in FDS simulations, including predefined species libraries, custom species definitions, and advanced combustion modeling.

## Overview

Chemical species in FDS define the composition of gases, fuels, and combustion products in fire simulations. PyFDS supports:

- **36+ predefined species** with accurate molecular weights and properties
- **Custom species definitions** with chemical formulas and thermodynamic properties
- **Lumped species** for modeling gas mixtures
- **Background species** for ambient air composition
- **Advanced combustion parameters** for extinction models and mixing

## Predefined Species

PyFDS includes a comprehensive library of predefined species commonly used in fire modeling:

```python
from pyfds.builders.libraries import list_predefined_species, get_species_info
from pyfds import Simulation

# List all available species
species = list_predefined_species()
print(f"Available species: {len(species)}")
# Output: Available species: 36

# Get detailed information about a species
propane_info = get_species_info('PROPANE')
print(propane_info)
# Output: {'formula': 'C3H8', 'mw': 44.0956, 'description': 'Propane (LPG)', ...}
```

### Common Predefined Species

| Category | Examples |
|----------|----------|
| **Atmospheric Gases** | NITROGEN, OXYGEN, ARGON, CARBON_DIOXIDE |
| **Fuel Gases** | METHANE, ETHANE, PROPANE, HYDROGEN |
| **Combustion Products** | WATER_VAPOR, CARBON_MONOXIDE, SOOT |
| **Halogenated** | HFC-125, CF3BR (Halon 1301) |

### Creating Standard Air

```python
from pyfds.builders.libraries import create_standard_air
from pyfds import Simulation

sim = Simulation(chid="air_simulation")

# Create standard air with 40% relative humidity
air_composition = create_standard_air(humidity=40.0)

# Add as background species
sim.add_species(Species(**air_composition))

print(sim.to_fds())
```

## Custom Species

Define custom species with chemical formulas and properties:

```python
from pyfds import Simulation

sim = Simulation(chid="custom_fuel")

# Define a custom hydrocarbon fuel
custom_fuel = Species(
    id="MY_FUEL",
    formula="C6H14",  # Hexane
    mw=86.18,         # Molecular weight
    c=6, h=14,        # Elemental composition
    mass_fraction_0=0.0  # Not present initially
)

sim.add_species(custom_fuel)

# Define combustion reaction
sim.add(Reaction(
    fuel="MY_FUEL",
    heat_of_combustion=45000,  # kJ/kg
    c=6, h=14, o=0, n=0,      # Stoichiometric coefficients
    soot_yield=0.01,
    co_yield=0.005
)
```

### Species Properties

#### Basic Properties
- `id`: Unique species identifier
- `formula`: Chemical formula (e.g., "C3H8", "CH4")
- `mw`: Molecular weight [g/mol]
- `c`, `h`, `o`, `n`: Elemental composition (atoms per molecule)

#### Ambient Fractions
- `mass_fraction_0`: Initial mass fraction in ambient air
- `volume_fraction_0`: Initial volume fraction in ambient air

#### Thermophysical Properties
- `enthalpy`: Specific enthalpy [kJ/kg]
- `specific_heat`: Specific heat capacity [kJ/(kg·K)]
- `conductivity`: Thermal conductivity [W/(m·K)]
- `viscosity`: Dynamic viscosity [kg/(m·s)]
- `diffusivity`: Mass diffusivity [m²/s]

#### Aerosol Properties (for particulates)
- `aerosol`: True for particulate species
- `density_solid`: Solid density [kg/m³]
- `mean_diameter`: Mean particle diameter [μm]

## Lumped Species

Lumped species represent mixtures of multiple component species:

```python
from pyfds import Simulation

sim = Simulation(chid="lumped_example")

# Define component species (must be marked as lumped components)
sim.add(Species(id="N2_COMPONENT", lumped_component_only=True))
sim.add(Species(id="O2_COMPONENT", lumped_component_only=True))
sim.add(Species(id="CO2_COMPONENT", lumped_component_only=True))

# Define lumped air mixture
sim.add(Species(
    id="AIR_MIX",
    background=True,
    spec_id=["N2_COMPONENT", "O2_COMPONENT", "CO2_COMPONENT"],
    volume_fraction=[0.78, 0.21, 0.01]  # Must sum to 1.0
)

# Use in combustion
sim.add(Reaction(
    fuel="PROPANE",
    spec_id_nu=["PROPANE", "AIR_MIX", "CO2_COMPONENT", "WATER_VAPOR"],
    nu=[-1, -5, 3, 4]  # Stoichiometric coefficients
)
```

### Lumped Species Rules

- Component species must have `lumped_component_only=True`
- Lumped species can be `background=True` for ambient air
- Mass or volume fractions must sum to 1.0
- Background species cannot have `lumped_component_only=True`

## Background Species

Background species define the ambient gas composition:

```python
from pyfds import Simulation

sim = Simulation(chid="background_example")

# Define air as background
sim.add(Species(
    id="AIR",
    background=True,
    formula="AIR",
    mw=28.97,  # Average molecular weight
    mass_fraction_0=1.0  # 100% of ambient atmosphere
)

# Alternative: Use predefined air
from pyfds.builders.libraries import create_standard_air

air_dict = create_standard_air(humidity=50.0)
sim.add_species(Species(**air_dict))
```

### Background Species Constraints

- Only one background species allowed per simulation
- Must have `background=True`
- Cannot have `lumped_component_only=True`
- Typically has `mass_fraction_0=1.0` or `volume_fraction_0=1.0`

## Advanced Species Properties

### Temperature-Dependent Properties

Define temperature-dependent properties using ramps:

```python
from pyfds import Simulation

sim = Simulation(chid="temp_dependent")

# Create temperature ramps
sim.add(Ramp(id="VISCOSITY_RAMP", x=[20, 100, 500], f=[1.8e-5, 2.1e-5, 3.5e-5]))

# Define species with temperature-dependent viscosity
sim.add(Species(
    id="HOT_GAS",
    formula="N2",
    mw=28.0134,
    ramp_mu="VISCOSITY_RAMP"  # Viscosity ramp ID
)
```

#### Available Temperature Ramps
- `ramp_k`: Thermal conductivity
- `ramp_d`: Mass diffusivity
- `ramp_mu`: Dynamic viscosity
- `ramp_cp`: Specific heat capacity
- `ramp_g_f`: Gibbs free energy

### Radiation Properties

```python
from pyfds import Simulation

sim = Simulation(chid="radiation_example")

# Species with radiation absorption
sim.add(Species(
    id="CO2",
    formula="CO2",
    mw=44.0095,
    radcal_id="CO2_SURROGATE"  # RadCal surrogate for radiation
)
```

### Liquid Properties (for droplets)

```python
from pyfds import Simulation

sim = Simulation(chid="droplet_example")

# Fuel with liquid properties for droplet evaporation
sim.add(Species(
    id="HEPTANE",
    formula="C7H16",
    mw=100.2,
    # Liquid properties
    boiling_temperature=371.6,      # [°C]
    density_liquid=684.0,           # [kg/m³]
    specific_heat_liquid=2.24,      # [kJ/(kg·K)]
    heat_of_vaporization=317.0,     # [kJ/kg]
    surface_tension=0.0204          # [N/m]
)
```

## Combustion Parameters

Control global combustion behavior with the COMB namelist:

```python
from pyfds import Simulation

sim = Simulation(chid="combustion_example")

# Enable extinction model 2
sim.combustion(
    extinction_model="EXTINCTION 2",
    initial_unmixed_fraction=0.8,  # 80% unmixed
    finite_rate_min_temp=100.0     # Minimum temperature for finite-rate
)
```

### Extinction Models

- `"EXTINCTION 1"`: Default extinction model
- `"EXTINCTION 2"`: Enhanced extinction model

### Turbulent Combustion

- `initial_unmixed_fraction`: Initial mixture fraction (0.0 = premixed, 1.0 = unmixed)
- `fixed_mix_time`: Fixed mixing time [s]
- `tau_chem`, `tau_flame`: Mixing time bounds [s]

### Species Thresholds

- `zz_min_global`: Minimum mass fraction for reactions (default: 1e-10)
- `finite_rate_min_temp`: Minimum temperature for finite-rate reactions [°C]

## Integration Examples

### Simple Combustion

```python
from pyfds import Simulation

sim = Simulation(chid="simple_combustion")
sim.add(Time(t_end=60.0))
sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

# Use predefined species
sim.add(Species(id="PROPANE", mass_fraction_0=0.0))
sim.add(Species(id="OXYGEN", mass_fraction_0=0.23))
sim.add(Species(id="NITROGEN", mass_fraction_0=0.77))

# Combustion reaction
sim.add(Reaction(
    fuel="PROPANE",
    soot_yield=0.01,
    co_yield=0.005
)

# Fire source
sim.add(Vent(
    id="FIRE",
    xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0, 0),
    surface="burner"
)

print(sim.to_fds())
```

### Finite-Rate Chemistry

```python
from pyfds import Simulation

sim = Simulation(chid="finite_rate")

# Species definitions
sim.add(Species(id="CH4", mass_fraction_0=0.0)  # Methane
sim.add(Species(id="O2", mass_fraction_0=0.23)  # Oxygen
sim.add(Species(id="CO2", mass_fraction_0=0.0)  # Carbon dioxide
sim.add(Species(id="H2O", mass_fraction_0=0.0)  # Water vapor

# Finite-rate reaction
sim.add(Reaction(
    fuel="CH4",
    # Arrhenius parameters
    a=8.6e11,      # Pre-exponential factor
    e=125520,      # Activation energy [J/mol]
    n_t=0.0,       # Temperature exponent
    # Concentration exponents
    spec_id_n_s=["CH4", "O2"],
    n_s=[0.2, 1.3],  # Reaction orders
    # Stoichiometry
    spec_id_nu=["CH4", "O2", "CO2", "H2O"],
    nu=[-1, -2, 1, 2]
)

# Combustion parameters
sim.combustion(
    finite_rate_min_temp=300.0,  # Minimum temperature for reaction
    zz_min_global=1e-8           # Species threshold
)
```

## Best Practices

### Species Definition Order
1. Define component species first (for lumped mixtures)
2. Define lumped species next
3. Define background species
4. Define fuel and product species

### Validation
- Use `get_species_info()` to verify predefined species
- Check that mass/volume fractions sum to 1.0 for mixtures
- Ensure background species are unique
- Validate molecular weights against literature values

### Performance
- Use lumped species for complex mixtures to reduce computational cost
- Set appropriate `zz_min_global` values to avoid tracking trace species
- Use temperature-dependent properties only when necessary

### Debugging
- Check FDS output with `sim.to_fds()` before running simulations
- Verify species IDs match between reactions and species definitions
- Use FDS diagnostic output to check species conservation

## See Also

- [API Reference: Species](../api/species.md) - Complete API documentation
- [Examples: Chemical Species](../examples/species.md) - Working examples
- [Combustion Chemistry](combustion_chemistry.md) - Advanced combustion modeling
- [FDS User Guide Chapter 12](https://pages.nist.gov/fds-smv/manuals/fds-user-guide/) - Official FDS documentation
