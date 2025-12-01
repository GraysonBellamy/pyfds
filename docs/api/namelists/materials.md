# Materials and Surfaces

FDS material and surface namelists define thermal and combustion properties of boundaries.

## Overview

Materials and surfaces in FDS control:

- **Heat transfer**: Convection, radiation, conduction
- **Combustion**: Pyrolysis, burning rates
- **Thermal properties**: Conductivity, specific heat, density
- **Boundary conditions**: Heat flux, temperature

## SURF - Surface Properties

The SURF namelist defines boundary conditions and material properties.

### Basic Thermal Properties

```python
from pyfds import Surface

# Simple surface with heat release
fire = Surface(
    id='FIRE',
    hrrpua=500.0,  # Heat release rate per unit area [kW/m²]
    tmp_front=800.0  # Surface temperature [°C]
)

# Inert surface
wall = Surface(
    id='WALL',
    emissivity=0.9,
    tmp_front=20.0
)
```

### Multi-Layer Materials

```python
# Multi-layer wall
wall = Surface(
    id='INSULATED_WALL',
    matl_id=['GYPSUM', 'INSULATION', 'GYPSUM'],  # Layer materials
    thickness=[0.013, 0.089, 0.013],             # Layer thicknesses [m]
    matl_mass_fraction=[[1.0], [1.0], [1.0]]     # Mass fractions
)
```

### Advanced Heat Transfer

```python
# Surface with 3D heat conduction
thermal_surface = Surface(
    id='THERMAL_WALL',
    matl_id='CONCRETE',
    thickness=0.2,
    ht3d=True,  # Enable 3D heat conduction
    tmp_inner=20.0  # Initial interior temperature
)

# Custom convection
custom_ht = Surface(
    id='CUSTOM_CONVECTION',
    heat_transfer_coefficient=10.0,  # Fixed HTC [W/(m²·K)]
    heat_transfer_model='LOGLAW'     # Heat transfer correlation
)
```

### Thermal Boundary Conditions

```python
# Fixed temperature back surface
insulated = Surface(
    id='INSULATED',
    tmp_back=20.0,  # Fixed back temperature
    adiabatic=False
)

# Adiabatic surface (no heat transfer)
adiabatic = Surface(
    id='ADIABATIC',
    adiabatic=True
)
```

### Time-Varying Conditions

```python
# Time-varying heat flux
transient = Surface(
    id='TRANSIENT_HEAT',
    ramp_ef='HEAT_RAMP'  # External flux ramp
)

# Temperature profile
temp_profile = Surface(
    id='TEMP_PROFILE',
    ramp_t='TEMPERATURE_RAMP'
)
```

## MATL - Material Definitions

The MATL namelist defines bulk material properties for pyrolysis and thermal response.

### Simple Materials

```python
from pyfds import Material

# Basic thermal material
steel = Material(
    id='STEEL',
    density=7850.0,        # [kg/m³]
    thermal_conductivity=45.0,  # [W/(m·K)]
    specific_heat=0.46,    # [kJ/(kg·K)]
    emissivity=0.9
)

# Insulating material
insulation = Material(
    id='INSULATION',
    density=50.0,
    thermal_conductivity=0.04,
    specific_heat=1.2,
    emissivity=0.9
)
```

### Pyrolysis Materials

```python
# Single-step pyrolysis
wood = Material(
    id='WOOD',
    density=500.0,
    thermal_conductivity=0.12,
    specific_heat=2.5,
    # Pyrolysis reaction
    a=1e10,        # Pre-exponential factor [1/s]
    e=150000.0,    # Activation energy [J/mol]
    nu_matl=1.0,   # Stoichiometric coefficient
    matl_id='CHAR' # Residue material
)

# Multi-step pyrolysis
polymer = Material(
    id='POLYMER',
    density=1200.0,
    thermal_conductivity=0.25,
    specific_heat=1.5,
    # Multiple reactions
    a=[1e12, 5e8],
    e=[180000.0, 120000.0],
    nu_matl=[0.3, 0.7],
    matl_id=['VOLATILES', 'CHAR'],
    nu_spec=[1.0, 0.5],      # Gas yields
    spec_id=['FUEL', 'CO']
)
```

### Structured Pyrolysis API

pyfds provides a structured API for defining pyrolysis reactions that is cleaner and less error-prone than using parallel arrays.

#### PyrolysisReaction Class

The `PyrolysisReaction` class represents a single decomposition reaction with validation for kinetic parameters and product yields.

```python
from pyfds.core.models import PyrolysisReaction, PyrolysisProduct

# Arrhenius kinetics with gas and char products
reaction = PyrolysisReaction(
    a=1e10,                    # Pre-exponential factor [1/s]
    e=100000,                  # Activation energy [kJ/kmol]
    heat_of_reaction=500,      # Endothermic [kJ/kg]
    products=[
        PyrolysisProduct(spec_id="FUEL_GAS", nu_spec=0.8),
        PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
    ]
)

# Simplified kinetics with reference temperature
reaction = PyrolysisReaction(
    reference_temperature=350,  # Peak temperature [°C]
    pyrolysis_range=80,        # Temperature width [°C]
    heat_of_reaction=1000,
    products=[
        PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
        PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
    ]
)
```

#### Using with Material Class

You can use `PyrolysisReaction` directly with the `Material` class:

```python
from pyfds.core.models import PyrolysisReaction, PyrolysisProduct
from pyfds.core.namelists import Material

material = Material(
    id="WOOD",
    density=500,
    conductivity=0.13,
    specific_heat=2.5,
    reactions=[
        PyrolysisReaction(
            reference_temperature=350,
            heat_of_reaction=500,
            products=[
                PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
            ]
        )
    ]
)
```

> **Note:** The `reactions` list provides structured pyrolysis modeling with full validation. Use `PyrolysisReaction` objects to define reaction kinetics and products.

### Advanced Material Properties

```python
# Material with oxidation
steel_oxidizing = Material(
    id='STEEL_OXIDIZING',
    density=7850.0,
    thermal_conductivity=45.0,
    specific_heat=0.46,
    # Oxidation
    surface_oxidation_model=True,
    nu_o2=1.0,      # Oxygen consumption
    nu_h2o=0.5      # Water production
)

# Temperature-dependent properties
temp_dependent = Material(
    id='TEMP_DEPENDENT',
    density=1000.0,
    # Temperature-dependent conductivity
    thermal_conductivity_ramp='K_RAMP',
    specific_heat_ramp='CP_RAMP'
)
```

## Integration Examples

### Complete Wall Assembly

```python
from pyfds import Surface, Material

# Define materials
gypsum = Material(
    id='GYPSUM',
    density=800.0,
    thermal_conductivity=0.16,
    specific_heat=1.0
)

insulation = Material(
    id='INSULATION',
    density=50.0,
    thermal_conductivity=0.04,
    specific_heat=1.2
)

# Create surface with layers
wall = Surface(
    id='WALL_ASSEMBLY',
    matl_id=['GYPSUM', 'INSULATION', 'GYPSUM'],
    thickness=[0.013, 0.089, 0.013],
    ht3d=True  # Enable 3D conduction
)
```

### Burning Object

```python
from pyfds import Surface, Material

# Fuel material
fuel = Material(
    id='FUEL',
    density=400.0,
    thermal_conductivity=0.1,
    specific_heat=2.0,
    # Pyrolysis
    a=1e8,
    e=100000.0,
    nu_matl=1.0,
    spec_id='PROPANE',
    nu_spec=1.0
)

# Burning surface
burner = Surface(
    id='BURNER',
    matl_id='FUEL',
    thickness=0.05,
    hrrpua=300.0,
    tmp_front=400.0
)
```

### Insulated Structure

```python
# Steel structure with insulation
steel = Material(id='STEEL', density=7850, thermal_conductivity=45, specific_heat=0.46)
mineral_wool = Material(id='MINERAL_WOOL', density=130, thermal_conductivity=0.04, specific_heat=0.8)

insulated_steel = Surface(
    id='INSULATED_STEEL',
    matl_id=['STEEL', 'MINERAL_WOOL'],
    thickness=[0.005, 0.05],  # 5mm steel, 50mm insulation
    tmp_back=20.0,            # Fixed back temperature
    ht3d=True
)
```

## Parameter Reference

### SURF Parameters

#### Basic Properties
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `id` | Surface identifier | - | Required |
| `rgb` | RGB color | 0-255 | - |
| `color` | Named color | - | - |
| `transparency` | Transparency | 0-1 | 1.0 |

#### Heat Release
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `hrrpua` | Heat release rate per unit area | kW/m² | 0.0 |
| `tmp_front` | Front surface temperature | °C | 20.0 |
| `tmp_front_initial` | Initial front temperature | °C | - |

#### Heat Transfer
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `emissivity` | Surface emissivity | - | 0.9 |
| `emissivity_back` | Back surface emissivity | - | - |
| `adiabatic` | Adiabatic surface | - | False |
| `heat_transfer_coefficient` | Fixed HTC | W/(m²·K) | - |
| `heat_transfer_model` | HTC model | - | - |

#### Material Layers
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `matl_id` | Material IDs | - | - |
| `thickness` | Layer thicknesses | m | - |
| `matl_mass_fraction` | Mass fractions | - | - |

#### Advanced Thermal
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `ht3d` | 3D heat conduction | - | False |
| `tmp_inner` | Initial interior temperature | °C | - |
| `tmp_back` | Fixed back temperature | °C | - |

### MATL Parameters

#### Basic Properties
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `id` | Material identifier | - | Required |
| `density` | Material density | kg/m³ | Required |
| `thermal_conductivity` | Thermal conductivity | W/(m·K) | Required |
| `specific_heat` | Specific heat capacity | kJ/(kg·K) | Required |
| `emissivity` | Surface emissivity | - | 0.9 |

#### Pyrolysis
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `a` | Pre-exponential factor | 1/s | - |
| `e` | Activation energy | J/mol | - |
| `nu_matl` | Material stoichiometric coeff | - | - |
| `matl_id` | Residue material | - | - |
| `spec_id` | Gas species | - | - |
| `nu_spec` | Gas stoichiometric coeff | - | - |

#### Advanced
| Parameter | Description | Units | Default |
|-----------|-------------|-------|---------|
| `surface_oxidation_model` | Enable oxidation | - | False |
| `thermal_conductivity_ramp` | Temperature-dependent k | - | - |
| `specific_heat_ramp` | Temperature-dependent Cp | - | - |

## See Also

- [Materials Guide](../../guide/materials-surfaces.md) - Complete usage guide
- [Builder API](../builders/index.md) - Fluent interfaces for materials
- [Examples](../../examples/index.md) - Complete material examples
