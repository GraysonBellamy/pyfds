# PyFDS Phase 3: Complex Namelists Implementation Guide

## 🎯 Quick Reference for AI Implementation

This guide provides the exact implementation instructions for Phase 3 of PyFDS, focusing on complex namelist groups: **&MATL**, **&REAC**, **&RAMP**, **&CTRL**, **&PROP**, and **&INIT**.

## 📁 File Structure to Create

```
pyfds/
├── namelists/
│   ├── complex/
│   │   ├── __init__.py
│   │   ├── material.py        # &MATL implementation
│   │   ├── reaction.py        # &REAC implementation
│   │   ├── ramp.py           # &RAMP implementation
│   │   ├── control.py        # &CTRL implementation
│   │   ├── property.py       # &PROP implementation
│   │   └── initial.py        # &INIT implementation
│   └── validators/
│       ├── material_validator.py
│       ├── reaction_validator.py
│       └── cross_reference.py
└── tests/
    └── test_complex_namelists/
        ├── test_material.py
        ├── test_reaction.py
        ├── test_ramp.py
        └── test_integration.py
```

## 🔧 Implementation Order

### Step 1: RAMP Implementation (Foundation)
**File:** `pyfds/namelists/complex/ramp.py`

**Key Classes:**
- `Ramp`: Core ramp function class
- `RampBuilder`: Builder pattern for complex ramps
- `PredefinedRamps`: Common ramp functions

**Critical Features:**
```python
- Points storage as List[Tuple[float, float]]
- Linear interpolation with numpy.interp
- Temperature vs time ramp distinction
- Validation of point uniqueness
- to_fds() method generating "&RAMP ID='X', T=Y, F=Z /"
```

**Test First:** Create 5 ramps (linear, step, sine, t-squared, temperature)

---

### Step 2: MATL Implementation
**File:** `pyfds/namelists/complex/material.py`

**Key Classes:**
- `Material`: Material properties dataclass
- `MaterialBuilder`: Complex material construction

**Critical Features:**
```python
@dataclass
class Material:
    id: str
    density: float
    conductivity: Optional[float]
    specific_heat: Optional[float]
    conductivity_ramp: Optional[str]  # Links to RAMP
    specific_heat_ramp: Optional[str]  # Links to RAMP

    # Pyrolysis arrays (length = n_reactions)
    a: Optional[List[float]]  # Pre-exponential
    e: Optional[List[float]]  # Activation energy
    heat_of_reaction: Optional[List[float]]
```

**Validation Rules:**
- Density: 1-10000 kg/m³
- Conductivity: 0.001-1000 W/(m·K)
- Must have conductivity OR conductivity_ramp
- Pyrolysis arrays must match n_reactions length

---

### Step 3: REAC Implementation
**File:** `pyfds/namelists/complex/reaction.py`

**Key Classes:**
- `Reaction`: Combustion reaction specification
- `ReactionBuilder`: Reaction configuration

**Fuel Database:**
```python
_fuel_database = {
    'METHANE': {'c': 1, 'h': 4, 'hoc': 50000},
    'PROPANE': {'c': 3, 'h': 8, 'hoc': 46000},
    'N-HEPTANE': {'c': 7, 'h': 16, 'hoc': 44600},
    'POLYURETHANE': {'c': 3.52, 'h': 5.48, 'o': 0.88, 'n': 0.32, 'hoc': 23200}
}
```

**Stoichiometry Calculation:**
```python
def calculate_stoichiometry(self):
    o2_required = self.c + self.h/4.0 - self.o/2.0
    co2 = self.c * (1 - self.co_yield - self.soot_yield)
    h2o = self.h/2.0 * (1 - self.h2_yield)
```

---

### Step 4: CTRL Implementation
**File:** `pyfds/namelists/complex/control.py`

**Control Functions Enum:**
```python
class ControlFunction(Enum):
    ANY = 'ANY'          # OR logic
    ALL = 'ALL'          # AND logic
    TIME_DELAY = 'TIME_DELAY'
    PID = 'PID'
    DEADBAND = 'DEADBAND'
    # ... 15 more functions
```

**Key Validations:**
- ANY/ALL require List[str] input_id
- PID requires at least one gain parameter
- Input devices must exist in simulation

---

### Step 5: PROP & INIT Implementation
**Files:** `property.py`, `initial.py`

**DeviceProperty Key Fields:**
- Sprinkler: `flow_rate`, `k_factor`, `rti`, `spray_angle`
- Detector: `activation_temperature`, `activation_obscuration`
- Special: `spark`, `cable` flags

**InitialCondition Key Fields:**
- Region: `xb` or `xyz`
- Values: `temperature`, `density`
- Species: `spec_id`, `mass_fraction`/`volume_fraction`

---

### Step 6: Cross-Reference Validator
**File:** `pyfds/namelists/validators/cross_reference.py`

**Validates:**
1. Material ramp references → Ramp exists
2. Material species → Species defined
3. Control input_id → Devices exist
4. Control ramp_id → Ramp exists

**Implementation:**
```python
class CrossReferenceValidator:
    materials: Dict[str, Material]
    ramps: Dict[str, Ramp]
    controls: Dict[str, Control]
    devices: Set[str]

    def validate_material(self, material: Material) -> List[str]:
        errors = []
        if material.conductivity_ramp:
            if material.conductivity_ramp not in self.ramps:
                errors.append(f"CONDUCTIVITY_RAMP '{material.conductivity_ramp}' not found")
        return errors
```

---

### Step 7: Simulation Integration
**File:** Update `pyfds/simulation.py`

**Add to Simulation class:**
```python
class Simulation:
    def __init__(self):
        # Add complex namelist storage
        self.materials: Dict[str, Material] = {}
        self.reactions: List[Reaction] = []
        self.ramps: Dict[str, Ramp] = {}
        self.controls: Dict[str, Control] = {}
        self.properties: Dict[str, DeviceProperty] = {}
        self.initial_conditions: List[InitialCondition] = []

        # Add validator
        self._validator = CrossReferenceValidator()

    def add_material(self, material: Material):
        # Register with validator
        self._validator.materials[material.id] = material
        # Validate references
        errors = self._validator.validate_material(material)
        if errors:
            raise ValueError("\n".join(errors))
        self.materials[material.id] = material
```

**Write Order in FDS file:**
1. &HEAD
2. &TIME, &MISC, &DUMP
3. &MESH
4. &RAMP (referenced by materials)
5. &MATL
6. &REAC
7. &PROP
8. &SURF (uses MATL)
9. &OBST, &VENT
10. &DEVC (uses PROP)
11. &CTRL (references DEVC)
12. &INIT
13. &TAIL

---

## ✅ Testing Requirements

### Unit Tests per Module:
- **test_material.py**: 15 tests minimum
  - Simple material
  - Temperature-dependent properties
  - Multi-reaction pyrolysis
  - Validation errors
  - Builder pattern

- **test_reaction.py**: 12 tests minimum
  - Predefined fuels
  - Custom fuel composition
  - Stoichiometry calculations
  - Product yields validation
  - Complex chemistry

- **test_ramp.py**: 10 tests minimum
  - Linear interpolation
  - Step functions
  - Sine waves
  - Temperature ramps
  - Evaluation at points

- **test_control.py**: 10 tests minimum
  - Logic functions (ANY, ALL)
  - PID controller
  - Deadband
  - Time delays
  - Input validation

### Integration Tests:
- Material with temperature ramps
- Control with device references
- Complete fire scenario
- Cross-reference validation
- FDS file generation

---

## 📝 Implementation Instructions for AI

### Prompt Template:
```
Implement the PyFDS Phase 3 complex namelists following these specifications:

1. Start with ramp.py - implement Ramp class with:
   - Points list sorted by T value
   - Linear interpolation using numpy
   - to_fds() method
   - RampBuilder with helper methods

2. Then implement material.py with:
   - Material dataclass with all fields listed
   - Validation ranges for properties
   - Support for temperature-dependent properties via ramp references
   - MaterialBuilder for complex materials

3. Continue with reaction.py including:
   - Fuel database dictionary
   - Stoichiometry calculations
   - Auto-setup from fuel name

4. Implement control.py with:
   - ControlFunction enum (all 20+ types)
   - Control dataclass
   - Function-specific validation
   - ControlBuilder helpers

5. Add cross_reference.py validator

6. Update simulation.py to integrate all components

7. Write comprehensive tests achieving >95% coverage

Use Python 3.8+, type hints, dataclasses, and follow PEP 8.
Generate complete, working code with docstrings.
```

---

## 🔥 Complete Working Example

```python
from pyfds import Simulation
from pyfds.namelists.complex import *

# Create simulation
sim = Simulation('apartment_fire')

# Add reaction
reaction = ReactionBuilder() \
    .use_fuel('POLYURETHANE') \
    .set_yields(soot=0.10, co=0.02) \
    .build()
sim.add_reaction(reaction)

# Add temperature ramps for material
k_ramp = RampBuilder('GYPSUM_K') \
    .temperature_dependent({
        20: 0.48, 100: 0.45, 500: 0.30
    }).build()
sim.add_ramp(k_ramp)

# Add material using ramp
gypsum = MaterialBuilder('GYPSUM') \
    .set_thermal_properties(
        conductivity_ramp='GYPSUM_K',
        density=930
    ) \
    .add_pyrolysis_reaction(
        a=1e10, e=80000,
        heat_of_reaction=1000
    ) \
    .build()
sim.add_material(gypsum)

# Add HRR growth ramp
fire_ramp = RampBuilder('FIRE_GROWTH') \
    .add_points([
        (0, 0), (120, 500), (300, 2000)
    ]).build()
sim.add_ramp(fire_ramp)

# Add control logic
control = ControlBuilder('SMOKE_ALARM') \
    .any_input(['SD_1', 'SD_2', 'SD_3']) \
    .build()
sim.add_control(control)

# Generate FDS file
sim.write('apartment_fire.fds')
```

---

## 🎯 Success Criteria

✅ All 6 namelist groups implemented
✅ Cross-reference validation working
✅ 50+ unit tests passing
✅ Integration tests with complete scenarios
✅ Generated FDS files validate in actual FDS
✅ Documentation with examples
✅ Type hints throughout
✅ >95% code coverage

---

## 📚 Key FDS Documentation References

- **&MATL**: FDS User Guide Section 8.3 (Material Properties)
- **&REAC**: Section 13 (Combustion)
- **&RAMP**: Section 11 (Time-Dependent Functions)
- **&CTRL**: Section 18.5 (Control Functions)
- **&PROP**: Section 18.2 (Device Properties)
- **&INIT**: Section 12.2 (Initial Conditions)

---

*Use this guide to implement Phase 3 systematically. Start with RAMP (foundation), then MATL (uses RAMP), then REAC, CTRL, PROP, and INIT. Test each component thoroughly before integration.*
