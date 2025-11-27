PyFDS Phase 3: Complex Namelist Implementation Plan

Detailed Implementation Guide for &MATL, &REAC, &RAMP, &CTRL, &PROP, and &INIT

Document Version: 1.0
Date: 2025-11-20
Phase 3 Technical Specification

# Executive Summary

This document provides the complete implementation specification for Phase 3 of the PyFDS project, focusing on the complex namelist groups that handle materials, reactions, time-dependent functions, control logic, device properties, and initial conditions. These components are essential for advanced fire simulations including multi-layer heat transfer, complex chemistry, and control systems.

Key deliverables:

- Material properties system (&MATL) with temperature-dependent properties
- Reaction chemistry module (&REAC) supporting multiple fuel types
- Time-dependent function framework (&RAMP) for dynamic parameters
- Control logic system (&CTRL) for device interactions
- Device properties module (&PROP) for sensors and actuators
- Initial conditions handler (&INIT) for complex setups

# Table of Contents

- 1\. Implementation Overview
- 2\. &MATL - Material Properties Implementation
- 3\. &REAC - Reaction Module Implementation
- 4\. &RAMP - Time Functions Implementation
- 5\. &CTRL - Control Logic Implementation
- 6\. &PROP - Device Properties Implementation
- 7\. &INIT - Initial Conditions Implementation
- 8\. Integration Architecture
- 9\. Testing Requirements
- 10\. Implementation Checklist

# 1\. Implementation Overview

## 1.1 Module Structure

Create the following module structure within the PyFDS project:

pyfds/
├── namelists/
│ ├── complex/
│ │ ├── \__init_\_.py
│ │ ├── material.py # &MATL implementation
│ │ ├── reaction.py # &REAC implementation
│ │ ├── ramp.py # &RAMP implementation
│ │ ├── control.py # &CTRL implementation
│ │ ├── property.py # &PROP implementation
│ │ └── initial.py # &INIT implementation
│ └── validators/
│ ├── material_validator.py
│ ├── reaction_validator.py
│ └── ramp_validator.py
└── tests/
└── test_complex_namelists/
├── test_material.py
├── test_reaction.py
├── test_ramp.py
├── test_control.py
├── test_property.py
└── test_initial.py

## 1.2 Design Principles

- Immutable data classes with validation
- Builder pattern for complex configurations
- Cross-reference validation between namelists
- Physical property validation (ranges, units)
- Temperature-dependent property support
- Comprehensive error messages with suggestions

# 2\. &MATL - Material Properties Implementation

## 2.1 Class Design

\# pyfds/namelists/complex/material.py
from typing import Optional, List, Union, Dict
from dataclasses import dataclass, field
from enum import Enum
import numpy as np
<br/>class MaterialModel(Enum):
"""Material model types"""
SOLID = 'SOLID'
LIQUID = 'LIQUID'
CHAR = 'CHAR'
<br/>class PyrolysisModel(Enum):
"""Pyrolysis model types"""
SIMPLE = 'SIMPLE'
COMPLEX = 'COMPLEX'
<br/>@dataclass
class Material:
"""
Represents FDS &MATL namelist group for material properties.
<br/>Attributes:
id: Unique identifier for the material
density: Material density \[kg/m³\]
conductivity: Thermal conductivity \[W/(m·K)\]
specific_heat: Specific heat capacity \[kJ/(kg·K)\]
emissivity: Surface emissivity \[0-1\]
absorption_coefficient: Radiation absorption \[1/m\]
n_reactions: Number of reaction components
reference_temperature: Reference temperature for properties \[°C\]
heat_of_reaction: Heat of pyrolysis/vaporization \[kJ/kg\]
heat_of_combustion: Heat of combustion \[kJ/kg\]
"""
id: str
density: float
conductivity: float = None
specific_heat: float = None
emissivity: float = 0.9
absorption_coefficient: float = 50000.0
n_reactions: int = 1
reference_temperature: float = None
heat_of_reaction: Optional\[List\[float\]\] = None
heat_of_combustion: Optional\[List\[float\]\] = None
<br/>\# Temperature-dependent properties
conductivity_ramp: Optional\[str\] = None
specific_heat_ramp: Optional\[str\] = None
<br/>\# Pyrolysis parameters
a: Optional\[List\[float\]\] = None # Pre-exponential factor
e: Optional\[List\[float\]\] = None # Activation energy
n_s: Optional\[List\[float\]\] = None # Reaction order
nu_spec: Optional\[List\[float\]\] = None # Stoichiometric coefficients
spec_id: Optional\[List\[str\]\] = None # Species produced
matl_id: Optional\[List\[str\]\] = None # Residue materials
nu_matl: Optional\[List\[float\]\] = None # Residue mass fractions
<br/>\# Validation ranges
\_valid_ranges = {
'density': (1.0, 10000.0),
'conductivity': (0.001, 1000.0),
'specific_heat': (0.1, 10.0),
'emissivity': (0.0, 1.0),
'absorption_coefficient': (0.0, 1e6),
'reference_temperature': (-273.0, 5000.0)
}
<br/>def \__post_init_\_(self):
"""Validate material properties after initialization"""
self.\_validate_properties()
self.\_setup_arrays()
<br/>def \_validate_properties(self):
"""Validate material property ranges"""
for prop, (min_val, max_val) in self.\_valid_ranges.items():
value = getattr(self, prop, None)
if value is not None:
if not min_val <= value <= max_val:
raise ValueError(
f"Material '{self.id}': {prop} = {value} "
f"is outside valid range \[{min_val}, {max_val}\]"
)
<br/>\# Check thermal properties
if self.conductivity is None and self.conductivity_ramp is None:
raise ValueError(f"Material '{self.id}': Must specify either "
"CONDUCTIVITY or CONDUCTIVITY_RAMP")
<br/>if self.specific_heat is None and self.specific_heat_ramp is None:
raise ValueError(f"Material '{self.id}': Must specify either "
"SPECIFIC_HEAT or SPECIFIC_HEAT_RAMP")
<br/>def \_setup_arrays(self):
"""Setup reaction parameter arrays"""
if self.n_reactions > 1:
\# Ensure all reaction arrays have correct length
reaction_params = \['a', 'e', 'n_s', 'heat_of_reaction'\]
for param in reaction_params:
value = getattr(self, param)
if value is not None:
if not isinstance(value, list):
setattr(self, param, \[value\])
if len(getattr(self, param)) != self.n_reactions:
raise ValueError(
f"Material '{self.id}': {param} must have "
f"{self.n_reactions} values for N_REACTIONS={self.n_reactions}"
)
<br/>def to_fds(self) -> str:
"""Generate FDS namelist string"""
lines = \[f"&MATL ID='{self.id}'"\]
<br/>\# Basic properties
if self.density:
lines.append(f" DENSITY={self.density:.1f}")
<br/>if self.conductivity is not None:
lines.append(f" CONDUCTIVITY={self.conductivity:.3f}")
elif self.conductivity_ramp:
lines.append(f" CONDUCTIVITY_RAMP='{self.conductivity_ramp}'")
<br/>if self.specific_heat is not None:
lines.append(f" SPECIFIC_HEAT={self.specific_heat:.3f}")
elif self.specific_heat_ramp:
lines.append(f" SPECIFIC_HEAT_RAMP='{self.specific_heat_ramp}'")
<br/>if self.emissivity != 0.9:
lines.append(f" EMISSIVITY={self.emissivity:.2f}")
<br/>\# Reaction parameters
if self.n_reactions > 1:
lines.append(f" N_REACTIONS={self.n_reactions}")
<br/>if self.a:
values = ','.join(f'{v:.2e}' for v in self.a)
lines.append(f" A={values}")
<br/>if self.e:
values = ','.join(f'{v:.0f}' for v in self.e)
lines.append(f" E={values}")
<br/>if self.heat_of_reaction:
values = ','.join(f'{v:.0f}' for v in self.heat_of_reaction)
lines.append(f" HEAT_OF_REACTION={values}")
<br/>if self.spec_id:
specs = ','.join(f"'{s}'" for s in self.spec_id)
lines.append(f" SPEC_ID={specs}")
<br/>lines\[-1\] += " /"
return '
'.join(lines)
<br/><br/>class MaterialBuilder:
"""Builder for complex material configurations"""
<br/>def \__init_\_(self, material_id: str):
self.material = Material(id=material_id, density=1000.0)
<br/>def set_thermal_properties(self, conductivity: float = None,
specific_heat: float = None,
conductivity_ramp: str = None,
specific_heat_ramp: str = None) -> 'MaterialBuilder':
"""Set thermal properties"""
if conductivity:
self.material.conductivity = conductivity
if specific_heat:
self.material.specific_heat = specific_heat
if conductivity_ramp:
self.material.conductivity_ramp = conductivity_ramp
if specific_heat_ramp:
self.material.specific_heat_ramp = specific_heat_ramp
return self
<br/>def add_pyrolysis_reaction(self, a: float, e: float,
heat_of_reaction: float,
spec_id: str = None) -> 'MaterialBuilder':
"""Add a pyrolysis reaction"""
if self.material.a is None:
self.material.a = \[\]
self.material.e = \[\]
self.material.heat_of_reaction = \[\]
<br/>self.material.a.append(a)
self.material.e.append(e)
self.material.heat_of_reaction.append(heat_of_reaction)
<br/>if spec_id:
if self.material.spec_id is None:
self.material.spec_id = \[\]
self.material.spec_id.append(spec_id)
<br/>self.material.n_reactions = len(self.material.a)
return self
<br/>def build(self) -> Material:
"""Build and validate the material"""
return self.material

## 2.2 Usage Examples

\# Example 1: Simple material
wood = Material(
id='PINE',
density=500.0,
conductivity=0.13,
specific_heat=2.5,
emissivity=0.9
)
<br/>\# Example 2: Material with temperature-dependent properties
gypsum = MaterialBuilder('GYPSUM') \\
.set_thermal_properties(
conductivity_ramp='GYPSUM_K',
specific_heat_ramp='GYPSUM_C'
) \\
.add_pyrolysis_reaction(
a=1.0e10,
e=80000,
heat_of_reaction=1000,
spec_id='WATER_VAPOR'
) \\
.build()
<br/>\# Example 3: Multi-reaction material
polymer = MaterialBuilder('PVC') \\
.set_thermal_properties(
conductivity=0.16,
specific_heat=1.0
) \\
.add_pyrolysis_reaction(
a=1.77e15, e=200000,
heat_of_reaction=800, spec_id='HCL'
) \\
.add_pyrolysis_reaction(
a=7.4e11, e=180000,
heat_of_reaction=400, spec_id='C3H5'
) \\
.build()
<br/>\# Add to simulation
sim.add_material(wood)
sim.add_material(gypsum)
sim.add_material(polymer)

# 3\. &REAC - Reaction Module Implementation

## 3.1 Class Design

\# pyfds/namelists/complex/reaction.py
from typing import Optional, List, Dict, Union
from dataclasses import dataclass
from enum import Enum
<br/>class ReactionModel(Enum):
"""Combustion model types"""
MIXING_CONTROLLED = 'MIXING_CONTROLLED'
FINITE_RATE = 'FINITE_RATE'
SIMPLE = 'SIMPLE_CHEMISTRY'
<br/>@dataclass
class Reaction:
"""
Represents FDS &REAC namelist for combustion reactions.
<br/>Supports multiple fuel types, complex chemistry, and various
combustion models.
"""
\# Fuel specification
fuel: Optional\[str\] = None
fuel_spec_id: Optional\[List\[str\]\] = None
<br/>\# Stoichiometry (for custom fuels)
c: Optional\[float\] = None # Carbon atoms
h: Optional\[float\] = None # Hydrogen atoms
o: Optional\[float\] = None # Oxygen atoms
n: Optional\[float\] = None # Nitrogen atoms
<br/>\# Combustion parameters
heat_of_combustion: Optional\[float\] = None # kJ/kg
soot_yield: float = 0.01 # kg soot/kg fuel
co_yield: float = 0.0 # kg CO/kg fuel
h2_yield: float = 0.0 # kg H2/kg fuel
<br/>\# Advanced parameters
critical_flame_temperature: Optional\[float\] = None # °C
auto_ignition_temperature: Optional\[float\] = None # °C
radiative_fraction: Optional\[float\] = None
<br/>\# Extinction parameters
extinction_model: Optional\[str\] = None
limiting_oxygen_index: float = 0.15
<br/>\# Complex chemistry
spec_id_nu: Optional\[List\[str\]\] = None # Species in reaction
nu: Optional\[List\[float\]\] = None # Stoichiometric coefficients
<br/>\# Model control
model: ReactionModel = ReactionModel.MIXING_CONTROLLED
suppression: bool = True
<br/>\# Pre-defined fuel database
\_fuel_database = {
'METHANE': {'c': 1, 'h': 4, 'hoc': 50000},
'PROPANE': {'c': 3, 'h': 8, 'hoc': 46000},
'N-HEPTANE': {'c': 7, 'h': 16, 'hoc': 44600},
'ETHANOL': {'c': 2, 'h': 6, 'o': 1, 'hoc': 26800},
'WOOD': {'c': 3.4, 'h': 6.2, 'o': 2.5, 'hoc': 17000},
'POLYURETHANE': {'c': 3.52, 'h': 5.48, 'o': 0.88, 'n': 0.32, 'hoc': 23200}
}
<br/>def \__post_init_\_(self):
"""Initialize and validate reaction parameters"""
self.\_setup_fuel()
self.\_validate_parameters()
<br/>def \_setup_fuel(self):
"""Setup fuel properties from database"""
if self.fuel and self.fuel.upper() in self.\_fuel_database:
fuel_data = self.\_fuel_database\[self.fuel.upper()\]
if self.c is None:
self.c = fuel_data.get('c')
if self.h is None:
self.h = fuel_data.get('h')
if self.o is None:
self.o = fuel_data.get('o', 0)
if self.n is None:
self.n = fuel_data.get('n', 0)
if self.heat_of_combustion is None:
self.heat_of_combustion = fuel_data.get('hoc')
<br/>def \_validate_parameters(self):
"""Validate reaction parameters"""
\# Check yields sum
total_yield = self.soot_yield + self.co_yield + self.h2_yield
if total_yield > 1.0:
raise ValueError(
f"Sum of product yields ({total_yield:.2f}) exceeds 1.0"
)
<br/>\# Check stoichiometry for custom fuel
if self.fuel is None and (self.c or self.h):
if self.c is None or self.h is None:
raise ValueError(
"Custom fuel requires both C and H atoms specified"
)
<br/>\# Validate extinction parameters
if self.limiting_oxygen_index &lt; 0 or self.limiting_oxygen_index &gt; 1:
raise ValueError(
f"LIMITING_OXYGEN_INDEX must be between 0 and 1"
)
<br/>\# Check complex chemistry
if self.spec_id_nu and self.nu:
if len(self.spec_id_nu) != len(self.nu):
raise ValueError(
"SPEC_ID_NU and NU must have same length"
)
<br/>def calculate_stoichiometry(self) -> Dict\[str, float\]:
"""Calculate stoichiometric coefficients"""
if not all(\[self.c, self.h\]):
raise ValueError("Cannot calculate stoichiometry without C and H")
<br/>\# Calculate oxygen required
o2_required = self.c + self.h/4.0 - (self.o or 0)/2.0
<br/>\# Calculate products
co2 = self.c \* (1 - self.co_yield - self.soot_yield)
h2o = self.h/2.0 \* (1 - self.h2_yield)
<br/>return {
'o2_required': o2_required,
'co2_produced': co2,
'h2o_produced': h2o,
'air_fuel_ratio': o2_required \* 137.0 # Assuming air is 23% O2 by mass
}
<br/>def to_fds(self) -> str:
"""Generate FDS namelist string"""
lines = \["&REAC"\]
<br/>if self.fuel:
lines.append(f" FUEL='{self.fuel}'")
elif self.fuel_spec_id:
specs = ','.join(f"'{s}'" for s in self.fuel_spec_id)
lines.append(f" FUEL_SPEC_ID={specs}")
<br/>\# Stoichiometry
if self.c is not None:
lines.append(f" C={self.c:.1f}")
if self.h is not None:
lines.append(f" H={self.h:.1f}")
if self.o and self.o > 0:
lines.append(f" O={self.o:.1f}")
if self.n and self.n > 0:
lines.append(f" N={self.n:.1f}")
<br/>\# Yields
if self.soot_yield != 0.01:
lines.append(f" SOOT_YIELD={self.soot_yield:.3f}")
if self.co_yield > 0:
lines.append(f" CO_YIELD={self.co_yield:.3f}")
<br/>\# Combustion parameters
if self.heat_of_combustion:
lines.append(f" HEAT_OF_COMBUSTION={self.heat_of_combustion:.0f}")
<br/>if self.auto_ignition_temperature:
lines.append(f" AUTO_IGNITION_TEMPERATURE={self.auto_ignition_temperature:.0f}")
<br/>\# Complex chemistry
if self.spec_id_nu and self.nu:
specs = ','.join(f"'{s}'" for s in self.spec_id_nu)
lines.append(f" SPEC_ID_NU={specs}")
nus = ','.join(f'{n:.1f}' for n in self.nu)
lines.append(f" NU={nus}")
<br/>lines\[-1\] += " /"
return '
'.join(lines)
<br/><br/>class ReactionBuilder:
"""Builder for complex reactions"""
<br/>def \__init_\_(self):
self.reaction = Reaction()
<br/>def use_fuel(self, fuel_name: str) -> 'ReactionBuilder':
"""Use a predefined fuel"""
self.reaction.fuel = fuel_name
return self
<br/>def custom_fuel(self, c: float, h: float,
o: float = 0, n: float = 0) -> 'ReactionBuilder':
"""Define custom fuel composition"""
self.reaction.c = c
self.reaction.h = h
self.reaction.o = o
self.reaction.n = n
return self
<br/>def set_yields(self, soot: float = 0.01,
co: float = 0.0) -> 'ReactionBuilder':
"""Set product yields"""
self.reaction.soot_yield = soot
self.reaction.co_yield = co
return self
<br/>def set_ignition(self, auto_ignition_temp: float = None,
critical_flame_temp: float = None) -> 'ReactionBuilder':
"""Set ignition parameters"""
if auto_ignition_temp:
self.reaction.auto_ignition_temperature = auto_ignition_temp
if critical_flame_temp:
self.reaction.critical_flame_temperature = critical_flame_temp
return self
<br/>def build(self) -> Reaction:
"""Build and validate reaction"""
return self.reaction

# 4\. &RAMP - Time Functions Implementation

## 4.1 Class Design

\# pyfds/namelists/complex/ramp.py
from typing import List, Tuple, Optional, Union
from dataclasses import dataclass, field
import numpy as np
from scipy import interpolate
<br/>@dataclass
class Ramp:
"""
Represents FDS &RAMP namelist for time-dependent functions.
<br/>Supports linear interpolation, custom functions, and
temperature-dependent properties.
"""
id: str
points: List\[Tuple\[float, float\]\] = field(default_factory=list)
<br/>\# Interpolation settings
interpolation: str = 'linear' # linear, cubic, step
extrapolation: str = 'hold' # hold, linear, cyclic
<br/>\# Temperature ramp flag
is_temperature_ramp: bool = False
<br/>def \__post_init_\_(self):
"""Sort points and validate"""
if self.points:
self.points.sort(key=lambda x: x\[0\])
self.\_validate_points()
<br/>def \_validate_points(self):
"""Validate ramp points"""
if len(self.points) < 2:
raise ValueError(f"Ramp '{self.id}' requires at least 2 points")
<br/>\# Check for duplicate T values
t_values = \[p\[0\] for p in self.points\]
if len(t_values) != len(set(t_values)):
raise ValueError(f"Ramp '{self.id}' has duplicate T values")
<br/>\# For non-temperature ramps, check time is positive
if not self.is_temperature_ramp:
if any(t < 0 for t in t_values):
raise ValueError(f"Ramp '{self.id}': Time values must be >= 0")
<br/>def add_point(self, t: float, f: float):
"""Add a point to the ramp"""
self.points.append((t, f))
self.points.sort(key=lambda x: x\[0\])
self.\_validate_points()
<br/>def evaluate(self, t: Union\[float, np.ndarray\]) -> Union\[float, np.ndarray\]:
"""Evaluate ramp at given time/temperature"""
if not self.points:
raise ValueError(f"Ramp '{self.id}' has no points")
<br/>t_values = \[p\[0\] for p in self.points\]
f_values = \[p\[1\] for p in self.points\]
<br/>if self.interpolation == 'linear':
return np.interp(t, t_values, f_values)
elif self.interpolation == 'cubic':
interp_func = interpolate.interp1d(
t_values, f_values, kind='cubic',
fill_value='extrapolate' if self.extrapolation == 'linear' else (f_values\[-1\] if self.extrapolation == 'hold' else np.nan)
)
return interp_func(t)
elif self.interpolation == 'step':
idx = np.searchsorted(t_values, t) - 1
idx = np.clip(idx, 0, len(f_values) - 1)
return f_values\[idx\]
<br/>def to_fds(self) -> str:
"""Generate FDS namelist strings"""
lines = \[\]
for t, f in self.points:
line = f"&RAMP ID='{self.id}', T={t:.2f}, F={f:.4f} /"
lines.append(line)
return '
'.join(lines)
<br/>def plot(self, t_range: Optional\[Tuple\[float, float\]\] = None):
"""Plot the ramp function"""
import matplotlib.pyplot as plt
<br/>if t_range is None:
t_min = self.points\[0\]\[0\]
t_max = self.points\[-1\]\[0\]
t_range = (t_min - 0.1\*(t_max-t_min), t_max + 0.1\*(t_max-t_min))
<br/>t = np.linspace(t_range\[0\], t_range\[1\], 200)
f = self.evaluate(t)
<br/>plt.figure(figsize=(8, 5))
plt.plot(t, f, 'b-', label=f'Ramp: {self.id}')
<br/>\# Plot original points
t_points = \[p\[0\] for p in self.points\]
f_points = \[p\[1\] for p in self.points\]
plt.plot(t_points, f_points, 'ro', markersize=8, label='Control points')
<br/>plt.xlabel('Temperature \[°C\]' if self.is_temperature_ramp else 'Time \[s\]')
plt.ylabel('Function Value')
plt.title(f'Ramp Function: {self.id}')
plt.grid(True, alpha=0.3)
plt.legend()
<br/>return plt.gcf()
<br/><br/>class RampBuilder:
"""Builder for creating ramp functions"""
<br/>def \__init_\_(self, ramp_id: str):
self.ramp = Ramp(id=ramp_id)
<br/>def add_points(self, points: List\[Tuple\[float, float\]\]) -> 'RampBuilder':
"""Add multiple points"""
for t, f in points:
self.ramp.add_point(t, f)
return self
<br/>def linear_ramp(self, t_start: float, t_end: float,
f_start: float, f_end: float,
n_points: int = 2) -> 'RampBuilder':
"""Create linear ramp"""
t = np.linspace(t_start, t_end, n_points)
f = np.linspace(f_start, f_end, n_points)
for ti, fi in zip(t, f):
self.ramp.add_point(ti, fi)
return self
<br/>def sine_wave(self, period: float, amplitude: float,
phase: float = 0, offset: float = 0,
duration: float = None, n_points: int = 20) -> 'RampBuilder':
"""Create sine wave ramp"""
if duration is None:
duration = period
t = np.linspace(0, duration, n_points)
f = offset + amplitude \* np.sin(2\*np.pi\*t/period + phase)
for ti, fi in zip(t, f):
self.ramp.add_point(ti, fi)
return self
<br/>def step_function(self, steps: List\[Tuple\[float, float, float\]\]) -> 'RampBuilder':
"""Create step function (t_start, t_end, value)"""
for t_start, t_end, value in steps:
self.ramp.add_point(t_start, value)
self.ramp.add_point(t_end - 0.001, value)
self.ramp.interpolation = 'step'
return self
<br/>def temperature_dependent(self, temp_values: Dict\[float, float\]) -> 'RampBuilder':
"""Create temperature-dependent property ramp"""
self.ramp.is_temperature_ramp = True
for temp, value in sorted(temp_values.items()):
self.ramp.add_point(temp, value)
return self
<br/>def build(self) -> Ramp:
"""Build and validate ramp"""
return self.ramp
<br/><br/>\# Predefined ramp functions
class PredefinedRamps:
"""Collection of commonly used ramp functions"""
<br/>@staticmethod
def t_squared(ramp_id: str, alpha: float = 0.0117,
t_max: float = 600) -> Ramp:
"""Create t-squared fire growth ramp"""
t = np.linspace(0, t_max, 20)
q = alpha \* t\*\*2 # Heat release rate
ramp = Ramp(id=ramp_id)
for ti, qi in zip(t, q):
ramp.add_point(ti, qi/1000.0) # Convert to MW
return ramp
<br/>@staticmethod
def iso_9705(ramp_id: str) -> Ramp:
"""ISO 9705 room corner test HRR curve"""
return RampBuilder(ramp_id) \\
.add_points(\[
(0, 0),
(300, 100),
(600, 100),
(900, 300),
(1200, 300)
\]) \\
.build()
<br/>@staticmethod
def sprinkler_activation(ramp_id: str,
activation_time: float,
ramp_time: float = 30) -> Ramp:
"""Sprinkler activation with ramp-up"""
return RampBuilder(ramp_id) \\
.add_points(\[
(0, 0),
(activation_time, 0),
(activation_time + ramp_time, 1),
(activation_time + 1000, 1)
\]) \\
.build()

# 5\. &CTRL - Control Logic Implementation

## 5.1 Class Design

\# pyfds/namelists/complex/control.py
from typing import List, Optional, Union, Callable
from dataclasses import dataclass, field
from enum import Enum
<br/>class ControlFunction(Enum):
"""Control function types"""
CUSTOM = 'CUSTOM'
TIME_DELAY = 'TIME_DELAY'
ANY = 'ANY'
ALL = 'ALL'
ONLY = 'ONLY'
LINEAR = 'LINEAR'
SUM = 'SUM'
SUBTRACT = 'SUBTRACT'
MULTIPLY = 'MULTIPLY'
DIVIDE = 'DIVIDE'
POWER = 'POWER'
EXP = 'EXP'
LOG = 'LOG'
COS = 'COS'
SIN = 'SIN'
ACOS = 'ACOS'
ASIN = 'ASIN'
PID = 'PID'
DEADBAND = 'DEADBAND'
KILL = 'KILL'
RE_START = 'RE_START'
<br/>@dataclass
class Control:
"""
Represents FDS &CTRL namelist for control logic.
<br/>Implements device interactions, logical operations, and
control algorithms.
"""
id: str
function_type: ControlFunction
input_id: Union\[str, List\[str\]\] = None
<br/>\# Control parameters
setpoint: Optional\[Tuple\[float, float\]\] = None
initial_state: bool = False
latch: bool = True
delay: float = 0.0
<br/>\# PID parameters
proportional_gain: float = 0.0
integral_gain: float = 0.0
differential_gain: float = 0.0
<br/>\# Logic parameters
on_bound: str = 'LOWER' # LOWER, UPPER, BOTH
ramp_id: Optional\[str\] = None
constant: Optional\[float\] = None
<br/>\# Custom function
custom_function: Optional\[str\] = None
<br/>def \__post_init_\_(self):
"""Validate control parameters"""
self.\_validate_function()
self.\_validate_inputs()
<br/>def \_validate_function(self):
"""Validate function-specific parameters"""
if self.function_type == ControlFunction.PID:
if not any(\[self.proportional_gain,
self.integral_gain,
self.differential_gain\]):
raise ValueError(
f"Control '{self.id}': PID requires at least one gain"
)
<br/>if self.function_type in \[ControlFunction.ANY, ControlFunction.ALL\]:
if not isinstance(self.input_id, list):
raise ValueError(
f"Control '{self.id}': {self.function_type.value} "
f"requires multiple INPUT_ID"
)
<br/>def \_validate_inputs(self):
"""Validate input connections"""
if self.function_type != ControlFunction.CUSTOM and not self.input_id:
raise ValueError(
f"Control '{self.id}': Requires INPUT_ID for "
f"{self.function_type.value}"
)
<br/>if self.on_bound not in \['LOWER', 'UPPER', 'BOTH'\]:
raise ValueError(
f"Control '{self.id}': ON_BOUND must be "
f"LOWER, UPPER, or BOTH"
)
<br/>def to_fds(self) -> str:
"""Generate FDS namelist string"""
lines = \[f"&CTRL ID='{self.id}'"\]
lines.append(f" FUNCTION_TYPE='{self.function_type.value}'")
<br/>if self.input_id:
if isinstance(self.input_id, list):
inputs = ','.join(f"'{i}'" for i in self.input_id)
lines.append(f" INPUT_ID={inputs}")
else:
lines.append(f" INPUT_ID='{self.input_id}'")
<br/>if self.setpoint:
lines.append(f" SETPOINT={self.setpoint\[0\]:.2f},{self.setpoint\[1\]:.2f}")
<br/>if self.initial_state:
lines.append(f" INITIAL_STATE=T")
<br/>if not self.latch:
lines.append(f" LATCH=F")
<br/>if self.delay > 0:
lines.append(f" DELAY={self.delay:.1f}")
<br/>\# PID parameters
if self.proportional_gain != 0:
lines.append(f" PROPORTIONAL_GAIN={self.proportional_gain:.2f}")
if self.integral_gain != 0:
lines.append(f" INTEGRAL_GAIN={self.integral_gain:.2f}")
if self.differential_gain != 0:
lines.append(f" DIFFERENTIAL_GAIN={self.differential_gain:.2f}")
<br/>if self.ramp_id:
lines.append(f" RAMP_ID='{self.ramp_id}'")
<br/>if self.constant is not None:
lines.append(f" CONSTANT={self.constant:.2f}")
<br/>lines\[-1\] += " /"
return '
'.join(lines)
<br/><br/>class ControlBuilder:
"""Builder for control logic"""
<br/>def \__init_\_(self, control_id: str):
self.control = Control(
id=control_id,
function_type=ControlFunction.CUSTOM
)
<br/>def any_input(self, device_ids: List\[str\]) -> 'ControlBuilder':
"""Activate when ANY input is true"""
self.control.function_type = ControlFunction.ANY
self.control.input_id = device_ids
return self
<br/>def all_inputs(self, device_ids: List\[str\]) -> 'ControlBuilder':
"""Activate when ALL inputs are true"""
self.control.function_type = ControlFunction.ALL
self.control.input_id = device_ids
return self
<br/>def time_delay(self, input_id: str, delay: float) -> 'ControlBuilder':
"""Add time delay to input"""
self.control.function_type = ControlFunction.TIME_DELAY
self.control.input_id = input_id
self.control.delay = delay
return self
<br/>def pid_controller(self, input_id: str,
kp: float = 1.0, ki: float = 0.0,
kd: float = 0.0) -> 'ControlBuilder':
"""Create PID controller"""
self.control.function_type = ControlFunction.PID
self.control.input_id = input_id
self.control.proportional_gain = kp
self.control.integral_gain = ki
self.control.differential_gain = kd
return self
<br/>def deadband(self, input_id: str,
low: float, high: float) -> 'ControlBuilder':
"""Create deadband controller"""
self.control.function_type = ControlFunction.DEADBAND
self.control.input_id = input_id
self.control.setpoint = (low, high)
return self
<br/>def build(self) -> Control:
"""Build and validate control"""
return self.control

# 6\. &PROP - Device Properties Implementation

\# pyfds/namelists/complex/property.py
from typing import Optional, Dict, List
from dataclasses import dataclass
<br/>@dataclass
class DeviceProperty:
"""
Represents FDS &PROP namelist for device properties.
"""
id: str
<br/>\# Sprinkler properties
quantity: Optional\[str\] = None
flow_rate: Optional\[float\] = None
k_factor: Optional\[float\] = None
operating_pressure: Optional\[float\] = None
spray_angle: Optional\[Tuple\[float, float\]\] = None
<br/>\# Detector properties
activation_temperature: Optional\[float\] = None
activation_obscuration: Optional\[float\] = None
rti: Optional\[float\] = None # Response Time Index
<br/>\# Particle properties
part_id: Optional\[str\] = None
flow_tau: Optional\[float\] = None
particle_velocity: Optional\[float\] = None
<br/>\# Special properties
spark: bool = False
cable: bool = False
<br/>def to_fds(self) -> str:
"""Generate FDS namelist string"""
lines = \[f"&PROP ID='{self.id}'"\]
<br/>if self.quantity:
lines.append(f" QUANTITY='{self.quantity}'")
<br/>\# Sprinkler properties
if self.flow_rate is not None:
lines.append(f" FLOW_RATE={self.flow_rate:.1f}")
if self.k_factor is not None:
lines.append(f" K_FACTOR={self.k_factor:.1f}")
<br/>\# Detector properties
if self.activation_temperature is not None:
lines.append(f" ACTIVATION_TEMPERATURE={self.activation_temperature:.1f}")
if self.rti is not None:
lines.append(f" RTI={self.rti:.1f}")
<br/>\# Special
if self.spark:
lines.append(f" SPARK=T")
<br/>lines\[-1\] += " /"
return '
'.join(lines)

# 7\. &INIT - Initial Conditions Implementation

\# pyfds/namelists/complex/initial.py
from typing import Optional, List, Tuple
from dataclasses import dataclass
<br/>@dataclass
class InitialCondition:
"""
Represents FDS &INIT namelist for initial conditions.
"""
\# Region specification
xb: Optional\[Tuple\[float, float, float, float, float, float\]\] = None
xyz: Optional\[Tuple\[float, float, float\]\] = None
<br/>\# Initial values
temperature: Optional\[float\] = None
density: Optional\[float\] = None
<br/>\# Species concentrations
spec_id: Optional\[List\[str\]\] = None
mass_fraction: Optional\[List\[float\]\] = None
volume_fraction: Optional\[List\[float\]\] = None
<br/>\# Particle initialization
part_id: Optional\[str\] = None
n_particles: Optional\[int\] = None
<br/>def to_fds(self) -> str:
"""Generate FDS namelist string"""
lines = \["&INIT"\]
<br/>if self.xb:
xb_str = ','.join(f'{x:.2f}' for x in self.xb)
lines.append(f" XB={xb_str}")
<br/>if self.temperature is not None:
lines.append(f" TEMPERATURE={self.temperature:.1f}")
<br/>if self.spec_id and self.mass_fraction:
specs = ','.join(f"'{s}'" for s in self.spec_id)
fracs = ','.join(f'{f:.4f}' for f in self.mass_fraction)
lines.append(f" SPEC_ID={specs}")
lines.append(f" MASS_FRACTION={fracs}")
<br/>lines\[-1\] += " /"
return '
'.join(lines)

# 8\. Integration Architecture

## 8.1 Cross-Reference Validation

\# pyfds/namelists/validators/cross_reference.py
from typing import Dict, List, Set
from dataclasses import dataclass
<br/>@dataclass
class CrossReferenceValidator:
"""Validates references between namelist groups"""
<br/>materials: Dict\[str, Material\] = field(default_factory=dict)
ramps: Dict\[str, Ramp\] = field(default_factory=dict)
controls: Dict\[str, Control\] = field(default_factory=dict)
properties: Dict\[str, DeviceProperty\] = field(default_factory=dict)
species: Set\[str\] = field(default_factory=set)
devices: Set\[str\] = field(default_factory=set)
<br/>def validate_material(self, material: Material) -> List\[str\]:
"""Validate material references"""
errors = \[\]
<br/>\# Check ramp references
if material.conductivity_ramp:
if material.conductivity_ramp not in self.ramps:
errors.append(
f"Material '{material.id}': CONDUCTIVITY_RAMP "
f"'{material.conductivity_ramp}' not found"
)
<br/>if material.specific_heat_ramp:
if material.specific_heat_ramp not in self.ramps:
errors.append(
f"Material '{material.id}': SPECIFIC_HEAT_RAMP "
f"'{material.specific_heat_ramp}' not found"
)
<br/>\# Check species references
if material.spec_id:
for spec in material.spec_id:
if spec not in self.species:
errors.append(
f"Material '{material.id}': SPEC_ID "
f"'{spec}' not defined"
)
<br/>return errors
<br/>def validate_control(self, control: Control) -> List\[str\]:
"""Validate control references"""
errors = \[\]
<br/>\# Check device references
if control.input_id:
input_ids = control.input_id if isinstance(control.input_id, list) else \[control.input_id\]
for device_id in input_ids:
if device_id not in self.devices and device_id not in self.controls:
errors.append(
f"Control '{control.id}': INPUT_ID "
f"'{device_id}' not found"
)
<br/>\# Check ramp reference
if control.ramp_id:
if control.ramp_id not in self.ramps:
errors.append(
f"Control '{control.id}': RAMP_ID "
f"'{control.ramp_id}' not found"
)
<br/>return errors
<br/>def validate_all(self) -> List\[str\]:
"""Validate all cross-references"""
all_errors = \[\]
<br/>for material in self.materials.values():
all_errors.extend(self.validate_material(material))
<br/>for control in self.controls.values():
all_errors.extend(self.validate_control(control))
<br/>return all_errors

## 8.2 Simulation Integration

\# pyfds/simulation.py - Integration with Simulation class
<br/>class Simulation:
"""Main simulation class with complex namelist support"""
<br/>def \__init_\_(self, chid: str, title: str = None):
self.chid = chid
self.title = title
<br/>\# Complex namelist storage
self.materials: Dict\[str, Material\] = {}
self.reactions: List\[Reaction\] = \[\]
self.ramps: Dict\[str, Ramp\] = {}
self.controls: Dict\[str, Control\] = {}
self.properties: Dict\[str, DeviceProperty\] = {}
self.initial_conditions: List\[InitialCondition\] = \[\]
<br/>\# Cross-reference validator
self.\_validator = CrossReferenceValidator()
<br/>def add_material(self, material: Material):
"""Add material with validation"""
if material.id in self.materials:
raise ValueError(f"Material '{material.id}' already exists")
<br/>\# Register with validator
self.\_validator.materials\[material.id\] = material
<br/>\# Validate references
errors = self.\_validator.validate_material(material)
if errors:
raise ValueError("\\n".join(errors))
<br/>self.materials\[material.id\] = material
return material
<br/>def add_reaction(self, reaction: Reaction):
"""Add reaction"""
self.reactions.append(reaction)
return reaction
<br/>def add_ramp(self, ramp: Ramp):
"""Add ramp function"""
if ramp.id in self.ramps:
raise ValueError(f"Ramp '{ramp.id}' already exists")
<br/>self.\_validator.ramps\[ramp.id\] = ramp
self.ramps\[ramp.id\] = ramp
return ramp
<br/>def add_control(self, control: Control):
"""Add control logic"""
if control.id in self.controls:
raise ValueError(f"Control '{control.id}' already exists")
<br/>self.\_validator.controls\[control.id\] = control
<br/>\# Validate references
errors = self.\_validator.validate_control(control)
if errors:
raise ValueError("\\n".join(errors))
<br/>self.controls\[control.id\] = control
return control
<br/>def write(self, filename: str):
"""Write complete FDS input file"""
with open(filename, 'w') as f:
\# Write header
f.write(f"&HEAD CHID='{self.chid}'")
if self.title:
f.write(f", TITLE='{self.title}'")
f.write(" /\\n\\n")
<br/>\# Write ramps first (referenced by materials)
if self.ramps:
f.write("! === RAMP Functions ===\\n")
for ramp in self.ramps.values():
f.write(ramp.to_fds())
f.write("\\n")
f.write("\\n")
<br/>\# Write materials
if self.materials:
f.write("! === Materials ===\\n")
for material in self.materials.values():
f.write(material.to_fds())
f.write("\\n")
f.write("\\n")
<br/>\# Write reactions
if self.reactions:
f.write("! === Reactions ===\\n")
for reaction in self.reactions:
f.write(reaction.to_fds())
f.write("\\n")
f.write("\\n")
<br/>\# Write properties
if self.properties:
f.write("! === Device Properties ===\\n")
for prop in self.properties.values():
f.write(prop.to_fds())
f.write("\\n")
f.write("\\n")
<br/>\# Write controls
if self.controls:
f.write("! === Control Logic ===\\n")
for control in self.controls.values():
f.write(control.to_fds())
f.write("\\n")
f.write("\\n")
<br/>\# Write initial conditions
if self.initial_conditions:
f.write("! === Initial Conditions ===\\n")
for init in self.initial_conditions:
f.write(init.to_fds())
f.write("\\n")
f.write("\\n")
<br/>\# Write TAIL
f.write("&TAIL /\\n")

# 9\. Testing Requirements

## 9.1 Unit Test Examples

\# tests/test_complex_namelists/test_material.py
import pytest
from pyfds.namelists.complex import Material, MaterialBuilder
<br/>class TestMaterial:
def test_simple_material(self):
"""Test basic material creation"""
wood = Material(
id='WOOD',
density=500.0,
conductivity=0.13,
specific_heat=2.5
)
<br/>assert wood.id == 'WOOD'
assert wood.density == 500.0
<br/>fds_str = wood.to_fds()
assert "&MATL ID='WOOD'" in fds_str
assert "DENSITY=500.0" in fds_str
<br/>def test_temperature_dependent(self):
"""Test temperature-dependent properties"""
gypsum = MaterialBuilder('GYPSUM') \\
.set_thermal_properties(
conductivity_ramp='GYPSUM_K'
) \\
.build()
<br/>fds_str = gypsum.to_fds()
assert "CONDUCTIVITY_RAMP='GYPSUM_K'" in fds_str
<br/>def test_validation_errors(self):
"""Test material validation"""
with pytest.raises(ValueError, match="outside valid range"):
Material(
id='BAD',
density=-100, # Invalid
conductivity=0.1,
specific_heat=1.0
)
<br/>def test_pyrolysis_reactions(self):
"""Test multi-reaction material"""
polymer = MaterialBuilder('POLYMER') \\
.set_thermal_properties(conductivity=0.2, specific_heat=1.5) \\
.add_pyrolysis_reaction(
a=1e10, e=180000,
heat_of_reaction=500
) \\
.add_pyrolysis_reaction(
a=5e12, e=200000,
heat_of_reaction=800
) \\
.build()
<br/>assert polymer.n_reactions == 2
assert len(polymer.a) == 2
<br/>fds_str = polymer.to_fds()
assert "N_REACTIONS=2" in fds_str
assert "A=1.00e+10,5.00e+12" in fds_str
<br/><br/>class TestReaction:
def test_predefined_fuel(self):
"""Test using predefined fuel"""
reaction = Reaction(fuel='PROPANE')
<br/>assert reaction.c == 3
assert reaction.h == 8
assert reaction.heat_of_combustion == 46000
<br/>def test_custom_fuel(self):
"""Test custom fuel definition"""
reaction = ReactionBuilder() \\
.custom_fuel(c=7, h=16) \\
.set_yields(soot=0.015, co=0.01) \\
.build()
<br/>stoich = reaction.calculate_stoichiometry()
assert stoich\['o2_required'\] == pytest.approx(11.0, rel=0.01)
<br/>def test_complex_chemistry(self):
"""Test complex chemistry specification"""
reaction = Reaction(
spec_id_nu=\['FUEL', 'O2', 'CO2', 'H2O'\],
nu=\[-1, -11, 7, 8\]
)
<br/>fds_str = reaction.to_fds()
assert "SPEC_ID_NU='FUEL','O2','CO2','H2O'" in fds_str
assert "NU=-1.0,-11.0,7.0,8.0" in fds_str
<br/><br/>class TestRamp:
def test_linear_ramp(self):
"""Test linear ramp creation"""
ramp = RampBuilder('HRR_RAMP') \\
.linear_ramp(0, 300, 0, 1000) \\
.build()
<br/>assert ramp.evaluate(0) == 0
assert ramp.evaluate(150) == pytest.approx(500, rel=0.01)
assert ramp.evaluate(300) == 1000
<br/>def test_sine_wave(self):
"""Test sine wave ramp"""
ramp = RampBuilder('OSCILLATION') \\
.sine_wave(period=10, amplitude=5, offset=20) \\
.build()
<br/>\# Check peak values
values = \[ramp.evaluate(t) for t in \[0, 2.5, 5, 7.5, 10\]\]
assert max(values) == pytest.approx(25, rel=0.1)
assert min(values) == pytest.approx(15, rel=0.1)
<br/>def test_temperature_ramp(self):
"""Test temperature-dependent property ramp"""
ramp = RampBuilder('STEEL_K') \\
.temperature_dependent({
20: 54,
100: 51,
500: 40,
800: 27
}) \\
.build()
<br/>assert ramp.is_temperature_ramp
assert ramp.evaluate(20) == 54
assert ramp.evaluate(800) == 27
<br/><br/>class TestControl:
def test_any_logic(self):
"""Test ANY control logic"""
control = ControlBuilder('SMOKE_ALARM') \\
.any_input(\['SD_1', 'SD_2', 'SD_3'\]) \\
.build()
<br/>assert control.function_type == ControlFunction.ANY
assert len(control.input_id) == 3
<br/>def test_pid_controller(self):
"""Test PID controller"""
control = ControlBuilder('TEMP_CONTROL') \\
.pid_controller('TEMP_SENSOR', kp=2.0, ki=0.5, kd=0.1) \\
.build()
<br/>assert control.proportional_gain == 2.0
assert control.integral_gain == 0.5
assert control.differential_gain == 0.1
<br/>def test_deadband(self):
"""Test deadband controller"""
control = ControlBuilder('HVAC_CONTROL') \\
.deadband('ROOM_TEMP', low=20, high=25) \\
.build()
<br/>assert control.setpoint == (20, 25)

## 9.2 Integration Test Examples

\# tests/test_complex_namelists/test_integration.py
import pytest
from pyfds import Simulation
from pyfds.namelists.complex import \*
<br/>class TestComplexIntegration:
def test_material_with_ramps(self):
"""Test material with temperature-dependent properties"""
sim = Simulation('test_matl')
<br/>\# Add temperature ramps
k_ramp = RampBuilder('GYPSUM_K') \\
.temperature_dependent({
20: 0.48,
100: 0.45,
200: 0.40,
500: 0.35
}) \\
.build()
<br/>c_ramp = RampBuilder('GYPSUM_C') \\
.temperature_dependent({
20: 0.84,
100: 1.0,
200: 1.1
}) \\
.build()
<br/>sim.add_ramp(k_ramp)
sim.add_ramp(c_ramp)
<br/>\# Add material referencing ramps
gypsum = MaterialBuilder('GYPSUM') \\
.set_thermal_properties(
conductivity_ramp='GYPSUM_K',
specific_heat_ramp='GYPSUM_C'
) \\
.build()
<br/>sim.add_material(gypsum)
<br/>\# Write and verify
sim.write('test_matl.fds')
<br/>with open('test_matl.fds') as f:
content = f.read()
assert "&RAMP ID='GYPSUM_K'" in content
assert "CONDUCTIVITY_RAMP='GYPSUM_K'" in content
<br/>def test_control_with_devices(self):
"""Test control logic with device references"""
sim = Simulation('test_ctrl')
<br/>\# Add device property
smoke_prop = DeviceProperty(
id='SMOKE_DETECTOR',
activation_obscuration=3.28
)
sim.add_property(smoke_prop)
<br/>\# Add devices (mock)
sim.\_validator.devices.add('SD_1')
sim.\_validator.devices.add('SD_2')
<br/>\# Add control
control = ControlBuilder('SMOKE_ALARM') \\
.any_input(\['SD_1', 'SD_2'\]) \\
.build()
<br/>sim.add_control(control)
<br/>\# Verify cross-references
assert 'SD_1' in sim.\_validator.devices
<br/>def test_complete_fire_scenario(self):
"""Test complete fire scenario with all complex namelists"""
sim = Simulation('complex_fire', 'Multi-layer wall fire test')
<br/>\# Define reaction
reaction = ReactionBuilder() \\
.use_fuel('PROPANE') \\
.set_yields(soot=0.01, co=0.005) \\
.set_ignition(auto_ignition_temp=450) \\
.build()
sim.add_reaction(reaction)
<br/>\# Define HRR ramp
hrr_ramp = PredefinedRamps.t_squared('FIRE_RAMP', alpha=0.047)
sim.add_ramp(hrr_ramp)
<br/>\# Define materials
concrete = Material(
id='CONCRETE',
density=2300,
conductivity=1.4,
specific_heat=0.88
)
<br/>insulation = Material(
id='INSULATION',
density=200,
conductivity=0.04,
specific_heat=0.84
)
<br/>sim.add_material(concrete)
sim.add_material(insulation)
<br/>\# Initial conditions
init = InitialCondition(
xb=(0, 10, 0, 10, 0, 0.1),
temperature=500,
spec_id=\['PROPANE'\],
mass_fraction=\[0.05\]
)
sim.add_initial_condition(init)
<br/>\# Write complete file
sim.write('complex_fire.fds')
<br/>\# Verify file structure
with open('complex_fire.fds') as f:
content = f.read()
sections = \['Materials', 'Reactions', 'RAMP Functions', 'Initial Conditions'\]
for section in sections:
assert section in content
<br/><br/>class TestValidation:
def test_cross_reference_validation(self):
"""Test cross-reference validation catches errors"""
sim = Simulation('test_validation')
<br/>\# Try to add material with non-existent ramp
material = MaterialBuilder('BAD_MATL') \\
.set_thermal_properties(
conductivity_ramp='NON_EXISTENT'
) \\
.build()
<br/>with pytest.raises(ValueError, match='NON_EXISTENT'):
sim.add_material(material)
<br/>def test_duplicate_ids(self):
"""Test duplicate ID detection"""
sim = Simulation('test_duplicates')
<br/>ramp1 = Ramp(id='RAMP_1')
ramp1.add_point(0, 0)
ramp1.add_point(10, 100)
<br/>ramp2 = Ramp(id='RAMP_1') # Duplicate ID
ramp2.add_point(0, 0)
ramp2.add_point(5, 50)
<br/>sim.add_ramp(ramp1)
<br/>with pytest.raises(ValueError, match='already exists'):
sim.add_ramp(ramp2)

# 10\. Implementation Checklist

Use this checklist to track implementation progress. Each item should be completed in order, with tests passing before moving to the next item.

## 10.1 Foundation Tasks

- \[ \] Create module structure under pyfds/namelists/complex/
- \[ \] Set up base imports in \__init_\_.py files
- \[ \] Create base validation framework
- \[ \] Set up pytest fixtures for complex namelists
- \[ \] Create sample FDS files for testing

## 10.2 MATL Implementation

- \[ \] Implement Material dataclass with validation
- \[ \] Add temperature-dependent property support
- \[ \] Implement MaterialBuilder class
- \[ \] Create pyrolysis reaction support
- \[ \] Write unit tests (target: 15 tests)
- \[ \] Integrate with Simulation class
- \[ \] Add cross-reference validation
- \[ \] Document with examples

## 10.3 REAC Implementation

- \[ \] Implement Reaction dataclass
- \[ \] Add fuel database
- \[ \] Implement stoichiometry calculations
- \[ \] Create ReactionBuilder
- \[ \] Add complex chemistry support
- \[ \] Write unit tests (target: 12 tests)
- \[ \] Test with multiple fuel types
- \[ \] Document combustion models

## 10.4 RAMP Implementation

- \[ \] Implement Ramp dataclass
- \[ \] Add interpolation methods (linear, cubic, step)
- \[ \] Create RampBuilder with helper methods
- \[ \] Implement PredefinedRamps collection
- \[ \] Add plotting capability
- \[ \] Write unit tests (target: 10 tests)
- \[ \] Test temperature vs time ramps
- \[ \] Create ramp library

## 10.5 CTRL Implementation

- \[ \] Implement Control dataclass
- \[ \] Add all function types from ControlFunction enum
- \[ \] Create ControlBuilder
- \[ \] Implement PID controller logic
- \[ \] Add device connection validation
- \[ \] Write unit tests (target: 10 tests)
- \[ \] Test control chains
- \[ \] Document control patterns

## 10.6 PROP and INIT Implementation

- \[ \] Implement DeviceProperty dataclass
- \[ \] Add sprinkler/detector properties
- \[ \] Implement InitialCondition dataclass
- \[ \] Add species concentration support
- \[ \] Write unit tests (target: 8 tests)
- \[ \] Test with real scenarios

## 10.7 Integration and Testing

- \[ \] Implement CrossReferenceValidator
- \[ \] Update Simulation class with all complex namelists
- \[ \] Create comprehensive integration tests
- \[ \] Test complete fire scenarios
- \[ \] Validate generated FDS files with actual FDS
- \[ \] Performance benchmarking
- \[ \] Memory usage optimization
- \[ \] Error message improvement

## 10.8 Documentation and Examples

- \[ \] Write API documentation for all classes
- \[ \] Create Jupyter notebook examples
- \[ \] Build example library (10+ scenarios)
- \[ \] Write migration guide from manual FDS
- \[ \] Create troubleshooting guide
- \[ \] Record video tutorials

# Appendix: Complete Usage Example

"""
Complete example demonstrating all Phase 3 complex namelists
in a realistic fire scenario: Multi-room apartment fire with
sprinkler activation and smoke detection.
"""
<br/>from pyfds import Simulation
from pyfds.namelists.complex import \*
<br/>\# Create simulation
sim = Simulation(
chid='apartment_fire',
title='Multi-room Apartment Fire with Suppression'
)
<br/>\# === Time and Mesh ===
sim.time(t_end=600.0, dt=0.1)
sim.mesh(ijk=(100, 100, 30), xb=(0, 10, 0, 10, 0, 3))
<br/>\# === Define Reaction ===
reaction = ReactionBuilder() \\
.use_fuel('POLYURETHANE') \\
.set_yields(soot=0.10, co=0.02) \\
.set_ignition(
auto_ignition_temp=350,
critical_flame_temp=1427
) \\
.build()
sim.add_reaction(reaction)
<br/>\# === Define Materials ===
\# Gypsum board with dehydration
k_ramp = RampBuilder('GYPSUM_K') \\
.temperature_dependent({
20: 0.48, 100: 0.45, 200: 0.35, 500: 0.30
}).build()
<br/>c_ramp = RampBuilder('GYPSUM_C') \\
.temperature_dependent({
20: 0.84, 100: 1.0, 150: 1.5, 200: 1.0
}).build()
<br/>sim.add_ramp(k_ramp)
sim.add_ramp(c_ramp)
<br/>gypsum = MaterialBuilder('GYPSUM_BOARD') \\
.set_thermal_properties(
conductivity_ramp='GYPSUM_K',
specific_heat_ramp='GYPSUM_C'
) \\
.add_pyrolysis_reaction(
a=1.0e10, e=80000,
heat_of_reaction=1000,
spec_id='WATER_VAPOR'
) \\
.build()
<br/>\# Foam material for furniture
foam = MaterialBuilder('FOAM') \\
.set_thermal_properties(
conductivity=0.05,
specific_heat=1.3
) \\
.add_pyrolysis_reaction(
a=1.3e8, e=140000,
heat_of_reaction=1500,
spec_id='POLYURETHANE'
) \\
.build()
<br/>sim.add_material(gypsum)
sim.add_material(foam)
<br/>\# === Define HRR Ramp for Fire Growth ===
fire_ramp = RampBuilder('FIRE_GROWTH') \\
.add_points(\[
(0, 0),
(30, 50), # Slow initial growth
(120, 500), # Rapid growth
(300, 2000), # Peak HRR
(450, 1000), # Decay after suppression
(600, 200) # Smoldering
\]) \\
.build()
sim.add_ramp(fire_ramp)
<br/>\# === Define Surfaces ===
wall_surf = sim.surface(
id='WALL',
matl_id='GYPSUM_BOARD',
thickness=0.013,
backing='INSULATED'
)
<br/>furniture_surf = sim.surface(
id='FURNITURE',
matl_id='FOAM',
thickness=0.1,
hrrpua=500,
ramp_q='FIRE_GROWTH'
)
<br/>\# === Build Geometry ===
\# Walls (with doors)
sim.obstruction(xb=(0, 10, 0, 0.1, 0, 3), surf_id='WALL') # South
sim.obstruction(xb=(0, 10, 9.9, 10, 0, 3), surf_id='WALL') # North
sim.obstruction(xb=(0, 0.1, 0, 10, 0, 3), surf_id='WALL') # West
sim.obstruction(xb=(9.9, 10, 0, 10, 0, 3), surf_id='WALL') # East
<br/>\# Interior walls
sim.obstruction(xb=(0, 5, 4.9, 5, 0, 3), surf_id='WALL') # Room divider
<br/>\# Furniture (fire source)
sim.obstruction(
xb=(1, 2, 1, 3, 0, 0.8),
surf_ids={'top': 'FURNITURE'},
devc_id='IGNITION'
)
<br/>\# === Devices and Properties ===
\# Smoke detector property
smoke_prop = DeviceProperty(
id='SMOKE_DETECT',
activation_obscuration=3.28, # %/m
rti=5.0
)
sim.add_property(smoke_prop)
<br/>\# Sprinkler property
sprinkler_prop = DeviceProperty(
id='SPRINKLER',
activation_temperature=68, # °C
rti=50,
flow_rate=60, # L/min
k_factor=80,
spray_angle=(0, 60)
)
sim.add_property(sprinkler_prop)
<br/>\# Place smoke detectors
for x in \[2.5, 7.5\]:
for y in \[2.5, 7.5\]:
sim.device(
id=f'SD_{x}\_{y}',
xyz=(x, y, 2.9),
prop_id='SMOKE_DETECT',
quantity='CHAMBER_OBSCURATION'
)
<br/>\# Place sprinklers
for x in \[2.5, 7.5\]:
for y in \[2.5, 7.5\]:
sim.device(
id=f'SPRK_{x}\_{y}',
xyz=(x, y, 2.95),
prop_id='SPRINKLER',
quantity='TEMPERATURE',
initial_state=False
)
<br/>\# === Control Logic ===
\# Smoke alarm activation
alarm_ctrl = ControlBuilder('SMOKE_ALARM') \\
.any_input(\[
'SD_2.5_2.5', 'SD_2.5_7.5',
'SD_7.5_2.5', 'SD_7.5_7.5'
\]) \\
.build()
sim.add_control(alarm_ctrl)
<br/>\# Sprinkler activation control with delay
for x in \[2.5, 7.5\]:
for y in \[2.5, 7.5\]:
sprk_ctrl = ControlBuilder(f'SPRK_CTRL_{x}\_{y}') \\
.time_delay(f'SPRK_{x}\_{y}', delay=2.0) \\
.build()
sim.add_control(sprk_ctrl)
<br/>\# HVAC shutdown on smoke detection
hvac_ctrl = ControlBuilder('HVAC_SHUTDOWN') \\
.all_inputs(\['SMOKE_ALARM'\]) \\
.build()
sim.add_control(hvac_ctrl)
<br/>\# === Initial Conditions ===
\# Pre-heat zone near ignition source
init_hot = InitialCondition(
xb=(0.5, 2.5, 0.5, 3.5, 0, 1),
temperature=50,
spec_id=\['OXYGEN', 'NITROGEN'\],
volume_fraction=\[0.20, 0.80\]
)
sim.add_initial_condition(init_hot)
<br/>\# === Output Controls ===
sim.slcf(quantity='TEMPERATURE', pby=5.0)
sim.slcf(quantity='VISIBILITY', pbz=1.8)
sim.slcf(quantity='CARBON_MONOXIDE', pbz=1.8)
<br/>sim.bndf(quantity='WALL_TEMPERATURE')
sim.bndf(quantity='BURNING_RATE')
<br/>\# === Generate Files ===
sim.write('apartment_fire.fds')
print("✅ Generated apartment_fire.fds")
<br/>\# Create a parameter study varying sprinkler RTI
from pyfds import ParametricStudy
<br/>study = ParametricStudy('sprinkler_sensitivity')
study.add_parameter('rti', \[25, 50, 100, 200\])
study.add_parameter('k_factor', \[60, 80, 115\])
<br/>def build_scenario(rti, k_factor):
s = sim.copy() # Copy base simulation
<br/>\# Modify sprinkler properties
new_prop = DeviceProperty(
id='SPRINKLER',
activation_temperature=68,
rti=rti,
k_factor=k_factor
)
s.properties\['SPRINKLER'\] = new_prop
<br/>s.chid = f'apartment_rti{rti}\_k{k_factor}'
return s
<br/>study.set_builder(build_scenario)
study.run(parallel=True, max_workers=12)
<br/>\# Analyze results
results = study.get_results()
results.plot_sensitivity('rti', 'activation_time')
results.plot_sensitivity('k_factor', 'peak_temperature')
results.to_csv('sprinkler_sensitivity.csv')
<br/>print("✅ Parametric study complete")
