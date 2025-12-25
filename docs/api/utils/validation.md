# Validation

::: pyfds.validation

## Overview

The validation module provides comprehensive validation for FDS simulations, including input sanitization, simulation-level validation, cross-reference checking, and execution configuration validation.

## Core Classes

### Validator

Main validation class for checking simulation configurations.

```python
from pyfds import Simulation, Mesh, Time
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.validation import Validator

sim = Simulation(chid="test")
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Validate simulation
validator = Validator()
result = validator.validate(sim)

if result.is_valid:
    print("Simulation is valid!")
else:
    for error in result.errors:
        print(f"Error: {error}")
```

### ValidationResult

Container for validation results with errors, warnings, and info messages.

```python
from pyfds.validation import validate_simulation

sim = Simulation(chid="test")
# ... build simulation ...

result = validate_simulation(sim)

print(f"Valid: {result.is_valid}")
print(f"Errors: {len(result.errors)}")
print(f"Warnings: {len(result.warnings)}")
print(f"Info: {len(result.info)}")

# Get all issues
for issue in result.all_issues:
    print(f"[{issue.severity}] {issue.message}")
```

### Issue

Represents a single validation issue with severity, message, and context.

```python
from pyfds.validation import Issue, Severity

# Issues are created by validators
issue = Issue(
    severity=Severity.ERROR,
    message="Missing required parameter",
    field="t_end",
    value=None
)

print(f"{issue.severity}: {issue.message}")
```

### Severity

Enumeration of validation issue severity levels.

```python
from pyfds.validation import Severity

# Severity levels
Severity.ERROR    # Critical issues that prevent FDS from running
Severity.WARNING  # Issues that may cause problems
Severity.INFO     # Informational messages
```

## Validation Functions

### validate_simulation()

Main entry point for validating a complete simulation.

```python
from pyfds import Simulation, Mesh, Time
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.validation import validate_simulation

sim = Simulation(chid="test")
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

result = validate_simulation(sim)

if not result.is_valid:
    for error in result.errors:
        print(f"Error: {error.message}")
else:
    sim.write("test.fds")
```

### validate_fds_file()

Validate an existing FDS input file.

```python
from pyfds.validation import validate_fds_file

result = validate_fds_file("simulation.fds")

if result.is_valid:
    print("FDS file is valid")
else:
    print("Validation errors found:")
    for error in result.errors:
        print(f"  - {error.message}")
```

## Input Validators

### validate_chid()

Validate simulation CHID (case ID).

```python
from pyfds.validation import validate_chid, CHID_MAX_LENGTH

# Valid CHID
try:
    validate_chid("my_simulation")
except ValueError as e:
    print(f"Invalid CHID: {e}")

# Too long
try:
    validate_chid("a" * (CHID_MAX_LENGTH + 1))
except ValueError as e:
    print(f"CHID too long: {e}")

# Invalid characters
try:
    validate_chid("my simulation")  # Spaces not allowed
except ValueError as e:
    print(f"Invalid characters: {e}")
```

### validate_path()

Validate file paths for safety and existence.

```python
from pyfds.validation import validate_path
from pathlib import Path

# Check if path exists
try:
    validate_path("/path/to/file.fds", must_exist=True)
except ValueError as e:
    print(f"Path validation failed: {e}")

# Check if parent directory exists
try:
    validate_path("/path/to/output.fds", check_parent=True)
except ValueError as e:
    print(f"Parent directory doesn't exist: {e}")
```

### validate_positive_number()

Ensure a number is positive.

```python
from pyfds.validation import validate_positive_number

# Valid
validate_positive_number(10.0, "t_end")  # OK

# Invalid
try:
    validate_positive_number(0, "t_end")
except ValueError as e:
    print(f"Error: {e}")  # "t_end must be positive"

try:
    validate_positive_number(-5.0, "dt")
except ValueError as e:
    print(f"Error: {e}")  # "dt must be positive"
```

### validate_non_negative_number()

Ensure a number is non-negative (zero allowed).

```python
from pyfds.validation import validate_non_negative_number

# Valid
validate_non_negative_number(0, "t_begin")    # OK
validate_non_negative_number(10.0, "t_begin") # OK

# Invalid
try:
    validate_non_negative_number(-1.0, "t_begin")
except ValueError as e:
    print(f"Error: {e}")  # "t_begin must be non-negative"
```

### validate_file_size()

Check if a file size is within acceptable limits.

```python
from pyfds.validation import validate_file_size, MAX_INPUT_FILE_SIZE
from pathlib import Path

fds_file = Path("large_simulation.fds")

try:
    validate_file_size(fds_file, MAX_INPUT_FILE_SIZE)
except ValueError as e:
    print(f"File too large: {e}")
```

### safe_read_text()

Safely read text files with size and encoding validation.

```python
from pyfds.validation import safe_read_text

try:
    content = safe_read_text("simulation.fds")
    print(f"Read {len(content)} characters")
except ValueError as e:
    print(f"Failed to read file: {e}")
```

## Specialized Validators

### SimulationValidator

Comprehensive validation for complete simulations.

```python
from pyfds.validation import SimulationValidator

sim = Simulation(chid="test")
# ... build simulation ...

validator = SimulationValidator(sim)
result = validator.validate()

# Check specific aspects
result.check_required_components()
result.check_cross_references()
result.check_geometry_quality()
result.check_physical_bounds()
```

### CrossReferenceValidator

Validates ID references between namelists.

```python
from pyfds.validation import CrossReferenceValidator

validator = CrossReferenceValidator(sim)
issues = validator.validate()

for issue in issues:
    if "surf_id" in issue.message.lower():
        print(f"Surface reference issue: {issue.message}")
```

### ExecutionValidator

Validates execution configuration.

```python
from pyfds.validation import ExecutionValidator
from pyfds.config import RunConfig

config = RunConfig(n_threads=4, timeout=3600)
validator = ExecutionValidator()

issues = validator.validate_config(config)
if issues:
    for issue in issues:
        print(f"Config issue: {issue.message}")
```

## Validation Constants

```python
from pyfds.validation import (
    CHID_MAX_LENGTH,      # Maximum CHID length
    CHID_PATTERN,         # Regex pattern for valid CHIDs
    MAX_INPUT_FILE_SIZE,  # Maximum input file size
    MAX_OUTPUT_FILE_SIZE, # Maximum output file size
)

print(f"Max CHID length: {CHID_MAX_LENGTH}")
print(f"Max input file: {MAX_INPUT_FILE_SIZE / 1024 / 1024} MB")
```

## Built-in Enums

### BuiltinSpecies

Enumeration of FDS built-in species.

```python
from pyfds.validation import BuiltinSpecies

# Check if species is built-in
if "OXYGEN" in [s.value for s in BuiltinSpecies]:
    print("OXYGEN is a built-in species")

# List all built-in species
for species in BuiltinSpecies:
    print(species.value)
```

### BuiltinSurface

Enumeration of FDS built-in surfaces.

```python
from pyfds.validation import BuiltinSurface

# Check if surface is built-in
if "INERT" in [s.value for s in BuiltinSurface]:
    print("INERT is a built-in surface")

# List all built-in surfaces
for surface in BuiltinSurface:
    print(surface.value)
```

## Usage Patterns

### Pre-Write Validation

```python
from pyfds import Simulation
from pyfds.validation import validate_simulation

sim = Simulation(chid="test")
# ... build simulation ...

# Validate before writing
result = validate_simulation(sim)
if result.is_valid:
    sim.write("test.fds")
else:
    print("Fix these errors before writing:")
    for error in result.errors:
        print(f"  - {error.message}")
```

### Progressive Validation

```python
from pyfds import Simulation
from pyfds.validation import SimulationValidator

sim = Simulation(chid="test")
validator = SimulationValidator(sim)

# Add components and validate incrementally
sim.add(Time(t_end=600.0))
if not validator.check_required_components():
    print("Still missing required components")

sim.add(Mesh(...))
if validator.validate().is_valid:
    print("Simulation is now valid!")
```

### Custom Validation

```python
from pyfds.validation import Validator, Issue, Severity

class CustomValidator(Validator):
    """Custom validator with project-specific rules."""

    def validate_custom(self, sim):
        """Check custom requirements."""
        issues = []

        # Example: require at least one device
        if not sim.get_all(Device):
            issues.append(Issue(
                severity=Severity.WARNING,
                message="No devices defined - results may be limited",
            ))

        return issues
```

## See Also

- [Exceptions](../exceptions.md) - Validation and other exceptions
- [Validation Reference](../../reference/validation.md) - Complete validation rules
- [Troubleshooting](../../reference/troubleshooting.md) - Common validation issues
- [Simulation](../core/simulation.md) - Main simulation class
