# Validator

!!! note "Auto-Generated Documentation"
    The API reference will be automatically populated from source code docstrings using mkdocstrings once the implementation is complete.

## Overview

The `Validator` class checks PyFDS simulation configurations for errors and warnings before writing FDS files.

## Basic Usage

```python
from pyfds import Simulation, Validator

sim = Simulation(chid='test')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Create validator
validator = Validator()

# Validate simulation
errors = validator.validate(sim)

if errors:
    for error in errors:
        print(f"Error: {error}")
else:
    print("Simulation is valid!")
    sim.write('test.fds')
```

## Validation Checks

### Required Parameters

Ensures all required parameters are present:

```python
sim = Simulation(chid='test')
# Missing TIME and MESH

validator = Validator()
errors = validator.validate(sim)
# Returns errors about missing T_END and MESH
```

### Reference Validation

Checks that all ID references are valid:

```python
# Invalid - SURF_ID 'FIRE' doesn't exist
sim.obstruction(xb=(1, 2, 1, 2, 0, 0.1), surf_id='FIRE')

errors = validator.validate(sim)
# Returns error about undefined SURF_ID 'FIRE'

# Add surface to fix
sim.surface(id='FIRE', hrrpua=1000.0)
errors = validator.validate(sim)
# No errors
```

### Geometric Validation

Validates mesh bounds and geometry:

```python
# Invalid bounds (x1 < x0)
try:
    sim.mesh(ijk=(50, 50, 25), xb=(5, 0, 0, 5, 0, 2.5))
except ValidationError as e:
    print(f"Invalid bounds: {e}")

# Valid bounds
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
```

### Mesh Quality

Checks mesh resolution and aspect ratios:

```python
# Coarse mesh warning
sim.mesh(ijk=(5, 5, 5), xb=(0, 10, 0, 10, 0, 5))

validator = Validator()
warnings = validator.get_warnings(sim)
# Returns warning about coarse mesh resolution
```

## Validation Levels

### Error

Critical issues that prevent FDS from running:

- Missing required parameters
- Invalid parameter types
- Mesh bounds issues
- Undefined references

### Warning

Issues that may cause problems:

- Poor mesh resolution
- Large aspect ratios
- Unusual parameter values

### Info

Informational messages:

- Best practice suggestions
- Performance recommendations

## Custom Validation

### Adding Rules

Extend the validator with custom rules:

```python
from pyfds.validation import Validator, ValidationRule

class CustomValidator(Validator):
    def validate_custom(self, sim):
        """Add custom validation logic."""
        errors = []

        # Example: require at least one device
        if not sim.instrumentation.devices:
            errors.append("No devices defined")

        return errors

# Use custom validator
validator = CustomValidator()
errors = validator.validate(sim)
```

### Validation Rule

Create reusable validation rules:

```python
from pyfds.validation import ValidationRule

rule = ValidationRule(
    name="max_meshes",
    level="warning",
    check=lambda sim: len(sim.geometry.meshes) <= 10,
    message="More than 10 meshes may impact performance"
)

validator.add_rule(rule)
```

## Disabling Validation

### Skip Specific Checks

```python
validator = Validator(
    skip_checks=['mesh_resolution', 'aspect_ratio']
)
```

### Suppress Warnings

```python
# Only show errors
errors = validator.validate(sim, level='error')

# Show all (errors, warnings, info)
all_issues = validator.validate(sim, level='info')
```

## Validation Reports

### Summary Report

```python
report = sim.validation_report()
print(report)
```

Output:
```
Validation Report
=================
Simulation: test
Status: Valid ✓

Errors: 0
Warnings: 1
Info: 0

Warnings:
  - Mesh resolution marginal (D*/dx = 3.2, recommend 4-16)
```

### Detailed Report

```python
report = sim.validation_report(detailed=True)
# Shows all checks performed with pass/fail status
```

## Integration

### With Write

Validation runs automatically when writing:

```python
sim.write('test.fds')  # Validates before writing
```

### With Run

Validation runs before execution:

```python
sim.run()  # Validates before running FDS
```

### Manual Check

Check validity without writing:

```python
if sim.is_valid():
    print("Ready to run")
else:
    errors = sim.validate()
    print("Fix these errors:", errors)
```

## Common Patterns

### Pre-Flight Check

```python
def safe_run(sim):
    """Run simulation with validation."""
    validator = Validator()
    errors = validator.validate(sim)

    if errors:
        raise ValidationError(f"Invalid simulation: {errors}")

    return sim.run()
```

### Batch Validation

```python
simulations = [create_sim(i) for i in range(10)]

validator = Validator()
for sim in simulations:
    errors = validator.validate(sim)
    if errors:
        print(f"{sim.chid}: FAILED")
        for error in errors:
            print(f"  - {error}")
    else:
        print(f"{sim.chid}: OK")
```

### CI/CD Integration

```python
import pytest

def test_simulation_valid():
    """Ensure simulation is valid."""
    sim = create_production_sim()

    validator = Validator()
    errors = validator.validate(sim)

    assert not errors, f"Validation failed: {errors}"
```

## See Also

- [Validation Guide](../../reference/validation.md) - Complete validation documentation
- [Simulation](simulation.md) - Main simulation class
- [Troubleshooting](../../reference/troubleshooting.md) - Common issues

---

[Simulation →](simulation.md){ .md-button .md-button--primary }
[Back to API →](../index.md){ .md-button }
