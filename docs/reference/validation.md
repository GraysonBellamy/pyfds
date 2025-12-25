# Validation

PyFDS validation system for ensuring correct FDS input files.

## Overview

PyFDS includes a comprehensive validation system that checks simulation configurations before writing FDS files. This catches errors early and ensures FDS compatibility.

```python
from pyfds import Simulation, Validator

sim = Simulation(chid='test')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Validate before writing
validator = Validator()
errors = validator.validate(sim)

if errors:
    for error in errors:
        print(f"Error: {error}")
else:
    sim.write('test.fds')
```

## Validation Levels

### Error

Critical issues that will cause FDS to fail or produce invalid results.

**Examples**:

- Missing required parameters
- Invalid parameter types
- Mesh bounds mismatch
- Non-positive dimensions
- Invalid ID references

### Warning

Issues that may cause problems but won't prevent FDS from running.

**Examples**:

- Poor mesh resolution
- Large aspect ratios
- Missing recommended parameters
- Unusual parameter values

### Info

Informational messages about best practices.

**Examples**:

- Mesh quality suggestions
- Performance recommendations
- Alternative approaches

## Automatic Validation

PyFDS performs automatic validation when:

```python
# During write
sim.write('test.fds')  # Validates automatically

# Explicitly
sim.validate()  # Returns list of errors

# Check validity
if sim.is_valid():
    sim.write('test.fds')
```

## Validation Rules

### Required Parameters

#### Simulation Level

| Check | Rule | Error |
|-------|------|-------|
| CHID | Must be set | "CHID is required" |
| TIME | Must have T_END | "T_END is required" |
| MESH | At least one mesh | "At least one MESH required" |

**Example**

```python
# This will fail validation
sim = Simulation(chid='test')
# Missing TIME and MESH

# This passes
sim = Simulation(chid='test')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
```

#### Namelist Level

Each namelist has specific required parameters:

```python
# MESH requires IJK and XB
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))  # Valid
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25)))  # Invalid - missing XB

# OBST requires XB
sim.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 0.1)))  # Valid
sim.add(Obstruction(surf_id='FIRE'))  # Invalid - missing XB

# DEVC requires ID and QUANTITY
sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(1, 1, 1)))  # Valid
sim.add(Device(id='TEMP'))  # Invalid - missing QUANTITY
```

### Parameter Types

PyFDS validates parameter types using Pydantic:

```python
# Correct types
sim.add(Time(t_end=600.0))  # float
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25)))  # tuple of ints
sim.add(Surface(id='FIRE', hrrpua=1000.0))  # str and float

# Type errors
sim.add(Time(t_end="600"))  # Error: expected float, got str
sim.add(Mesh(ijk="50,50,25"))  # Error: expected tuple, got str
sim.add(Surface(id='FIRE', hrrpua="1000"))  # Error: expected float, got str
```

### Geometric Validation

#### Mesh Bounds

Meshes must have valid bounds:

```python
# Valid bounds (x1 > x0, y1 > y0, z1 > z0)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))  # ✓

# Invalid bounds
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(5, 0, 0, 5, 0, 2.5)))  # ✗ x1 < x0
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 0, 0, 2.5)))  # ✗ y1 = y0
```

#### Cell Count Validation

Cell counts must be positive integers:

```python
# Valid
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))  # ✓

# Invalid
sim.add(Mesh(ijk=Grid3D.of(0, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))  # ✗ I = 0
sim.add(Mesh(ijk=Grid3D.of(-10, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))  # ✗ negative
sim.add(Mesh(ijk=Grid3D.of(50.5, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))  # ✗ float
```

#### Obstruction Validation

Obstructions must be within mesh bounds:

```python
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Valid - inside mesh
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1)))  # ✓

# Warning - outside mesh
sim.add(Obstruction(xb=Bounds3D.of(6, 7, 1, 2, 0, 0.1)))  # ⚠ x > 5 (outside domain)
```

### Mesh Quality

PyFDS checks mesh resolution and quality:

#### Cell Size

```python
# Check cell size for fire
D_star = 1.0  # Characteristic fire diameter (m)
dx = 0.1  # Cell size (m)
ratio = D_star / dx  # Should be 4-16

if ratio < 4:
    print("Warning: Mesh too coarse")
elif ratio > 16:
    print("Warning: Mesh finer than necessary")
```

#### Aspect Ratio

```python
# Calculate aspect ratios
mesh_xb = (0, 5, 0, 5, 0, 2.5)
mesh_ijk = (50, 50, 25)

dx = (mesh_xb[1] - mesh_xb[0]) / mesh_ijk[0]  # 0.1
dy = (mesh_xb[3] - mesh_xb[2]) / mesh_ijk[1]  # 0.1
dz = (mesh_xb[5] - mesh_xb[4]) / mesh_ijk[2]  # 0.1

aspect_xy = max(dx, dy) / min(dx, dy)  # 1.0 (ideal)
aspect_xz = max(dx, dz) / min(dx, dz)  # 1.0 (ideal)
aspect_yz = max(dy, dz) / min(dy, dz)  # 1.0 (ideal)

# Warning if aspect ratio > 2
if max(aspect_xy, aspect_xz, aspect_yz) > 2:
    print("Warning: High aspect ratio")
```

### Reference Validation

PyFDS validates ID references:

```python
# Surface reference
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE'))  # ✓

# Invalid reference
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='UNKNOWN'))  # ✗

# Material reference
sim.add(Material(id='CONCRETE', conductivity=1.0, specific_heat=0.88, density=2280))
sim.add(Surface(id='WALL', matl_id='CONCRETE'))  # ✓

# Invalid reference
sim.add(Surface(id='WALL', matl_id='UNKNOWN'))  # ✗

# RAMP reference
sim.add(Ramp(id='GROWTH', t=0, f=0.0))
sim.add(Ramp(id='GROWTH', t=60, f=1.0))
sim.add(Surface(id='FIRE', hrrpua=1000.0, ramp_q='GROWTH'))  # ✓

# Invalid reference
sim.add(Surface(id='FIRE', hrrpua=1000.0, ramp_q='UNKNOWN'))  # ✗

# Control reference
sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(1, 1, 1)))
sim.add(Ctrl(id='CTRL', input_id='TEMP', setpoint=100.0))  # ✓

# Invalid reference
sim.add(Ctrl(id='CTRL', input_id='UNKNOWN', setpoint=100.0))  # ✗
```

### Physical Validation

#### Temperature Ranges

```python
# Valid temperatures
sim.add(Misc(tmpa=20.0))  # ✓
sim.add(Init(xb=Bounds3D.of(0, 5, 0, 5, 2, 2.5), temperature=600.0))  # ✓

# Warnings for extreme values
sim.add(Misc(tmpa=-50.0))  # ⚠ Very cold
sim.add(Init(xb=Bounds3D.of(0, 5, 0, 5, 2, 2.5), temperature=2000.0))  # ⚠ Very hot
```

#### Velocity Limits

```python
# Reasonable velocities
sim.add(Surface(id='SUPPLY', vel=-1.5))  # ✓

# Warning for high velocity
sim.add(Surface(id='SUPPLY', vel=-50.0))  # ⚠ Very high velocity
```

#### Material Properties

```python
# Valid material properties
sim.add(
    Material(
        id='CONCRETE',
        conductivity=1.0,  # W/m/K (reasonable)
        specific_heat=0.88,  # kJ/kg/K (reasonable)
        density=2280,  # kg/m³ (reasonable)
        emissivity=0.9,  # 0-1 (valid)
    )
)  # ✓

# Invalid emissivity
sim.add(
    Material(
        id='TEST',
        conductivity=1.0,
        specific_heat=0.88,
        density=2280,
        emissivity=1.5,  # ✗ Must be 0-1
    )
)
```

## Validation Reports

### Summary Report

```python
sim = Simulation(chid='test')
# ... configure simulation ...

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
Warnings: 2
Info: 1

Warnings:
  - Mesh resolution marginal (D*/dx = 3.2, recommend 4-16)
  - OBST 'DESK' partially outside mesh bounds

Info:
  - Consider adding devices for temperature monitoring
```

### Detailed Report

```python
report = sim.validation_report(detailed=True)
```

Output shows:
- Each validation check performed
- Pass/fail status
- Recommendations
- FDS best practices

## Custom Validation

### Adding Validators

```python
from pyfds.validation import Validator, ValidationRule

class CustomValidator(Validator):
    def validate_custom(self, sim):
        """Custom validation logic."""
        errors = []

        # Check for specific condition
        if not sim.instrumentation.devices:
            errors.append("No devices defined - add measurement points")

        return errors

# Use custom validator
validator = CustomValidator()
errors = validator.validate(sim)
```

### Validation Rules

```python
from pyfds.validation import ValidationRule

# Define custom rule
rule = ValidationRule(
    name="mesh_count",
    level="warning",
    check=lambda sim: len(sim.geometry.meshes) > 10,
    message="More than 10 meshes may impact performance"
)

# Add to validator
validator.add_rule(rule)
```

## Disabling Validation

### Skip Validation

```python
# Skip validation on write
sim.write('test.fds', validate=False)

# Disable specific checks
validator = Validator(skip_checks=['mesh_resolution', 'aspect_ratio'])
```

### Suppress Warnings

```python
# Only show errors
sim.validate(level='error')

# Show all
sim.validate(level='info')
```

## Common Validation Errors

### Missing MESH

```python
# Error
sim = Simulation(chid='test')
sim.add(Time(t_end=600.0))
sim.write('test.fds')  # ✗ No MESH defined

# Fix
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
sim.write('test.fds')  # ✓
```

### Invalid Bounds

```python
# Error
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(5, 0, 0, 5, 0, 2.5))  # ✗ x1 < x0

# Fix
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))  # ✓
```

### Undefined Reference

```python
# Error
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE'))  # ✗ 'FIRE' not defined

# Fix
sim.add(Surface(id='FIRE', hrrpua=1000.0))
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE'))  # ✓
```

### Device Missing Location

```python
# Error
sim.add(Device(id='TEMP', quantity='TEMPERATURE')  # ✗ No XYZ or XB

# Fix
sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(1, 1, 1))  # ✓
```

## Best Practices

### Always Validate

Run validation before long simulations:

```python
if sim.is_valid():
    sim.write('test.fds')
    sim.run()
else:
    errors = sim.validate()
    for error in errors:
        print(error)
```

### Use Type Hints

Leverage IDE type checking:

```python
from pyfds import Simulation

def create_simulation(name: str, end_time: float) -> Simulation:
    sim = Simulation(chid=name)
    sim.add(Time(t_end=end_time))  # IDE will check types
    return sim
```

### Check Reports

Review validation reports:

```python
# Always check before parametric studies
for hrr in [500, 1000, 1500]:
    sim = create_fire_sim(hrr)
    if not sim.is_valid():
        print(f"HRR {hrr}: {sim.validate()}")
```

### Fix Warnings

Address warnings for better results:

```python
report = sim.validation_report()
if report.warnings:
    print("Fix these warnings:")
    for warning in report.warnings:
        print(f"  - {warning}")
```

## Integration with CI/CD

### Automated Validation

```python
# test_validation.py
import pytest
from pyfds import Simulation

def test_simulation_valid():
    sim = Simulation(chid='test')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    assert sim.is_valid(), f"Validation errors: {sim.validate()}"

def test_mesh_resolution():
    sim = create_standard_sim()
    report = sim.validation_report()

    # No errors allowed
    assert report.error_count == 0

    # Warnings are acceptable but should be tracked
    if report.warning_count > 0:
        print(f"Warnings: {report.warnings}")
```

### Pre-commit Hooks

```bash
# .git/hooks/pre-commit
#!/bin/bash
python -m pytest tests/test_validation.py
```

## See Also

- [User Guide](../guide/building-simulations.md) - Building valid simulations
- [Troubleshooting](troubleshooting.md) - Fixing common issues
- [API Reference](../api/utils/validation.md) - Validation API

---

[Glossary →](glossary.md){ .md-button .md-button--primary }
