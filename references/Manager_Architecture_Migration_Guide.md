# PyFDS Manager Architecture - Migration Guide

## Overview

PyFDS has been refactored to use a **manager-based architecture** that organizes simulation components into specialized managers following the Single Responsibility Principle.

## Key Changes

### Before (Legacy API - Removed)
```python
# Direct property access (NO LONGER AVAILABLE)
sim.meshes              # ❌ Removed
sim.surfaces            # ❌ Removed
sim.devices             # ❌ Removed
sim.obstructions        # ❌ Removed
sim.vents               # ❌ Removed
sim.ramps               # ❌ Removed
sim.materials           # ❌ Removed
sim.reactions           # ❌ Removed
```

### After (Current Manager API)
```python
# Manager-based access (CURRENT)
sim.geometry.meshes             # ✅ Meshes via GeometryManager
sim.material_mgr.surfaces       # ✅ Surfaces via MaterialManager
sim.instrumentation.devices     # ✅ Devices via InstrumentationManager
sim.geometry.obstructions       # ✅ Obstructions via GeometryManager
sim.geometry.vents              # ✅ Vents via GeometryManager
sim.ramps.ramps                 # ✅ RAMPs via RampManager (NEW!)
sim.material_mgr.materials      # ✅ Materials via MaterialManager
sim.physics.reactions           # ✅ Reactions via PhysicsManager
sim.controls.ctrls              # ✅ Controls via ControlManager
sim.controls.inits              # ✅ Initial conditions via ControlManager
```

### Convenience Methods (UNCHANGED)
```python
# These still work exactly as before
sim.mesh(...)           # ✅ Delegates to GeometryManager
sim.surface(...)        # ✅ Delegates to MaterialManager
sim.device(...)         # ✅ Delegates to InstrumentationManager
sim.obstruction(...)    # ✅ Delegates to GeometryManager
sim.add_ramp(...)       # ✅ Delegates to RampManager
sim.add_material(...)   # ✅ Delegates to MaterialManager
```

## The 7 Managers

| Manager | Responsibilities | Access Property |
|---------|-----------------|-----------------|
| **GeometryManager** | Meshes, Obstructions, Vents | `sim.geometry` |
| **MaterialManager** | Materials, Surfaces | `sim.material_mgr` |
| **RampManager** | Time-varying functions | `sim.ramps` |
| **PhysicsManager** | Reactions, MISC parameters | `sim.physics` |
| **InstrumentationManager** | Devices, Props | `sim.instrumentation` |
| **ControlManager** | Controls, Initial conditions | `sim.controls` |
| **OutputManager** | FDS file generation | (internal) |

## Why RampManager is Separate

RAMPs were moved from MaterialManager to their own dedicated manager because they are **cross-cutting entities** used by multiple namelists:

- **Materials**: `conductivity_ramp`, `specific_heat_ramp`
- **Surfaces**: `ramp_q` (HRR), `ramp_t` (temperature)
- **Vents**: `ramp_v` (flow rates)
- **Controls**: Time-based setpoints
- **Devices**: Output controls

This separation follows the principle that RAMPs are a foundational FDS concept, not specific to materials.

## Migration Examples

### Example 1: Checking Number of Meshes
```python
# Old (deprecated)
num_meshes = len(sim.meshes)  # ❌ No longer works

# New
num_meshes = len(sim.geometry.meshes)  # ✅ Correct
```

### Example 2: Iterating Over Surfaces
```python
# Old (deprecated)
for surf in sim.surfaces:  # ❌ No longer works
    print(surf.id)

# New
for surf in sim.material_mgr.surfaces:  # ✅ Correct
    print(surf.id)
```

### Example 3: Adding and Accessing RAMPs
```python
from pyfds.core.namelists import Ramp

# Adding ramps (both methods work)
hrr_ramp = Ramp(id="FIRE_RAMP", points=[(0, 0), (60, 1000)])
sim.add_ramp(hrr_ramp)              # ✅ Convenience method
sim.ramps.add_ramp(hrr_ramp)        # ✅ Direct manager access

# Accessing ramps
# Old (deprecated)
all_ramps = sim.material_mgr.ramps  # ❌ No longer here

# New
all_ramps = sim.ramps.ramps         # ✅ Correct (dedicated manager)
print(f"Total ramps: {len(all_ramps)}")
```

### Example 4: Validation
```python
# Validate entire simulation (unchanged)
warnings = sim.validate()  # ✅ Still works, calls all managers

# Validate specific managers (new capability)
geo_warnings = sim.geometry.validate()        # Geometry-specific checks
ramp_warnings = sim.ramps.validate()          # Check duplicate RAMP IDs
device_warnings = sim.instrumentation.validate()  # Check duplicate device IDs
```

### Example 5: Complete Example
```python
from pyfds import Simulation
from pyfds.core.namelists import Ramp, Material

# Create simulation
sim = Simulation(chid='fire_test')
sim.time(t_end=300.0)

# Add mesh (convenience method - unchanged)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Add ramp (convenience method)
hrr_ramp = Ramp(id="HRR", points=[(0, 0), (60, 1000)])
sim.add_ramp(hrr_ramp)

# Add surface with ramp reference
sim.surface(id='FIRE', hrrpua=500.0, ramp_q='HRR')

# Inspect components (manager access)
print(f"Meshes: {len(sim.geometry.meshes)}")
print(f"Surfaces: {len(sim.material_mgr.surfaces)}")
print(f"Ramps: {len(sim.ramps.ramps)}")

# Validate
warnings = sim.validate()
if warnings:
    for w in warnings:
        print(f"Warning: {w}")

# Write FDS file (unchanged)
sim.write('fire_test.fds')
```

## Benefits

1. **Cleaner Code**: Each manager handles one domain
2. **Better Testing**: Test managers in isolation
3. **Easier Validation**: Manager-specific validation logic
4. **Clearer API**: `sim.geometry.meshes` is more explicit than `sim.meshes`
5. **Extensibility**: Easy to add new managers for new FDS features

## For More Information

See the full documentation:
- [Manager Architecture](docs/development/managers.md) - Complete manager reference
- [Architecture Overview](docs/development/architecture.md) - System design
- [Examples](examples/) - Updated examples using manager API

## Questions?

If you encounter issues migrating your code, please:
1. Check the [examples/](examples/) directory for updated patterns
2. Review the [Manager Architecture docs](docs/development/managers.md)
3. Open an issue on GitHub with your specific use case
