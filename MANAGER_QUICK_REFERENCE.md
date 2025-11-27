# PyFDS Manager API Quick Reference

## Manager Access Patterns

| What You Want | Old API (Removed) | New API (Current) |
|---------------|-------------------|-------------------|
| **Meshes** | `sim.meshes` | `sim.geometry.meshes` |
| **Obstructions** | `sim.obstructions` | `sim.geometry.obstructions` |
| **Vents** | `sim.vents` | `sim.geometry.vents` |
| **Materials** | `sim.materials` | `sim.material_mgr.materials` |
| **Surfaces** | `sim.surfaces` | `sim.material_mgr.surfaces` |
| **RAMPs** | `sim.material_mgr.ramps` | `sim.ramps.ramps` |
| **Reactions** | `sim.reactions` | `sim.physics.reactions` |
| **MISC** | `sim.misc_params` | `sim.physics.misc_params` |
| **Devices** | `sim.devices` | `sim.instrumentation.devices` |
| **Props** | `sim.props` | `sim.instrumentation.props` |
| **Controls** | `sim.ctrls` | `sim.controls.ctrls` |
| **Init Conditions** | `sim.inits` | `sim.controls.inits` |

## Convenience Methods (Unchanged)

These still work exactly as before:
```python
sim.mesh(...)
sim.obstruction(...)
sim.vent(...)
sim.add_material(...)
sim.surface(...)
sim.add_ramp(...)
sim.add_reaction(...)
sim.misc(...)
sim.device(...)
sim.add_prop(...)
sim.add_ctrl(...)
sim.add_init(...)
```

## Common Patterns

### Check Component Count
```python
# Meshes
num_meshes = len(sim.geometry.meshes)

# Surfaces
num_surfaces = len(sim.material_mgr.surfaces)

# RAMPs
num_ramps = len(sim.ramps.ramps)

# Devices
num_devices = len(sim.instrumentation.devices)
```

### Iterate Over Components
```python
# Iterate over meshes
for mesh in sim.geometry.meshes:
    print(f"Mesh: {mesh.ijk}")

# Iterate over surfaces
for surf in sim.material_mgr.surfaces:
    print(f"Surface: {surf.id}")

# Iterate over RAMPs
for ramp in sim.ramps.ramps:
    print(f"RAMP {ramp.id}: {len(ramp.points)} points")
```

### Manager Validation
```python
# Validate specific manager
geo_warnings = sim.geometry.validate()
ramp_warnings = sim.ramps.validate()
device_warnings = sim.instrumentation.validate()

# Validate entire simulation (calls all managers)
all_warnings = sim.validate()
```

## Complete Example

```python
from pyfds import Simulation
from pyfds.core.namelists import Ramp

# Create simulation
sim = Simulation(chid='test')
sim.time(t_end=300.0)

# Add components using convenience methods
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Add RAMP
hrr_ramp = Ramp(id="HRR", points=[(0, 0), (60, 1000)])
sim.add_ramp(hrr_ramp)

# Add surface with RAMP reference
sim.surface(id='FIRE', hrrpua=500.0, ramp_q='HRR')

# Add obstruction
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# Inspect using managers
print(f"Components:")
print(f"  Meshes: {len(sim.geometry.meshes)}")
print(f"  Surfaces: {len(sim.material_mgr.surfaces)}")
print(f"  RAMPs: {len(sim.ramps.ramps)}")
print(f"  Obstructions: {len(sim.geometry.obstructions)}")

# Validate and write
warnings = sim.validate()
if not warnings:
    sim.write('test.fds')
```

## The 7 Managers

1. **GeometryManager** (`sim.geometry`)
   - Meshes, Obstructions, Vents

2. **MaterialManager** (`sim.material_mgr`)
   - Materials, Surfaces

3. **RampManager** (`sim.ramps`)
   - Time-varying functions

4. **PhysicsManager** (`sim.physics`)
   - Reactions, MISC parameters

5. **InstrumentationManager** (`sim.instrumentation`)
   - Devices, Props

6. **ControlManager** (`sim.controls`)
   - Controls, Initial conditions

7. **OutputManager** (internal)
   - FDS file generation

## Why Managers?

- **Separation of Concerns**: Each manager handles one domain
- **Better Testing**: Test managers independently
- **Clearer Code**: `sim.geometry.meshes` is explicit
- **Easier Validation**: Manager-specific validation
- **Extensibility**: Easy to add new managers

## More Information

- Full documentation: `docs-mkdocs/development/managers.md`
- Migration guide: `docs/Manager_Architecture_Migration_Guide.md`
- Examples: `examples/` directory
