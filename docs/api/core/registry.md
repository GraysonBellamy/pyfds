# Registry System

::: pyfds.core.registry
::: pyfds.core.registry_view

## Overview

The registry system manages namelist objects and their IDs within a simulation. It provides centralized object storage, ID validation, and type-safe retrieval.

## Core Components

### Registry

Generic registry for managing objects of a specific type with ID-based lookup.

```python
from pyfds.core.registry import Registry
from pyfds import Surface

# Create a registry for surfaces
registry = Registry[Surface]()

# Add objects
surface = Surface(id="FIRE", hrrpua=1000)
registry.add(surface)

# Retrieve by ID
fire_surface = registry.get("FIRE")

# Check existence
if registry.contains("FIRE"):
    print("Surface exists")

# Get all objects
all_surfaces = registry.get_all()
```

**Type Parameters:**

- `T`: The type of object stored in the registry

**Key Methods:**

- `add(obj)`: Add an object to the registry
- `get(id)`: Retrieve an object by ID
- `contains(id)`: Check if an ID exists
- `get_all()`: Get all registered objects
- `clear()`: Remove all objects

### SimulationRegistry

Main registry system for a simulation, managing all namelist types.

```python
from pyfds import Simulation

sim = Simulation(chid="test")

# Registry is accessed internally
# Add objects to simulation
sim.add(Surface(id="FIRE", hrrpua=1000))
sim.add(Mesh(id="MAIN", ijk=(50, 50, 25), xb=(0, 10, 0, 10, 0, 5)))

# The registry manages these objects and enforces ID uniqueness
```

The `SimulationRegistry` maintains separate registries for each namelist type:

- Surfaces
- Materials
- Meshes
- Devices
- Reactions
- Species
- And all other namelist types

### RegistryView

Read-only view of a registry, providing safe access without modification.

```python
from pyfds import Simulation

sim = Simulation(chid="test")
sim.add(Surface(id="FIRE", hrrpua=1000))
sim.add(Surface(id="WALL", color="GRAY"))

# Get read-only view of surfaces
surfaces_view = sim.surfaces  # Returns RegistryView

# Read operations work
fire = surfaces_view.get("FIRE")
all_surfaces = surfaces_view.get_all()
exists = surfaces_view.contains("FIRE")

# But cannot modify through view
# surfaces_view.add(...)  # Not available
```

## ID Management

### Duplicate ID Detection

```python
from pyfds import Simulation, Surface
from pyfds.exceptions import DuplicateIdError

sim = Simulation(chid="test")
sim.add(Surface(id="FIRE", hrrpua=1000))

try:
    # This will raise an error
    sim.add(Surface(id="FIRE", hrrpua=2000))
except DuplicateIdError as e:
    print(f"Cannot add duplicate: {e}")
```

### ID Resolution

```python
from pyfds import Simulation, Obstruction
from pyfds.exceptions import UnknownIdError

sim = Simulation(chid="test")

try:
    # Reference to undefined surface
    sim.add(Obstruction(xb=(0, 1, 0, 1, 0, 1), surf_id="UNKNOWN"))
    sim.write("test.fds")  # Validation catches this
except UnknownIdError as e:
    print(f"Unknown surface ID: {e}")
```

## Type-Safe Access

The registry system is fully typed for IDE support:

```python
from pyfds import Simulation, Surface

sim = Simulation(chid="test")
sim.add(Surface(id="FIRE", hrrpua=1000))

# Type-safe retrieval
surface: Surface = sim.surfaces.get("FIRE")

# IDE knows the type and provides autocomplete
print(surface.hrrpua)  # IDE suggests 'hrrpua' attribute
```

## Common Patterns

### Checking for Existence

```python
sim = Simulation(chid="test")

# Check before adding
if not sim.surfaces.contains("FIRE"):
    sim.add(Surface(id="FIRE", hrrpua=1000))
```

### Retrieving All Objects

```python
sim = Simulation(chid="test")
# ... add multiple surfaces ...

# Get all surfaces
for surface in sim.surfaces.get_all():
    print(f"Surface {surface.id}: {surface.hrrpua} kW/m²")
```

### Conditional Object Creation

```python
sim = Simulation(chid="test")

# Add default surface if not present
if not sim.surfaces.contains("INERT"):
    sim.add(Surface(id="INERT", color="GRAY"))
```

### Getting Object Count

```python
sim = Simulation(chid="test")
# ... add objects ...

# Check registry sizes
surface_count = len(sim.surfaces.get_all())
mesh_count = len(sim.meshes.get_all())

print(f"Simulation has {surface_count} surfaces and {mesh_count} meshes")
```

## Advanced Usage

### Programmatic Object Creation

```python
from pyfds import Simulation, Device
from pyfds.core.geometry import Point3D

sim = Simulation(chid="test")

# Create devices programmatically
for i in range(10):
    for j in range(10):
        device = Device(
            id=f"TEMP_{i}_{j}",
            quantity="TEMPERATURE",
            xyz=Point3D.of(i * 1.0, j * 1.0, 2.4)
        )
        sim.add(device)

# Registry ensures all IDs are unique
print(f"Created {len(sim.devices.get_all())} devices")
```

### Bulk Operations

```python
sim = Simulation(chid="test")
# ... add multiple materials ...

# Process all materials
for material in sim.materials.get_all():
    if material.density and material.density < 100:
        print(f"Warning: {material.id} has low density")
```

### Cross-Reference Validation

```python
sim = Simulation(chid="test")

# Add surfaces and obstructions
sim.add(Surface(id="FIRE", hrrpua=1000))
sim.add(Obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id="FIRE"))

# Registry enables validation
# Can check if all surf_id references are valid
for obst in sim.obstructions.get_all():
    if obst.surf_id and not sim.surfaces.contains(obst.surf_id):
        print(f"Warning: {obst.id} references unknown surface {obst.surf_id}")
```

## Implementation Details

### Registry Properties

Each namelist type has a corresponding registry accessed via properties on the `Simulation` class:

```python
sim.surfaces      # Registry[Surface]
sim.materials     # Registry[Material]
sim.meshes        # Registry[Mesh]
sim.obstructions  # Registry[Obstruction]
sim.vents         # Registry[Vent]
sim.devices       # Registry[Device]
sim.reactions     # Registry[Reaction]
sim.species       # Registry[Species]
# ... and more
```

### ID Uniqueness Scope

IDs must be unique **within their type**:

```python
sim = Simulation(chid="test")

# This is OK - different types can have same ID
sim.add(Surface(id="FIRE", hrrpua=1000))
sim.add(Device(id="FIRE", quantity="TEMPERATURE", xyz=(5, 5, 2)))

# This is NOT OK - same type, duplicate ID
sim.add(Surface(id="FIRE", color="RED"))  # Raises DuplicateIdError
```

## See Also

- [Simulation](simulation.md) - Main simulation class that uses registries
- [Exceptions](../exceptions.md) - DuplicateIdError and UnknownIdError
- [Validation](../utils/validation.md) - Cross-reference validation
