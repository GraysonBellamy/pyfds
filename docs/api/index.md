# API Reference

Complete API documentation for all PyFDS classes and functions.

## Overview

The API Reference provides detailed technical documentation auto-generated from the source code docstrings. All public APIs are documented with parameters, return values, examples, and type information.

## Modules

<div class="grid cards" markdown>

-   :material-cog-outline: **Core**

    ---

    Main simulation classes and core components

    - [Simulation](core/simulation.md)
    - [Registry](core/registry.md)
    - [Models](core/models.md)
    - [Enumerations](core/enums.md)

-   :material-hammer-wrench: **Builders**

    ---

    Fluent APIs and pre-configured objects

    - [Builders](builders/index.md)
    - [Libraries](builders/libraries.md)

-   :material-format-list-bulleted: **Namelists**

    ---

    FDS namelist group classes

    - [Base Classes](namelists/base.md)
    - [Metadata](namelists/metadata.md)
    - [Domain](namelists/domain.md)
    - [Geometry](namelists/geometry.md)
    - [Materials](namelists/materials.md)
    - [Devices](namelists/devices.md)
    - [Complex](namelists/complex.md)

-   :material-shield-check: **Validation**

    ---

    Input validation and error checking

    - [Validation](utils/validation.md)

-   :material-alert-circle: **Exceptions**

    ---

    Error handling and exception types

    - [Exceptions](exceptions.md)

-   :material-play-circle-outline: **Execution**

    ---

    Simulation execution and job management

    - [Runner](execution/runner.md)
    - [Job](execution/job.md)
    - [Monitor](execution/monitor.md)

-   :material-chart-line-variant: **Analysis**

    ---

    Results analysis and visualization

    - [Results](analysis/results.md)

-   :material-file-document-outline: **I/O**

    ---

    File input/output and parsing

    - [Parsers](io/parsers.md)

-   :material-tools: **Utilities**

    ---

    Helper functions and utilities

    - [Logging](utils/logging.md)

</div>

## Quick Links

### Most Used Classes

| Class | Description |
|-------|-------------|
| [`Simulation`](core/simulation.md) | Main class for building FDS simulations |
| [`Results`](analysis/results.md) | Container for simulation results |
| [`FDSRunner`](execution/runner.md) | Execute FDS simulations |
| [`Job`](execution/job.md) | Manage running simulations |
| [`validate_simulation()`](utils/validation.md) | Validate simulation configuration |

### Namelist Classes

| Namelist | Class | Purpose |
|----------|-------|---------|
| HEAD | [`Head`](namelists/metadata.md) | Simulation metadata |
| TIME | [`Time`](namelists/metadata.md) | Time parameters |
| MESH | [`Mesh`](namelists/domain.md) | Computational domain |
| SURF | [`Surface`](namelists/materials.md) | Surface properties |
| OBST | [`Obstruction`](namelists/geometry.md) | Solid obstructions |
| VENT | [`Vent`](namelists/geometry.md) | Boundaries and vents |
| HOLE | [`Hole`](namelists/geometry.md) | Openings in obstructions |
| MULT | [`Multiplier`](namelists/geometry.md) | Array replication |
| GEOM | [`Geometry`](namelists/geometry.md) | Unstructured geometry (beta) |
| MOVE | [`Move`](namelists/geometry.md) | Geometry transformations |
| DEVC | [`Device`](namelists/devices.md) | Measurement devices |
| MATL | [`Material`](namelists/materials.md) | Material definitions |
| RAMP | [`Ramp`](namelists/complex.md) | Time-varying properties |
| REAC | [`Reaction`](namelists/complex.md) | Combustion reactions |
| PART | [`Particle`](namelists/complex.md) | Particle definitions |
| PROP | [`Property`](namelists/devices.md) | Device properties |
| CTRL | [`Control`](namelists/complex.md) | Control logic |
| INIT | [`Initialization`](namelists/complex.md) | Initial conditions |
| SPEC | [`Species`](namelists/species.md) | Chemical species |
| COMB | [`Combustion`](namelists/complex.md) | Combustion parameters |
| HVAC | [`Hvac`](namelists/complex.md) | HVAC systems |
| MISC | [`Misc`](namelists/metadata.md) | Global settings |

## Usage Examples

### Importing Classes

```python
# Import main simulation class
from pyfds import Simulation

# Import specific namelist classes
from pyfds import Mesh, Surface, Device, Time

# Import execution classes
from pyfds.execution import FDSRunner, Job
from pyfds.analysis import Results

# Import validation
from pyfds.validation import validate_simulation, SimulationValidator
from pyfds.exceptions import ValidationError, DuplicateIdError

# Import builders
from pyfds import RampBuilder, MaterialBuilder, SurfaceBuilder
from pyfds.builders.libraries import CommonMaterials, CommonRamps
```

### Type Hints

All classes have full type hints for IDE support:

```python
from pyfds import Simulation
from pyfds.core.namelists import Mesh, Surface

def create_simulation(name: str) -> Simulation:
    """Create a simulation with type safety."""
    sim = Simulation(chid=name)
    sim.add(Time(t_end=600.0))
    return sim

# IDE will autocomplete and type-check
sim: Simulation = create_simulation("test")
```

## Documentation Conventions

### Parameters

Parameters are documented with their type and description:

```python
def time(
    self,
    t_end: float,
    t_begin: float = 0.0,
    dt: float | None = None
) -> Simulation:
    """
    Set time parameters for the simulation.

    Parameters
    ----------
    t_end : float
        End time in seconds
    t_begin : float, optional
        Start time in seconds (default: 0.0)
    dt : float, optional
        Initial time step in seconds (default: auto)

    Returns
    -------
    Simulation
        Self for method chaining
    """
```

### Return Values

Return values include type and description:

```python
Returns
-------
Simulation
    Self for method chaining
```

### Examples

Executable code examples:

```python
Examples
--------
>>> sim = Simulation(chid='test')
>>> sim.add(Time(t_end=600.0, dt=0.1))
>>> sim.write('test.fds')
```

### See Also

Related functions and classes:

```python
See Also
--------
mesh : Define computational domain
surface : Create surface properties
```

## Navigating the API

### By Module

Browse by module organization:

- **[Core](core/simulation.md)** - Main simulation building
- **[Builders](builders/index.md)** - Fluent APIs and libraries
- **[Namelists](namelists/index.md)** - All FDS namelist classes
- **[Validation](utils/validation.md)** - Input validation
- **[Exceptions](exceptions.md)** - Error handling
- **[Execution](execution/runner.md)** - Running simulations
- **[Analysis](analysis/results.md)** - Results and visualization
- **[I/O](io/parsers.md)** - File operations
- **[Utils](utils/logging.md)** - Helper utilities

### By Task

Find what you need based on what you want to do:

| Task | See |
|------|-----|
| Create a simulation | [`Simulation`](core/simulation.md) |
| Add a mesh | [`sim.add(Mesh())`](core/simulation.md) |
| Create fire | [`sim.add(Surface())`](core/simulation.md) |
| Add geometry | [`sim.add(Obstruction())`](core/simulation.md) |
| Add vent | [`sim.add(Vent())`](core/simulation.md) |
| Add device | [`sim.add(Device())`](core/simulation.md) |
| Build complex objects | [`MaterialBuilder`](builders/material.md), [`RampBuilder`](builders/ramp.md) |
| Use pre-configured objects | [`CommonMaterials`](builders/libraries.md), [`CommonRamps`](builders/libraries.md) |
| Validate | [`validate_simulation()`](utils/validation.md) |
| Run simulation | [`run_fds()`](execution/runner.md) or [`sim.run()`](core/simulation.md) |
| Load results | [`Results`](analysis/results.md) |
| Plot data | [`Results.plot_hrr()`](analysis/results.md) |

## Source Code

All API documentation is generated from the source code. To view the source:

```python
import inspect
from pyfds import Simulation

# View source code
print(inspect.getsource(Simulation.add))

# View docstring
print(Simulation.add.__doc__)
```

Or browse the source on [GitHub](https://github.com/GraysonBellamy/pyfds/tree/main/src/pyfds).

---

[Simulation Class →](core/simulation.md){ .md-button .md-button--primary }
[All Namelists →](namelists/index.md){ .md-button }
