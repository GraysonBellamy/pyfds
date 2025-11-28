# API Reference

Complete API documentation for all PyFDS classes and functions.

## Overview

The API Reference provides detailed technical documentation auto-generated from the source code docstrings. All public APIs are documented with parameters, return values, examples, and type information.

## Modules

<div class="grid cards" markdown>

-   :material-cog-outline: **Core**

    ---

    Main simulation and validation classes

    - [Simulation](core/simulation.md)
    - [Validator](core/validator.md)

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

-   :material-play-circle-outline: **Execution**

    ---

    Simulation execution and job management

    - [Runner](execution/runner.md)
    - [Job](execution/job.md)
    - [Monitor](execution/monitor.md)
    - [Exceptions](execution/exceptions.md)

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
    - [Validation](utils/validation.md)

</div>

## Quick Links

### Most Used Classes

| Class | Description |
|-------|-------------|
| [`Simulation`](core/simulation.md) | Main class for building FDS simulations |
| [`Results`](analysis/results.md) | Container for simulation results |
| [`FDSRunner`](execution/runner.md) | Execute FDS simulations |
| [`Job`](execution/job.md) | Manage running simulations |
| [`Validator`](core/validator.md) | Validate simulation configuration |

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
| MULT | [`Mult`](namelists/geometry.md) | Array replication |
| GEOM | [`Geom`](namelists/geometry.md) | Unstructured geometry (beta) |
| MOVE | [`Move`](namelists/geometry.md) | Geometry transformations |
| DEVC | [`Device`](namelists/devices.md) | Measurement devices |
| MATL | [`Material`](namelists/materials.md) | Material definitions |
| RAMP | [`Ramp`](namelists/complex.md) | Time-varying properties |
| REAC | [`Reaction`](namelists/complex.md) | Combustion reactions |
| PROP | [`Prop`](namelists/devices.md) | Device properties |
| CTRL | [`Ctrl`](namelists/complex.md) | Control logic |
| INIT | [`Init`](namelists/complex.md) | Initial conditions |
| MISC | [`Misc`](namelists/metadata.md) | Global settings |

## Usage Examples

### Importing Classes

```python
# Import main simulation class
from pyfds import Simulation

# Import specific namelist classes
from pyfds import Mesh, Surface, Device

# Import execution classes
from pyfds import FDSRunner, Job, Results

# Import validators
from pyfds import Validator, ValidationError
```

### Type Hints

All classes have full type hints for IDE support:

```python
from pyfds import Simulation
from pyfds.core.namelists import Mesh, Surface

def create_simulation(name: str) -> Simulation:
    """Create a simulation with type safety."""
    sim = Simulation(chid=name)
    sim.time(t_end=600.0)
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
>>> sim.time(t_end=600.0, dt=0.1)
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
- **[Namelists](namelists/index.md)** - All FDS namelist classes
- **[Execution](execution/runner.md)** - Running simulations
- **[Analysis](analysis/results.md)** - Results and visualization
- **[I/O](io/parsers.md)** - File operations
- **[Utils](utils/logging.md)** - Helper utilities

### By Task

Find what you need based on what you want to do:

| Task | See |
|------|-----|
| Create a simulation | [`Simulation`](core/simulation.md) |
| Add a mesh | [`Simulation.mesh()`](core/simulation.md) |
| Create fire | [`Simulation.surface()`](core/simulation.md) |
| Add geometry | [`Simulation.obstruction()`](core/simulation.md) |
| Add vent | [`Simulation.vent()`](core/simulation.md) |
| Add device | [`Simulation.device()`](core/simulation.md) |
| Validate | [`Validator`](core/validator.md) |
| Run simulation | [`Simulation.run()`](core/simulation.md) |
| Load results | [`Results`](analysis/results.md) |
| Plot data | [`Results.plot_hrr()`](analysis/results.md) |

## Source Code

All API documentation is generated from the source code. To view the source:

```python
import inspect
from pyfds import Simulation

# View source code
print(inspect.getsource(Simulation.mesh))

# View docstring
print(Simulation.mesh.__doc__)
```

Or browse the source on [GitHub](https://github.com/GraysonBellamy/pyfds/tree/main/src/pyfds).

---

[Simulation Class →](core/simulation.md){ .md-button .md-button--primary }
[All Namelists →](namelists/index.md){ .md-button }
