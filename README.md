# PyFDS - Python Interface to FDS

[![Python Version](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

A comprehensive Python library for creating, executing, and analyzing NIST Fire Dynamics Simulator (FDS) simulations programmatically.

## Overview

PyFDS transforms the traditional FDS workflow by providing a Pythonic API for:

- **Creating FDS input files** programmatically using Python objects
- **Validating simulations** before execution with comprehensive checks
- **Automating parametric studies** with ease
- **Integrating with the Python scientific stack** (NumPy, Polars, Matplotlib)

## Features

- **Intuitive API** - Create FDS simulations with clean, readable Python code
- **Manager-Based Architecture** - Organized into specialized managers (Geometry, Material, Physics, etc.)
- **Chemical Species Support** - 36+ predefined species with custom fuel definitions and combustion modeling
- **Automatic Validation** - Catch errors before running simulations
- **Type Safety** - Full type hints for IDE support and error prevention
- **Method Chaining** - Fluid interface for building simulations
- **Comprehensive Testing** - 42+ tests with high coverage
- **Well Documented** - Extensive docstrings and examples

## Installation

### Using uv (recommended)

```bash
uv add pyfds
```

### Using pip

```bash
pip install pyfds
```

### Development Installation

```bash
git clone https://github.com/GraysonBellamy/pyfds.git
cd pyfds
uv sync --extra dev
```

## Quick Start

### Basic Fire Simulation

Create a simple room fire simulation in just a few lines:

```python
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Time, Mesh, Surface, Obstruction, Device

# Create simulation
sim = Simulation(chid='room_fire', title='Simple Room Fire')

# Add components using the unified add() API
sim.add(
    Time(t_end=600.0),
    Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)),
    Surface(id='BURNER', hrrpua=1000.0, color='RED'),
    Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='BURNER'),
    Device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))
)

# Validate and write
sim.write('room_fire.fds')
```

### Advanced Fire Simulation (Stage 1 Features ✨)

Use the new builder API for advanced fire scenarios:

```python
from pyfds import Simulation
from pyfds.builders import SurfaceBuilder, DeviceBuilder, MeshBuilder, ReactionBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Time

# Create simulation
sim = Simulation(chid='advanced_fire', title='Fire with Sprinkler Control')

# Add components using the unified add() API
sim.add(
    Time(t_end=300.0),

    # Mesh with parallel processing
    MeshBuilder()
        .with_bounds(Bounds3D.of(0, 6, 0, 6, 0, 3))
        .with_grid(Grid3D.of(60, 60, 30))
        .build(),

    # Growing fire with ramped heat release
    SurfaceBuilder('FIRE')
        .with_heat_release(500.0, ramp_id='t2_growth')
        .with_ignition(temperature=250.0, burn_away=True)
        .with_radiation(emissivity=0.9)
        .build(),

    # Sprinkler with activation control
    DeviceBuilder('SPRINK1')
        .with_quantity('SPRINKLER_LINK_TEMPERATURE')
        .with_control(setpoint=74.0, trip_direction=1, latch=True)
        .at_point(Point3D.of(3.0, 3.0, 2.8))
        .build(),

    # Statistical temperature monitoring
    DeviceBuilder('AVG_TEMP')
        .with_quantity('TEMPERATURE')
        .with_statistics('MEAN', start_time=10.0)
        .in_bounds(Bounds3D.of(0, 6, 0, 6, 2.0, 3.0))
        .build(),

    # Advanced combustion with suppression
    ReactionBuilder()
        .fuel('PROPANE')
        .with_extinction('EXTINCTION 1', critical_temp=1200.0)
        .with_suppression(k_suppression=0.3)
        .radiative_fraction(0.35)
        .build()
)

# Write FDS input file
sim.write('advanced_fire.fds')
```

## Phase 2 Features: Execution and Analysis

PyFDS now supports executing and analyzing FDS simulations directly from Python!

### Running Simulations

```python
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D

# Create simulation
sim = Simulation(chid='room_fire', title='Room Fire')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))
sim.surface(id='FIRE', hrrpua=1000.0)
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 2.4))

# Run simulation (blocks until complete)
results = sim.run(n_threads=4)

# Analyze results
print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
temp_data = results.devices['TEMP']
results.plot_hrr('hrr.png')
```

### Non-Blocking Execution with Progress Monitoring

```python
import time

# Start simulation in background
job = sim.run(wait=False, monitor=True)

# Monitor progress
while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
    if job.estimated_time_remaining:
        print(f"ETA: {job.estimated_time_remaining:.0f}s")
    time.sleep(5)

# Get results when done
results = job.get_results()
```

### Analyzing Results

```python
from pyfds import Results

# Load results from completed simulation
results = Results(chid='room_fire', output_dir='./outputs')

# Access data as Polars DataFrames
hrr_df = results.hrr
device_df = results.devc

# Get specific device data
temp_data = results.get_device('TEMP_1')
max_temp = temp_data['TEMP_1'].max()

# List available devices
devices = results.list_devices()

# Generate summary statistics
summary = results.summary()
print(f"Peak HRR: {summary['peak_hrr']:.1f} kW")
print(f"Duration: {summary['duration']:.1f} s")

# Plot results
results.plot_hrr('hrr_plot.png')
results.plot_device('TEMP_1', 'temp_plot.png')
```

### Parallel Execution

PyFDS automatically validates parallel configuration and provides helpful warnings to optimize your simulations.

```python
# OpenMP threading (single machine, single mesh)
results = sim.run(n_threads=8)

# MPI processes (multi-mesh simulations)
results = sim.run(n_mpi=4)  # Validates mesh count matches MPI processes

# Combine MPI and OpenMP
results = sim.run(n_mpi=2, n_threads=4)  # 2 MPI ranks, 4 threads each

# Get optimal configuration recommendation
from pyfds.validation import ExecutionValidator
validator = ExecutionValidator()
config = validator.suggest_config(sim)
print(f"Recommended: {config['n_mpi']} MPI, {config['n_threads']} threads")
print(f"Rationale: {config['rationale']}")
```

### Graceful Stopping

Request FDS to stop gracefully during execution:

```python
# Start simulation in background
job = sim.run(wait=False)

# Monitor progress
while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
    if some_condition:
        job.request_stop()  # Creates CHID.stop file for graceful shutdown
        break

results = job.get_results()
```

## Usage Examples

### Basic Fire Simulation

```python
from pyfds import Simulation
from pyfds.builders import SurfaceBuilder, DeviceBuilder, MeshBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Time

sim = Simulation(chid='room_fire', title='Room Fire Test')
sim.add(
    Time(t_end=600.0, dt=0.1),
    MeshBuilder()
        .with_bounds(Bounds3D.of(0, 5, 0, 5, 0, 2.5))
        .with_grid(Grid3D.of(50, 50, 25))
        .build(),
    SurfaceBuilder('BURNER')
        .with_heat_release(1000.0)
        .with_color('RED')
        .build(),
    Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='BURNER'),
    DeviceBuilder('TEMP')
        .with_quantity('TEMPERATURE')
        .at_point(Point3D.of(2.5, 2.5, 2.4))
        .build()
)

# Write FDS input file
sim.write('room_fire.fds')
```

### Method Chaining

```python
from pyfds import Simulation
from pyfds.builders import SurfaceBuilder, DeviceBuilder, MeshBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Time

sim = (Simulation(chid='fire', title='My Fire Simulation')
       .add(Time(t_end=600.0))
       .add(MeshBuilder()
            .with_bounds(Bounds3D.of(0, 5, 0, 5, 0, 2.5))
            .with_grid(Grid3D.of(50, 50, 25))
            .build())
       .add(SurfaceBuilder('FIRE')
            .with_heat_release(1000.0)
            .build())
       .add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE'))
       .add(DeviceBuilder('TEMP1')
            .with_quantity('TEMPERATURE')
            .at_point(Point3D.of(2.5, 2.5, 2.4))
            .build()))

sim.write('fire.fds')
```

### Parametric Studies

```python
from pyfds import Simulation
from pyfds.builders import SurfaceBuilder, DeviceBuilder, MeshBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Time

# Study effect of heat release rate
hrr_values = [500, 1000, 1500, 2000]

for hrr in hrr_values:
    sim = Simulation(chid=f'fire_{hrr}')
    sim.add(
        Time(t_end=300.0),
        MeshBuilder()
            .with_bounds(Bounds3D.of(0, 3, 0, 3, 0, 1.5))
            .with_grid(Grid3D.of(30, 30, 15))
            .build(),
        SurfaceBuilder('FIRE')
            .with_heat_release(float(hrr))
            .build(),
        Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE'),
        DeviceBuilder('TEMP')
            .with_quantity('TEMPERATURE')
            .at_point(Point3D.of(1.5, 1.5, 1.4))
            .build()
    )

    sim.write(f'fire_{hrr}.fds')
```

### Validation

```python
from pyfds import Simulation
from pyfds.builders import MeshBuilder
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Time

sim = Simulation(chid='test')
sim.add(
    Time(t_end=100.0),
    MeshBuilder()
        .with_bounds(Bounds3D.of(0, 1, 0, 1, 0, 1))
        .with_grid(Grid3D.of(10, 10, 10))
        .build()
)

# Validate before running
warnings = sim.validate()
if warnings:
    for w in warnings:
        print(f"Warning: {w}")
```

## CLI Usage

PyFDS includes a command-line interface for validation and execution:

```bash
# Validate a simulation file
pyfds validate my_simulation.py

# Run a simulation file directly
pyfds run my_simulation.py

# Get help
pyfds --help
```

The CLI provides immediate feedback on validation errors and supports running simulations with proper error handling.

## API Reference

### Core Classes

#### Simulation
Main orchestrator class for building FDS simulations using specialized managers.

**Manager Properties:**
- `materials` - MaterialManager (materials, surfaces)
- `species` - SpeciesManager (gas species, reactions)
- `geometry` - GeometryManager (meshes, obstructions, vents)
- `devices` - InstrumentationManager (devices, props)
- `controls` - ControlManager (controls, initial conditions)
- `ramps` - RampManager (time-varying functions)
- `physics` - PhysicsManager (reactions, misc parameters)

**Convenience Methods:**
- `time(t_end, t_begin=None, dt=None)` - Set time parameters
- `mesh(ijk, xb, id=None)` - Add computational mesh
- `surface(id, **kwargs)` - Define surface properties
- `obstruction(xb, surf_id=None, **kwargs)` - Add obstruction
- `device(id, quantity, xyz=None, xb=None)` - Add measurement device
- `add_ramp(ramp)` - Add time-varying function
- `validate()` - Validate simulation configuration
- `write(filename)` - Write FDS input file
- `to_fds()` - Generate FDS file content

**Manager Access Examples:**
```python
# Access components via managers
num_meshes = len(sim.geometry.meshes)
num_surfaces = len(sim.materials.surfaces)
num_species = len(sim.species.species)
num_devices = len(sim.devices.devices)
num_ramps = len(sim.ramps.ramps)

# Iterate over components
for mesh in sim.geometry.meshes:
    print(f"Mesh: {mesh.ijk}")

for ramp in sim.ramps.ramps:
    print(f"Ramp {ramp.id}: {len(ramp.points)} points")
```

#### Namelist Classes

- **Head** - Simulation metadata
- **Time** - Time control parameters
- **Mesh** - Computational domain
- **Surface** - Surface properties
- **Obstruction** - Solid objects
- **Device** - Measurement devices

### Validation

The `Validator` class provides comprehensive validation:

```python
from pyfds.core import Validator

validator = Validator()
is_valid = validator.validate_simulation(sim)

if not is_valid:
    for error in validator.get_errors():
        print(f"Error: {error}")

for warning in validator.get_warnings():
    print(f"Warning: {warning}")
```

## Examples

See the `examples/` directory for complete examples:

### Stage 1 Examples (NEW! ✨)
- [`01_simple_room_fire.py`](examples/01_simple_room_fire.py) - Basic fire with heat release and monitoring
- [`02_fire_with_sprinkler.py`](examples/02_fire_with_sprinkler.py) - Growing fire with sprinkler control and suppression
- [`03_multi_mesh_parallel.py`](examples/03_multi_mesh_parallel.py) - Multi-mesh parallel simulation with MPI
- [`04_advanced_combustion.py`](examples/04_advanced_combustion.py) - Advanced combustion with extinction and species tracking

### Foundation Examples
- [`basic_room_fire.py`](examples/basic_room_fire.py) - Simple room fire simulation
- [`parametric_study.py`](examples/parametric_study.py) - HRR sensitivity study
- [`advanced_room.py`](examples/advanced_room.py) - Complex geometry with multiple surfaces
- [`execution_demo.py`](examples/execution_demo.py) - Complete execution and analysis workflow

## Testing

Run the test suite:

```bash
# Run all tests
uv run pytest

# Run with coverage
uv run pytest --cov=pyfds --cov-report=html

# Run specific test file
uv run pytest tests/unit/test_simulation.py -v
```

## Development

### Project Structure

```
pyfds/
 src/pyfds/
    core/              # Core simulation classes
       namelist.py    # FDS namelist definitions
       simulation.py  # Main Simulation class
       validator.py   # Validation logic
    execution/         # Execution management (Phase 2)
    io/                # File I/O and parsing (Phase 2)
    analysis/          # Results analysis (Phase 3)
    utils/             # Utility functions
    tests/             # Test suite
        unit/          # Unit tests
        integration/   # Integration tests
 examples/              # Example scripts
 docs/                  # Documentation
```

### Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`uv run pytest`)
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## Implementation Status

### Phase 1: Foundation ✅ (Complete)
- [x] Project structure and dependencies
- [x] Basic namelist classes (HEAD, TIME, MESH, SURF, OBST, DEVC)
- [x] FDS file writer
- [x] Validation framework
- [x] Unit and integration tests
- [x] Documentation and examples

### Phase 2: Execution and Analysis ✅ (Complete)
- [x] Local execution runner with process management
- [x] CSV output parser for device and HRR data
- [x] Real-time progress monitoring
- [x] Results container with Polars DataFrames
- [x] Non-blocking execution with callbacks
- [x] OpenMP and MPI support
- [x] Built-in plotting utilities
- [x] Comprehensive test coverage (100 tests)

### Phase 3: Architecture Refinement ✅ (Complete)
- [x] Unified FDS output generation through single `Simulation.to_fds()` path
- [x] Fixed namelist output order to match FDS User Guide convention
- [x] Standardized manager property names (material_mgr → materials, etc.)
- [x] Removed local storage workarounds from managers
- [x] Eliminated legacy registry compatibility code
- [x] Renamed confusing validation files (validation.py → input_validators.py)
- [x] Organized surface mixins in dedicated subdirectory
- [x] Comprehensive test suite validation (743/743 tests passing)

### Phase 4: Polish & Documentation ✅ (Complete)
- [x] Audited all 8 builders (ControlBuilder, GeomBuilder, HoleBuilder, MeshBuilder, MoveBuilder, MultBuilder, PartBuilder, PropBuilder)
- [x] Updated README.md to reflect refactored architecture
- [x] Updated API documentation with new manager property names
- [x] Final integration testing and validation
- [x] Code review and final cleanup

### Stage 2: Particle Systems (Planned)
- [ ] PART namelist for particle tracking
- [ ] PROP namelist for device properties
- [ ] Enhanced SURF for particle generation
- [ ] Sprinkler and spray systems

### Stage 3: Vegetation and HVAC (Planned)
- [ ] Vegetation parameters for wildfire modeling
- [ ] MULT namelist for geometry replication
- [ ] GEOM namelist for complex geometry
- [ ] HVAC namelist for HVAC systems

### Stage 4: Advanced Features (Planned)
- [ ] Finite-rate chemistry
- [ ] CTRL namelist for control functions
- [ ] INIT namelist for initial conditions
- [ ] Advanced radiation modeling

## Requirements

- Python 3.11+
- NumPy >= 1.20.0
- Polars >= 0.19.0
- Pydantic >= 2.0.0
- Click >= 8.0.0

For full requirements, see [pyproject.toml](pyproject.toml).

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- NIST Fire Research Division for developing FDS
- The FDS community for documentation and examples
- Implementation based on the comprehensive [FDS Python Connector Implementation Plan](FDS_Python_Connector_Implementation_Plan.md)

## References

- [FDS Official Website](https://pages.nist.gov/fds-smv/)
- [FDS User Guide](https://pages.nist.gov/fds-smv/manuals.html)
- [FDS GitHub Repository](https://github.com/firemodels/fds)

## Support

- 📚 [Documentation](docs/)
- 💬 [Discussions](https://github.com/GraysonBellamy/pyfds/discussions)
- 🐛 [Issue Tracker](https://github.com/GraysonBellamy/pyfds/issues)

## Citation

If you use PyFDS in your research, please cite:

```bibtex
@software{pyfds2025,
  title = {PyFDS: Python Interface to Fire Dynamics Simulator},
  author = {Bellamy, Grayson},
  year = {2025},
  url = {https://github.com/GraysonBellamy/pyfds}
}
```
