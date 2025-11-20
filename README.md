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
- **Automatic Validation** - Catch errors before running simulations
- **Type Safety** - Full type hints for IDE support and error prevention
- **Method Chaining** - Fluid interface for building simulations
- **Comprehensive Testing** - 62 tests with >90% coverage
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

Create a simple room fire simulation in just a few lines:

```python
from pyfds import Simulation

# Create simulation
sim = Simulation(chid='room_fire', title='Simple Room Fire')

# Set time parameters
sim.time(t_end=600.0)

# Define computational domain (5m x 5m x 2.5m)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Create fire surface (1000 kW/m²)
sim.surface(id='BURNER', hrrpua=1000.0, color='RED')

# Add fire source (1m x 1m burner)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='BURNER')

# Add temperature measurement at ceiling
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE',
           xyz=(2.5, 2.5, 2.4))

# Validate and write
sim.write('room_fire.fds')
```

## Phase 2 Features: Execution and Analysis

PyFDS now supports executing and analyzing FDS simulations directly from Python!

### Running Simulations

```python
from pyfds import Simulation

# Create simulation
sim = Simulation(chid='room_fire', title='Room Fire')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))

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

```python
# OpenMP threading (single machine)
results = sim.run(n_threads=8)

# MPI processes (multi-core or cluster)
results = sim.run(n_mpi=4)

# Combine MPI and OpenMP
results = sim.run(n_mpi=2, n_threads=4)  # 2 MPI ranks, 4 threads each
```

## Usage Examples

### Basic Fire Simulation

```python
from pyfds import Simulation

sim = Simulation(chid='room_fire', title='Room Fire Test')
sim.time(t_end=600.0, dt=0.1)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
sim.surface(id='BURNER', hrrpua=1000.0, color='RED')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='BURNER')
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))

# Write FDS input file
sim.write('room_fire.fds')
```

### Method Chaining

```python
from pyfds import Simulation

sim = (Simulation(chid='fire', title='My Fire Simulation')
       .time(t_end=600.0)
       .mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
       .surface(id='FIRE', hrrpua=1000.0)
       .obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
       .device(id='TEMP1', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4)))

sim.write('fire.fds')
```

### Parametric Studies

```python
from pyfds import Simulation

# Study effect of heat release rate
hrr_values = [500, 1000, 1500, 2000]

for hrr in hrr_values:
    sim = Simulation(chid=f'fire_{hrr}')
    sim.time(t_end=300.0)
    sim.mesh(ijk=(30, 30, 15), xb=(0, 3, 0, 3, 0, 1.5))
    sim.surface(id='FIRE', hrrpua=float(hrr))
    sim.obstruction(xb=(1, 2, 1, 2, 0, 0.1), surf_id='FIRE')
    sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(1.5, 1.5, 1.4))

    sim.write(f'fire_{hrr}.fds')
```

### Validation

```python
from pyfds import Simulation

sim = Simulation(chid='test')
sim.time(t_end=100.0)
sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

# Validate before running
warnings = sim.validate()
if warnings:
    for w in warnings:
        print(f"Warning: {w}")
```

## API Reference

### Core Classes

#### Simulation
Main class for building FDS simulations.

**Methods:**
- `time(t_end, t_begin=None, dt=None)` - Set time parameters
- `mesh(ijk, xb, id=None)` - Add computational mesh
- `surface(id, **kwargs)` - Define surface properties
- `obstruction(xb, surf_id=None, **kwargs)` - Add obstruction
- `device(id, quantity, xyz=None, xb=None)` - Add measurement device
- `validate()` - Validate simulation configuration
- `write(filename)` - Write FDS input file
- `to_fds()` - Generate FDS file content

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

- [`basic_room_fire.py`](examples/basic_room_fire.py) - Simple room fire simulation
- [`parametric_study.py`](examples/parametric_study.py) - HRR sensitivity study
- [`advanced_room.py`](examples/advanced_room.py) - Complex geometry with multiple surfaces
- [`execution_demo.py`](examples/execution_demo.py) - **NEW!** Complete execution and analysis workflow

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

### Phase 1: Foundation  (Complete)
- [x] Project structure and dependencies
- [x] Basic namelist classes (HEAD, TIME, MESH, SURF, OBST, DEVC)
- [x] FDS file writer
- [x] Validation framework
- [x] Unit and integration tests
- [x] Documentation and examples

### Phase 2: Core Features ✅ (Complete)
- [x] Local execution runner with process management
- [x] CSV output parser for device and HRR data
- [x] Real-time progress monitoring
- [x] Results container with Polars DataFrames
- [x] Non-blocking execution with callbacks
- [x] OpenMP and MPI support
- [x] Built-in plotting utilities
- [x] Comprehensive test coverage (100 tests)

### Phase 3: Advanced Features (Planned)
- [ ] HPC cluster execution support
- [ ] Binary output parsers
- [ ] Visualization utilities
- [ ] Parametric study framework

### Phase 4: Polish and Release (Planned)
- [ ] Complete documentation
- [ ] Example notebooks
- [ ] CI/CD pipeline
- [ ] PyPI release

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
@software{pyfds2024,
  title = {PyFDS: Python Interface to Fire Dynamics Simulator},
  author = {Bellamy, Grayson},
  year = {2024},
  url = {https://github.com/GraysonBellamy/pyfds}
}
```
