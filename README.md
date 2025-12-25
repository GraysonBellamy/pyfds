# PyFDS - Python Interface to FDS

[![Python Version](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

A comprehensive Python library for creating, executing, and analyzing NIST Fire Dynamics Simulator (FDS) simulations programmatically.

## Overview

PyFDS transforms the traditional FDS workflow by providing a Pythonic API for:

- **Creating FDS input files** programmatically using Python objects
- **Validating simulations** before execution with comprehensive checks
- **Automating parametric studies** with ease
- **Executing simulations** locally with process management
- **Analyzing results** using the Python scientific stack (NumPy, Polars, Matplotlib)

## Features

- **Intuitive API** - Create FDS simulations with clean, readable Python code
- **Fluent Builders** - Construct complex objects using chainable builder patterns
- **Manager-Based Architecture** - Organized into specialized managers (Geometry, Material, Physics, etc.)
- **Chemical Species Support** - 36+ predefined species with custom fuel definitions and combustion modeling
- **Automatic Validation** - Catch errors before running simulations
- **Execution Management** - Run simulations with OpenMP/MPI support and real-time monitoring
- **Results Analysis** - Parse and analyze FDS output data (HRR, devices) as Polars DataFrames
- **Type Safety** - Full type hints for IDE support and error prevention

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

### Advanced Fire Simulation

Use the builder API for advanced fire scenarios:

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

## Execution and Analysis

PyFDS supports executing and analyzing FDS simulations directly from Python.

### Running Simulations

```python
from pyfds import Simulation

# ... create simulation ...

# Run simulation (blocks until complete)
# Automatically detects FDS executable
results = sim.run(n_threads=4)

# Analyze results
print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
```

### Non-Blocking Execution

```python
import time

# Start simulation in background
job = sim.run(wait=False, monitor=True)

# Monitor progress
while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
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

# Plot results
results.plot_hrr('hrr_plot.png')
```

## CLI Usage

PyFDS includes a command-line interface for validation and execution of FDS files:

```bash
# Validate an FDS file
pyfds validate my_simulation.fds

# Run an FDS simulation
pyfds run my_simulation.fds --threads 4

# Get help
pyfds --help
```

## Project Structure

```
pyfds/
 src/pyfds/
    analysis/          # Results analysis and plotting
    builders/          # Fluent builders for complex objects
    cli/               # Command-line interface
    core/              # Core simulation classes
       namelists/      # FDS namelist definitions (MESH, SURF, etc.)
       simulation.py   # Main Simulation class
    execution/         # Execution management (Runner, Job)
    io/                # File I/O and parsing
    utils/             # Utility functions
 examples/             # Example scripts
 docs/                 # Documentation
 tests/                # Test suite
```

## Requirements

- Python 3.11+
- NumPy >= 2.3.5
- Polars >= 1.35.2
- Pydantic >= 2.12.4
- Click >= 8.3.1

For full requirements, see [pyproject.toml](pyproject.toml).

## License

This project is licensed under the MIT License - see the LICENSE file for details.

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
