Python FDS Connector Implementation Plan

A Comprehensive Guide for Building a Python Interface to NIST Fire Dynamics Simulator

Document Version: 1.0
Date: 2025-11-19
Purpose: Implementation blueprint for AI-assisted development

# Table of Contents

- 1\. Executive Summary
- 2\. System Overview
- 3\. Architecture Design
- 4\. Core Components
- 5\. Implementation Phases
- 6\. Technical Specifications
- 7\. API Design
- 8\. Testing Strategy
- 9\. Documentation Requirements
- 10\. Deliverables

# 1\. Executive Summary

## 1.1 Project Overview

This document outlines the complete implementation plan for PyFDS, a Python connector library that provides a programmatic interface to the NIST Fire Dynamics Simulator (FDS). The library will enable Python developers to create, execute, and analyze FDS simulations without manual file manipulation, significantly streamlining CFD fire modeling workflows.

## 1.2 Key Objectives

- Provide a Pythonic API for creating FDS input files programmatically
- Automate FDS simulation execution and process management
- Parse and analyze FDS output files with polars DataFrames
- Enable batch processing and parametric studies
- Integrate with scientific Python ecosystem (NumPy, Polars, Matplotlib)
- Support both local and cluster/HPC execution environments

## 1.3 Scope

The connector will support FDS version 6.9+ and focus on the most commonly used features. Advanced features like custom reaction mechanisms and complex geometries will be added in subsequent versions based on user feedback.

# 2\. System Overview

## 2.1 FDS Background

Fire Dynamics Simulator (FDS) is a computational fluid dynamics (CFD) model developed by the National Institute of Standards and Technology (NIST). It numerically solves a form of the Navier-Stokes equations appropriate for low-speed, thermally-driven flow with an emphasis on smoke and heat transport from fires.

## 2.2 Current FDS Workflow

The traditional FDS workflow involves:

- Manually creating text-based input files (.fds) using Fortran namelist format
- Running FDS executable from command line
- Waiting for simulation completion
- Processing multiple output files (.csv, .out, .smv, etc.)
- Using Smokeview for visualization or custom scripts for data analysis

## 2.3 Proposed Python Connector Workflow

The Python connector will transform this into:

- Define simulation using Python objects and methods
- Automatically generate and validate FDS input files
- Execute simulations with progress monitoring
- Load results directly into polars DataFrames
- Analyze and visualize using Python scientific stack

# 3\. Architecture Design

## 3.1 High-Level Architecture

The PyFDS connector follows a modular, layered architecture with clear separation of concerns:

| Layer | Components | Responsibilities |
| --- | --- | --- |
| API Layer | FDSSimulation, Mesh, Device classes | User-facing Python API |
| Core Layer | NamelistBuilder, Validator, Parser | FDS file generation and parsing |
| Execution Layer | Runner, ProcessManager, Monitor | Simulation execution and monitoring |
| I/O Layer | FileHandler, OutputReader, DataConverter | File operations and data conversion |
| Utility Layer | Logger, Config, Helpers | Supporting utilities and helpers |

## 3.2 Design Patterns

- Builder Pattern: For constructing complex FDS input files
- Factory Pattern: For creating different types of output parsers
- Observer Pattern: For monitoring simulation progress
- Strategy Pattern: For different execution backends (local/cluster)
- Facade Pattern: For simplified high-level API

# 4\. Core Components

## 4.1 Input Generation Module

### 4.1.1 Namelist Groups

The module will support all major FDS namelist groups with Python classes:

\# Example usage
from pyfds import Simulation, Mesh, Surface, Obstruction
<br/>sim = Simulation(chid='example_fire', title='Office Fire Simulation')
sim.time(t_end=300.0, dt=0.1)
<br/>mesh = Mesh(
ijk=(100, 100, 50),
xb=(0, 10, 0, 10, 0, 5)
)
sim.add_mesh(mesh)
<br/>fire_surf = Surface(
id='FIRE',
hrrpua=1000.0,
color='RED'
)
sim.add_surface(fire_surf)
<br/>burner = Obstruction(
xb=(4, 6, 4, 6, 0, 0.5),
surf_ids={'top': 'FIRE'}
)
sim.add_obstruction(burner)

### 4.1.2 Supported Namelist Groups

- &HEAD - Simulation metadata
- &TIME - Time parameters
- &MESH - Computational domain
- &REAC - Reaction parameters
- &SURF - Surface properties
- &OBST - Obstructions
- &VENT - Vents and openings
- &DEVC - Measurement devices
- &SLCF - Slice files
- &BNDF - Boundary files
- &CTRL - Control logic
- &PROP - Device properties
- &MATL - Material properties
- &RAMP - Time-dependent functions
- &INIT - Initial conditions

## 4.2 Validation Module

The validation module ensures FDS input files are syntactically and semantically correct:

- Syntax validation for namelist format
- Parameter type checking and range validation
- Cross-reference validation (e.g., SURF_ID references)
- Mesh alignment and overlap checking
- Physical reasonableness checks (e.g., temperature ranges)
- Warning for deprecated parameters
- Suggestions for common errors

## 4.3 Execution Module

### 4.3.1 Local Execution

Support for running FDS simulations on local machines with process management:

\# Local execution example
from pyfds import Runner
<br/>runner = Runner()
job = runner.run_simulation(
'example.fds',
n_threads=4,
n_mpi=2,
monitor=True
)
<br/>\# Monitor progress
while job.is_running():
print(f"Progress: {job.progress}%")
print(f"ETA: {job.estimated_time_remaining}")
time.sleep(10)
<br/>results = job.get_results()

### 4.3.2 Cluster/HPC Execution

Support for submitting jobs to HPC clusters using standard schedulers:

- SLURM support with sbatch templates
- PBS/Torque support
- LSF support
- Custom scheduler adapters
- Automatic job script generation
- Queue status monitoring

## 4.4 Output Processing Module

### 4.4.1 File Parsers

Specialized parsers for each FDS output file type:

| File Type | Description | Python Output |
| --- | --- | --- |
| \*.csv | Device output data | polars DataFrame |
| \*\_hrr.csv | Heat release rate data | polars DataFrame |
| \*.out | Diagnostic output | Parsed dictionary |
| \*.smv | Smokeview data | Metadata object |
| \*.sf | Surface data files | numpy arrays |
| \*.q | Quantity files | numpy arrays |
| \*.s3d | 3D slice data | xarray Dataset |
| \*.prt5d | Particle data | polars DataFrame |

### 4.4.2 Data Access API

\# Output processing example
from pyfds import Results
<br/>results = Results('example_fire')
<br/>\# Access device data
temps = results.devices\['TEMP_SENSOR_1'\]
temps.plot()
<br/>\# Get HRR data
hrr = results.hrr
print(f"Peak HRR: {hrr\['HRR'\].max()} kW")
<br/>\# Load slice data
temp_slice = results.slices\['TEMPERATURE'\]
temp_slice.animate(save_as='temperature.mp4')

# 5\. Implementation Phases

## 5.1 Phase 1: Foundation (Weeks 1-3)

- Set up project structure and development environment
- Implement basic namelist classes (&HEAD, &TIME, &MESH)
- Create FDS file writer with proper formatting
- Develop basic validation framework
- Write unit tests for core functionality
- Create initial documentation structure

## 5.2 Phase 2: Core Features (Weeks 4-6)

- Implement remaining essential namelist groups (&SURF, &OBST, &VENT, &DEVC)
- Build local execution runner with process management
- Create CSV output parser for device and HRR data
- Develop progress monitoring system
- Add integration tests
- Write user guide for basic usage

## 5.3 Phase 3: Advanced Features (Weeks 7-9)

- Add support for complex namelist groups (&MATL, &REAC, &RAMP)
- Implement HPC cluster execution support
- Create parsers for binary output files
- Develop visualization utilities
- Build parametric study framework
- Add performance benchmarks

## 5.4 Phase 4: Polish and Release (Weeks 10-12)

- Complete comprehensive test suite
- Perform code review and refactoring
- Create example notebooks and tutorials
- Write API reference documentation
- Set up CI/CD pipeline
- Prepare for PyPI release
- Create Docker container for easy deployment

# 6\. Technical Specifications

## 6.1 Dependencies

Core dependencies:

- Python 3.8+: Minimum Python version
- NumPy: Numerical operations and array handling
- Polars: Data manipulation and CSV processing
- Matplotlib: Basic plotting capabilities
- h5py: HDF5 file support for binary outputs
- xarray: Multi-dimensional data handling
- pydantic: Data validation and settings management
- click: Command-line interface
- tqdm: Progress bar display
- pytest: Testing framework

## 6.2 File Structure

pyfds/
├── \__init_\_.py
├── core/
│ ├── \__init_\_.py
│ ├── simulation.py # Main Simulation class
│ ├── namelist.py # Namelist group classes
│ ├── validator.py # Input validation
│ └── writer.py # FDS file writer
├── execution/
│ ├── \__init_\_.py
│ ├── runner.py # Execution management
│ ├── monitor.py # Progress monitoring
│ └── schedulers/ # HPC scheduler support
├── io/
│ ├── \__init_\_.py
│ ├── parsers/ # Output file parsers
│ ├── readers.py # File reading utilities
│ └── converters.py # Data conversion
├── analysis/
│ ├── \__init_\_.py
│ ├── results.py # Results container
│ ├── visualization.py # Plotting utilities
│ └── statistics.py # Statistical analysis
├── utils/
│ ├── \__init_\_.py
│ ├── config.py # Configuration management
│ ├── logger.py # Logging setup
│ └── helpers.py # Helper functions
├── cli/
│ └── main.py # Command-line interface
└── tests/
├── unit/
├── integration/
└── fixtures/

# 7\. API Design

## 7.1 Design Principles

- Intuitive and Pythonic: Follow Python conventions and idioms
- Progressive disclosure: Simple tasks simple, complex tasks possible
- Fail fast with clear errors: Validate early and provide helpful messages
- Immutable by default: Prevent accidental modifications
- Comprehensive documentation: Every public method documented
- Type hints throughout: Full typing support for IDE assistance

## 7.2 Core API Examples

### 7.2.1 Basic Fire Simulation

from pyfds import Simulation
<br/>\# Create simulation
sim = Simulation(chid='room_fire', title='Room Fire Test')
sim.time(t_end=600.0)
<br/>\# Define computational domain
sim.mesh(
ijk=(50, 50, 25),
xb=(0, 5, 0, 5, 0, 2.5)
)
<br/>\# Create fire surface
sim.surface(
id='BURNER',
hrrpua=1000.0, # kW/m²
color='RED'
)
<br/>\# Add fire source
sim.obstruction(
xb=(2, 3, 2, 3, 0, 0.1),
surf_id_top='BURNER'
)
<br/>\# Add measurement device
sim.device(
id='TEMP_CEILING',
quantity='TEMPERATURE',
xyz=(2.5, 2.5, 2.4)
)
<br/>\# Generate and run
sim.write('room_fire.fds')
results = sim.run(n_threads=4)
<br/>\# Analyze results
print(f"Max temperature: {results.devices\['TEMP_CEILING'\].max()}")

### 7.2.2 Parametric Study

from pyfds import ParametricStudy
<br/>study = ParametricStudy('hrr_sensitivity')
<br/>\# Define parameter ranges
study.add_parameter('hrrpua', \[500, 1000, 1500, 2000\])
study.add_parameter('ventilation', \['closed', 'open'\])
<br/>\# Define base simulation
def create_simulation(hrrpua, ventilation):
sim = Simulation(chid=f'fire_{hrrpua}\_{ventilation}')
sim.time(t_end=300.0)
<br/>\# ... simulation setup ...
<br/>if ventilation == 'open':
sim.vent(xb=(5, 5, 1, 4, 0, 2), surf_id='OPEN')
<br/>return sim
<br/>study.set_simulation_builder(create_simulation)
<br/>\# Run all combinations
results = study.run(parallel=True, max_workers=8)
<br/>\# Analyze
results.plot_sensitivity('hrrpua', 'peak_hrr')
results.to_csv('sensitivity_results.csv')

# 8\. Testing Strategy

## 8.1 Test Coverage Goals

Target: >90% code coverage with comprehensive test suite

## 8.2 Test Categories

| Type | Purpose | Tools |
| --- | --- | --- |
| Unit Tests | Individual component testing | pytest, mock |
| Integration Tests | Module interaction testing | pytest, fixtures |
| End-to-End Tests | Complete workflow validation | Real FDS runs |
| Performance Tests | Execution speed and memory usage | pytest-benchmark |
| Regression Tests | Prevent feature breakage | pytest, CI/CD |

## 8.3 Test Examples

\# Unit test example
def test_mesh_validation():
"""Test mesh parameter validation"""
mesh = Mesh()
<br/>\# Valid mesh
mesh.set_dimensions(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
assert mesh.is_valid()
<br/>\# Invalid mesh (negative dimensions)
with pytest.raises(ValidationError):
mesh.set_dimensions(ijk=(-10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
<br/>\# Integration test example
def test_simulation_workflow(tmp_path):
"""Test complete simulation workflow"""
sim = Simulation(chid='test')
sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
<br/>fds_file = tmp_path / 'test.fds'
sim.write(fds_file)
<br/>assert fds_file.exists()
content = fds_file.read_text()
assert '&MESH' in content
assert 'IJK=10,10,10' in content

# 9\. Documentation Requirements

## 9.1 Documentation Types

- API Reference: Auto-generated from docstrings using Sphinx
- User Guide: Step-by-step tutorials and examples
- Developer Guide: Architecture, contributing guidelines
- Example Gallery: Jupyter notebooks with real-world cases
- FAQ: Common issues and solutions
- Changelog: Version history and migration guides

## 9.2 Documentation Standards

All public APIs must include:

- Clear description of purpose
- Parameter descriptions with types
- Return value documentation
- Usage examples
- Notes on edge cases
- Links to related functions

## 9.3 Example Docstring

def add_device(self, id: str, quantity: str, xyz: Tuple\[float, float, float\],
\*\*kwargs) -> Device:
"""
Add a measurement device to the simulation.
<br/>Parameters
\----------
id : str
Unique identifier for the device
quantity : str
FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY')
xyz : tuple of float
Device location coordinates (x, y, z) in meters
\*\*kwargs
Additional FDS device parameters
<br/>Returns
\-------
Device
The created device object
<br/>Examples
\--------
\>>> sim = Simulation('test')
\>>> sim.add_device('TEMP1', 'TEMPERATURE', (1.0, 1.0, 2.0))
\>>> sim.add_device('VEL1', 'VELOCITY', (2.0, 2.0, 1.0),
... orientation=(0, 0, 1))
<br/>Notes
\-----
Device IDs must be unique within the simulation.
<br/>See Also
\--------
add_device_array : Add multiple devices in a grid pattern
"""

# 10\. Deliverables

## 10.1 Software Deliverables

- PyFDS Python package (pip installable)
- Source code repository (GitHub)
- Comprehensive test suite
- CI/CD pipeline configuration
- Docker container image
- Example scripts and notebooks

## 10.2 Documentation Deliverables

- API reference documentation
- User guide with tutorials
- Developer documentation
- Installation instructions
- Quick start guide
- Example gallery

## 10.3 Quality Metrics

- Code coverage > 90%
- All tests passing
- Documentation coverage 100% for public APIs
- Pylint score > 9.0
- Type hints for all public functions
- Performance benchmarks documented

## 10.4 Success Criteria

The project will be considered successful when:

- Users can create and run FDS simulations entirely from Python
- Common workflows are 50% faster than manual methods
- Output data is automatically available in polars/numpy formats
- The library supports 80% of common FDS use cases
- Documentation enables new users to create simulations within 1 hour
- The package is stable and production-ready

# Appendix A: FDS Namelist Reference

This appendix provides a quick reference for FDS namelist groups that will be supported by the Python connector.

| Namelist | Purpose | Priority |
| --- | --- | --- |
| &HEAD | Simulation identification | Critical |
| &TIME | Time control parameters | Critical |
| &MESH | Computational mesh definition | Critical |
| &SURF | Surface properties | Critical |
| &OBST | Obstructions/solids | Critical |
| &VENT | Vents and openings | Critical |
| &DEVC | Measurement devices | High |
| &REAC | Reaction parameters | High |
| &MATL | Material properties | High |
| &RAMP | Time-dependent functions | Medium |
| &CTRL | Control logic | Medium |
| &PROP | Device properties | Medium |
| &SLCF | Slice file output | Medium |
| &BNDF | Boundary file output | Low |
| &INIT | Initial conditions | Low |

# Appendix B: Example Use Cases

## B.1 Room Fire Simulation

"""
Complete example: ISO 9705 room fire test simulation
"""
from pyfds import Simulation, Material, Surface, Reaction
<br/>\# Create simulation
sim = Simulation(
chid='iso9705_room',
title='ISO 9705 Room Corner Test'
)
<br/>\# Set time parameters
sim.time(t_end=1200.0, dt=0.1)
<br/>\# Define mesh (room is 2.4m x 3.6m x 2.4m high)
sim.mesh(
ijk=(48, 72, 48),
xb=(-0.6, 3.0, -0.6, 4.2, 0.0, 2.4)
)
<br/>\# Define reaction (propane)
sim.reaction(
fuel='PROPANE',
soot_yield=0.01,
co_yield=0.01
)
<br/>\# Define materials
gypsum = Material(
id='GYPSUM',
density=930.0,
conductivity=0.48,
specific_heat=0.84,
emissivity=0.9
)
sim.add_material(gypsum)
<br/>\# Create wall surface
wall_surf = Surface(
id='WALL',
matl_id='GYPSUM',
thickness=0.012,
color='GRAY'
)
sim.add_surface(wall_surf)
<br/>\# Create burner surface with HRR ramp
burner_ramp = sim.ramp(
id='BURNER_RAMP',
t=\[0, 300, 600, 900\],
f=\[0, 100, 100, 300\] # kW
)
<br/>burner_surf = Surface(
id='BURNER',
hrrpua=1000.0, # Will be scaled by ramp
tau_q=-burner_ramp.id,
color='RED'
)
sim.add_surface(burner_surf)
<br/>\# Build room geometry
sim.obstruction(xb=(0, 2.4, 0, 3.6, 0, 0), surf_id='WALL') # Floor
sim.obstruction(xb=(0, 2.4, 0, 3.6, 2.4, 2.4), surf_id='WALL') # Ceiling
\# ... walls ...
<br/>\# Add doorway
sim.vent(xb=(1.0, 1.8, 0, 0, 0, 2.0), surf_id='OPEN')
<br/>\# Add devices
sim.device_array(
id_prefix='TEMP',
quantity='TEMPERATURE',
x_range=(0.3, 2.1, 0.6),
y_range=(0.3, 3.3, 0.6),
z_range=(0.6, 2.1, 0.3)
)
<br/>\# Write and run
sim.write('iso9705_room.fds')
results = sim.run(n_threads=8)
<br/>\# Post-process
results.plot_hrr()
results.plot_device_grid('TEMP', t=600)
results.save_report('iso9705_results.pdf')

# Appendix C: Implementation Checklist

This checklist can be used to track implementation progress:

| Component | Status | Notes |
| --- | --- | --- |
| Project setup | \[ \] | Repository, structure, dependencies |
| Core classes | \[ \] | Simulation, Mesh, Surface, etc. |
| Namelist writer | \[ \] | FDS file generation |
| Input validation | \[ \] | Parameter checking |
| Local runner | \[ \] | Process execution |
| CSV parser | \[ \] | Device and HRR data |
| Binary parsers | \[ \] | SMV, S3D files |
| Progress monitor | \[ \] | Real-time updates |
| HPC support | \[ \] | Cluster execution |
| Unit tests | \[ \] | \>90% coverage |
| Documentation | \[ \] | API docs, tutorials |
| Examples | \[ \] | Notebooks, scripts |
| CI/CD | \[ \] | GitHub Actions |
| PyPI package | \[ \] | pip installable |
| Docker image | \[ \] | Container deployment |

# Appendix D: Resources and References

## D.1 FDS Documentation

- FDS User Guide: <https://pages.nist.gov/fds-smv/>
- FDS Technical Reference: NIST Special Publication 1018
- FDS Verification Guide: NIST Special Publication 1018-1
- FDS Validation Guide: NIST Special Publication 1018-2
- FDS Examples: <https://github.com/firemodels/fds/tree/master/Verification>

## D.2 Python Resources

- Python Packaging Guide: <https://packaging.python.org/>
- NumPy Documentation: <https://numpy.org/doc/>
- Polars Documentation: <https://polars.pydata.org/docs/>
- pytest Documentation: <https://docs.pytest.org/>
- Sphinx Documentation: <https://www.sphinx-doc.org/>

## D.3 Similar Projects

Projects that can serve as inspiration or reference:

- PyFoam: Python library for OpenFOAM
- FiPy: Finite volume PDE solver in Python
- PyMesh: Geometry processing library
- CFD Python: Barba group CFD lessons
