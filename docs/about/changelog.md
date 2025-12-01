# Changelog

All notable changes to PyFDS will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- MkDocs Material documentation site
- Comprehensive user guides and API reference

## [0.1.0] - -11-26

### Added - Phase 3: VENT and MISC Namelists
- **VENT namelist** with comprehensive boundary condition support
  - Open boundaries (`SURF_ID='OPEN'`)
  - HVAC vents with volume flow rates
  - Circular and annular vent geometries
  - Mesh boundary (MB) support for domain boundaries
  - Pressure zone and leak area calculations
- **MISC namelist** for global simulation settings
  - Ambient conditions (temperature, pressure, humidity)
  - Turbulence model selection (Deardorff, Vreman, WALE, Dynamic Smagorinsky)
  - Special simulation modes (wildfire, solid-phase only, isothermal)
  - CFL control and solver parameters
  - Radiation and stratification settings
- New example simulations:
  - HVAC system with supply/exhaust vents
  - Wildfire spread simulation
  - Circular and annular burner geometries
  - Solid-phase heat transfer only
- Enhanced logging system with configurable levels
- Validation utilities for common parameter checks

### Changed
- Updated examples README with VENT and MISC feature overview
- Improved test coverage for VENT and MISC namelists

### Fixed
- Validation edge cases for VENT geometries
- MISC parameter type checking

## [0.0.3] - -11-19

### Added - Phase 3: Complex Namelists
- **RAMP namelist** for time-varying and temperature-dependent properties
- **MATL namelist** for material definitions
- **REAC namelist** for combustion reactions
- **PROP namelist** for device properties
- **CTRL namelist** for control logic
- **INIT namelist** for initial conditions
- Comprehensive validation for all new namelists
- Phase 3 example demonstrating all complex features
- Complete test coverage for new namelists

### Changed
- Enhanced `Simulation` class with new namelist methods
- Improved type hints across all modules
- Updated documentation with complex feature examples

## [0.0.2] - -11-19

### Added - Phase 2: Execution and Analysis
- **Execution Engine**
  - `FDSRunner` class for local execution
  - `Job` class for job management
  - `ProgressMonitor` for real-time progress tracking
  - OpenMP multi-threading support
  - MPI parallel execution support
  - Non-blocking execution with callbacks
- **Results Analysis**
  - `Results` class for output parsing
  - CSV parser for HRR and device data
  - Polars DataFrame integration
  - Built-in plotting utilities
  - Summary statistics generation
- **Examples**
  - `execution_demo.py` showing complete workflow
  - Progress monitoring examples
  - Parallel execution examples
- **Testing**
  - 100+ total tests (62 unit + 38 integration)
  - >90% code coverage
  - Execution integration tests

### Changed
- `Simulation.run()` method for execution
- Enhanced error handling and exceptions
- Improved logging throughout

## [0.0.1] - -11-19

### Added - Phase 1: Foundation
- **Core Classes**
  - `Simulation` class for building simulations
  - `Validator` class for validation
  - Namelist base classes
- **Basic Namelists**
  - `Head` - Simulation metadata
  - `Time` - Time control
  - `Mesh` - Computational domain
  - `Surface` - Surface properties
  - `Obstruction` - Solid objects
  - `Device` - Measurement devices
- **Features**
  - FDS file writer
  - Input validation
  - Method chaining
  - Type hints throughout
- **Testing**
  - 62 unit tests
  - Integration tests
  - >90% code coverage
- **Documentation**
  - README with examples
  - Docstrings for all public APIs
  - Contributing guide
- **Examples**
  - `basic_room_fire.py`
  - `advanced_room.py`
  - `parametric_study.py`
- **Development Tools**
  - Ruff for linting and formatting
  - MyPy for type checking
  - Pytest for testing
  - Pre-commit hooks

### Changed
- Initial project structure
- uv-based dependency management

### Fixed
- N/A (initial release)

## Version History

| Version | Date | Phase | Status |
|---------|------|-------|--------|
| 0.1.0 | -11-26 | Phase 3 Extended | ✅ Complete |
| 0.0.3 | -11-19 | Phase 3 | ✅ Complete |
| 0.0.2 | -11-19 | Phase 2 | ✅ Complete |
| 0.0.1 | -11-19 | Phase 1 | ✅ Complete |

## Migration Guides

### Migrating to 0.1.0

No breaking changes. New features:
- Use `sim.add(Vent())` for boundary conditions
- Use `sim.set_misc()` for global settings

```python
# New in 0.1.0
sim.add(Vent(mb='XMIN', surf_id='OPEN'))
sim.set_misc(tmpa=25.0, radiation=True)
```

### Migrating to 0.0.3

No breaking changes. New features:
- Use `sim.add(Ramp())`, `sim.add(Material())`, `sim.add(Reaction())`, etc. for complex features

```python
from pyfds import Ramp, Material

# New in 0.0.3
sim.add(Ramp(id='T_RAMP', t=[0, 100, 200], f=[0, 0.5, 1.0]))
sim.add(Material(id='WOOD', conductivity=0.12, density=500.0))
```

### Migrating to 0.0.2

No breaking changes. New features:
- Use `.run()` to execute simulations
- Use `Results` class to analyze output

```python
# New in 0.0.2
results = sim.run(n_threads=4)
print(results.hrr['HRR'].max())
```

## Roadmap

### Phase 4: Documentation & Polish (In Progress)
- [x] MkDocs Material documentation
- [ ] Complete API reference
- [ ] Video tutorials
- [ ] Example notebooks
- [ ] PyPI release

### Phase 5: Advanced Features (Planned)
- [ ] HPC cluster execution
- [ ] Binary output parsers (SLCF, PL3D)
- [ ] Advanced visualization
- [ ] Parametric study framework
- [ ] Optimization tools

### Phase 6: Release (Planned)
- [ ] Comprehensive testing
- [ ] Performance optimization
- [ ] Security audit
- [ ] v1.0.0 release

---

[View License →](license.md){ .md-button }
[Back to About →](index.md){ .md-button }
