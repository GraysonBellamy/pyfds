# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Phase 3: SURF Thermal Enhancements** - Complete thermal boundary condition capabilities
  - Temperature boundary conditions (TMP_FRONT_INITIAL, TMP_INNER, TMP_BACK, TMP_GAS_BACK)
  - Time-varying temperature ramps (RAMP_T, RAMP_TMP_BACK, RAMP_TMP_GAS_FRONT, RAMP_TMP_GAS_BACK, RAMP_T_I)
  - Heat transfer parameters (ADIABATIC, HEAT_TRANSFER_COEFFICIENT_BACK, RAMP_HEAT_TRANSFER_COEFFICIENT_BACK)
  - Heat transfer models (HEAT_TRANSFER_MODEL, CONVECTION_LENGTH_SCALE, BLOWING)
  - Custom Nusselt correlations (NUSSELT_C0, NUSSELT_C1, NUSSELT_C2, NUSSELT_M)
  - Impinging jet heat transfer (HEAT_TRANSFER_COEFFICIENT_SIGMA)
  - Back surface emissivity (EMISSIVITY_BACK)
  - Solid phase geometry (GEOMETRY, INNER_RADIUS, LENGTH, RADIUS, WIDTH, HORIZONTAL)
  - 3D heat conduction (HT3D, VARIABLE_THICKNESS)
  - Numerical parameters (STRETCH_FACTOR, CELL_SIZE_FACTOR, CELL_SIZE, N_LAYER_CELLS_MAX, TIME_STEP_FACTOR, DELTA_TMP_MAX, MINIMUM_LAYER_THICKNESS, MINIMUM_LAYER_MASS_FRACTION, REMESH_RATIO)
  - Internal heat sources (INTERNAL_HEAT_SOURCE, RAMP_IHS)
  - Visualization parameters (DEFAULT, TEXTURE_MAP, TEXTURE_WIDTH, TEXTURE_HEIGHT, TRANSPARENCY)
  - Comprehensive SurfBuilder fluent API methods for all thermal parameters
  - Full FDS output generation with conditional parameter inclusion
  - Complete unit test coverage for all new thermal surface capabilities
  - Phase 3 thermal boundary conditions example demonstrating all features
- **Structured Pyrolysis API** - Modern, type-safe pyrolysis modeling
  - `PyrolysisReaction` class for structured reaction definitions with validation
  - `PyrolysisProduct` class for gas/solid/particle products with yield validation
  - `MaterialBuilder.add_reaction()` method for structured pyrolysis reactions
  - Automatic yield validation and cross-reference checking
  - Backward compatibility with legacy array-based API
  - Comprehensive examples demonstrating structured pyrolysis
  - Updated documentation with structured API as preferred method
- **Manager-based architecture** following Single Responsibility Principle
  - GeometryManager for meshes, obstructions, and vents
  - MaterialManager for materials and surfaces
  - RampManager for time-varying functions (NEW dedicated manager)
  - PhysicsManager for reactions and misc parameters
  - InstrumentationManager for devices and props
  - ControlManager for controls and initial conditions
  - OutputManager for FDS file generation
- Dedicated RampManager separating RAMPs from MaterialManager
- Manager-specific validation methods
- **MULT namelist support** for creating arrays of repeated geometry objects
  - Complete Mult class with all FDS MULT parameters (spacing, bounds, offsets, skip ranges)
  - MultBuilder fluent API for intuitive MULT configuration
  - mult() convenience method on Simulation class
  - MULT_ID support added to obstructions for array replication
  - Comprehensive validation and FDS output generation
  - Integration tests and examples demonstrating MULT usage
- Comprehensive manager test suite (42+ tests)
- Manager architecture documentation
- Migration guide for manager API
- Pre-commit hooks configuration for automated code quality checks
- MyPy strict type checking configuration
- Ruff linting and formatting configuration
- Comprehensive CONTRIBUTING.md with development guidelines
- CHANGELOG.md for tracking project changes
- Improved type annotations throughout codebase
- **Phase 1 FDS Chapters 6-7 Implementation** ✅
  - Complete HOLE namelist implementation with all FDS Chapter 6 parameters
  - Comprehensive OBST namelist extensions with 20+ new parameters:
    - Identification (ID)
    - Visualization (RGB, TRANSPARENCY, OUTLINE)
    - Geometry control (THICKEN, OVERLAY, PERMIT_HOLE, REMOVABLE, ALLOW_VENT)
    - Control/activation (CTRL_ID, DEVC_ID)
    - Array replication (MULT_ID)
    - Texture mapping (TEXTURE_ORIGIN)
    - 3D heat transfer (HT3D, MATL_ID, CELL_SIZE, etc.)
    - Shape-based geometry (SHAPE, XYZ, RADIUS, HEIGHT, etc.)
  - HoleBuilder fluent API for creating HOLE namelists
  - Factory methods for common hole patterns (door, window)
  - Comprehensive validation for all new parameters
  - Complete test coverage (25 OBST tests, 20 HoleBuilder tests)
  - Updated documentation and examples

### Changed
- **BREAKING**: Refactored Simulation class to use manager delegation
  - `sim.meshes` → `sim.geometry.meshes`
  - `sim.surfaces` → `sim.material_mgr.surfaces`
  - `sim.devices` → `sim.instrumentation.devices`
  - `sim.obstructions` → `sim.geometry.obstructions`
  - `sim.vents` → `sim.geometry.vents`
  - `sim.ramps` → `sim.ramps.ramps` (moved to RampManager)
  - Convenience methods (mesh(), surface(), etc.) still work unchanged
- Moved RAMPs from MaterialManager to dedicated RampManager
- Updated all examples to use new manager API
- Updated all tests to use new manager API
- Updated all documentation to reflect manager architecture
- Replaced Black and Pylint with Ruff for faster linting and formatting
- Updated code style to use combined `if` statements where appropriate
- Improved code formatting and consistency across all modules
- Fixed type annotations in namelist.py to use `Dict[str, Any]` for params

### Fixed
- Type annotation errors in namelist.py (13 MyPy errors resolved)
- Code formatting issues (11 files reformatted)
- Linting violations (SIM102, RET504, B017, PTH123)
- Indentation issues in validator.py
- Test file imports for ValidationError

## [0.1.0] - 2025-11-19

### Added
- Initial release of PyFDS
- Core namelist classes (HEAD, TIME, MESH, SURF, OBST, DEVC)
- Simulation class with fluent method chaining API
- Validation framework with comprehensive checks
- FDS file I/O functionality
- 62 tests with 85% code coverage
- Complete documentation with examples
- Type hints throughout codebase
- Pydantic-based validation

### Supported Features
- Programmatic FDS input file generation
- Mesh quality validation
- Surface and obstruction management
- Device (sensor) placement
- Parameter validation
- File writing and reading

### Documentation
- Complete README with quickstart guide
- API documentation in docstrings (NumPy style)
- Example scripts for common use cases
- Sphinx documentation structure

### Testing
- Unit tests for all core components
- Integration tests for complete workflows
- 85% test coverage
- pytest-based test suite
