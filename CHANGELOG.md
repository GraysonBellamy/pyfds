# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Pre-commit hooks configuration for automated code quality checks
- MyPy strict type checking configuration
- Ruff linting and formatting configuration
- Comprehensive CONTRIBUTING.md with development guidelines
- CHANGELOG.md for tracking project changes
- Improved type annotations throughout codebase

### Changed
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

## [0.1.0] - 2024-11-19

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
