# Contributing to PyFDS

Thank you for your interest in contributing to PyFDS! This document provides guidelines and instructions for contributing to the project.

## Development Setup

### Prerequisites

- Python 3.8 or higher
- [uv](https://github.com/astral-sh/uv) package manager

### Initial Setup

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd pyfds
   ```

2. Install dependencies:
   ```bash
   uv sync --extra dev
   ```

3. Install pre-commit hooks:
   ```bash
   uv run pre-commit install
   ```

## Code Standards

### Python Version Compatibility

- Code must be compatible with Python 3.8+
- Use type hints for all functions and methods
- Avoid features not available in Python 3.8 (e.g., `X | Y` union syntax)

### Code Quality Tools

We use the following tools to maintain code quality:

- **Ruff**: For linting and formatting (replaces Black and Pylint)
- **MyPy**: For static type checking
- **pytest**: For testing with coverage reporting

All tool configurations are centralized in `pyproject.toml` for easy maintenance.

### Code Style

- Line length: 100 characters (enforced by Ruff)
- Use double quotes for strings
- Follow PEP 8 naming conventions
- Type hints are required for all public APIs
- Docstrings required for all public modules, classes, and functions (NumPy style)

### Running Quality Checks

Before submitting a pull request, ensure all checks pass:

```bash
# Format code
uv run ruff format src/pyfds tests examples

# Lint code
uv run ruff check src/pyfds tests examples --fix

# Type check
uv run mypy src/pyfds

# Run tests
uv run pytest

# Run tests with coverage
uv run pytest --cov=src/pyfds --cov-report=html
```

### Pre-commit Hooks

Pre-commit hooks run automatically on `git commit` and will:
- Fix trailing whitespace
- Fix end-of-file issues
- Validate YAML and TOML files
- Run Ruff formatting and linting
- Run MyPy type checking

If the hooks fail, they will either auto-fix issues or report errors that you must fix manually.

## Testing

### Test Organization

Tests are organized into:
- `tests/unit/`: Unit tests for individual components
- `tests/integration/`: Integration tests for complete workflows

### Writing Tests

- Write tests for all new features
- Aim for >90% code coverage
- Use descriptive test names that explain what is being tested
- Use pytest fixtures for common test setup
- Follow the Arrange-Act-Assert pattern

### Test Example

```python
def test_mesh_cell_size_calculation():
    """Test that mesh calculates cell sizes correctly."""
    # Arrange
    mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))

    # Act
    dx, dy, dz = mesh.get_cell_size()

    # Assert
    assert dx == 0.1
    assert dy == 0.1
    assert dz == 0.1
```

### Running Tests

```bash
# Run all tests
uv run pytest

# Run specific test file
uv run pytest tests/unit/test_namelist.py

# Run with coverage
uv run pytest --cov=src/pyfds --cov-report=term-missing

# Run specific test
uv run pytest tests/unit/test_namelist.py::test_head_namelist
```

## Documentation

### Docstring Style

We use NumPy-style docstrings:

```python
def function_name(param1: str, param2: int) -> bool:
    """
    Brief description of function.

    Longer description if needed, explaining the function's purpose,
    behavior, and any important details.

    Parameters
    ----------
    param1 : str
        Description of param1
    param2 : int
        Description of param2

    Returns
    -------
    bool
        Description of return value

    Raises
    ------
    ValueError
        Description of when this is raised

    Examples
    --------
    >>> result = function_name("test", 42)
    >>> print(result)
    True
    """
    pass
```

### Building Documentation

```bash
# Build Sphinx documentation (when available)
cd docs
uv run make html
```

## Pull Request Process

### Before Submitting

1. Ensure all tests pass
2. Update tests if you've added functionality
3. Update documentation if you've changed APIs
4. Run all quality checks (linting, formatting, type checking)
5. Update CHANGELOG.md with your changes

### PR Guidelines

1. **Create a feature branch** from `main`:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the code standards above

3. **Commit your changes** with clear, descriptive messages:
   ```bash
   git add .
   git commit -m "Add feature: brief description"
   ```

4. **Push to your branch**:
   ```bash
   git push origin feature/your-feature-name
   ```

5. **Open a Pull Request** with:
   - Clear title describing the change
   - Description of what changed and why
   - Reference to any related issues
   - Screenshots/examples if applicable

### PR Review Process

- All PRs require passing CI checks
- Code review from at least one maintainer
- All conversations must be resolved
- No merge conflicts with main branch

## Commit Message Guidelines

- Use present tense ("Add feature" not "Added feature")
- Use imperative mood ("Fix bug" not "Fixes bug")
- Start with a capital letter
- Keep first line under 72 characters
- Reference issues and PRs when applicable

Examples:
```
Add validation for mesh aspect ratio

Fix type annotation error in namelist.py

Update documentation for Surface class

Refactor validator to be stateless
```

## Issue Reporting

### Bug Reports

When reporting bugs, please include:
- Python version
- PyFDS version
- Operating system
- Minimal code example that reproduces the bug
- Expected behavior
- Actual behavior
- Stack trace if applicable

### Feature Requests

When requesting features, please include:
- Clear description of the feature
- Use case / motivation
- Example of how the API might look
- Any alternative solutions you've considered

## Development Workflow

### Typical Development Cycle

1. Pick an issue or create one describing what you'll work on
2. Create a feature branch
3. Make changes with tests
4. Run quality checks locally
5. Commit changes
6. Push and create PR
7. Address review feedback
8. Merge after approval

### Release Process (Maintainers Only)

1. Update version in `src/pyfds/__init__.py`
2. Update CHANGELOG.md with release notes
3. Create git tag: `git tag -a v0.x.0 -m "Release v0.x.0"`
4. Push tag: `git push origin v0.x.0`
5. CI will build and publish to PyPI

## Code of Conduct

### Our Standards

- Be respectful and inclusive
- Welcome newcomers and help them learn
- Focus on constructive criticism
- Accept responsibility for mistakes
- Put the community first

### Unacceptable Behavior

- Harassment or discrimination
- Trolling or insulting comments
- Publishing private information
- Any conduct inappropriate for a professional setting

## Questions?

If you have questions about contributing:
- Open a GitHub issue with the "question" label
- Check existing issues and PRs for similar questions

## License

By contributing to PyFDS, you agree that your contributions will be licensed under the MIT License.
