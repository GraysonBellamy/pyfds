# Developer Guide

Information for contributors and developers working on PyFDS.

## Overview

This guide is for anyone who wants to contribute to PyFDS development, whether it's fixing bugs, adding features, improving documentation, or reporting issues.

## Developer Resources

<div class="grid cards" markdown>

-   :material-source-pull: **Contributing**

    ---

    How to contribute to PyFDS

    [:octicons-arrow-right-24: Contributing Guide](contributing.md)

-   :material-sitemap: **Architecture**

    ---

    Project structure and design

    [:octicons-arrow-right-24: Architecture](architecture.md)

-   :material-test-tube: **Testing**

    ---

    Running and writing tests

    [:octicons-arrow-right-24: Testing Guide](testing.md)

-   :material-rocket-launch: **Release Process**

    ---

    How releases are made

    [:octicons-arrow-right-24: Release Process](releases.md)

</div>

## Quick Start for Contributors

### 1. Set Up Development Environment

```bash
# Clone the repository
git clone https://github.com/GraysonBellamy/pyfds.git
cd pyfds

# Install with development dependencies
uv sync --extra dev

# Install pre-commit hooks
uv run pre-commit install
```

### 2. Make Your Changes

```bash
# Create a feature branch
git checkout -b feature/my-awesome-feature

# Make changes
# ... edit code ...

# Run tests
uv run pytest

# Check code quality
uv run ruff check src/pyfds tests
uv run mypy src/pyfds
```

### 3. Submit a Pull Request

```bash
# Commit your changes
git add .
git commit -m "Add awesome feature"

# Push to your fork
git push origin feature/my-awesome-feature

# Open a PR on GitHub
```

## Development Tools

### Code Quality

| Tool | Purpose | Command |
|------|---------|---------|
| **Ruff** | Linting & Formatting | `uv run ruff check src/` |
| **MyPy** | Type Checking | `uv run mypy src/pyfds` |
| **Pytest** | Testing | `uv run pytest` |
| **Pre-commit** | Git Hooks | `pre-commit run --all-files` |

### Running All Checks

```bash
# Format code
uv run ruff format src/pyfds tests examples

# Lint
uv run ruff check src/pyfds tests examples --fix

# Type check
uv run mypy src/pyfds

# Test with coverage
uv run pytest --cov=src/pyfds --cov-report=html
```

## Project Structure

```
pyfds/
├── src/pyfds/          # Source code
│   ├── core/          # Core simulation classes
│   ├── execution/     # Execution engine
│   ├── analysis/      # Results analysis
│   ├── io/            # File I/O
│   └── utils/         # Utilities
├── tests/             # Test suite
│   ├── unit/         # Unit tests
│   └── integration/  # Integration tests
├── examples/          # Example scripts
├── docs-mkdocs/       # Documentation
└── pyproject.toml     # Project configuration
```

## Code Standards

### Python Style

- **Line length**: 100 characters
- **Quotes**: Double quotes for strings
- **Type hints**: Required for all public APIs
- **Docstrings**: NumPy style for all public functions/classes

### Example

```python
def example_function(param1: str, param2: int) -> bool:
    """
    Brief description.

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

    Examples
    --------
    >>> example_function("test", 42)
    True
    """
    return True
```

## Testing

### Running Tests

```bash
# All tests
uv run pytest

# Specific file
uv run pytest tests/unit/test_simulation.py

# With coverage
uv run pytest --cov=src/pyfds --cov-report=html

# Markers
uv run pytest -m "not slow"  # Skip slow tests
```

### Writing Tests

```python
def test_feature():
    """Test description."""
    # Arrange
    sim = Simulation(chid='test')

    # Act
    sim.time(t_end=100.0)

    # Assert
    assert sim.time_params.t_end == 100.0
```

## Documentation

### Building Docs

```bash
# Install docs dependencies
uv sync --extra docs

# Serve locally
uv run mkdocs serve

# Build static site
uv run mkdocs build
```

### Writing Docs

- Use Markdown for all documentation
- Follow the existing structure
- Include code examples
- Add cross-references

## Getting Help

- :material-chat: [Discussions](https://github.com/GraysonBellamy/pyfds/discussions) - Ask questions
- :material-bug: [Issues](https://github.com/GraysonBellamy/pyfds/issues) - Report bugs
- :material-email: [Email](mailto:graysontbellamy@gmail.com) - Direct contact

## Code of Conduct

- Be respectful and inclusive
- Welcome newcomers
- Focus on constructive feedback
- Put the community first

See full guidelines in [Contributing](contributing.md).

---

[Start Contributing →](contributing.md){ .md-button .md-button--primary }
[View Architecture →](architecture.md){ .md-button }
