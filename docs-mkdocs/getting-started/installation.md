# Installation

This guide covers everything you need to install and set up PyFDS on your system.

## Requirements

### Python Version

PyFDS requires **Python 3.11 or higher**. Check your Python version:

```bash
python --version
```

If you need to install or upgrade Python, visit [python.org/downloads](https://www.python.org/downloads/).

### System Requirements

- **Operating System**: Linux, macOS, or Windows
- **Memory**: Minimum 4GB RAM (8GB+ recommended for larger simulations)
- **Storage**: ~500MB for PyFDS and dependencies

### Optional: FDS Installation

!!! info "FDS Required for Execution"
    PyFDS can create and validate FDS input files without FDS installed. However, to actually **run simulations**, you need FDS installed on your system.

To install FDS:

1. Visit the [FDS-SMV Downloads page](https://pages.nist.gov/fds-smv/downloads.html)
2. Download the installer for your operating system
3. Follow the installation instructions
4. Verify installation: `fds -v`

---

## Installation Methods

### Method 1: Using `uv` (Recommended)

[`uv`](https://github.com/astral-sh/uv) is a fast Python package manager. This is the recommended installation method.

=== "Install uv"

    ```bash
    # On macOS and Linux
    curl -LsSf https://astral.sh/uv/install.sh | sh

    # On Windows
    powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
    ```

=== "Install PyFDS"

    ```bash
    uv add pyfds
    ```

=== "Create New Project"

    ```bash
    # Create a new project with PyFDS
    mkdir my-fds-project
    cd my-fds-project
    uv init
    uv add pyfds

    # Run Python with PyFDS
    uv run python your_script.py
    ```

### Method 2: Using `pip`

Standard installation using pip:

```bash
pip install pyfds
```

For a specific version:

```bash
pip install pyfds==0.1.0
```

### Method 3: From Source (Development)

For development or to get the latest features:

=== "Using uv"

    ```bash
    # Clone the repository
    git clone https://github.com/GraysonBellamy/pyfds.git
    cd pyfds

    # Install with development dependencies
    uv sync --extra dev
    ```

=== "Using pip"

    ```bash
    # Clone the repository
    git clone https://github.com/GraysonBellamy/pyfds.git
    cd pyfds

    # Install in editable mode with dev dependencies
    pip install -e ".[dev]"
    ```

---

## Verifying Installation

After installation, verify that PyFDS is installed correctly:

=== "Python Script"

    ```python
    import pyfds

    print(f"PyFDS version: {pyfds.__version__}")
    print("Installation successful!")
    ```

=== "Command Line"

    ```bash
    python -c "import pyfds; print(pyfds.__version__)"
    ```

Expected output:
```
PyFDS version: 0.1.0
Installation successful!
```

### Check Available Components

Verify that all major components are available:

```python
from pyfds import (
    Simulation,      # Main simulation class
    Results,         # Results analysis
    FDSRunner,       # Execution engine
    Validator,       # Validation tools
)

print("All components imported successfully!")
```

---

## Dependencies

PyFDS automatically installs these dependencies:

### Core Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| **numpy** | ≥2.3.5 | Numerical computations |
| **polars** | ≥1.35.2 | Fast DataFrame operations |
| **pydantic** | ≥2.12.4 | Data validation |
| **matplotlib** | ≥3.10.7 | Plotting and visualization |
| **h5py** | ≥3.15.1 | HDF5 file reading |
| **xarray** | ≥2025.11.0 | Multi-dimensional arrays |
| **click** | ≥8.3.1 | CLI interface |
| **tqdm** | ≥4.67.1 | Progress bars |

### Development Dependencies (Optional)

For development work, additional dependencies are installed:

| Package | Purpose |
|---------|---------|
| **pytest** | Testing framework |
| **pytest-cov** | Coverage reporting |
| **pytest-benchmark** | Performance testing |
| **ruff** | Linting and formatting |
| **mypy** | Type checking |
| **pre-commit** | Git hooks |

---

## Virtual Environments

!!! tip "Use Virtual Environments"
    Always use a virtual environment to avoid dependency conflicts.

### Using `venv` (Built-in)

```bash
# Create virtual environment
python -m venv .venv

# Activate (Linux/macOS)
source .venv/bin/activate

# Activate (Windows)
.venv\Scripts\activate

# Install PyFDS
pip install pyfds
```

### Using `conda`

```bash
# Create conda environment
conda create -n pyfds python=3.11
conda activate pyfds

# Install PyFDS
pip install pyfds
```

### Using `uv` (Automatic)

`uv` automatically manages virtual environments for you:

```bash
# uv creates and manages .venv automatically
uv add pyfds
uv run python your_script.py
```

---

## Platform-Specific Notes

### Linux

PyFDS works out-of-the-box on most Linux distributions.

```bash
# Ubuntu/Debian
sudo apt update
python3 --version  # Verify Python 3.11+

# Install PyFDS
pip install pyfds
```

### macOS

```bash
# Check Python version
python3 --version

# Install PyFDS
pip3 install pyfds
```

!!! note "macOS Note"
    On macOS, you may need to use `python3` and `pip3` instead of `python` and `pip`.

### Windows

```powershell
# Check Python version
python --version

# Install PyFDS
pip install pyfds
```

!!! warning "Windows Path"
    Ensure Python is in your system PATH. During Python installation, check "Add Python to PATH".

---

## Upgrading PyFDS

To upgrade to the latest version:

=== "Using uv"

    ```bash
    uv add --upgrade pyfds
    ```

=== "Using pip"

    ```bash
    pip install --upgrade pyfds
    ```

To upgrade to a specific version:

```bash
pip install --upgrade pyfds==0.2.0
```

---

## Uninstalling

To remove PyFDS:

=== "Using uv"

    ```bash
    uv remove pyfds
    ```

=== "Using pip"

    ```bash
    pip uninstall pyfds
    ```

---

## Troubleshooting Installation

### Common Issues

??? question "ImportError: No module named 'pyfds'"

    **Solution**: PyFDS is not installed in the current Python environment.

    ```bash
    # Verify installation
    pip list | grep pyfds

    # Reinstall if needed
    pip install pyfds
    ```

??? question "Version conflict errors"

    **Solution**: Use a fresh virtual environment.

    ```bash
    python -m venv fresh-env
    source fresh-env/bin/activate  # or fresh-env\Scripts\activate on Windows
    pip install pyfds
    ```

??? question "Permission errors on Linux/macOS"

    **Solution**: Don't use `sudo` with pip. Use virtual environments or `--user` flag.

    ```bash
    pip install --user pyfds
    ```

??? question "SSL certificate errors"

    **Solution**: Update pip and try again.

    ```bash
    pip install --upgrade pip
    pip install pyfds
    ```

### Getting Help

If you encounter installation issues:

1. Check the [Troubleshooting Guide](../reference/troubleshooting.md)
2. Search [GitHub Issues](https://github.com/GraysonBellamy/pyfds/issues)
3. Ask in [GitHub Discussions](https://github.com/GraysonBellamy/pyfds/discussions)
4. Open a [new issue](https://github.com/GraysonBellamy/pyfds/issues/new)

---

## Next Steps

Now that PyFDS is installed, you're ready to create your first simulation!

[Quick Start Tutorial →](quickstart.md){ .md-button .md-button--primary }
