# PyFDS - Python Interface to Fire Dynamics Simulator

<div align="center">

[![Python Version](https://img.shields.io/badge/python-3.11+-blue.svg)](https://www.python.org/downloads/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](about/license.md)
[![Tests](https://img.shields.io/badge/tests-passing-brightgreen.svg)](development/testing.md)
[![Coverage](https://img.shields.io/badge/coverage-90%25-brightgreen.svg)](development/testing.md)

</div>

A comprehensive Python library for creating, executing, and analyzing **NIST Fire Dynamics Simulator (FDS)** simulations programmatically.

---

## :fire: What is PyFDS?

PyFDS transforms the traditional FDS workflow by providing a **Pythonic API** that allows you to:

<div class="grid cards" markdown>

-   :material-code-braces: **Create FDS Input Files**

    ---

    Build simulations using clean, readable Python code instead of manual text editing

    [:octicons-arrow-right-24: Quick Start](getting-started/quickstart.md)

-   :material-check-circle: **Validate Before Running**

    ---

    Catch errors early with comprehensive validation checks before execution

    [:octicons-arrow-right-24: Validation Guide](reference/validation.md)

-   :material-speedometer: **Automate Parametric Studies**

    ---

    Generate hundreds of simulations programmatically for sensitivity analysis

    [:octicons-arrow-right-24: Parametric Examples](examples/parametric.md)

-   :material-chart-line: **Analyze Results**

    ---

    Access simulation data as Polars DataFrames with built-in plotting

    [:octicons-arrow-right-24: Analysis Guide](execution/analysis.md)

</div>

---

## :rocket: Quick Example

Create a complete room fire simulation in just a few lines:

=== "Python Code"

    ```python
    from pyfds import Simulation, Time, Mesh, Surface, Obstruction, Device
    from pyfds.core.geometry import Bounds3D, Grid3D, Point3D

    # Create simulation
    sim = Simulation(chid='room_fire', title='Simple Room Fire')

    # Set time parameters
    sim.add(Time(t_end=600.0))

    # Define computational domain (5m x 5m x 2.5m room)
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    # Create fire surface (1000 kW/m²)
    sim.add(Surface(id='BURNER', hrrpua=1000.0, color='RED'))

    # Add fire source (1m x 1m burner at floor)
    sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='BURNER'))

    # Add temperature measurement at ceiling
    sim.add(Device(id='TEMP_CEILING', quantity='TEMPERATURE',
               xyz=Point3D.of(2.5, 2.5, 2.4)))

    # Validate and write FDS input file
    sim.write('room_fire.fds')
    ```

=== "Generated FDS File"

    ```fortran
    &HEAD CHID='room_fire', TITLE='Simple Room Fire' /
    &TIME T_END=600.0 /
    &MESH IJK=50,50,25, XB=0,5,0,5,0,2.5 /
    &SURF ID='BURNER', HRRPUA=1000.0, RGB=255,0,0 /
    &OBST XB=2,3,2,3,0,0.1, SURF_ID='BURNER' /
    &DEVC ID='TEMP_CEILING', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.4 /
    &TAIL /
    ```

=== "Run & Analyze"

    ```python
    # Execute simulation (requires FDS installation)
    results = sim.run(n_threads=4)

    # Access results as DataFrames
    print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
    temp_data = results.devices['TEMP_CEILING']

    # Generate plots
    results.plot_hrr('hrr.png')
    results.plot_device('TEMP_CEILING', 'temperature.png')
    ```

---

## :star2: Key Features

### Intuitive API Design
- **Fluent Interface**: Method chaining for readable simulation building
- **Type Safety**: Full type hints for IDE autocomplete and error prevention
- **Pydantic Models**: Automatic validation with informative error messages

### Comprehensive Coverage
- **All Major Namelists**: HEAD, TIME, MESH, SURF, OBST, DEVC, VENT, MISC, and more
- **Complex Features**: RAMP, MATL, REAC, PROP, CTRL for advanced scenarios
- **Special Modes**: Wildfire simulation, solid-phase heat transfer, HVAC systems

### Execution & Analysis
- **Local Execution**: Run FDS directly from Python with progress monitoring
- **Parallel Processing**: OpenMP multi-threading and MPI support
- **Data Analysis**: Results as Polars DataFrames with built-in plotting
- **Non-blocking Jobs**: Background execution with status tracking

### Quality & Testing
- **100+ Tests**: Comprehensive unit and integration test coverage (>90%)
- **Type Checked**: Full MyPy static type checking
- **Well Documented**: Extensive docstrings and examples

---

## :package: Installation

=== "Using uv (Recommended)"

    ```bash
    uv add pyfds
    ```

=== "Using pip"

    ```bash
    pip install pyfds
    ```

=== "From Source"

    ```bash
    git clone https://github.com/GraysonBellamy/pyfds.git
    cd pyfds
    uv sync --extra dev
    ```

!!! tip "FDS Required for Execution"
    To actually run simulations (not just create input files), you need FDS installed.
    See [Installation Guide](getting-started/installation.md) for details.

---

## :books: Documentation Overview

<div class="grid cards" markdown>

-   :material-rocket-launch: **Getting Started**

    ---

    New to PyFDS? Start here!

    - [Installation](getting-started/installation.md)
    - [Quick Start Tutorial](getting-started/quickstart.md)
    - [Key Concepts](getting-started/concepts.md)

-   :material-book-open-variant: **User Guide**

    ---

    Comprehensive guides for all features

    - [Building Simulations](guide/building-simulations.md)
    - [Computational Domain](guide/domain.md)
    - [Boundary Conditions](guide/boundaries.md)
    - [Fire Sources](guide/fire-sources.md)
    - [And more...](guide/index.md)

-   :material-play-circle: **Examples**

    ---

    Learn from practical examples

    - [Basic Room Fire](examples/basic.md)
    - [HVAC Systems](examples/advanced.md)
    - [Wildfire Simulation](examples/special.md)
    - [Parametric Studies](examples/parametric.md)

-   :material-api: **API Reference**

    ---

    Complete API documentation

    - [Simulation Class](api/core/simulation.md)
    - [Namelist Classes](api/namelists/index.md)
    - [Execution Module](api/execution/runner.md)
    - [Analysis Tools](api/analysis/results.md)

</div>

---

## :zap: What Can You Build?

PyFDS supports a wide range of fire simulation scenarios:

### :office: Building Fire Safety
- Room fires and compartment modeling
- Smoke spread and evacuation analysis
- Sprinkler and detector activation
- HVAC system interactions

### :factory: Industrial Applications
- Warehouse and factory fires
- Hazardous material storage
- Explosion modeling
- Fire suppression systems

### :deciduous_tree: Wildfire Simulation
- Vegetation fire spread
- Terrain effects
- Wind-driven fires
- Fire breaks and barriers

### :microscope: Research & Analysis
- Parametric sensitivity studies
- Model validation
- Heat transfer analysis
- Custom fire scenarios

---

## :handshake: Getting Help

<div class="grid" markdown>

<div markdown>

### :material-help-circle: Support Channels

- :fontawesome-brands-github: [GitHub Issues](https://github.com/GraysonBellamy/pyfds/issues) - Bug reports and feature requests
- :material-forum: [Discussions](https://github.com/GraysonBellamy/pyfds/discussions) - Questions and community help
- :material-file-document: [FAQ](reference/faq.md) - Common questions answered
- :material-bug: [Troubleshooting](reference/troubleshooting.md) - Solutions to common issues

</div>

<div markdown>

### :material-school: Learning Resources

- :material-clock-fast: [Quick Start](getting-started/quickstart.md) - Get up and running in 5 minutes
- :material-book: [User Guide](guide/index.md) - Comprehensive feature documentation
- :material-code-tags: [Examples](examples/index.md) - Real-world code examples
- :material-school: [FDS Background](reference/fds-background.md) - Understanding FDS fundamentals

</div>

</div>

---

## :building_construction: Project Status

### Phase 1: Foundation ✅ **Complete**
- [x] Core namelist classes (HEAD, TIME, MESH, SURF, OBST, DEVC)
- [x] FDS file writer and validation framework
- [x] Comprehensive test suite

### Phase 2: Execution & Analysis ✅ **Complete**
- [x] Local execution with progress monitoring
- [x] CSV output parsing (HRR, device data)
- [x] Results analysis with Polars DataFrames
- [x] OpenMP and MPI parallel support

### Phase 3: Advanced Features ✅ **Complete**
- [x] Complex namelists (RAMP, MATL, REAC, PROP, CTRL, INIT)
- [x] VENT and MISC namelists
- [x] Special simulation modes (wildfire, solid-phase, HVAC)

### Phase 4: Documentation & Release 🚧 **In Progress**
- [x] MkDocs Material documentation
- [ ] Complete API reference
- [ ] Video tutorials
- [ ] PyPI release

---

## :balance_scale: License

PyFDS is released under the [MIT License](about/license.md).

## :pray: Acknowledgments

- **NIST Fire Research Division** for developing FDS
- The **FDS community** for documentation and support
- All **contributors** to the PyFDS project

---

## :mortar_board: Citation

If you use PyFDS in your research, please cite:

```bibtex
@software{pyfds2025,
  title = {PyFDS: Python Interface to Fire Dynamics Simulator},
  author = {Bellamy, Grayson},
  year = {2025},
  url = {https://github.com/GraysonBellamy/pyfds}
}
```

[Learn more about citing PyFDS](about/citation.md)

---

<div align="center">

**Ready to get started?**

[Install PyFDS](getting-started/installation.md){ .md-button .md-button--primary }
[Quick Start Tutorial](getting-started/quickstart.md){ .md-button }

</div>
