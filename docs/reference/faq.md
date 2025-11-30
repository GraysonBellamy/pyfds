# Frequently Asked Questions

Common questions about PyFDS.

## General Questions

### What is PyFDS?

PyFDS is a Python interface to the NIST Fire Dynamics Simulator (FDS). It allows you to create, execute, and analyze FDS simulations using Python code instead of manually editing text files.

### Do I need FDS installed?

- **To create FDS input files**: No, PyFDS can generate `.fds` files without FDS
- **To run simulations**: Yes, you need FDS installed on your system

### What Python versions are supported?

Python 3.11 or higher. PyFDS uses modern Python features and type hints.

### Is PyFDS free?

Yes, PyFDS is open source under the MIT license.

## Installation

### How do I install PyFDS?

```bash
# Using uv (recommended)
uv add pyfds

# Using pip
pip install pyfds
```

See the [Installation Guide](../getting-started/installation.md) for details.

### Installation fails with dependency errors

Try using a fresh virtual environment:

```bash
python -m venv fresh-env
source fresh-env/bin/activate  # or fresh-env\Scripts\activate on Windows
pip install pyfds
```

## Usage

### How do I create my first simulation?

See the [Quick Start Tutorial](../getting-started/quickstart.md). Basic example:

```python
from pyfds import Simulation

sim = Simulation(chid='test')
sim.add(Time(t_end=100.0)
sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 1))
sim.surface(id='FIRE', hrrpua=500.0)
sim.add(Obstruction(xb=Bounds3D.of(0.5, 1.5, 0.5, 1.5, 0, 0.1), surf_id='FIRE')
sim.write('test.fds')
```

### How do I run a simulation?

```python
# Requires FDS installed
results = sim.run(n_threads=4)
```

### Can I use PyFDS with existing FDS files?

PyFDS focuses on creating new simulations. For parsing existing FDS files, consider other tools like `fdsreader`.

### How do I access simulation results?

```python
from pyfds import Results

results = Results(chid='simulation_name')
hrr_data = results.hrr
device_data = results.devc
```

## Performance

### Why is my simulation slow?

Common causes:
1. **Too many cells** - Reduce mesh resolution
2. **Small cells** - Larger cells run faster
3. **Long simulation time** - Reduce `t_end`
4. **Not using parallelization** - Use `n_threads` or `n_mpi`

```python
# Faster: 15,000 cells
sim.add(Mesh(ijk=Grid3D.of(30, 25, 20), xb=Bounds3D.of(0, 3, 0, 2.5, 0, 2))

# Slower: 240,000 cells
sim.add(Mesh(ijk=Grid3D.of(120, 100, 80), xb=Bounds3D.of(0, 3, 0, 2.5, 0, 2))
```

### How many threads should I use?

Start with the number of physical CPU cores:

```python
import os
n_cores = os.cpu_count()
results = sim.run(n_threads=n_cores)
```

### Should I use MPI or OpenMP?

- **OpenMP** (`n_threads`): Single machine, shared memory, easier
- **MPI** (`n_mpi`): Multiple meshes, distributed memory, more scalable

For most users, start with OpenMP.

## Errors and Warnings

### "Obstruction outside mesh bounds"

Your geometry extends beyond the mesh:

```python
# Bad: obstruction at x=6, but mesh ends at x=5
sim.add(Mesh(xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))
sim.add(Obstruction(xb=Bounds3D.of(4, 6, 0, 5, 0, 2.5))

# Good: obstruction within mesh
sim.add(Obstruction(xb=Bounds3D.of(4, 5, 0, 5, 0, 2.5))
```

### "Surface ID not found"

Define surfaces before using them:

```python
# Correct order
sim.surface(id='FIRE', hrrpua=1000.0)  # Define first
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')  # Use second
```

### "Validation failed"

Run explicit validation to see issues:

```python
warnings = sim.validate()
for warning in warnings:
    print(warning)
```

### Simulation crashes or gives NaN values

Common causes:
1. Fire too intense - Reduce HRRPUA
2. Cells too small - Increase cell size
3. Time step too large - FDS adjusts automatically
4. Physical inconsistencies - Check material properties

## Features

### Does PyFDS support all FDS features?

PyFDS currently supports:
- ✅ Basic namelists (HEAD, TIME, MESH, SURF, OBST, DEVC)
- ✅ Complex namelists (RAMP, MATL, REAC, PROP, CTRL, INIT)
- ✅ VENT and MISC
- ✅ Local execution with OpenMP/MPI
- ✅ CSV results parsing (HRR, devices)
- ⚠️ Binary outputs (SLCF, PL3D) - coming soon
- ⚠️ HPC cluster execution - coming soon

### Can I create circular fires?

Yes, using VENT with radius:

```python
sim.add(Vent(
    xb=Bounds3D.of(-1, 1, -1, 1, 0, 0),
    surf_id='FIRE',
    xyz=Point3D.of(0, 0, 0),
    radius=0.5
)
```

### How do I create time-varying properties?

Use RAMP namelists:

```python
sim.ramp(id='FIRE_GROWTH', t=[0, 60, 120], f=[0, 0.5, 1.0])
sim.surface(id='FIRE', hrrpua=1000.0, ramp_q='FIRE_GROWTH')
```

See [RAMP Guide](../guide/ramps.md).

### Can I model HVAC systems?

Yes, using VENT with volume flow:

```python
# Supply vent
sim.add(Vent(xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3), surf_id='HVAC', volume_flow=0.5)

# Exhaust vent
sim.add(Vent(xb=Bounds3D.of(4, 4.5, 4, 4.5, 3, 3), surf_id='HVAC', volume_flow=-0.4)
```

## Comparisons

### PyFDS vs. manual FDS files?

**PyFDS advantages:**
- Type checking and validation
- IDE autocomplete
- Programmatic generation
- Easier parametric studies
- Integrated execution and analysis

**Manual FDS advantages:**
- More control over every detail
- Easier to share (single text file)
- Works with any FDS version

### PyFDS vs. other tools?

- **vs. BlenderFDS**: PyFDS is code-based, BlenderFDS is GUI-based
- **vs. fdsreader**: PyFDS creates simulations, fdsreader reads results
- **vs. Smokeview**: Smokeview for visualization, PyFDS for simulation setup

## Getting Help

### Where can I get help?

1. [Documentation](../index.md)
2. [GitHub Issues](https://github.com/GraysonBellamy/pyfds/issues)
3. [GitHub Discussions](https://github.com/GraysonBellamy/pyfds/discussions)
4. [FDS-SMV Google Group](https://groups.google.com/g/fds-smv) for FDS questions

### How do I report a bug?

Open an issue on [GitHub](https://github.com/GraysonBellamy/pyfds/issues) with:
- Python version
- PyFDS version
- Code to reproduce
- Error message
- Expected vs actual behavior

### How do I request a feature?

Open a feature request on [GitHub](https://github.com/GraysonBellamy/pyfds/issues/new) describing:
- What you want to do
- Why it's useful
- Example of how it might work

## Contributing

### Can I contribute to PyFDS?

Yes! See the [Contributing Guide](../development/contributing.md).

### What can I contribute?

- Code (bug fixes, features)
- Documentation
- Examples
- Bug reports
- Feature requests
- Testing

---

**Still have questions?** Ask in [GitHub Discussions](https://github.com/GraysonBellamy/pyfds/discussions).
