# Troubleshooting

Solutions to common problems with PyFDS.

## Installation Issues

### ImportError: No module named 'pyfds'

**Cause**: PyFDS not installed or wrong Python environment

**Solution**:
```bash
# Check installation
pip list | grep pyfds

# Install if missing
pip install pyfds

# Or with uv
uv add pyfds
```

### Version conflicts

**Cause**: Incompatible dependency versions

**Solution**: Use fresh virtual environment
```bash
python -m venv clean-env
source clean-env/bin/activate
pip install pyfds
```

## Simulation Creation

### "Obstruction outside mesh bounds"

**Cause**: Geometry coordinates exceed mesh limits

**Solution**: Check coordinates
```python
# Mesh: (0, 5) in X
sim.mesh(xb=(0, 5, 0, 5, 0, 2.5))

# Bad: X goes to 6
sim.obstruction(xb=(4, 6, 0, 5, 0, 2.5))  # ERROR

# Good: X within bounds
sim.obstruction(xb=(4, 5, 0, 5, 0, 2.5))  # OK
```

### "Surface ID 'FIRE' not found"

**Cause**: Using surface before defining it

**Solution**: Define surfaces first
```python
# Correct order
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
```

### Validation warnings

**Cause**: Potential issues with simulation

**Solution**: Review and fix warnings
```python
warnings = sim.validate()
for w in warnings:
    print(f"Warning: {w}")
```

## Execution Problems

### "FDS not found"

**Cause**: FDS not installed or not in PATH

**Solution**: Install FDS from https://pages.nist.gov/fds-smv/

Verify installation:
```bash
fds -v
```

### Simulation crashes immediately

**Causes and solutions**:

1. **Fire too intense**
   ```python
   # Too intense
   sim.surface(id='FIRE', hrrpua=5000.0)

   # More reasonable
   sim.surface(id='FIRE', hrrpua=1500.0)
   ```

2. **Cells too small**
   ```python
   # Too fine (very slow)
   sim.mesh(ijk=(200, 200, 100), xb=(0, 5, 0, 5, 0, 2.5))

   # More reasonable
   sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
   ```

3. **Physical inconsistencies**
   - Check material properties
   - Verify boundary conditions
   - Review FDS output (.out) file for errors

### NaN or Inf values in results

**Cause**: Numerical instability

**Solutions**:
- Reduce fire intensity
- Increase cell size
- Check for unrealistic inputs
- Review FDS warnings in .out file

## Performance Issues

### Simulation too slow

**Solutions**:

1. **Reduce cell count**
   ```python
   # Before: 500,000 cells
   sim.mesh(ijk=(100, 100, 50), xb=(0, 5, 0, 5, 0, 2.5))

   # After: 62,500 cells (8x faster)
   sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
   ```

2. **Use parallelization**
   ```python
   # Single thread (slow)
   results = sim.run()

   # Multi-threaded (faster)
   results = sim.run(n_threads=8)
   ```

3. **Reduce simulation time**
   ```python
   # Test run
   sim.time(t_end=30.0)

   # Production run
   sim.time(t_end=600.0)
   ```

### Out of memory

**Cause**: Too many cells

**Solution**: Reduce mesh resolution or use multiple meshes

## Results Analysis

### "File not found" when loading results

**Cause**: Wrong path or simulation didn't complete

**Solution**: Check file location
```python
# Specify correct directory
results = Results(chid='simulation', output_dir='./outputs')

# Check files exist
import os
print(os.path.exists('simulation_hrr.csv'))
```

### Empty or missing device data

**Cause**: Device output not written

**Solution**: Ensure simulation completed and devices defined
```python
# Check devices were added
print(f"Number of devices: {len(sim.devices)}")
```

## Development Issues

### Type checking errors with MyPy

**Cause**: Missing type hints or imports

**Solution**: Check imports and types
```python
from typing import Optional
from pyfds import Simulation

def create_sim() -> Simulation:
    return Simulation(chid='test')
```

### Tests failing

**Cause**: Environment or dependency issues

**Solution**: Clean install
```bash
# Remove existing installation
pip uninstall pyfds

# Reinstall with dev dependencies
uv sync --extra dev

# Run tests
uv run pytest
```

## Getting More Help

### Check FDS Output

Always check the FDS output file (`{chid}.out`) for detailed error messages:

```python
# After execution failure
with open('simulation.out', 'r') as f:
    print(f.read())
```

### Enable Logging

```python
import logging
logging.basicConfig(level=logging.DEBUG)

# Now run your code
```

### Ask for Help

If you're still stuck:

1. Check [FAQ](faq.md)
2. Search [GitHub Issues](https://github.com/GraysonBellamy/pyfds/issues)
3. Ask in [Discussions](https://github.com/GraysonBellamy/pyfds/discussions)
4. Open a [new issue](https://github.com/GraysonBellamy/pyfds/issues/new)

When asking for help, include:
- Python version (`python --version`)
- PyFDS version (`pip show pyfds`)
- Minimal code that reproduces the issue
- Full error message
- FDS version (if execution issue)

---

[Back to Reference →](index.md){ .md-button }
