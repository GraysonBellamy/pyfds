# Testing

Comprehensive testing guide for PyFDS development.

## Overview

PyFDS uses pytest for testing with comprehensive coverage of all functionality. The test suite includes unit tests, integration tests, and end-to-end tests.

```bash
# Run all tests
uv run pytest

# Run with coverage
uv run pytest --cov=pyfds --cov-report=html

# Run specific test file
uv run pytest tests/test_simulation.py

# Run specific test
uv run pytest tests/test_simulation.py::test_mesh_creation
```

## Test Organization

```
tests/
├── unit/                    # Unit tests
│   ├── test_namelists.py    # Namelist tests
│   ├── test_validation.py   # Validation tests
│   └── test_io.py           # I/O tests
├── integration/             # Integration tests
│   ├── test_simulation.py   # Simulation building
│   └── test_execution.py    # Running FDS
├── e2e/                     # End-to-end tests
│   └── test_workflows.py    # Complete workflows
└── conftest.py              # Shared fixtures
```

## Unit Tests

### Testing Namelists

Test individual namelist creation and serialization:

```python
# tests/unit/test_namelists.py
import pytest
from pyfds.namelists import Mesh, Surface, Obstruction

def test_mesh_creation():
    """Test MESH namelist creation."""
    mesh = Mesh(
        ijk=Grid3D.of(50, 50, 25),
        xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)
    )

    assert mesh.ijk == (50, 50, 25)
    assert mesh.xb == (0, 5, 0, 5, 0, 2.5)

def test_mesh_to_fds():
    """Test MESH FDS output."""
    mesh = Mesh(
        id='MESH1',
        ijk=Grid3D.of(10, 10, 10),
        xb=Bounds3D.of(0, 1, 0, 1, 0, 1)
    )

    fds_str = mesh.to_fds()

    assert '&MESH' in fds_str
    assert 'ID=\'MESH1\'' in fds_str
    assert 'IJK=10,10,10' in fds_str
    assert 'XB=0.0,1.0,0.0,1.0,0.0,1.0' in fds_str

def test_mesh_validation():
    """Test MESH validation."""
    # Valid mesh
    mesh = Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))
    assert mesh is not None

    # Invalid - negative cell count
    with pytest.raises(ValidationError):
        Mesh(ijk=Grid3D.of(-10, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

    # Invalid - wrong XB length
    with pytest.raises(ValidationError):
        Mesh(ijk=Grid3D.of(50, 50, 25), xb=(0, 5, 0, 5))

def test_surface_fire():
    """Test fire surface."""
    surf = Surface(
        id='FIRE',
        hrrpua=1000.0,
        color='RED'
    )

    assert surf.id == 'FIRE'
    assert surf.hrrpua == 1000.0
    assert 'HRRPUA=1000.0' in surf.to_fds()

def test_obstruction():
    """Test obstruction creation."""
    obst = Obstruction(
        xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1),
        surf_id='FIRE'
    )

    assert obst.surf_id == 'FIRE'
    assert '&OBST' in obst.to_fds()
```

### Testing Validation

Test validation rules:

```python
# tests/unit/test_validation.py
import pytest
from pyfds import Simulation
from pyfds.validation import Validator

def test_required_parameters():
    """Test required parameter validation."""
    sim = Simulation(chid='test')

    # Missing TIME
    validator = Validator()
    errors = validator.validate(sim)
    assert any('T_END' in str(e) for e in errors)

    # Add TIME
    sim.add(Time(t_end=600.0))
    errors = validator.validate(sim)
    assert not any('T_END' in str(e) for e in errors)

def test_mesh_bounds_validation():
    """Test mesh bounds validation."""
    sim = Simulation(chid='test')
    sim.add(Time(t_end=600.0))

    # Invalid bounds (x1 < x0)
    with pytest.raises(ValidationError):
        sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(5, 0, 0, 5, 0, 2.5)))

    # Valid bounds
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    assert len(sim.geometry.meshes) == 1

def test_reference_validation():
    """Test ID reference validation."""
    sim = Simulation(chid='test')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    # Invalid SURF_ID reference
    sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='UNKNOWN'))

    validator = Validator()
    errors = validator.validate(sim)
    assert any('UNKNOWN' in str(e) for e in errors)

    # Add surface
    sim.add(Surface(id='FIRE', hrrpua=1000.0))
    sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE'))

    errors = validator.validate(sim)
    assert not any('FIRE' in str(e) for e in errors)

def test_mesh_quality():
    """Test mesh quality checks."""
    sim = Simulation(chid='test')
    sim.add(Time(t_end=600.0))

    # Very coarse mesh
    sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

    validator = Validator()
    warnings = validator.get_warnings(sim)
    assert any('coarse' in str(w).lower() for w in warnings)
```

### Testing I/O

Test file reading and writing:

```python
# tests/unit/test_io.py
import pytest
from pathlib import Path
from pyfds import Simulation
from pyfds.io import FDSWriter, FDSReader

def test_write_fds_file(tmp_path):
    """Test writing FDS file."""
    sim = Simulation(chid='test')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

    output = tmp_path / 'test.fds'
    sim.write(str(output))

    assert output.exists()

    # Check contents
    content = output.read_text()
    assert '&HEAD' in content
    assert '&TIME' in content
    assert '&MESH' in content

def test_read_fds_file(tmp_path):
    """Test reading FDS file."""
    # Create test file
    fds_content = """
&HEAD CHID='test', TITLE='Test Simulation' /
&TIME T_END=600.0 /
&MESH IJK=50,50,25, XB=0.0,5.0,0.0,5.0,0.0,2.5 /
&TAIL /
"""
    fds_file = tmp_path / 'test.fds'
    fds_file.write_text(fds_content)

    # Read file
    reader = FDSReader()
    sim = reader.read(str(fds_file))

    assert sim.chid == 'test'
    assert len(sim.geometry.meshes) == 1
    assert sim.geometry.meshes[0].ijk == (50, 50, 25)

def test_parse_device_output(tmp_path):
    """Test parsing device CSV output."""
    # Create test CSV
    csv_content = """Time,TEMP
0.0,20.0
1.0,25.0
2.0,30.0
"""
    csv_file = tmp_path / 'test_devc.csv'
    csv_file.write_text(csv_content)

    # Parse
    from pyfds.io import DeviceParser
    parser = DeviceParser()
    data = parser.parse(str(csv_file))

    assert len(data) == 3
    assert data['Time'].to_list() == [0.0, 1.0, 2.0]
    assert data['TEMP'].to_list() == [20.0, 25.0, 30.0]
```

## Integration Tests

### Testing Simulation Building

Test complete simulation workflows:

```python
# tests/integration/test_simulation.py
import pytest
from pyfds import Simulation

def test_basic_room_fire():
    """Test creating basic room fire simulation."""
    sim = Simulation(chid='room_fire')

    # Setup
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5)))

    # Fire
    sim.add(Surface(id='FIRE', hrrpua=1000.0))
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE'))

    # Boundaries
    sim.add(Vent(xb=Bounds3D.of(6, 6, 2, 3, 0, 2.1), surf_id='OPEN'))

    # Devices
    sim.add(Device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4)))
    sim.add(Device(id='HF_WALL', quantity='GAUGE HEAT FLUX', xyz=Point3D.of(0.1, 2.5, 1.5), ior=1))

    # Validate
    assert sim.is_valid()

    # Write
    sim.write('room_fire.fds')

    # Check file exists
    from pathlib import Path
    assert Path('room_fire.fds').exists()

def test_parametric_study():
    """Test creating parametric study."""
    hrr_values = [500, 1000, 1500]
    simulations = []

    for hrr in hrr_values:
        sim = Simulation(chid=f'fire_{hrr}')
        sim.add(Time(t_end=600.0))
        sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
        sim.add(Surface(id='FIRE', hrrpua=hrr))
        sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE'))

        assert sim.is_valid()
        simulations.append(sim)

    assert len(simulations) == 3

def test_complex_geometry():
    """Test complex geometry with multiple obstructions."""
    sim = Simulation(chid='complex')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(100, 80, 40), xb=Bounds3D.of(0, 10, 0, 8, 0, 4)))

    # Walls
    sim.add(Obstruction(xb=Bounds3D.of(0, 0.2, 0, 8, 0, 3), surf_id='WALL'))
    sim.add(Obstruction(xb=Bounds3D.of(9.8, 10, 0, 8, 0, 3), surf_id='WALL'))
    sim.add(Obstruction(xb=Bounds3D.of(0, 10, 0, 0.2, 0, 3), surf_id='WALL'))
    sim.add(Obstruction(xb=Bounds3D.of(0, 10, 7.8, 8, 0, 3), surf_id='WALL'))

    # Furniture
    for x in range(1, 9, 2):
        for y in range(1, 7, 2):
            sim.add(Obstruction(
                xb=Bounds3D.of(x, x+0.8, y, y+0.8, 0, 0.75),
                surf_id='DESK'
            )

    assert len(sim.geometry.obstructions) > 10
    assert sim.is_valid()
```

### Testing Execution

Test running simulations (requires FDS):

```python
# tests/integration/test_execution.py
import pytest
from pyfds import Simulation
from pyfds.execution import FDSRunner, check_fds_installed

pytestmark = pytest.mark.skipif(
    not check_fds_installed(),
    reason="FDS not installed"
)

def test_run_simple_simulation():
    """Test running simple FDS simulation."""
    # Create minimal simulation
    sim = Simulation(chid='test_run')
    sim.add(Time(t_end=10.0))  # Short duration
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
    sim.write('test_run.fds')

    # Run
    runner = FDSRunner()
    job = runner.run('test_run.fds', wait=True)

    assert job.is_complete()
    assert not job.has_failed()

def test_background_execution():
    """Test running in background."""
    sim = Simulation(chid='test_bg')
    sim.add(Time(t_end=20.0))
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
    sim.write('test_bg.fds')

    # Run in background
    runner = FDSRunner()
    job = runner.run('test_bg.fds', wait=False)

    assert job.is_running()

    # Wait for completion
    job.wait()

    assert job.is_complete()

def test_parallel_execution():
    """Test running with OpenMP."""
    sim = Simulation(chid='test_omp')
    sim.add(Time(t_end=10.0))
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))
    sim.write('test_omp.fds')

    # Run with 4 threads
    runner = FDSRunner()
    job = runner.run('test_omp.fds', n_threads=4, wait=True)

    assert job.is_complete()
```

## End-to-End Tests

Complete workflow testing:

```python
# tests/e2e/test_workflows.py
import pytest
from pathlib import Path
from pyfds import Simulation
from pyfds.analysis import FDSResults

@pytest.mark.skipif(
    not check_fds_installed(),
    reason="FDS not installed"
)
def test_complete_workflow(tmp_path):
    """Test complete simulation workflow."""
    # Change to temp directory
    import os
    os.chdir(tmp_path)

    # 1. Create simulation
    sim = Simulation(chid='workflow_test')
    sim.add(Time(t_end=30.0))
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 1)))

    # Fire
    sim.add(Surface(id='FIRE', hrrpua=500.0))
    sim.add(Obstruction(xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0, 0.05), surf_id='FIRE'))

    # Device
    sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(1, 1, 0.8)))

    # 2. Validate
    assert sim.is_valid()

    # 3. Write
    sim.write('workflow_test.fds')
    assert Path('workflow_test.fds').exists()

    # 4. Run
    job = sim.run(wait=True)
    assert job.is_complete()

    # 5. Analyze
    results = FDSResults('workflow_test')
    temp_data = results.get_device('TEMP')

    assert len(temp_data) > 0
    assert temp_data['Time'].max() >= 30.0
    assert temp_data['Value'].max() > 20.0  # Temperature should increase

def test_parametric_workflow(tmp_path):
    """Test parametric study workflow."""
    import os
    os.chdir(tmp_path)

    # Run multiple cases
    hrr_values = [250, 500, 750]
    peak_temps = []

    for hrr in hrr_values:
        sim = Simulation(chid=f'param_{hrr}')
        sim.add(Time(t_end=20.0))
        sim.add(Mesh(ijk=Grid3D.of(15, 15, 10), xb=Bounds3D.of(0, 1.5, 0, 1.5, 0, 1)))
        sim.add(Surface(id='FIRE', hrrpua=hrr))
        sim.add(Obstruction(xb=Bounds3D.of(0.6, 0.9, 0.6, 0.9, 0, 0.05), surf_id='FIRE'))
        sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(0.75, 0.75, 0.8)))

        sim.write(f'param_{hrr}.fds')
        job = sim.run(wait=True)

        assert job.is_complete()

        results = FDSResults(f'param_{hrr}')
        temp = results.get_device('TEMP')
        peak_temps.append(temp['Value'].max())

    # Higher HRR should give higher temperatures
    assert peak_temps[0] < peak_temps[1] < peak_temps[2]
```

## Fixtures

Shared test fixtures in `conftest.py`:

```python
# tests/conftest.py
import pytest
from pathlib import Path
from pyfds import Simulation

@pytest.fixture
def tmp_working_dir(tmp_path, monkeypatch):
    """Change to temporary working directory."""
    monkeypatch.chdir(tmp_path)
    return tmp_path

@pytest.fixture
def simple_simulation():
    """Create simple test simulation."""
    sim = Simulation(chid='test')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
    return sim

@pytest.fixture
def room_fire_simulation():
    """Create room fire simulation."""
    sim = Simulation(chid='room_fire')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5)))

    # Fire
    sim.add(Surface(id='FIRE', hrrpua=1000.0))
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE'))

    # Door
    sim.add(Vent(xb=Bounds3D.of(6, 6, 2, 3, 0, 2.1), surf_id='OPEN'))

    # Devices
    sim.add(Device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4)))

    return sim

@pytest.fixture
def mock_fds_output(tmp_path):
    """Create mock FDS output files."""
    # Create device output
    csv_content = """Time,TEMP
0.0,20.0
10.0,50.0
20.0,100.0
"""
    (tmp_path / 'test_devc.csv').write_text(csv_content)

    # Create .out file
    out_content = """
Fire Dynamics Simulator

Current Time: 0.0
Current Time: 10.0
Current Time: 20.0
"""
    (tmp_path / 'test.out').write_text(out_content)

    return tmp_path
```

## Running Tests

### Basic Usage

```bash
# Run all tests
uv run pytest

# Run with verbose output
uv run pytest -v

# Run specific file
uv run pytest tests/unit/test_namelists.py

# Run specific test
uv run pytest tests/unit/test_namelists.py::test_mesh_creation

# Run tests matching pattern
uv run pytest -k "mesh"
```

### Coverage

```bash
# Run with coverage
uv run pytest --cov=pyfds

# Generate HTML report
uv run pytest --cov=pyfds --cov-report=html

# View report
open htmlcov/index.html
```

### Markers

```bash
# Skip slow tests
uv run pytest -m "not slow"

# Run only integration tests
uv run pytest -m integration

# Skip tests requiring FDS
uv run pytest -m "not requires_fds"
```

## Best Practices

### Test Naming

Use descriptive test names:

```python
# Good
def test_mesh_validates_positive_cell_counts():
    ...

# Bad
def test_mesh():
    ...
```

### Test Independence

Tests should not depend on each other:

```python
# Good - each test is independent
def test_create_mesh():
    mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
    assert mesh is not None

def test_mesh_to_fds():
    mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
    assert '&MESH' in mesh.to_fds()

# Bad - test2 depends on test1
mesh = None
def test1_create():
    global mesh
    mesh = Mesh(...)

def test2_use():
    assert mesh is not None  # Fails if test1 didn't run
```

### Use Fixtures

Use fixtures for common setup:

```python
# Good - reusable fixture
@pytest.fixture
def standard_mesh():
    return Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

def test_mesh_volume(standard_mesh):
    volume = standard_mesh.get_volume()
    assert volume == 62.5

# Bad - repeated setup
def test_mesh_volume():
    mesh = Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))
    volume = mesh.get_volume()
    assert volume == 62.5
```

### Test Edge Cases

```python
def test_mesh_edge_cases():
    """Test mesh edge cases."""
    # Minimum size
    mesh = Mesh(ijk=Grid3D.of(1, 1, 1), xb=Bounds3D.of(0, 0.1, 0, 0.1, 0, 0.1))
    assert mesh is not None

    # Maximum reasonable size
    mesh = Mesh(ijk=Grid3D.of(1000, 1000, 1000), xb=Bounds3D.of(0, 100, 0, 100, 0, 100))
    assert mesh is not None

    # Invalid - zero cells
    with pytest.raises(ValidationError):
        Mesh(ijk=Grid3D.of(0, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
```

## CI/CD Integration

### GitHub Actions

```yaml
# .github/workflows/test.yml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install uv
      uses: astral-sh/setup-uv@v1

    - name: Set up Python
      run: uv python install 3.11

    - name: Install dependencies
      run: uv sync

    - name: Run tests
      run: uv run pytest --cov=pyfds --cov-report=xml

    - name: Upload coverage
      uses: codecov/codecov-action@v3
      with:
        files: ./coverage.xml
```

## See Also

- [Architecture](architecture.md) - Code organization
- [Contributing](contributing.md) - Development guide
- [Releases](releases.md) - Release process

---

[Releases →](releases.md){ .md-button .md-button--primary }
