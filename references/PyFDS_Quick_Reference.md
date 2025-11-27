# PyFDS - Python FDS Connector Quick Reference

## 🎯 Project Overview
**PyFDS** is a Python library that provides a programmatic interface to NIST Fire Dynamics Simulator (FDS), enabling automated fire simulation workflows through Python.

## 📋 Key Features
- **Pythonic API** for creating FDS input files
- **Automated execution** with progress monitoring
- **Output parsing** to polars/numpy formats
- **Batch processing** and parametric studies
- **HPC cluster** support
- **Integrated visualization** tools

## 🏗️ Architecture

```
┌─────────────────┐
│   Python API    │  ← User interacts here
├─────────────────┤
│  Core Engine    │  ← Namelist generation
├─────────────────┤
│  Execution      │  ← Process management
├─────────────────┤
│  I/O Layer      │  ← File operations
├─────────────────┤
│  FDS Binary     │  ← Actual simulation
└─────────────────┘
```

## 🚀 Quick Start Example

```python
from pyfds import Simulation

# Create simulation
sim = Simulation(chid='room_fire', title='Simple Room Fire')
sim.time(t_end=300.0)

# Define mesh
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Add fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id_top='FIRE')

# Add sensor
from pyfds.core.geometry import Point3D
sim.device(id='TEMP1', quantity='TEMPERATURE', xyz=Point3D(2.5, 2.5, 2.4))
# Or using tuple (backward compatible)
# sim.device(id='TEMP1', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))

# Run simulation
results = sim.run()
print(f"Peak temp: {results.devices['TEMP1'].max()}°C")
```

## 📦 Core Components

### 1. Input Generation
- **Namelist Classes**: `Mesh`, `Surface`, `Obstruction`, `Device`, etc.
- **Validation**: Type checking, range validation, cross-references
- **Writer**: Fortran namelist format generation

### 2. Execution Management
- **Local Runner**: Multi-threaded/MPI execution
- **HPC Support**: SLURM, PBS, LSF schedulers
- **Monitor**: Real-time progress tracking

### 3. Output Processing
- **CSV Parser**: Device data, HRR curves → polars
- **Binary Parser**: 3D data, particles → numpy/xarray
- **Visualization**: Matplotlib, animation support

## 📊 Supported FDS Features

| Namelist | Description | Priority |
|----------|-------------|----------|
| `&HEAD` | Simulation ID | ✅ Critical |
| `&TIME` | Time control | ✅ Critical |
| `&MESH` | Domain mesh | ✅ Critical |
| `&SURF` | Surfaces | ✅ Critical |
| `&OBST` | Obstructions | ✅ Critical |
| `&VENT` | Vents/openings | ✅ Critical |
| `&DEVC` | Devices | 🔶 High |
| `&REAC` | Reactions | 🔶 High |
| `&MATL` | Materials | 🔶 High |
| `&CTRL` | Control logic | 🔷 Medium |
| `&RAMP` | Time functions | 🔷 Medium |

## 🛠️ Implementation Timeline

### Phase 1: Foundation (Weeks 1-3)
- [ ] Project setup
- [ ] Basic namelist classes
- [ ] File writer
- [ ] Validation framework

### Phase 2: Core (Weeks 4-6)
- [ ] Essential namelists
- [ ] Local execution
- [ ] CSV parsing
- [ ] Progress monitoring

### Phase 3: Advanced (Weeks 7-9)
- [ ] Complex namelists
- [ ] HPC support
- [ ] Binary parsers
- [ ] Visualization

### Phase 4: Release (Weeks 10-12)
- [ ] Testing suite
- [ ] Documentation
- [ ] CI/CD pipeline
- [ ] PyPI release

## 🧪 Testing Strategy

```python
# Unit test example
def test_mesh_validation():
    mesh = Mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
    assert mesh.is_valid()

    with pytest.raises(ValidationError):
        Mesh(ijk=(-10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

# Integration test
def test_simulation_workflow():
    sim = Simulation(chid='test')
    sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
    sim.write('test.fds')
    assert Path('test.fds').exists()
```

## 📚 API Design Principles

1. **Intuitive**: Pythonic, follows conventions
2. **Progressive**: Simple tasks simple, complex possible
3. **Validated**: Fail fast with clear errors
4. **Documented**: Every public method documented
5. **Typed**: Full type hints for IDE support

## 🔧 Dependencies

- **Python 3.8+**
- **NumPy**: Numerical arrays
- **polars**: Data manipulation
- **Matplotlib**: Plotting
- **h5py**: Binary file support
- **xarray**: Multi-dimensional data
- **pydantic**: Validation
- **click**: CLI
- **pytest**: Testing

## 📖 Documentation Structure

```
docs/
├── quickstart.md       # 5-minute intro
├── user_guide/         # Tutorials
├── api_reference/      # Auto-generated
├── examples/           # Jupyter notebooks
└── developer_guide/    # Architecture
```

## ✅ Success Metrics

- Code coverage > 90%
- All tests passing
- API docs 100% complete
- Pylint score > 9.0
- Performance benchmarks met
- User can create simulation in < 1 hour

## 🎓 Example: Parametric Study

```python
from pyfds import ParametricStudy

study = ParametricStudy('hrr_sensitivity')
study.add_parameter('hrrpua', [500, 1000, 1500])
study.add_parameter('ventilation', ['open', 'closed'])

def build_sim(hrrpua, ventilation):
    sim = Simulation(f'fire_{hrrpua}_{ventilation}')
    # ... setup simulation ...
    return sim

study.set_builder(build_sim)
results = study.run(parallel=True)
results.plot_sensitivity('hrrpua', 'peak_temp')
```

## 📝 Next Steps for Implementation

1. **Review** this document and the full implementation plan
2. **Set up** development environment
3. **Start** with Phase 1 foundation
4. **Follow** test-driven development
5. **Document** as you code
6. **Iterate** based on feedback

## 🔗 Resources

- [FDS Documentation](https://pages.nist.gov/fds-smv/)
- [Implementation Plan](FDS_Python_Connector_Implementation_Plan.docx)
- GitHub: `github.com/your-org/pyfds` (to be created)
- PyPI: `pip install pyfds` (future)

---

*This quick reference is a companion to the full implementation plan document.*
