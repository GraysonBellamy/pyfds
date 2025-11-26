# PyFDS Examples

This directory contains example scripts demonstrating various PyFDS features and capabilities.

## Basic Examples

### [basic_room_fire.py](basic_room_fire.py)
Simple room fire simulation demonstrating:
- Basic simulation setup
- Simple geometry (mesh, obstructions)
- Fire source using SURF and OBST
- Temperature monitoring with devices

### [advanced_room.py](advanced_room.py)
More complex room setup with:
- Multiple surfaces and materials
- Advanced geometry
- Multiple measurement devices

### [parametric_study.py](parametric_study.py)
Demonstrates how to:
- Create multiple simulations programmatically
- Vary parameters systematically
- Generate batch simulations

## Phase 3 Examples (Complex Namelists)

### [phase3_example.py](phase3_example.py)
Comprehensive demonstration of Phase 3 namelists:
- **RAMP**: Time-varying properties and temperature-dependent conductivity
- **MATL**: Material definitions (steel, wood)
- **REAC**: Custom combustion reactions
- **PROP**: Device properties (sprinklers)
- **CTRL**: Control logic for activation
- **INIT**: Initial conditions (hot gas layer)

## VENT and MISC Examples (Phase 3 Extended)

### [hvac_system.py](hvac_system.py) ⭐ NEW
HVAC system with supply and exhaust vents:
- **MISC**: Custom ambient conditions (temperature, humidity)
- **VENT**: HVAC vents with volume flow rates
  - Supply vent (0.5 m³/s inflow)
  - Exhaust vent (-0.4 m³/s outflow)
- **VENT**: Door opening to ambient
- Room pressurization analysis

**Key Features:**
```python
# Set ambient conditions
sim.set_misc(tmpa=22.0, humidity=50.0)

# HVAC supply vent
sim.vent(xb=(2, 2.5, 2, 2.5, 3, 3), surf_id='HVAC', volume_flow=0.5)

# Door opening
sim.vent(xb=(4, 4, 4, 6, 0.5, 2), surf_id='OPEN')
```

### [wildfire_simulation.py](wildfire_simulation.py) ⭐ NEW
Large-scale outdoor wildfire simulation:
- **MISC**: Wildfire mode settings
  - `LEVEL_SET_MODE=1` for fire spread
  - Custom turbulence model (Vreman)
  - High temperature (35°C), low humidity (15%)
- **VENT**: Open mesh boundaries for wind flow
- Large domain (100m × 100m × 30m)

**Key Features:**
```python
# Configure for wildfire
sim.set_misc(
    level_set_mode=1,
    tmpa=35.0,
    humidity=15.0,
    turbulence_model=TurbulenceModel.VREMAN
)

# Open boundaries for wind
sim.vent(mb='XMIN', surf_id='OPEN')
```

### [circular_burner.py](circular_burner.py) ⭐ NEW
Circular and annular vent geometries:
- **VENT**: Circular burner using XYZ and RADIUS
- **VENT**: Annular (ring-shaped) burner with inner and outer radii
- Automatic area calculation for different shapes
- Multiple mesh demonstration

**Key Features:**
```python
# Circular burner
burner = Vent(
    xb=(-2, 2, -2, 2, 0, 0),
    surf_id='BURNER',
    xyz=(0, 0, 0),
    radius=1.0
)

# Annular burner (ring)
annular = Vent(
    xb=(-2, 2, -2, 2, 7, 7),
    surf_id='BURNER',
    xyz=(0, 0, 7),
    radius=1.5,
    radius_inner=0.5
)
```

### [solid_phase_heat_transfer.py](solid_phase_heat_transfer.py) ⭐ NEW
Pure heat conduction simulation:
- **MISC**: `SOLID_PHASE_ONLY=True` mode
  - No fluid flow calculation
  - Radiation disabled for speed
- **VENT**: Temperature boundary conditions
- **MATL**: Concrete and steel materials
- Heat flux and temperature monitoring

**Key Features:**
```python
# Solid phase only mode
sim.set_misc(
    solid_phase_only=True,
    radiation=False
)

# Temperature boundary
hot_vent = Vent(xb=(0.3, 0.7, 0.3, 0.7, 0, 0), surf_id='HOT')
```

## Execution Examples

### [execution_demo.py](execution_demo.py)
Demonstrates FDS execution capabilities:
- Running simulations
- Progress monitoring
- Results analysis
- Error handling

## Running Examples

All examples can be run directly:

```bash
# Run any example
python examples/hvac_system.py

# Or with uv
uv run python examples/hvac_system.py
```

Each example will:
1. Create a simulation programmatically
2. Write an FDS input file
3. Print summary information

To actually run the FDS simulation (requires FDS installation):

```python
from pyfds import Simulation

sim = Simulation(chid='test')
# ... configure simulation ...
sim.write('test.fds')

# Execute with FDS
job = sim.run(wait=True)
```

## Feature Overview

| Feature | Examples |
|---------|----------|
| Basic Geometry | basic_room_fire.py, advanced_room.py |
| HVAC Systems | hvac_system.py |
| Circular/Annular Vents | circular_burner.py |
| Wildfire Simulation | wildfire_simulation.py |
| Heat Transfer Only | solid_phase_heat_transfer.py |
| Material Properties | phase3_example.py |
| Time-Varying Properties | phase3_example.py |
| Control Logic | phase3_example.py |
| Parametric Studies | parametric_study.py |
| Execution & Monitoring | execution_demo.py |

## VENT Namelist Summary

The VENT namelist handles various boundary conditions:

| Type | SURF_ID | Use Case | Example |
|------|---------|----------|---------|
| Open | 'OPEN' | Openings to ambient | Doors, windows |
| HVAC | 'HVAC' | Ventilation systems | Supply/exhaust vents |
| Surface | Custom | Fire sources, boundaries | Circular burners |
| Mirror | 'MIRROR' | Symmetry planes | Half-domain simulations |
| Periodic | 'PERIODIC' | Periodic boundaries | Repeating geometries |

**Shapes:**
- **Rectangular**: Standard XB definition (one dimension zero)
- **Circular**: Requires `xyz` (center) and `radius`
- **Annular**: Requires `xyz`, `radius`, and `radius_inner`

## MISC Namelist Summary

The MISC namelist controls global simulation parameters:

**Ambient Conditions:**
- `tmpa`: Ambient temperature [°C]
- `humidity`: Relative humidity [%]
- `p_inf`: Background pressure [Pa]

**Turbulence Models:**
- `DEARDORFF` (default)
- `DYNAMIC_SMAGORINSKY`
- `VREMAN`
- `WALE`

**Special Modes:**
- `solid_phase_only`: Heat conduction only
- `isothermal`: Constant temperature flow
- `level_set_mode`: Wildfire spread (0, 1, or 2)

**Solver Options:**
- `cfl_min`, `cfl_max`: CFL number bounds
- `radiation`: Enable/disable radiation
- `stratification`: Enable/disable stratification

## Next Steps

After running these examples, explore:
- Modifying parameters to understand their effects
- Combining features from different examples
- Creating your own simulations
- Running actual FDS simulations and analyzing results

For more information, see the [PyFDS documentation](../README.md).
