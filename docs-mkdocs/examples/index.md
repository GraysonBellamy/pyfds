# Examples

Learn PyFDS through practical, real-world examples.

## Overview

This section contains complete, working examples demonstrating PyFDS features and capabilities. Each example includes full source code with explanations.

## Example Categories

<div class="grid cards" markdown>

-   :material-home-fire: **Basic Examples**

    ---

    Simple simulations to get started

    [:octicons-arrow-right-24: Basic Examples](basic.md)

-   :material-fire-truck: **Advanced Examples**

    ---

    Complex scenarios with multiple features

    [:octicons-arrow-right-24: Advanced Examples](advanced.md)

-   :material-fire-alert: **Special Applications**

    ---

    Specialized simulation types

    [:octicons-arrow-right-24: Special Applications](special.md)

-   :material-chart-multiple: **Parametric Studies**

    ---

    Automating multiple simulations

    [:octicons-arrow-right-24: Parametric Studies](parametric.md)

-   :material-workflow: **Complete Workflows**

    ---

    End-to-end simulation workflows

    [:octicons-arrow-right-24: Workflows](workflows.md)

</div>

## Featured Examples

### Basic Room Fire
A simple 5m × 5m room with a 1m² fire source.

```python
from pyfds import Simulation

sim = Simulation(chid='room_fire', title='Basic Room Fire')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))
sim.write('room_fire.fds')
```

[:octicons-arrow-right-24: View full example](basic.md#basic-room-fire)

### HVAC System
Room with supply and exhaust ventilation.

```python
from pyfds import Simulation

sim = Simulation(chid='hvac_room', title='HVAC Room Simulation')
sim.time(t_end=600.0)
sim.mesh(ijk=(60, 60, 30), xb=(0, 6, 0, 6, 0, 3))

# Set ambient conditions
sim.set_misc(tmpa=22.0, humidity=50.0)

# Supply vent (0.5 m³/s)
sim.vent(xb=(2, 2.5, 2, 2.5, 3, 3), surf_id='HVAC', volume_flow=0.5)

# Exhaust vent (-0.4 m³/s)
sim.vent(xb=(4, 4.5, 4, 4.5, 3, 3), surf_id='HVAC', volume_flow=-0.4)

sim.write('hvac_room.fds')
```

[:octicons-arrow-right-24: View full example](advanced.md#hvac-system)

### Wildfire Simulation
Large-scale outdoor fire spread.

```python
from pyfds import Simulation
from pyfds.core.namelist import TurbulenceModel

sim = Simulation(chid='wildfire', title='Wildfire Spread')
sim.time(t_end=1800.0)
sim.mesh(ijk=(100, 100, 30), xb=(0, 100, 0, 100, 0, 30))

# Configure for wildfire
sim.set_misc(
    level_set_mode=1,           # Fire spread mode
    tmpa=35.0,                   # Hot ambient
    humidity=15.0,               # Low humidity
    turbulence_model=TurbulenceModel.VREMAN
)

# Open boundaries
sim.vent(mb='XMIN', surf_id='OPEN')
sim.vent(mb='XMAX', surf_id='OPEN')
sim.vent(mb='YMIN', surf_id='OPEN')
sim.vent(mb='YMAX', surf_id='OPEN')

sim.write('wildfire.fds')
```

[:octicons-arrow-right-24: View full example](special.md#wildfire-simulation)

## Example Index

### By Difficulty

| Level | Examples |
|-------|----------|
| **Beginner** | Basic room fire, Simple corridor |
| **Intermediate** | HVAC system, Circular burner, Multiple rooms |
| **Advanced** | Wildfire, Heat transfer only, Sprinkler system |

### By Feature

| Feature | Examples |
|---------|----------|
| **VENT** | HVAC system, Circular burner, Wildfire |
| **MISC** | Wildfire, Heat transfer, HVAC system |
| **RAMP** | Time-varying HRR, Temperature-dependent properties |
| **MATL** | Multi-layer wall, Heat transfer |
| **CTRL** | Sprinkler activation, HVAC control |
| **Execution** | Background jobs, Progress monitoring |

### By Application

| Application | Examples |
|-------------|----------|
| **Building Fire** | Room fire, Corridor, Multi-room |
| **HVAC** | Supply/exhaust, Pressurization |
| **Wildfire** | Vegetation fire, Wind-driven spread |
| **Research** | Grid convergence, Parametric study |

## Running Examples

All examples are available in the project repository:

```bash
# Clone repository
git clone https://github.com/GraysonBellamy/pyfds.git
cd pyfds/examples

# Run an example
python basic_room_fire.py

# Or with uv
uv run python basic_room_fire.py
```

## Example Structure

Each example follows this structure:

1. **Description** - What the example demonstrates
2. **Key Features** - Main PyFDS features used
3. **Complete Code** - Full working Python script
4. **Explanation** - Line-by-line breakdown
5. **Expected Output** - What the FDS file looks like
6. **Variations** - How to modify the example

## Contributing Examples

Have a useful example? We'd love to include it!

1. Follow the [Contributing Guide](../development/contributing.md)
2. Add your example to `examples/`
3. Document it following the structure above
4. Open a pull request

---

[Browse Basic Examples →](basic.md){ .md-button .md-button--primary }
[View Advanced Examples →](advanced.md){ .md-button }
