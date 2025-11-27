# Basic Examples

Simple simulations to get started with PyFDS.

## Simple Room Fire

A 5m × 5m room with a 1m² fire source.

```python
from pyfds import Simulation
from pyfds.core.geometry import Point3D

# Create simulation
sim = Simulation(chid='room_fire', title='Simple Room Fire')

# Time: 10 minutes
sim.time(t_end=600.0)

# Domain: 5m × 5m × 2.5m room, 0.1m cells
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Fire: 1m × 1m burner, 1000 kW/m²
sim.surface(id='BURNER', hrrpua=1000.0, color='RED')
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id='BURNER')

# Measurements
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=Point3D(2.5, 2.5, 2.4))
sim.device(id='TEMP_FLOOR', quantity='TEMPERATURE', xyz=Point3D(2.5, 2.5, 0.1))

# Write FDS file
sim.write('room_fire.fds')
```

**Features Demonstrated:**
- Basic simulation setup
- Simple mesh definition
- Fire surface creation
- Temperature measurement with Point3D coordinates

**Expected Results:**
- Peak ceiling temperature: ~300-400°C
- Smoke layer forms at ~1.5m height
- Total HRR: 1000 kW (1 MW)

## Corridor Fire

A 20m long corridor with fire at one end.

```python
from pyfds import Simulation

sim = Simulation(chid='corridor', title='Corridor Fire')

sim.time(t_end=300.0)

# Long narrow corridor: 20m × 2m × 2.4m
sim.mesh(ijk=(100, 10, 12), xb=(0, 20, 0, 2, 0, 2.4))

# Fire at one end
sim.surface(id='FIRE', hrrpua=500.0, color='ORANGE')
sim.obstruction(xb=(1, 2, 0.5, 1.5, 0, 0.1), surf_id='FIRE')

# Temperature array along corridor
for i, x in enumerate([2, 5, 10, 15, 18]):
    sim.device(id=f'TEMP_{i+1}', quantity='TEMPERATURE', xyz=(x, 1, 2.2))

sim.write('corridor.fds')
```

**Features:**
- Elongated domain
- Temperature gradient
- Smoke propagation

## Growing Fire

Fire that increases in intensity over time.

```python
from pyfds import Simulation

sim = Simulation(chid='growing_fire', title='Growing Fire')

sim.time(t_end=300.0)
sim.mesh(ijk=(40, 40, 20), xb=(0, 4, 0, 4, 0, 2))

# Define fire growth (0 to 100% over 180 seconds)
sim.ramp(
    id='GROWTH',
    t=[0, 60, 120, 180],
    f=[0.0, 0.2, 0.6, 1.0]
)

# Fire with time-varying HRR
sim.surface(
    id='GROWING',
    hrrpua=1500.0,
    ramp_q='GROWTH',
    color='RED'
)

sim.obstruction(xb=(1.5, 2.5, 1.5, 2.5, 0, 0.1), surf_id='GROWING')

# Monitor HRR and temperature
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(2, 2, 1.8))

sim.write('growing_fire.fds')
```

**Features:**
- Time-varying fire using RAMP
- Growing HRR profile
- Real-time temperature response

## Room with Door

Room fire with an open door.

```python
from pyfds import Simulation

sim = Simulation(chid='room_door', title='Room with Door')

sim.time(t_end=300.0)
sim.mesh(ijk=(50, 40, 25), xb=(0, 5, 0, 4, 0, 2.5))

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 1.5, 2.5, 0, 0.1), surf_id='FIRE')

# Walls (with door opening)
sim.surface(id='WALL', matl_id='GYPSUM', thickness=0.013)
sim.obstruction(xb=(0, 0.2, 0, 4, 0, 2.5), surf_id='WALL')  # West
sim.obstruction(xb=(4.8, 5, 0, 4, 0, 2.5), surf_id='WALL')  # East
sim.obstruction(xb=(0, 5, 0, 0.2, 0, 2.5), surf_id='WALL')  # South

# North wall with door
sim.obstruction(xb=(0, 2, 3.8, 4, 0, 2.5), surf_id='WALL')      # Left
sim.obstruction(xb=(3, 5, 3.8, 4, 0, 2.5), surf_id='WALL')      # Right
sim.obstruction(xb=(2, 3, 3.8, 4, 2.1, 2.5), surf_id='WALL')    # Above

# Door opening
sim.vent(xb=(2, 3, 3.8, 3.8, 0, 2.1), surf_id='OPEN')

# Measurements
sim.device(id='TEMP_INSIDE', quantity='TEMPERATURE', xyz=(2.5, 2, 2.2))
sim.device(id='TEMP_DOOR', quantity='TEMPERATURE', xyz=(2.5, 3.8, 1.0))
sim.device(id='VEL_DOOR', quantity='VELOCITY', xyz=(2.5, 3.8, 1.0))

sim.write('room_door.fds')
```

**Features:**
- Room geometry with walls
- Door opening using VENT
- Airflow measurements
- Material properties

## Multiple Fires

Two separate fires in the same space.

```python
from pyfds import Simulation

sim = Simulation(chid='two_fires', title='Two Fire Sources')

sim.time(t_end=300.0)
sim.mesh(ijk=(80, 40, 25), xb=(0, 8, 0, 4, 0, 2.5))

# Two different fire intensities
sim.surface(id='FIRE_A', hrrpua=750.0, color='ORANGE')
sim.surface(id='FIRE_B', hrrpua=1250.0, color='RED')

# Fire A (smaller)
sim.obstruction(xb=(1.5, 2.5, 1.5, 2.5, 0, 0.1), surf_id='FIRE_A')

# Fire B (larger)
sim.obstruction(xb=(5.5, 6.5, 1.5, 2.5, 0, 0.1), surf_id='FIRE_B')

# Temperature measurements above each fire
sim.device(id='TEMP_A', quantity='TEMPERATURE', xyz=(2, 2, 2.2))
sim.device(id='TEMP_B', quantity='TEMPERATURE', xyz=(6, 2, 2.2))

# Center of room
sim.device(id='TEMP_CENTER', quantity='TEMPERATURE', xyz=(4, 2, 2.2))

sim.write('two_fires.fds')
```

**Features:**
- Multiple fire sources
- Different fire intensities
- Thermal interaction

## Quick Test Simulation

Fast-running test for development.

```python
from pyfds import Simulation

# Coarse mesh, short time for quick testing
sim = Simulation(chid='quick_test', title='Quick Test')

sim.time(t_end=30.0)  # Only 30 seconds
sim.mesh(ijk=(20, 20, 10), xb=(0, 2, 0, 2, 0, 1))  # Coarse cells

# Simple fire
sim.surface(id='FIRE', hrrpua=500.0)
sim.obstruction(xb=(0.75, 1.25, 0.75, 1.25, 0, 0.05), surf_id='FIRE')

# Basic measurement
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(1, 1, 0.9))

sim.write('quick_test.fds')

# Run immediately (requires FDS)
# results = sim.run(n_threads=4)
# print(f"Peak temp: {results.devices['TEMP'].max():.1f} °C")
```

**Features:**
- Fast execution (~30 seconds)
- Suitable for testing and debugging
- Can iterate quickly

## Running Examples

All examples can be run directly:

```bash
# Create FDS file
python room_fire.py

# Or run with execution
python -c "
from room_fire import sim
results = sim.run(n_threads=4)
print(f'Simulation complete: {results.hrr[\"HRR\"].max():.0f} kW peak')
"
```

## Next Steps

- [Advanced Examples](advanced.md) - More complex scenarios
- [Special Applications](special.md) - Specialized simulations
- [Parametric Studies](parametric.md) - Multiple simulations

---

[Advanced Examples →](advanced.md){ .md-button .md-button--primary }
