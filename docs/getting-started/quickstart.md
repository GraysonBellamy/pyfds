# Quick Start Tutorial

Create your first FDS fire simulation in just 5 minutes! This tutorial will walk you through building a simple room fire simulation using PyFDS.

## What We'll Build

A basic room fire simulation with:

- 5m × 5m × 2.5m room
- 1m × 1m fire source in the center
- Temperature measurement at the ceiling
- 600 second (10 minute) simulation time

## Step 1: Import PyFDS

First, import the main `Simulation` class:

```python
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
```

## Step 2: Create a Simulation

Create a new simulation with a case identifier (`chid`) and title:

```python
sim = Simulation(
    chid='my_first_fire',
    title='My First Room Fire Simulation'
)
```

!!! info "What is CHID?"
    The `chid` (Case ID) is used as the prefix for all output files. For example, with `chid='my_first_fire'`, FDS will create files like `my_first_fire.fds`, `my_first_fire.out`, `my_first_fire_devc.csv`, etc.

## Step 3: Set Time Parameters

Define how long the simulation will run:

```python
sim.time(t_end=600.0)  # 10 minutes
```

You can also specify:

```python
sim.time(
    t_end=600.0,      # End time (seconds)
    t_begin=0.0,      # Start time (default: 0.0)
    dt=0.1            # Initial time step (optional)
)
```

## Step 4: Define the Computational Domain

Create a mesh that defines the simulation space:

```python
sim.mesh(
    ijk=Grid3D(50, 50, 25),              # Grid cells in each direction
    xb=Bounds3D(0, 5, 0, 5, 0, 2.5)        # Domain bounds: (xmin, xmax, ymin, ymax, zmin, zmax)
)
```

!!! tip "Mesh Resolution"
    The `ijk` parameter defines the number of cells:

    - **50 cells** in X direction (5m / 50 = 0.1m cell size)
    - **50 cells** in Y direction (5m / 50 = 0.1m cell size)
    - **25 cells** in Z direction (2.5m / 25 = 0.1m cell size)

    A good starting point is 0.05m to 0.2m cell size for room fires.

## Step 5: Create a Fire Surface

Define the properties of the fire source:

```python
sim.surface(
    id='FIRE',
    hrrpua=1000.0,    # Heat release rate per unit area (kW/m²)
    color='RED'        # Color for visualization
)
```

!!! note "Heat Release Rate"
    HRRPUA (Heat Release Rate Per Unit Area) of 1000 kW/m² represents a moderately intense fire.

    - **500 kW/m²**: Small fire
    - **1000 kW/m²**: Medium fire
    - **2000 kW/m²**: Intense fire

## Step 6: Place the Fire Source

Create a 1m × 1m burner in the center of the room:

```python
sim.obstruction(
    xb=Bounds3D(2.0, 3.0, 2.0, 3.0, 0.0, 0.1),  # Bounds: (xmin, xmax, ymin, ymax, zmin, zmax)
    surf_id='FIRE'                        # Apply fire surface to top
)
```

The burner is:

- Centered at (2.5, 2.5) in the XY plane
- 1m × 1m in area
- 0.1m tall (very thin, essentially a surface)

## Step 7: Add a Measurement Device

Add a temperature sensor at the ceiling:

```python
sim.device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=(2.5, 2.5, 2.4)    # Location: center of room, near ceiling
)
```

## Step 8: Write the FDS Input File

Generate and save the FDS input file:

```python
sim.write('my_first_fire.fds')
print("FDS input file created successfully!")
```

## Complete Code

Here's the complete simulation in one script:

```python
from pyfds import Simulation

# Create simulation
sim = Simulation(chid='my_first_fire', title='My First Room Fire Simulation')

# Set time
sim.time(t_end=600.0)

# Define domain (5m × 5m × 2.5m room)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Create fire surface
sim.surface(id='FIRE', hrrpua=1000.0, color='RED')

# Place 1m × 1m fire in center
sim.obstruction(xb=(2.0, 3.0, 2.0, 3.0, 0.0, 0.1), surf_id='FIRE')

# Add ceiling temperature sensor
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))

# Write FDS file
sim.write('my_first_fire.fds')
print("Simulation created successfully!")
```

Save this as `my_first_fire.py` and run it:

```bash
python my_first_fire.py
```

## Understanding the Output

After running the script, you'll see:

```
Simulation created successfully!
```

And a new file `my_first_fire.fds` will be created with content like:

```fortran
&HEAD CHID='my_first_fire', TITLE='My First Room Fire Simulation' /
&TIME T_END=600.0 /
&MESH IJK=50,50,25, XB=0,5,0,5,0,2.5 /
&SURF ID='FIRE', HRRPUA=1000.0, RGB=255,0,0 /
&OBST XB=2.0,3.0,2.0,3.0,0.0,0.1, SURF_ID='FIRE' /
&DEVC ID='TEMP_CEILING', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.4 /
&TAIL /
```

## Next Steps: Run the Simulation

If you have FDS installed, you can execute the simulation:

=== "Using FDS Directly"

    ```bash
    fds my_first_fire.fds
    ```

=== "Using PyFDS"

    ```python
    # Add to your script
    results = sim.run(n_threads=4)

    # Analyze results
    print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
    temp_data = results.devices['TEMP_CEILING']
    print(f"Peak Temperature: {temp_data.max():.1f} °C")

    # Create plots
    results.plot_hrr('hrr.png')
    results.plot_device('TEMP_CEILING', 'temperature.png')
    ```

!!! tip "Execution Time"
    This simulation should take 1-5 minutes on a modern computer. Larger domains or finer meshes will take longer.

## Method Chaining

PyFDS supports method chaining for more concise code:

```python
from pyfds import Simulation

sim = (Simulation(chid='my_first_fire', title='My First Room Fire')
       .time(t_end=600.0)
       .mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
       .surface(id='FIRE', hrrpua=1000.0, color='RED')
       .obstruction(xb=(2.0, 3.0, 2.0, 3.0, 0.0, 0.1), surf_id='FIRE')
       .device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4)))

sim.write('my_first_fire.fds')
```

## Validation

PyFDS automatically validates your simulation. You can also explicitly validate:

```python
# Validate before writing
warnings = sim.validate()

if warnings:
    print("Warnings:")
    for warning in warnings:
        print(f"  - {warning}")
else:
    print("No validation warnings!")

# Write the file
sim.write('my_first_fire.fds')
```

## Common Modifications

### Change Fire Size

```python
# Larger fire (2m × 2m)
sim.obstruction(xb=(1.5, 3.5, 1.5, 3.5, 0.0, 0.1), surf_id='FIRE')
```

### Change Fire Intensity

```python
# More intense fire
sim.surface(id='FIRE', hrrpua=2000.0, color='RED')

# Less intense fire
sim.surface(id='FIRE', hrrpua=500.0, color='ORANGE')
```

### Add More Devices

```python
# Add multiple temperature sensors
sim.device(id='TEMP_CEILING', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.4))
sim.device(id='TEMP_DOOR', quantity='TEMPERATURE', xyz=(5.0, 2.5, 2.0))
sim.device(id='TEMP_FLOOR', quantity='TEMPERATURE', xyz=(2.5, 2.5, 0.1))

# Add velocity sensor
sim.device(id='VEL_CEILING', quantity='VELOCITY', xyz=(2.5, 2.5, 2.4))
```

### Longer/Shorter Simulation

```python
# 30 second quick test
sim.time(t_end=30.0)

# 1 hour simulation
sim.time(t_end=3600.0)
```

### Finer/Coarser Mesh

```python
# Finer mesh (0.05m cells, 4x more cells)
sim.mesh(ijk=(100, 100, 50), xb=(0, 5, 0, 5, 0, 2.5))

# Coarser mesh (0.2m cells, faster but less accurate)
sim.mesh(ijk=(25, 25, 12), xb=(0, 5, 0, 5, 0, 2.4))
```

## What You Learned

✅ How to create a `Simulation` object
✅ Setting time parameters with `.time()`
✅ Defining computational domains with `.mesh()`
✅ Creating surfaces with `.surface()`
✅ Placing obstructions with `.obstruction()`
✅ Adding devices with `.device()`
✅ Writing FDS input files with `.write()`
✅ Method chaining for concise code

## Next Steps

Now that you've created your first simulation, explore:

- [Key Concepts](concepts.md) - Understand FDS and PyFDS fundamentals
- [User Guide](../guide/index.md) - Learn about all PyFDS features
- [Examples](../examples/index.md) - See more complex simulations
- [Execution & Analysis](../execution/index.md) - Run simulations and analyze results

---

[Learn Key Concepts →](concepts.md){ .md-button .md-button--primary }
[Explore Examples →](../examples/index.md){ .md-button }
