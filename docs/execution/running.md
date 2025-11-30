# Running Simulations

Execute FDS simulations directly from Python.

## Overview

PyFDS can execute FDS simulations and monitor progress in real-time.

```python
# Create and run simulation
results = sim.run(n_threads=4)
```

## Basic Execution

### Blocking Execution

Wait for simulation to complete:

```python
from pyfds import Simulation

sim = Simulation(chid='test')
sim.add(Time(t_end=100.0)
sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 1))
sim.surface(id='FIRE', hrrpua=500.0)
sim.add(Obstruction(xb=Bounds3D.of(0.5, 1.5, 0.5, 1.5, 0, 0.1), surf_id='FIRE')

# Execute and wait
results = sim.run(n_threads=4)

# Access results
print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
```

### Non-Blocking Execution

Run in background:

```python
import time

# Start in background
job = sim.run(wait=False, monitor=True)

# Do other work while simulation runs
while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
    time.sleep(5)

# Get results when complete
results = job.get_results()
```

## Execution Parameters

### Threading (OpenMP)

Use multiple cores on single machine:

```python
# Use 4 threads
results = sim.run(n_threads=4)

# Use 8 threads
results = sim.run(n_threads=8)
```

### MPI Parallelization

Distribute across multiple meshes:

```python
# 4 MPI processes
results = sim.run(n_mpi=4)

# Combine MPI and OpenMP
results = sim.run(n_mpi=2, n_threads=4)  # 2 processes × 4 threads = 8 cores
```

### Output Directory

Specify where to run:

```python
results = sim.run(
    output_dir='./outputs',
    n_threads=4
)
```

## Progress Monitoring

### Real-Time Progress

```python
job = sim.run(wait=False, monitor=True)

while job.is_running():
    if job.progress is not None:
        print(f"Progress: {job.progress:.1f}%")
    if job.estimated_time_remaining:
        mins = job.estimated_time_remaining / 60
        print(f"ETA: {mins:.1f} minutes")
    time.sleep(10)
```

### Progress Callback

```python
def progress_callback(progress: float, eta: float):
    print(f"Simulation: {progress:.1f}% complete, ETA: {eta:.0f}s")

job = sim.run(
    wait=False,
    monitor=True,
    callback=progress_callback
)
```

## Error Handling

```python
from pyfds import FDSExecutionError

try:
    results = sim.run(n_threads=4)
except FDSExecutionError as e:
    print(f"Simulation failed: {e}")
    # Check log file for details
```

## Complete Example

```python
from pyfds import Simulation
import time

# Create simulation
sim = Simulation(chid='demo')
sim.add(Time(t_end=300.0)
sim.add(Mesh(ijk=Grid3D.of(30, 30, 15), xb=Bounds3D.of(0, 3, 0, 3, 0, 1.5))
sim.surface(id='FIRE', hrrpua=1000.0)
sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id='FIRE')
sim.device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(1.5, 1.5, 1.4))

# Run with progress monitoring
print("Starting simulation...")
job = sim.run(wait=False, monitor=True, n_threads=4)

while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
    time.sleep(10)

# Get and analyze results
results = job.get_results()
print(f"\nSimulation complete!")
print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
print(f"Peak Temperature: {results.devices['TEMP'].max():.1f} °C")
```

## Next Steps

- [Job Management](jobs.md) - Control running jobs
- [Results Analysis](analysis.md) - Work with output
- [Visualization](visualization.md) - Create plots

---

[Job Management →](jobs.md){ .md-button .md-button--primary }
