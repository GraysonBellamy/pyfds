# Job Management

Manage background FDS simulations and job queues with PyFDS.

## Overview

The Job management system allows you to:

- Run simulations in the background
- Monitor job progress
- Manage multiple concurrent jobs
- Queue jobs for sequential execution
- Handle job failures and restarts

```python
from pyfds import Simulation

sim = Simulation(chid='my_simulation')
# ... configure simulation ...

# Run in background
job = sim.run(wait=False)

# Check status
print(f"Job status: {job.status}")
```

## Basic Job Control

### Running Jobs in Background

```python
from pyfds import Simulation

sim = Simulation(chid='background_job')
sim.add(Time(t_end=600.0)
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

# Run without waiting
job = sim.run(wait=False)

# Continue with other work while simulation runs
print("Simulation running in background...")
print(f"Job ID: {job.id}")
print(f"PID: {job.pid}")

# Do other work here...

# Wait for completion when needed
job.wait()
print("Simulation complete!")
```

### Checking Job Status

```python
# Get job status
status = job.status  # 'running', 'completed', 'failed', 'queued'

# Check if job is still running
if job.is_running():
    print(f"Job is running (PID: {job.pid})")

# Check if job completed
if job.is_complete():
    print("Job finished successfully")

# Check if job failed
if job.has_failed():
    print(f"Job failed: {job.error_message}")
```

### Job Properties

```python
# Access job information
print(f"CHID: {job.chid}")
print(f"Status: {job.status}")
print(f"Start time: {job.start_time}")
print(f"Elapsed time: {job.elapsed_time}s")
print(f"FDS file: {job.fds_file}")
print(f"Output directory: {job.output_dir}")
```

## Monitoring Job Progress

### Progress Callbacks

```python
def progress_callback(job, progress):
    """Called periodically with job progress."""
    print(f"Progress: {progress:.1f}%")
    print(f"Simulation time: {job.current_time:.1f}/{job.total_time:.1f}s")

# Run with progress monitoring
job = sim.run(
    wait=False,
    monitor=True,
    callback=progress_callback,
    callback_interval=10.0  # Call every 10 seconds
)
```

### Real-Time Output

```python
# Monitor FDS output in real-time
job = sim.run(wait=False, monitor=True)

# Stream output
for line in job.stream_output():
    print(line, end='')

    # Check for specific messages
    if 'STOP' in line:
        print("Simulation stopping...")
```

### Job Metrics

```python
# Get job performance metrics
metrics = job.get_metrics()

print(f"CPU time: {metrics['cpu_time']:.1f}s")
print(f"Wall time: {metrics['wall_time']:.1f}s")
print(f"Memory usage: {metrics['memory_mb']:.0f} MB")
print(f"Time steps: {metrics['time_steps']}")
print(f"Cells: {metrics['total_cells']}")
```

## Job Control

### Pausing and Resuming

```python
# Pause a running job
job.pause()
print("Job paused")

# Resume paused job
job.resume()
print("Job resumed")
```

### Stopping Jobs

```python
# Stop job gracefully
job.stop()

# Force terminate if needed
job.kill()
```

### Job Restart

```python
# Restart failed job
if job.has_failed():
    print("Restarting failed job...")
    new_job = job.restart()
```

## Multiple Jobs

### Running Jobs in Parallel

```python
from pyfds import Simulation

# Create multiple simulations
jobs = []

for hrr in [500, 1000, 1500]:
    sim = Simulation(chid=f'fire_{hrr}')
    sim.add(Time(t_end=600.0)
    sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

    sim.surface(id='FIRE', hrrpua=hrr)
    sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

    # Run in background
    job = sim.run(wait=False, n_threads=2)
    jobs.append(job)

print(f"Running {len(jobs)} jobs in parallel...")

# Wait for all jobs to complete
for job in jobs:
    job.wait()
    print(f"{job.chid}: {job.status}")

print("All jobs complete!")
```

### Job Queue

```python
from pyfds import JobQueue

# Create job queue (runs jobs sequentially)
queue = JobQueue(max_concurrent=2)

# Add jobs to queue
for hrr in [500, 750, 1000, 1250, 1500]:
    sim = Simulation(chid=f'fire_{hrr}')
    # ... configure simulation ...

    queue.add(sim, n_threads=4)

# Start processing queue
print(f"Queue size: {queue.size}")
queue.start()

# Monitor queue
while not queue.is_complete():
    print(f"Running: {queue.running_count}/{queue.max_concurrent}")
    print(f"Completed: {queue.completed_count}/{queue.total_count}")
    print(f"Failed: {queue.failed_count}")
    time.sleep(10)

# Get results
results = queue.get_results()
for chid, status in results.items():
    print(f"{chid}: {status}")
```

## Job Persistence

### Saving Job State

```python
# Save job information for later
job.save_state('job_state.json')

# Load job state
from pyfds import Job

job = Job.load_state('job_state.json')

# Check if still running
if job.is_running():
    print("Job is still running")
    job.wait()
```

### Job Database

```python
from pyfds import JobDatabase

# Create database to track all jobs
db = JobDatabase('jobs.db')

# Add job to database
db.add_job(job)

# Query jobs
running_jobs = db.get_running_jobs()
failed_jobs = db.get_failed_jobs()
completed_jobs = db.get_completed_jobs()

# Get job by CHID
job = db.get_job('my_simulation')

# Update job status
db.update_status(job.id, 'completed')
```

## Complete Examples

### Parametric Study with Job Queue

```python
from pyfds import Simulation, JobQueue
import itertools

# Create job queue
queue = JobQueue(max_concurrent=4)

# Parameter combinations
hrr_values = [500, 1000, 1500]
door_widths = [0.75, 1.0, 1.5]

cases = list(itertools.product(hrr_values, door_widths))

# Create and queue all jobs
for i, (hrr, width) in enumerate(cases):
    sim = Simulation(chid=f'case_{i+1:03d}')
    sim.add(Time(t_end=600.0)
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

    sim.surface(id='FIRE', hrrpua=hrr)
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

    y_center = 2.5
    sim.add(Vent(xb=Bounds3D.of(6, 6, y_center-width/2, y_center+width/2, 0, 2.1), surf_id='OPEN')

    sim.device(id='TEMP', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4))

    queue.add(sim, n_threads=2)

print(f"Queued {len(cases)} jobs")

# Start queue and monitor
queue.start()

while not queue.is_complete():
    status = queue.get_status()
    print(f"\rRunning: {status['running']}, Completed: {status['completed']}/{status['total']}, Failed: {status['failed']}", end='')
    time.sleep(5)

print("\n\nAll jobs complete!")

# Check for failures
if queue.failed_count > 0:
    print(f"\n{queue.failed_count} jobs failed:")
    for job in queue.get_failed_jobs():
        print(f"  - {job.chid}: {job.error_message}")
```

### Monitored Long-Running Simulation

```python
from pyfds import Simulation
import time

def progress_callback(job, progress):
    """Monitor long simulation."""
    print(f"\n[{job.chid}] Progress: {progress:.1f}%")
    print(f"  Simulation time: {job.current_time:.1f}/{job.total_time:.1f}s")
    print(f"  Wall time: {job.elapsed_time:.0f}s")
    print(f"  ETA: {job.estimated_time_remaining:.0f}s")

sim = Simulation(chid='long_simulation')
sim.add(Time(t_end=3600.0)  # 1 hour simulation time
sim.add(Mesh(ijk=Grid3D.of(100, 100, 50), xb=Bounds3D.of(0, 10, 0, 10, 0, 5))

# Large fire
sim.surface(id='FIRE', hrrpua=2000.0)
sim.add(Obstruction(xb=Bounds3D.of(4, 6, 4, 6, 0, 0.5), surf_id='FIRE')

# Run with monitoring
job = sim.run(
    wait=False,
    monitor=True,
    callback=progress_callback,
    callback_interval=60.0,  # Update every minute
    n_threads=8
)

# Wait for completion
job.wait()

if job.is_complete():
    print("\n✓ Simulation completed successfully")
    print(f"Total time: {job.elapsed_time:.0f}s")
else:
    print(f"\n✗ Simulation failed: {job.error_message}")
```

### Job Recovery

```python
from pyfds import JobDatabase
import time

# Database to track jobs
db = JobDatabase('simulation_jobs.db')

# Function to run with auto-recovery
def run_with_recovery(sim, max_retries=3):
    """Run simulation with automatic retry on failure."""
    for attempt in range(max_retries):
        print(f"Attempt {attempt + 1}/{max_retries}")

        job = sim.run(wait=False)
        db.add_job(job)

        job.wait()

        if job.is_complete():
            print("✓ Success")
            db.update_status(job.id, 'completed')
            return job
        else:
            print(f"✗ Failed: {job.error_message}")
            db.update_status(job.id, 'failed')

            if attempt < max_retries - 1:
                print("Retrying...")
                time.sleep(5)

    print("Max retries exceeded")
    return None

# Use it
sim = Simulation(chid='retry_test')
# ... configure ...

job = run_with_recovery(sim, max_retries=3)
```

## Best Practices

### 1. Resource Management

```python
# Limit concurrent jobs based on available resources
import multiprocessing

max_jobs = multiprocessing.cpu_count() // 4  # 4 threads per job
queue = JobQueue(max_concurrent=max_jobs)
```

### 2. Error Handling

```python
try:
    job = sim.run(wait=False)
    job.wait()

    if job.has_failed():
        raise RuntimeError(f"Simulation failed: {job.error_message}")

except Exception as e:
    print(f"Error: {e}")
    # Handle failure
```

### 3. Job Logging

```python
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def logged_callback(job, progress):
    logger.info(f"{job.chid}: {progress:.1f}% complete")

job = sim.run(wait=False, monitor=True, callback=logged_callback)
```

### 4. Clean Termination

```python
import signal
import sys

jobs = []

def cleanup(signum, frame):
    """Clean up jobs on exit."""
    print("\nCleaning up...")
    for job in jobs:
        if job.is_running():
            job.stop()
    sys.exit(0)

signal.signal(signal.SIGINT, cleanup)

# Run jobs
# ...
```

## Next Steps

- [Running Simulations](running.md) - Execution basics
- [Analysis](analysis.md) - Processing results
- [Examples](../examples/workflows.md) - Complete workflows
- [API Reference](../api/execution/job.md) - Job API documentation

---

[Visualization →](visualization.md){ .md-button .md-button--primary }
