# Job

::: pyfds.execution.Job

## Overview

The `Job` class represents a running or completed FDS simulation. It provides methods to monitor progress, wait for completion, and retrieve results.

Jobs are typically created by calling `FDSRunner.run()` with `wait=False`.

## Creating Jobs

```python
from pyfds import run_fds
from pyfds.execution import FDSRunner

# Non-blocking execution returns a Job
runner = FDSRunner()
job = runner.run("simulation.fds", wait=False)

# Or using convenience function
job = run_fds("simulation.fds", wait=False)
```

## Monitoring Progress

### Check if Running

```python
job = run_fds("simulation.fds", wait=False)

while job.is_running():
    print("Simulation still running...")
    time.sleep(5)

print("Simulation complete!")
```

### Get Progress Percentage

```python
job = run_fds("simulation.fds", wait=False)

while job.is_running():
    progress = job.progress
    print(f"Progress: {progress:.1f}%")
    time.sleep(10)
```

### Get Detailed Progress Info

```python
job = run_fds("simulation.fds", wait=False)

while job.is_running():
    info = job.progress_info
    if info:
        print(f"Time step: {info.current_time:.1f}s / {info.total_time:.1f}s")
        print(f"Progress: {info.percent_complete:.1f}%")
        print(f"ETA: {info.eta_seconds:.0f} seconds")
    time.sleep(10)
```

### Estimated Time Remaining

```python
job = run_fds("simulation.fds", wait=False)

while job.is_running():
    eta = job.estimated_time_remaining
    if eta is not None:
        print(f"Estimated time remaining: {eta:.0f} seconds")
    time.sleep(10)
```

## Waiting for Completion

### Wait Indefinitely

```python
job = run_fds("simulation.fds", wait=False)

# Do other work...

# Wait for completion and get results
results = job.wait()
print(f"Max HRR: {results.hrr.max()}")
```

### Wait with Timeout

```python
from pyfds.exceptions import FDSTimeoutError

job = run_fds("simulation.fds", wait=False)

try:
    results = job.wait(timeout=3600)  # 1 hour timeout
except FDSTimeoutError:
    print("Simulation timed out")
    job.kill()
```

## Controlling Jobs

### Kill Job

```python
job = run_fds("simulation.fds", wait=False)

# Cancel if taking too long
if job.progress < 10 and time.time() - start_time > 300:
    print("Simulation not making progress, killing...")
    job.kill()
```

### Request Stop

```python
job = run_fds("simulation.fds", wait=False)

# Gracefully request FDS to stop
job.request_stop()

# Wait for graceful shutdown
try:
    results = job.wait(timeout=60)
except FDSTimeoutError:
    # Force kill if graceful shutdown fails
    job.kill()
```

## Job Properties

### Exit Code

```python
job = run_fds("simulation.fds", wait=False)
job.wait()

if job.exit_code == 0:
    print("Simulation completed successfully")
else:
    print(f"Simulation failed with exit code {job.exit_code}")
```

### File Paths

```python
job = run_fds("simulation.fds", wait=False)

print(f"Input file: {job.fds_file}")
print(f"Output directory: {job.output_dir}")
print(f"CHID: {job.chid}")
```

## Error Handling

### Execution Errors

```python
from pyfds.exceptions import ExecutionError

job = run_fds("simulation.fds", wait=False)

try:
    results = job.wait()
except ExecutionError as e:
    print(f"Execution failed: {e}")
    print(f"Exit code: {e.exit_code}")
    print(f"Stdout: {e.stdout}")
    print(f"Stderr: {e.stderr}")
```

### Timeout Handling

```python
from pyfds.exceptions import FDSTimeoutError

job = run_fds("long_simulation.fds", wait=False)

try:
    results = job.wait(timeout=7200)  # 2 hours
except FDSTimeoutError as e:
    print(f"Simulation timed out: {e}")
    job.kill()
    # Inspect partial results
    print(f"Made it to {job.progress:.1f}% before timeout")
```

## Common Patterns

### Progress Bar

```python
import time
from tqdm import tqdm

job = run_fds("simulation.fds", wait=False)

with tqdm(total=100, desc="Simulation Progress") as pbar:
    last_progress = 0
    while job.is_running():
        current = job.progress
        pbar.update(current - last_progress)
        last_progress = current
        time.sleep(5)
    pbar.update(100 - last_progress)

results = job.wait()
```

### Parallel Jobs

```python
from concurrent.futures import ThreadPoolExecutor

def run_simulation(fds_file):
    """Run simulation and return results."""
    job = run_fds(fds_file, wait=False)
    return job.wait()

# Run multiple simulations in parallel
sim_files = ["sim1.fds", "sim2.fds", "sim3.fds"]

with ThreadPoolExecutor(max_workers=3) as executor:
    futures = [executor.submit(run_simulation, f) for f in sim_files]
    results = [f.result() for f in futures]
```

### Monitoring Multiple Jobs

```python
jobs = []
for fds_file in ["sim1.fds", "sim2.fds", "sim3.fds"]:
    job = run_fds(fds_file, wait=False)
    jobs.append(job)

# Monitor all jobs
while any(job.is_running() for job in jobs):
    for i, job in enumerate(jobs):
        if job.is_running():
            print(f"Job {i+1}: {job.progress:.1f}%")
    time.sleep(10)

# Collect results
results = [job.wait() for job in jobs]
```

### Conditional Stopping

```python
job = run_fds("simulation.fds", wait=False)

while job.is_running():
    # Check progress info
    info = job.progress_info
    if info and info.current_time > 300:
        # Check some condition from partial output
        out_file = job.output_dir / f"{job.chid}.out"
        if out_file.exists():
            content = out_file.read_text()
            if "INSTABILITY" in content:
                print("Instability detected, stopping simulation")
                job.request_stop()
                break
    time.sleep(10)

try:
    results = job.wait(timeout=60)
except FDSTimeoutError:
    job.kill()
```

## Integration with Results

```python
# Start simulation
job = run_fds("simulation.fds", wait=False)

# Monitor and wait
while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
    time.sleep(10)

# Get results
results = job.wait()

# Analyze results
print(f"Peak HRR: {results.hrr.max():.1f} kW")
print(f"Max temperature: {results.max_temp:.1f} °C")

# Plot results
results.plot_hrr()
results.plot_temperature("TEMP_1")
```

## See Also

- [FDSRunner](runner.md) - Creating and managing jobs
- [Results](../analysis/results.md) - Working with simulation results
- [Exceptions](../exceptions.md) - Error handling
- [Running Simulations](../../execution/running.md) - User guide
