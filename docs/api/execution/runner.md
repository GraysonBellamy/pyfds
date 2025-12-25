# FDSRunner

!!! note "Auto-Generated Documentation"
    The API reference will be automatically populated from source code docstrings using mkdocstrings once the implementation is complete.

## Overview

The `FDSRunner` class executes FDS simulations and manages the FDS process.

## Basic Usage

```python
from pyfds import Simulation
from pyfds.execution import FDSRunner

# Create simulation
sim = Simulation(chid='test')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
sim.write('test.fds')

# Run FDS
runner = FDSRunner()
job = runner.run('test.fds', wait=True)

if job.is_complete():
    print("Simulation completed successfully!")
```

## Running Simulations

### Synchronous Execution

Wait for simulation to complete:

```python
runner = FDSRunner()
job = runner.run('simulation.fds', wait=True)

# Job is complete when this returns
print(f"Exit code: {job.exit_code}")
```

### Asynchronous Execution

Run in background:

```python
runner = FDSRunner()
job = runner.run('simulation.fds', wait=False)

# Do other work while simulation runs
print(f"Job PID: {job.pid}")
print(f"Status: {job.status}")

# Wait when ready
job.wait()
```

### With Threading

Parallel execution with OpenMP:

```python
# Run with 4 OpenMP threads
runner = FDSRunner()
job = runner.run('simulation.fds', n_threads=4, wait=True)
```

### With MPI

Distributed execution (requires MPI-enabled FDS):

```python
# Run with 4 MPI processes
runner = FDSRunner()
job = runner.run('simulation.fds', n_mpi=4, wait=True)
```

## Configuration

### FDS Executable

Specify FDS executable path:

```python
runner = FDSRunner(fds_command='/path/to/fds')
job = runner.run('simulation.fds')
```

Or use environment variable:

```bash
export FDS_COMMAND=/path/to/fds
```

### Working Directory

Set working directory for execution:

```python
runner = FDSRunner(working_dir='/path/to/simulations')
job = runner.run('test.fds')
```

## Checking FDS Installation

### Verify Installation

```python
from pyfds.execution import check_fds_installed

if check_fds_installed():
    print("FDS is installed")
else:
    print("FDS not found")
```

### Get FDS Version

```python
from pyfds.execution import get_fds_version

version = get_fds_version()
print(f"FDS version: {version}")
# Output: FDS version: 6.8.0
```

## Job Management

### Job Object

The `run()` method returns a `Job` object:

```python
job = runner.run('simulation.fds', wait=False)

# Job properties
print(job.chid)          # 'simulation'
print(job.pid)           # Process ID
print(job.status)        # 'running', 'completed', 'failed'
print(job.start_time)    # Start timestamp
print(job.elapsed_time)  # Elapsed seconds
```

### Monitoring Progress

```python
def progress_callback(job, progress):
    """Called periodically with progress updates."""
    print(f"Progress: {progress:.1f}%")
    print(f"Time: {job.current_time:.1f}/{job.total_time:.1f}s")

job = runner.run(
    'simulation.fds',
    wait=False,
    monitor=True,
    callback=progress_callback,
    callback_interval=10.0  # Every 10 seconds
)

job.wait()
```

### Streaming Output

```python
job = runner.run('simulation.fds', wait=False)

# Stream FDS output in real-time
for line in job.stream_output():
    print(line, end='')

    # Check for specific messages
    if 'STOP' in line:
        print("\nSimulation stopping...")
```

## Error Handling

### Check for Errors

```python
job = runner.run('simulation.fds', wait=True)

if job.has_failed():
    print(f"Simulation failed: {job.error_message}")
    print(f"Exit code: {job.exit_code}")
else:
    print("Simulation completed successfully")
```

### Timeout

Set maximum execution time:

```python
runner = FDSRunner()
job = runner.run(
    'simulation.fds',
    wait=True,
    timeout=3600  # Max 1 hour
)

if job.status == 'timeout':
    print("Simulation timed out")
```

### Retry on Failure

```python
def run_with_retry(fds_file, max_attempts=3):
    """Run simulation with automatic retry."""
    runner = FDSRunner()

    for attempt in range(max_attempts):
        print(f"Attempt {attempt + 1}/{max_attempts}")

        job = runner.run(fds_file, wait=True)

        if job.is_complete():
            return job

        print(f"Failed: {job.error_message}")
        if attempt < max_attempts - 1:
            print("Retrying...")

    raise RuntimeError("Max retry attempts exceeded")

job = run_with_retry('simulation.fds')
```

## Advanced Usage

### Custom Environment

Set environment variables for FDS:

```python
import os

env = os.environ.copy()
env['OMP_NUM_THREADS'] = '4'
env['FDS_DEBUG'] = '1'

runner = FDSRunner(env=env)
job = runner.run('simulation.fds')
```

### Capture Output

Redirect stdout/stderr:

```python
runner = FDSRunner()
job = runner.run(
    'simulation.fds',
    wait=True,
    stdout='simulation.out',
    stderr='simulation.err'
)

# Read captured output
with open('simulation.out') as f:
    output = f.read()
```

### Process Priority

Set process priority (Unix):

```python
import os

runner = FDSRunner()
job = runner.run('simulation.fds', wait=False)

# Lower priority (nice +10)
os.nice(10)
```

## Integration with Simulation

### Direct Execution

Run directly from `Simulation` object:

```python
sim = Simulation(chid='test')
sim.add(Time(t_end=600.0))
sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

# Write and run in one step
job = sim.run(wait=True, n_threads=4)

if job.is_complete():
    print("Done!")
```

### With Validation

Validate before running:

```python
sim = Simulation(chid='test')
# ... configure simulation ...

if not sim.is_valid():
    errors = sim.validate()
    print(f"Validation errors: {errors}")
else:
    job = sim.run(wait=True)
```

## Common Patterns

### Batch Execution

Run multiple simulations:

```python
runner = FDSRunner()
jobs = []

for i, fds_file in enumerate(fds_files):
    print(f"Starting {fds_file}...")
    job = runner.run(fds_file, wait=False, n_threads=2)
    jobs.append(job)

# Wait for all to complete
for job in jobs:
    job.wait()
    status = "✓" if job.is_complete() else "✗"
    print(f"{status} {job.chid}")
```

### Sequential Queue

Run simulations one at a time:

```python
from pyfds.execution import JobQueue

queue = JobQueue(max_concurrent=1)

for fds_file in fds_files:
    queue.add(fds_file, n_threads=8)

queue.start()

while not queue.is_complete():
    print(f"Progress: {queue.completed_count}/{queue.total_count}")
    time.sleep(10)
```

### Parallel Execution

Run multiple simulations in parallel:

```python
from concurrent.futures import ThreadPoolExecutor

def run_simulation(fds_file):
    """Run single simulation."""
    runner = FDSRunner()
    job = runner.run(fds_file, wait=True, n_threads=2)
    return job.chid, job.is_complete()

# Run 4 simulations in parallel
with ThreadPoolExecutor(max_workers=4) as executor:
    results = executor.map(run_simulation, fds_files)

    for chid, success in results:
        print(f"{chid}: {'Success' if success else 'Failed'}")
```

## Platform Support

### Windows

```python
# Use fds.exe on Windows
runner = FDSRunner(fds_command='fds.exe')
job = runner.run('simulation.fds')
```

### Linux/macOS

```python
# Standard FDS binary
runner = FDSRunner(fds_command='fds')
job = runner.run('simulation.fds')
```

### Docker

Run FDS in Docker container:

```python
runner = FDSRunner(
    fds_command='docker',
    fds_args=['run', '--rm', '-v', f'{os.getcwd()}:/workdir',
              'fds-image', 'fds']
)
job = runner.run('simulation.fds')
```

## Troubleshooting

### FDS Not Found

```python
from pyfds.execution import check_fds_installed

if not check_fds_installed():
    print("FDS not found in PATH")
    print("Set FDS_COMMAND environment variable")
    print("Or install FDS: https://pages.nist.gov/fds-smv/")
```

### Permission Denied

```bash
# Make FDS executable
chmod +x /path/to/fds
```

### OpenMP Issues

```bash
# Set number of threads explicitly
export OMP_NUM_THREADS=4
```

## See Also

- [Job](job.md) - Job management
- [Monitor](monitor.md) - Progress monitoring
- [Running Guide](../../execution/running.md) - Execution guide
- [Exceptions](../exceptions.md) - Error handling

---

[Job →](job.md){ .md-button .md-button--primary }
[Back to API →](../index.md){ .md-button }
