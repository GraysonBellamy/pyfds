# FDS User Guide Chapter 3 Analysis: Running FDS

## Executive Summary

This document analyzes FDS User Guide Chapter 3 "Running FDS" (lines 354-620) against the current PyFDS implementation to identify:
- ✅ Features we have implemented
- ❌ Features we are missing
- 🔧 Architectural improvements needed to align with FDS best practices

## Chapter 3 Structure Overview

Chapter 3 covers the operational aspects of running FDS simulations:

1. **Computer Basics** (§3.1)
   - Hardware understanding (sockets, cores, logical processors)
   - OpenMP vs MPI parallelization strategies

2. **Launching an FDS Job** (§3.2)
   - Platform-specific execution (Windows, macOS, Linux)
   - Single and multi-computer configurations
   - Combining OpenMP and MPI

3. **Efficiency of Multi-Process Simulations** (§3.3)
   - Weak vs strong scaling analysis
   - CPU profiling with `CHID_cpu.csv`

4. **Monitoring Progress** (§3.4)
   - `.out` file diagnostics
   - Live monitoring with Smokeview
   - Stopping simulations gracefully
   - Restart functionality

---

## Feature Analysis

### 1. Computer Hardware Understanding (§3.1.1)

**FDS Guide Coverage:**
- Explains sockets, cores, logical processors
- Platform-specific commands: `lscpu` (Linux), `sysctl hw` (macOS)
- Emphasis on understanding available compute resources

**PyFDS Implementation:**
✅ **Implemented:**
- N/A - This is educational content for users, not a programming feature

❌ **Missing:**
- Could add utility function to detect available cores/threads
- Could provide recommendations for optimal thread counts

**Recommendation:**
```python
# Potential addition to pyfds.utils
def get_system_info() -> dict:
    """Get system compute resources for FDS optimization."""
    return {
        'logical_cores': os.cpu_count(),
        'recommended_omp_threads': os.cpu_count() // 2,
        'platform': platform.system(),
    }
```

---

### 2. Parallel Processing Strategies (§3.1.2)

**FDS Guide Coverage:**

#### 2.1 OpenMP (§3.1.2.1)
- Single mesh or multi-mesh on one computer
- Default: ~50% of available cores
- Set via `OMP_NUM_THREADS` environment variable
- Best for exploiting cores on a single physical processor

**PyFDS Implementation:**
✅ **Implemented:**
- `n_threads` parameter in `Simulation.run()` and `FDSRunner.run()`
- Sets `OMP_NUM_THREADS` environment variable correctly
- See: [runner.py:269](src/pyfds/execution/runner.py#L269), [process.py:182-207](src/pyfds/execution/process.py#L182-L207)

❌ **Missing:**
- No automatic detection/recommendation of optimal thread count
- No warning when user requests more threads than logical cores
- No documentation of the 50% guideline from FDS

**Example from PyFDS:**
```python
# ✅ We support this
results = sim.run(n_threads=4)  # Sets OMP_NUM_THREADS=4
```

#### 2.2 MPI (§3.1.2.2)
- Multiple computers or multiple cores
- Requires multi-mesh domain decomposition
- Each mesh typically assigned to one MPI process
- Processes numbered 0, 1, 2... (meshes numbered 1, 2, 3...)

**PyFDS Implementation:**
✅ **Implemented:**
- `n_mpi` parameter in `Simulation.run()` and `FDSRunner.run()`
- Uses `mpiexec -n <n_mpi>` correctly
- See: [runner.py:270](src/pyfds/execution/runner.py#L270), [process.py:166-168](src/pyfds/execution/process.py#L166-L168)

❌ **Missing:**
- No validation that number of meshes matches number of MPI processes
- No support for assigning multiple meshes to single MPI process
- No support for `mpiexec` advanced options (machine file, host specification)
- No MPI library detection (Intel MPI vs Open MPI)

**Example from PyFDS:**
```python
# ✅ We support basic MPI
results = sim.run(n_mpi=4)  # Runs mpiexec -n 4 fds file.fds

# ❌ We don't validate mesh count
sim.mesh(...)  # Only 1 mesh defined
results = sim.run(n_mpi=4)  # Should warn: only 1 mesh but 4 MPI processes!
```

#### 2.3 Combined OpenMP + MPI
- 4 MPI processes × 8 OpenMP threads = 32 total cores
- MPI provides most speedup, OpenMP up to ~2× additional

**PyFDS Implementation:**
✅ **Implemented:**
- Both parameters supported simultaneously
- Correctly sets environment and command
- See: [runner.py:267-271](src/pyfds/execution/runner.py#L267-L271)

```python
# ✅ We support this
results = sim.run(n_mpi=4, n_threads=8)
```

---

### 3. Launching FDS Jobs (§3.2)

#### 3.1 Windows (§3.2.1)

**FDS Guide Coverage:**
```bash
fds_local -p 4 -o 2 job_name.fds  # 4 MPI procs × 2 OpenMP threads
```
- Special `CMDfds` command prompt
- `fds_local` script wrapper
- `-p` parameter for MPI processes
- `-o` parameter for OpenMP threads
- Redirection: `fds_local ... job_name.fds > job_name.err`

**PyFDS Implementation:**
❌ **Not Implemented:**
- No `fds_local` wrapper script detection
- No `-p` and `-o` flag support (we use `mpiexec` directly)
- We call `fds` directly, not through `fds_local`

**Architectural Issue:**
PyFDS currently bypasses the FDS-provided wrapper scripts. On Windows, this may cause issues with library paths and environment setup.

**Recommendation:**
```python
# Should detect and use fds_local on Windows
def find_fds_command(platform: str) -> tuple[str, bool]:
    """Find appropriate FDS launch command."""
    if platform == "Windows":
        # Look for fds_local script
        fds_local = shutil.which("fds_local")
        if fds_local:
            return fds_local, True  # Uses -p/-o flags

    # Fall back to direct fds executable
    return find_fds_executable(), False  # Uses mpiexec
```

#### 3.2 Windows Multi-Computer (§3.2.2)

**FDS Guide Coverage:**
```bash
mpiexec -register  # First time setup
mpiexec -wdir <dir> -machinefile hosts.txt fds job_name.fds
mpiexec ... -env OMP_NUM_THREADS 4 fds_openmp job_name.fds
```
- Domain network requirement
- `mpiexec -register` for credentials
- Machine file: `fred:3` (computer:processes)
- `-wdir` for working directory
- `-env` for environment variables

**PyFDS Implementation:**
❌ **Not Implemented:**
- No machine file support
- No `-wdir` support
- No `-env` flag support (we use Python's `env` parameter instead)
- No multi-computer configuration at all

**Gap:** PyFDS only supports single-machine execution (either OpenMP or MPI on one machine).

#### 3.3 macOS (§3.2.3)

**FDS Guide Coverage:**
```bash
fds job_name.fds
export OMP_NUM_THREADS=4
fds_openmp job_name.fds
mpiexec -n 4 fds job_name.fds
mpiexec -n 4 -x OMP_NUM_THREADS=4 fds_openmp job_name.fds
```

**PyFDS Implementation:**
✅ **Implemented:**
- OpenMP via environment variable ✅
- MPI via `mpiexec -n` ✅
- Combined OpenMP+MPI ✅

❌ **Missing:**
- No detection of `fds` vs `fds_openmp` executables
- We assume single `fds` executable (FDS 6.2.0+ convention)

#### 3.4 Linux (§3.2.4)

**FDS Guide Coverage:**
```bash
# Job scheduler (PBS/Torque/Slurm)
mpiexec -n 4 -host node001,node002 /path/to/fds job_name.fds
mpiexec -n 4 -genv OMP_NUM_THREADS 4 /path/to/fds_openmp job_name.fds
```

**PyFDS Implementation:**
✅ **Implemented:**
- Basic `mpiexec -n` ✅

❌ **Missing:**
- No `-host` specification for multi-node
- No `-genv` for global environment variables
- No job scheduler integration (PBS/Slurm)
- No cluster support

**Gap:** PyFDS cannot execute on compute clusters or multi-node environments.

#### 3.5 Very Large Jobs (§3.2.5)

**FDS Guide Coverage:**
- Use MPI only (no OpenMP)
- Set `DT_CPU` on `&DUMP` line for CPU profiling
- Practice with restart feature
- Strong scaling studies

**PyFDS Implementation:**
❌ **Not Implemented:**
- No CPU profiling integration
- No restart file support
- No scaling study utilities

---

### 4. Efficiency Monitoring (§3.3)

**FDS Guide Coverage:**
- `CHID_cpu.csv` file analysis
- Weak scaling: $E_w = t_1 / t_N$
- Strong scaling: $E_s = t_1 / (N \cdot t_N)$
- COMM (communication) timing
- Major routine breakdown: VELO, MASS, MAIN

**PyFDS Implementation:**
❌ **Not Implemented:**
- No CPU profiling data parsing
- No efficiency analysis tools
- No scaling study utilities

**Recommendation:**
```python
# Potential Phase 3 feature
class CPUProfile:
    """Parse and analyze CHID_cpu.csv for performance insights."""

    def calculate_weak_scaling(self, runs: list[Results]) -> dict:
        """Calculate weak scaling efficiency."""
        pass

    def calculate_strong_scaling(self, runs: list[Results]) -> dict:
        """Calculate strong scaling efficiency."""
        pass

    def identify_bottlenecks(self) -> list[str]:
        """Identify performance bottlenecks from CPU timing."""
        pass
```

---

### 5. Progress Monitoring (§3.4)

**FDS Guide Coverage:**
- `CHID.out` file for diagnostics
- `&DUMP DIAGNOSTICS_INTERVAL=100` (every 100 timesteps)
- `&DUMP SUPPRESS_DIAGNOSTICS=T` for large MPI jobs (>32 meshes)
- Create `CHID.stop` file to gracefully stop
- Restart feature with `&DUMP` parameters
- Control functions for dynamic control (§CTRL)

**PyFDS Implementation:**

✅ **Implemented:**
- `.out` file monitoring via `ProgressMonitor`
- Real-time progress tracking
- `Job.is_running()`, `Job.progress`, `Job.progress_info`
- See: [monitor.py](src/pyfds/execution/monitor.py), [runner.py:89-121](src/pyfds/execution/runner.py#L89-L121)

❌ **Missing:**
- No `CHID.stop` file creation utility
- No restart file support
- No parsing of `DIAGNOSTICS_INTERVAL`
- No detection of `SUPPRESS_DIAGNOSTICS`
- No BNDF velocity error monitoring
- No control function integration

**Example:**
```python
# ✅ We support progress monitoring
job = sim.run(wait=False, monitor=True)
while job.is_running():
    print(f"Progress: {job.progress:.1f}%")
    print(f"ETA: {job.estimated_time_remaining:.0f}s")

# ❌ We don't support graceful stopping
# Should add:
job.request_stop()  # Creates CHID.stop file
```

---

## Summary: Implementation Coverage

### ✅ Well Implemented (80-100%)

1. **Basic OpenMP execution** - `n_threads` parameter, `OMP_NUM_THREADS` environment
2. **Basic MPI execution** - `mpiexec -n` for single machine
3. **Combined OpenMP+MPI** - Both parameters work together
4. **Progress monitoring** - Real-time .out file parsing
5. **Non-blocking execution** - `wait=False` for background jobs
6. **Basic process management** - Start, monitor, wait, kill

### 🟡 Partially Implemented (30-70%)

1. **Platform-specific execution** - Works on all platforms but doesn't use native wrappers
2. **FDS executable detection** - Basic but doesn't handle `fds_local`, `fds_openmp` variants
3. **Error handling** - Parses .out for errors but limited

### ❌ Not Implemented (0-30%)

1. **Multi-computer/cluster execution** - No machine file, no `-host` specification
2. **Windows-specific features** - No `fds_local`, no `-p/-o` flags
3. **CPU profiling** - No `CHID_cpu.csv` parsing
4. **Scaling studies** - No efficiency analysis utilities
5. **Restart functionality** - No restart file support
6. **Graceful stopping** - No `CHID.stop` file creation
7. **Job scheduler integration** - No PBS/Slurm support
8. **Advanced MPI options** - No `-genv`, `-wdir`, `-env` flags
9. **Resource validation** - No mesh/MPI count validation
10. **Performance recommendations** - No optimal thread count suggestions

---

## Architectural Improvements Recommended

### Priority 1: Critical for Robustness

#### 1.1 Validation of Parallel Configuration
```python
class ParallelValidator:
    """Validate parallel execution configuration."""

    def validate_mpi_mesh_count(self, sim: Simulation, n_mpi: int) -> list[str]:
        """
        Warn if n_mpi doesn't match mesh count.

        FDS User Guide: "Usually, each mesh is assigned its own process."
        """
        mesh_count = len(sim.geometry.meshes)
        warnings = []

        if n_mpi > mesh_count:
            warnings.append(
                f"Warning: {n_mpi} MPI processes requested but only "
                f"{mesh_count} mesh(es) defined. Extra processes will be idle."
            )
        elif n_mpi < mesh_count and mesh_count > 1:
            warnings.append(
                f"Warning: {mesh_count} meshes defined but only {n_mpi} "
                f"MPI process(es). Multiple meshes per process may reduce efficiency."
            )

        return warnings

    def validate_thread_count(self, n_threads: int) -> list[str]:
        """
        Warn if thread count exceeds system resources.

        FDS User Guide: "By default, FDS uses ~50% of available cores."
        """
        warnings = []
        cpu_count = os.cpu_count() or 1

        if n_threads > cpu_count:
            warnings.append(
                f"Warning: {n_threads} OpenMP threads requested but only "
                f"{cpu_count} logical cores available. This will oversubscribe the system."
            )
        elif n_threads > cpu_count // 2:
            warnings.append(
                f"Info: Using more than 50% of cores ({cpu_count//2}). "
                f"This may impact system responsiveness."
            )

        return warnings

    def recommend_configuration(self, sim: Simulation) -> dict:
        """Suggest optimal parallel configuration based on system and simulation."""
        cpu_count = os.cpu_count() or 1
        mesh_count = len(sim.geometry.meshes)

        if mesh_count == 1:
            # Single mesh: use OpenMP only
            return {
                'n_mpi': 1,
                'n_threads': cpu_count // 2,
                'rationale': 'Single mesh simulation - using OpenMP with 50% of cores'
            }
        else:
            # Multi-mesh: prefer MPI
            return {
                'n_mpi': min(mesh_count, cpu_count),
                'n_threads': 1,
                'rationale': f'Multi-mesh ({mesh_count}) - using MPI for best performance'
            }
```

#### 1.2 Platform-Specific Execution Handling
```python
class PlatformExecutor:
    """Handle platform-specific FDS execution details."""

    def __init__(self):
        self.platform = platform.system()
        self.fds_command, self.uses_wrapper = self._detect_fds_command()

    def _detect_fds_command(self) -> tuple[Path, bool]:
        """Detect appropriate FDS command for platform."""
        if self.platform == "Windows":
            # Look for fds_local wrapper first
            fds_local = shutil.which("fds_local")
            if fds_local:
                return Path(fds_local), True

        # Fall back to standard fds executable
        return find_fds_executable(), False

    def build_command(self, fds_file: Path, n_mpi: int, n_threads: int) -> list[str]:
        """Build platform-appropriate command."""
        if self.uses_wrapper and self.platform == "Windows":
            # Use Windows fds_local syntax
            return [
                str(self.fds_command),
                "-p", str(n_mpi),
                "-o", str(n_threads),
                str(fds_file)
            ]
        else:
            # Use standard mpiexec syntax
            return build_fds_command(
                fds_file=fds_file,
                fds_executable=self.fds_command,
                n_threads=n_threads,
                n_mpi=n_mpi
            )
```

#### 1.3 Graceful Stopping
```python
class Job:
    # ... existing code ...

    def request_stop(self) -> None:
        """
        Request graceful shutdown by creating CHID.stop file.

        FDS User Guide §3.4: "To stop a calculation before its scheduled time,
        create a file in the same directory as the output files called CHID.stop."
        """
        stop_file = self.output_dir / f"{self.chid}.stop"
        stop_file.touch()
        logger.info(f"Created stop file: {stop_file}")
        logger.info("FDS will stop gracefully after current timestep completes")
```

### Priority 2: Important for Advanced Users

#### 2.1 Restart Support
```python
class RestartManager:
    """Handle FDS restart functionality."""

    def find_restart_file(self, output_dir: Path, chid: str) -> Path | None:
        """Find most recent restart file."""
        restart_files = sorted(output_dir.glob(f"{chid}_*.restart"))
        return restart_files[-1] if restart_files else None

    def can_restart(self, output_dir: Path, chid: str) -> bool:
        """Check if restart is possible."""
        return self.find_restart_file(output_dir, chid) is not None

    def restart_simulation(self, runner: FDSRunner, fds_file: Path, **kwargs):
        """
        Restart simulation from last checkpoint.

        Requires &DUMP RESTART=.TRUE. in input file.
        """
        # Implementation would:
        # 1. Verify restart file exists
        # 2. Ensure RESTART=.TRUE. in input file
        # 3. Launch FDS which auto-detects restart
        pass
```

#### 2.2 CPU Profiling Integration
```python
from dataclasses import dataclass

@dataclass
class CPUProfile:
    """CPU profiling data from CHID_cpu.csv."""

    mpi_process: int
    total_time: float
    velo_time: float  # Velocity computation
    mass_time: float  # Species/density computation
    comm_time: float  # MPI communication
    main_time: float  # Main control loop

    @property
    def comm_percentage(self) -> float:
        """Communication overhead percentage."""
        return (self.comm_time / self.total_time) * 100

    @property
    def efficiency(self) -> float:
        """Estimate efficiency (1 - comm_overhead)."""
        return 1.0 - (self.comm_time / self.total_time)

class CPUProfileParser:
    """Parse CHID_cpu.csv file."""

    def parse(self, cpu_file: Path) -> list[CPUProfile]:
        """Parse CPU profile data."""
        # Read CSV with polars
        df = pl.read_csv(cpu_file)
        # Extract timing data per MPI process
        # Return list of CPUProfile objects
        pass

    def calculate_weak_scaling(self, profiles: list[list[CPUProfile]]) -> dict:
        """
        Calculate weak scaling efficiency.

        E_w = t_1 / t_N

        profiles: List of profile lists, one per simulation with increasing N
        """
        pass

    def calculate_strong_scaling(self, profiles: list[list[CPUProfile]]) -> dict:
        """
        Calculate strong scaling efficiency.

        E_s = t_1 / (N * t_N)
        """
        pass
```

### Priority 3: Nice-to-Have Enhancements

#### 3.1 Cluster Support
```python
class ClusterConfig:
    """Configuration for cluster execution."""

    machine_file: Path | None = None
    hosts: list[str] | None = None
    scheduler: Literal["none", "pbs", "slurm"] = "none"

class ClusterExecutor:
    """Execute FDS on compute clusters."""

    def __init__(self, config: ClusterConfig):
        self.config = config

    def build_mpi_command(self, n_mpi: int) -> list[str]:
        """Build MPI command with cluster options."""
        cmd = ["mpiexec", "-n", str(n_mpi)]

        if self.config.machine_file:
            cmd.extend(["-machinefile", str(self.config.machine_file)])
        elif self.config.hosts:
            cmd.extend(["-host", ",".join(self.config.hosts)])

        return cmd

    def submit_job(self, fds_file: Path, **kwargs) -> str:
        """Submit job to scheduler (PBS/Slurm)."""
        if self.config.scheduler == "slurm":
            # Generate and submit SLURM script
            pass
        elif self.config.scheduler == "pbs":
            # Generate and submit PBS script
            pass
        else:
            raise ValueError("No scheduler configured")
```

#### 3.2 Resource Recommendations
```python
class ResourceAdvisor:
    """Provide recommendations for simulation resources."""

    def estimate_memory_usage(self, sim: Simulation) -> float:
        """
        Estimate memory requirements in GB.

        Rule of thumb: ~1 MB per grid cell
        """
        total_cells = sum(
            mesh.ijk.i * mesh.ijk.j * mesh.ijk.k
            for mesh in sim.geometry.meshes
        )
        return total_cells * 1e-6  # MB to GB

    def estimate_runtime(self, sim: Simulation, n_cores: int) -> float:
        """
        Estimate wall clock time.

        Very rough: ~10 cell-updates per second per core
        """
        total_cells = sum(
            mesh.ijk.i * mesh.ijk.j * mesh.ijk.k
            for mesh in sim.geometry.meshes
        )

        # Get time parameters
        time_obj = next((obj for obj in sim._objects if isinstance(obj, Time)), None)
        if not time_obj:
            return 0.0

        t_end = time_obj.t_end or 0.0

        # Rough estimate: 10 cell-updates/sec/core
        cell_updates = total_cells * t_end
        return cell_updates / (n_cores * 10)

    def recommend_mesh_count(self, sim: Simulation, target_cells_per_mesh: int = 1_000_000) -> int:
        """Recommend number of meshes for efficient parallelization."""
        total_cells = sum(
            mesh.ijk.i * mesh.ijk.j * mesh.ijk.k
            for mesh in sim.geometry.meshes
        )
        return max(1, total_cells // target_cells_per_mesh)
```

---

## Recommended Implementation Roadmap

### Phase 2.1: Robustness (1-2 weeks)
1. Add `ParallelValidator` with mesh/MPI validation
2. Add `PlatformExecutor` for Windows `fds_local` support
3. Add `Job.request_stop()` for graceful stopping
4. Add warnings for thread/core mismatch

### Phase 2.2: Advanced Features (2-3 weeks)
5. Add `RestartManager` for restart support
6. Add `CPUProfileParser` for performance analysis
7. Add `ResourceAdvisor` for memory/time estimation

### Phase 3: Cluster Support (3-4 weeks)
8. Add `ClusterConfig` and `ClusterExecutor`
9. Add PBS/Slurm job scheduler integration
10. Add multi-node MPI support with machine files

### Phase 4: Analysis Tools (2-3 weeks)
11. Add weak/strong scaling analysis utilities
12. Add performance visualization
13. Add bottleneck identification

---

## Key Architectural Gaps

### 1. No Multi-Computer Execution
**Impact:** Cannot run on compute clusters or multi-node systems
**FDS Guide:** §3.2.2, §3.2.4 - Core functionality for large simulations
**Fix:** Add machine file support and cluster integration

### 2. No Validation of Parallel Config
**Impact:** Users may waste resources with misconfigured parallelization
**FDS Guide:** §3.1.2 - "Usually, each mesh is assigned its own process"
**Fix:** Add `ParallelValidator` to warn about mismatches

### 3. No Restart Support
**Impact:** Cannot recover from interruptions or extend simulations
**FDS Guide:** §3.4 - "Restart feature" essential for long runs
**Fix:** Add `RestartManager` and `.restart` file handling

### 4. No Performance Analysis
**Impact:** Cannot optimize or debug parallel efficiency
**FDS Guide:** §3.3 - CPU profiling for scalability studies
**Fix:** Add `CPUProfileParser` and scaling utilities

### 5. Platform-Specific Quirks Not Handled
**Impact:** May not work correctly on Windows with FDS installation
**FDS Guide:** §3.2.1 - Windows uses `fds_local` wrapper
**Fix:** Detect and use platform-appropriate commands

---

## Namelist Parameters Mentioned in Chapter 3

### Implemented in PyFDS
- ✅ `&TIME T_END=...` - Already in `Time` namelist
- ✅ `&MESH IJK=..., XB=...` - Already in `Mesh` namelist

### Not Yet Implemented
- ❌ `&DUMP DIAGNOSTICS_INTERVAL=100` - Monitoring control
- ❌ `&DUMP SUPPRESS_DIAGNOSTICS=T` - Quiet large jobs
- ❌ `&DUMP DT_CPU=...` - CPU profiling interval
- ❌ `&DUMP RESTART=.TRUE.` - Enable restart files
- ❌ `&MISC LIMITING_DT_RATIO=...` - Timestep stability threshold
- ❌ `&DUMP FLUSH_FILE_BUFFERS=F` - Windows file writing issue workaround
- ❌ `&DUMP MPI_TIMEOUT=600` - MPI communication timeout

These should be added to the `Dump` and `Misc` namelists.

---

## Conclusion

PyFDS has **solid foundation** for basic parallel execution on single machines (OpenMP, MPI, combined). However, it lacks:

1. **Multi-computer/cluster support** - Critical gap for serious users
2. **Restart functionality** - Essential for long simulations
3. **Performance analysis** - Needed for optimization
4. **Platform-specific refinements** - Windows compatibility issues possible
5. **Resource validation** - Could prevent user errors

**Recommended Next Steps:**
1. Implement Priority 1 items (validation, graceful stop, platform handling)
2. Add missing `&DUMP` and `&MISC` parameters
3. Consider Phase 2.2 features (restart, CPU profiling) for completeness
4. Defer cluster support (Phase 3) unless there's user demand

The current implementation is **good for desktop users** but needs enhancement for **HPC/cluster environments** and **production workflows**.
