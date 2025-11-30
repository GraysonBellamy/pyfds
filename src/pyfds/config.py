"""Configuration objects for PyFDS operations."""

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True, slots=True)
class RunConfig:
    """Configuration for simulation execution."""

    # Parallelization
    n_threads: int = 1
    n_mpi: int = 1
    mpiexec_path: str = "mpiexec"

    # Paths
    output_dir: Path | None = None
    fds_executable: Path | None = None

    # Execution
    monitor: bool = True
    wait: bool = True
    timeout: float | None = None

    # Validation
    validate: bool = True
    strict: bool = False


@dataclass(frozen=True, slots=True)
class ValidationConfig:
    """Configuration for simulation validation."""

    check_mesh_quality: bool = True
    check_cross_references: bool = True
    check_physical_bounds: bool = True
    strict: bool = False  # Treat warnings as errors


__all__ = ["RunConfig", "ValidationConfig"]
