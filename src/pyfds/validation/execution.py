"""Execution configuration validation.

Validates MPI and OpenMP parallel execution settings.
"""

import os
from typing import TYPE_CHECKING

from pyfds.core.enums import Severity
from pyfds.validation.base import Issue, ValidationResult

if TYPE_CHECKING:
    from pyfds.core.simulation import Simulation


class ExecutionValidator:
    """Validates parallel execution configuration.

    Checks MPI process count, OpenMP thread count, and
    provides recommendations for optimal performance.

    Based on FDS User Guide Chapter 3 "Running FDS" guidelines for
    optimal parallel performance.
    """

    def __init__(self) -> None:
        self._cpu_count = os.cpu_count() or 1

    def validate(
        self,
        simulation: "Simulation",
        n_mpi: int = 1,
        n_threads: int = 1,
    ) -> ValidationResult:
        """Validate parallel configuration.

        Parameters
        ----------
        simulation : Simulation
            Simulation to validate
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads per process

        Returns
        -------
        ValidationResult
            Validation result containing any issues found.
        """
        issues: list[Issue] = []
        issues.extend(self._check_mpi_vs_meshes(simulation, n_mpi))
        issues.extend(self._check_thread_count(n_threads))
        if n_mpi > 1 or n_threads > 1:
            issues.extend(self._check_total_parallelism(n_mpi, n_threads))
        return ValidationResult(issues=issues)

    def _check_mpi_vs_meshes(self, simulation: "Simulation", n_mpi: int) -> list[Issue]:
        """Check MPI process count vs mesh count."""
        issues = []

        mesh_count = len(simulation.meshes)

        if n_mpi > mesh_count:
            issues.append(
                Issue(
                    Severity.WARNING,
                    f"More MPI processes ({n_mpi}) than meshes ({mesh_count}). "
                    f"Extra processes will be idle.",
                    "execution",
                    "n_mpi",
                )
            )
        elif mesh_count > n_mpi and mesh_count > 1 and mesh_count % n_mpi != 0:
            issues.append(
                Issue(
                    Severity.INFO,
                    f"Mesh count ({mesh_count}) not evenly divisible by MPI processes ({n_mpi}). "
                    f"Load may be unbalanced.",
                    "execution",
                    "n_mpi",
                )
            )

        return issues

    def _check_thread_count(self, n_threads: int) -> list[Issue]:
        """Check OpenMP thread count."""
        issues = []

        if n_threads > self._cpu_count:
            issues.append(
                Issue(
                    Severity.WARNING,
                    f"Thread count ({n_threads}) exceeds CPU count ({self._cpu_count}). "
                    f"This may reduce performance due to oversubscription.",
                    "execution",
                    "n_threads",
                )
            )
        elif n_threads > self._cpu_count // 2:
            issues.append(
                Issue(
                    Severity.INFO,
                    f"Using {n_threads} threads (>{self._cpu_count // 2}, which is >50% of "
                    f"{self._cpu_count} cores). This may impact system responsiveness.",
                    "execution",
                    "n_threads",
                )
            )

        return issues

    def _check_total_parallelism(self, n_mpi: int, n_threads: int) -> list[Issue]:
        """Check total parallel processes."""
        issues = []

        total = n_mpi * n_threads
        if total > self._cpu_count:
            issues.append(
                Issue(
                    Severity.WARNING,
                    f"Total parallel units ({total} = {n_mpi} MPI x {n_threads} threads) "
                    f"exceeds CPU count ({self._cpu_count}).",
                    "execution",
                )
            )

        if n_mpi > 1 and n_threads > 4:
            issues.append(
                Issue(
                    Severity.INFO,
                    f"Using both MPI ({n_mpi}) and OpenMP ({n_threads}). Note: OpenMP provides "
                    f"diminishing returns beyond ~4 threads. Consider reducing n_threads.",
                    "execution",
                )
            )

        return issues

    def suggest_config(self, simulation: "Simulation") -> dict[str, int | str]:
        """Suggest optimal parallel configuration.

        Parameters
        ----------
        simulation : Simulation
            Simulation to analyze

        Returns
        -------
        dict
            Suggested n_mpi and n_threads values with rationale
        """
        mesh_count = len(simulation.meshes)

        if mesh_count == 0:
            return {
                "n_mpi": 1,
                "n_threads": 1,
                "rationale": "No meshes defined - cannot recommend configuration",
            }

        if mesh_count == 1:
            # Single mesh: use OpenMP only
            recommended_threads = max(1, self._cpu_count // 2)
            return {
                "n_mpi": 1,
                "n_threads": recommended_threads,
                "rationale": (
                    f"Single mesh simulation - using OpenMP with ~50% of {self._cpu_count} cores."
                ),
            }

        # Multi-mesh: prefer MPI
        n_mpi = min(mesh_count, self._cpu_count)

        return {
            "n_mpi": n_mpi,
            "n_threads": 1,
            "rationale": (
                f"Multi-mesh ({mesh_count} meshes) - using MPI with "
                f"{n_mpi} processes for best performance."
            ),
        }


__all__ = ["ExecutionValidator"]
