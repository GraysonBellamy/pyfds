"""
Validation utilities for FDS execution configuration.

This module provides validation for parallel execution parameters to ensure
optimal configuration and prevent common errors.
"""

import os
from typing import TYPE_CHECKING

from ..utils import get_logger

if TYPE_CHECKING:
    from ..core.simulation import Simulation

logger = get_logger(__name__)


class ParallelValidator:
    """
    Validate parallel execution configuration.

    Based on FDS User Guide Chapter 3 "Running FDS" guidelines for
    optimal parallel performance.

    Examples
    --------
    >>> validator = ParallelValidator()
    >>> warnings = validator.validate_mpi_mesh_count(sim, n_mpi=4)
    >>> for warning in warnings:
    ...     print(warning)
    """

    def validate_mpi_mesh_count(self, sim: "Simulation", n_mpi: int) -> list[str]:
        """
        Validate MPI process count matches mesh configuration.

        From FDS User Guide §3.1.2.2:
        "Usually, each mesh is assigned its own process in an MPI calculation."

        Parameters
        ----------
        sim : Simulation
            The simulation to validate
        n_mpi : int
            Number of MPI processes requested

        Returns
        -------
        list[str]
            List of warning messages (empty if no issues)

        Examples
        --------
        >>> sim = Simulation()
        >>> sim.mesh(ijk=Grid3D(10, 10, 10), xb=Bounds3D(0, 1, 0, 1, 0, 1))
        >>> validator = ParallelValidator()
        >>> warnings = validator.validate_mpi_mesh_count(sim, n_mpi=4)
        >>> len(warnings) > 0  # Will warn: 1 mesh but 4 processes
        True
        """
        mesh_count = len(sim.geometry.meshes)
        warnings = []

        if n_mpi > mesh_count:
            warnings.append(
                f"MPI Configuration Warning: {n_mpi} MPI processes requested "
                f"but only {mesh_count} mesh(es) defined. "
                f"Extra processes will be idle and waste resources. "
                f"Consider using n_mpi={mesh_count} instead."
            )
            logger.warning(f"MPI process count ({n_mpi}) exceeds mesh count ({mesh_count})")

        elif n_mpi < mesh_count and mesh_count > 1:
            warnings.append(
                f"MPI Configuration Warning: {mesh_count} meshes defined "
                f"but only {n_mpi} MPI process(es) requested. "
                f"Multiple meshes per process may reduce parallel efficiency. "
                f"Consider using n_mpi={mesh_count} for best performance."
            )
            logger.warning(f"Mesh count ({mesh_count}) exceeds MPI processes ({n_mpi})")

        return warnings

    def validate_thread_count(self, n_threads: int) -> list[str]:
        """
        Validate OpenMP thread count against system resources.

        From FDS User Guide §3.1.2.1:
        "By default, FDS will use approximately half of the available cores."

        Parameters
        ----------
        n_threads : int
            Number of OpenMP threads requested

        Returns
        -------
        list[str]
            List of warning/info messages

        Examples
        --------
        >>> validator = ParallelValidator()
        >>> # On a 4-core system:
        >>> warnings = validator.validate_thread_count(n_threads=8)
        >>> len(warnings) > 0  # Will warn about oversubscription
        True
        """
        warnings = []
        cpu_count = os.cpu_count()

        if cpu_count is None:
            warnings.append("Could not determine CPU count. Cannot validate thread count.")
            logger.warning("os.cpu_count() returned None")
            return warnings

        if n_threads > cpu_count:
            warnings.append(
                f"OpenMP Thread Warning: {n_threads} threads requested but "
                f"only {cpu_count} logical cores available. "
                f"This will oversubscribe the system and likely degrade performance. "
                f"Recommended: Use n_threads={cpu_count} at most."
            )
            logger.warning(f"Thread count ({n_threads}) exceeds CPU count ({cpu_count})")

        elif n_threads > cpu_count // 2:
            warnings.append(
                f"OpenMP Thread Info: Using {n_threads} threads (>{cpu_count // 2}, "
                f"which is >50% of {cpu_count} cores). "
                f"FDS typically defaults to ~50% to avoid system overload. "
                f"This may impact system responsiveness during simulation."
            )
            logger.info(f"Thread count ({n_threads}) > 50% of cores ({cpu_count // 2})")

        return warnings

    def validate_combined_parallelism(self, n_mpi: int, n_threads: int) -> list[str]:
        """
        Validate combined MPI + OpenMP configuration.

        From FDS User Guide §3.2.6:
        "MPI is the better choice when using multiple meshes because it more
        efficiently divides the computational work than OpenMP."

        Parameters
        ----------
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads per process

        Returns
        -------
        list[str]
            List of warning/info messages

        Examples
        --------
        >>> validator = ParallelValidator()
        >>> # On an 8-core system:
        >>> warnings = validator.validate_combined_parallelism(n_mpi=4, n_threads=4)
        >>> len(warnings) > 0  # May warn about total core usage
        True
        """
        warnings: list[str] = []
        cpu_count = os.cpu_count()

        if cpu_count is None:
            return warnings

        total_cores_requested = n_mpi * n_threads

        if total_cores_requested > cpu_count:
            warnings.append(
                f"Combined Parallelism Warning: {n_mpi} MPI processes x "
                f"{n_threads} threads = {total_cores_requested} total cores "
                f"requested, but only {cpu_count} available. "
                f"System will be severely oversubscribed."
            )
            logger.warning(
                f"Total cores requested ({total_cores_requested}) > available ({cpu_count})"
            )

        if n_mpi > 1 and n_threads > 4:
            warnings.append(
                f"Combined Parallelism Info: Using both MPI ({n_mpi}) and "
                f"OpenMP ({n_threads}). Note from FDS User Guide: "
                f"'OpenMP can provide an extra factor up to about 2, "
                f"regardless of the number of cores used beyond about 4.' "
                f"Consider reducing n_threads to 2-4 for better efficiency."
            )
            logger.info(f"High OpenMP thread count ({n_threads}) with MPI may be inefficient")

        return warnings

    def recommend_configuration(self, sim: "Simulation") -> dict[str, int | str]:
        """
        Recommend optimal parallel configuration.

        Based on FDS User Guide best practices and system resources.

        Parameters
        ----------
        sim : Simulation
            The simulation to analyze

        Returns
        -------
        dict
            Recommended configuration with 'n_mpi', 'n_threads', and 'rationale'

        Examples
        --------
        >>> sim = Simulation()
        >>> sim.mesh(ijk=Grid3D(50, 50, 50), xb=Bounds3D(0, 5, 0, 5, 0, 5))
        >>> validator = ParallelValidator()
        >>> config = validator.recommend_configuration(sim)
        >>> config['n_mpi']
        1
        >>> config['n_threads'] > 1
        True
        """
        cpu_count = os.cpu_count() or 1
        mesh_count = len(sim.geometry.meshes)

        if mesh_count == 0:
            logger.warning("No meshes defined in simulation")
            return {
                "n_mpi": 1,
                "n_threads": 1,
                "rationale": "No meshes defined - cannot recommend configuration",
            }

        if mesh_count == 1:
            # Single mesh: use OpenMP only
            # FDS default is ~50% of cores
            recommended_threads = max(1, cpu_count // 2)
            return {
                "n_mpi": 1,
                "n_threads": recommended_threads,
                "rationale": (
                    f"Single mesh simulation - using OpenMP with ~50% of "
                    f"{cpu_count} cores. OpenMP is the only option for "
                    f"single-mesh cases."
                ),
            }

        # Multi-mesh: prefer MPI
        # Use up to min(mesh_count, cpu_count) MPI processes
        recommended_mpi = min(mesh_count, cpu_count)

        return {
            "n_mpi": recommended_mpi,
            "n_threads": 1,
            "rationale": (
                f"Multi-mesh ({mesh_count} meshes) - using MPI with "
                f"{recommended_mpi} processes for best performance. "
                f"MPI is more efficient than OpenMP for multi-mesh cases. "
                f"Could optionally add n_threads=2-4 for additional speedup."
            ),
        }

    def validate_all(self, sim: "Simulation", n_mpi: int, n_threads: int) -> list[str]:
        """
        Run all validation checks.

        Parameters
        ----------
        sim : Simulation
            The simulation to validate
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads

        Returns
        -------
        list[str]
            Combined list of all warnings and info messages

        Examples
        --------
        >>> validator = ParallelValidator()
        >>> all_warnings = validator.validate_all(sim, n_mpi=4, n_threads=2)
        >>> for warning in all_warnings:
        ...     print(warning)
        """
        warnings = []

        # Validate each aspect
        warnings.extend(self.validate_mpi_mesh_count(sim, n_mpi))
        warnings.extend(self.validate_thread_count(n_threads))

        if n_mpi > 1 or n_threads > 1:
            warnings.extend(self.validate_combined_parallelism(n_mpi, n_threads))

        return warnings
