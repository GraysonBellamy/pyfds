"""
Unit tests for parallel execution validation.
"""

import os

import pytest

from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Mesh
from pyfds.validation import ParallelValidator


class TestParallelValidator:
    """Test parallel configuration validation."""

    @pytest.fixture
    def validator(self) -> ParallelValidator:
        """Create validator instance."""
        return ParallelValidator()

    @pytest.fixture
    def single_mesh_sim(self) -> Simulation:
        """Create simulation with single mesh."""
        sim = Simulation(chid="test")
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        return sim

    @pytest.fixture
    def multi_mesh_sim(self) -> Simulation:
        """Create simulation with 4 meshes."""
        sim = Simulation(chid="test")
        for i in range(4):
            sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(i, i + 1, 0, 1, 0, 1)))
        return sim


class TestMPIMeshValidation(TestParallelValidator):
    """Test MPI process count vs mesh count validation."""

    def test_mpi_matches_mesh_count(
        self, validator: ParallelValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test no warnings when MPI count matches mesh count."""
        warnings = validator.validate_mpi_mesh_count(multi_mesh_sim, n_mpi=4)
        assert len(warnings) == 0

    def test_mpi_exceeds_mesh_count(
        self, validator: ParallelValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test warning when MPI processes exceed mesh count."""
        warnings = validator.validate_mpi_mesh_count(single_mesh_sim, n_mpi=4)

        assert len(warnings) == 1
        assert "(4)" in warnings[0]  # 4 MPI processes
        assert "(1)" in warnings[0]  # 1 mesh
        assert "idle" in warnings[0].lower()

    def test_mpi_less_than_mesh_count_uneven(
        self, validator: ParallelValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test info when MPI processes unevenly divide mesh count."""
        # 4 meshes with 3 MPI processes = uneven division
        warnings = validator.validate_mpi_mesh_count(multi_mesh_sim, n_mpi=3)

        assert len(warnings) == 1
        assert "4" in warnings[0]  # 4 meshes
        assert "3" in warnings[0]  # 3 MPI processes
        assert "unbalanced" in warnings[0].lower()

    def test_mpi_less_than_mesh_count_even(
        self, validator: ParallelValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test no warning when MPI processes evenly divide mesh count."""
        # 4 meshes with 2 MPI processes = even division
        warnings = validator.validate_mpi_mesh_count(multi_mesh_sim, n_mpi=2)
        assert len(warnings) == 0

    def test_single_mesh_single_mpi_ok(
        self, validator: ParallelValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test single mesh with single MPI is fine."""
        warnings = validator.validate_mpi_mesh_count(single_mesh_sim, n_mpi=1)
        assert len(warnings) == 0


class TestThreadCountValidation(TestParallelValidator):
    """Test OpenMP thread count validation."""

    def test_thread_count_within_limits(self, validator: ParallelValidator) -> None:
        """Test no warnings for reasonable thread count."""
        cpu_count = os.cpu_count() or 4

        # Use 25% of cores - should be fine
        warnings = validator.validate_thread_count(n_threads=max(1, cpu_count // 4))
        assert len(warnings) == 0

    def test_thread_count_exceeds_cores(self, validator: ParallelValidator) -> None:
        """Test warning when threads exceed available cores."""
        cpu_count = os.cpu_count() or 4

        warnings = validator.validate_thread_count(n_threads=cpu_count + 4)

        assert len(warnings) == 1
        assert "oversubscription" in warnings[0].lower()
        assert str(cpu_count + 4) in warnings[0]

    def test_thread_count_over_50_percent(self, validator: ParallelValidator) -> None:
        """Test info message when using >50% of cores."""
        cpu_count = os.cpu_count() or 4

        # Use 75% of cores
        n_threads = max(1, int(cpu_count * 0.75))
        if n_threads <= cpu_count // 2:
            pytest.skip("System doesn't have enough cores for this test")

        warnings = validator.validate_thread_count(n_threads=n_threads)

        assert len(warnings) == 1
        assert "50%" in warnings[0]
        assert "responsiveness" in warnings[0].lower()


class TestCombinedParallelismValidation(TestParallelValidator):
    """Test combined MPI + OpenMP validation."""

    def test_combined_within_limits(self, validator: ParallelValidator) -> None:
        """Test no warnings for reasonable combined parallelism."""
        cpu_count = os.cpu_count() or 8

        # Use 50% of cores total
        n_mpi = 2
        n_threads = max(1, cpu_count // 4)

        warnings = validator.validate_combined_parallelism(n_mpi, n_threads)

        # May get info message about high OpenMP threads, but not error
        for warning in warnings:
            assert "oversubscription" not in warning.lower()

    def test_combined_exceeds_cores(self, validator: ParallelValidator) -> None:
        """Test warning when combined parallelism exceeds cores."""
        cpu_count = os.cpu_count() or 8

        warnings = validator.validate_combined_parallelism(n_mpi=cpu_count, n_threads=2)

        assert len(warnings) >= 1
        # Should warn about exceeding CPU count
        exceeds_warning = any("exceeds" in w.lower() for w in warnings)
        assert exceeds_warning

    def test_high_openmp_with_mpi(self, validator: ParallelValidator) -> None:
        """Test info message about inefficient high OpenMP with MPI."""
        warnings = validator.validate_combined_parallelism(n_mpi=4, n_threads=8)

        # Should get info about OpenMP inefficiency beyond ~4 threads
        openmp_info = any("OpenMP" in w and "diminishing" in w for w in warnings)
        assert openmp_info


class TestConfigurationRecommendations(TestParallelValidator):
    """Test optimal configuration recommendations."""

    def test_recommend_for_single_mesh(
        self, validator: ParallelValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test recommendation for single mesh simulation."""
        config = validator.recommend_configuration(single_mesh_sim)

        assert config["n_mpi"] == 1
        assert config["n_threads"] >= 1  # Should use at least 1 thread
        assert "OpenMP" in config["rationale"]
        assert "Single mesh" in config["rationale"]

    def test_recommend_for_multi_mesh(
        self, validator: ParallelValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test recommendation for multi-mesh simulation."""
        config = validator.recommend_configuration(multi_mesh_sim)

        assert config["n_mpi"] >= 2  # Should use MPI
        assert "MPI" in config["rationale"]
        assert "Multi-mesh" in config["rationale"]

    def test_recommend_with_limited_cores(self, validator: ParallelValidator) -> None:
        """Test recommendation adapts to available cores."""
        sim = Simulation(chid="test")

        # Create more meshes than likely available cores
        for i in range(100):
            sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(i, i + 1, 0, 1, 0, 1)))

        config = validator.recommend_configuration(sim)

        cpu_count = os.cpu_count() or 1
        # Should not recommend more MPI processes than available cores
        assert config["n_mpi"] <= cpu_count

    def test_recommend_with_no_meshes(self, validator: ParallelValidator) -> None:
        """Test recommendation when no meshes defined."""
        sim = Simulation(chid="test")

        config = validator.recommend_configuration(sim)

        assert config["n_mpi"] == 1
        assert config["n_threads"] == 1
        assert "No meshes" in config["rationale"]


class TestValidateAll(TestParallelValidator):
    """Test comprehensive validation."""

    def test_validate_all_combines_checks(
        self, validator: ParallelValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test validate_all runs all validation checks."""
        cpu_count = os.cpu_count() or 8

        # Configuration that should trigger multiple warnings
        # Use 3 MPI for 4 meshes (uneven) and exceed CPU count for threads
        all_warnings = validator.validate_all(
            multi_mesh_sim,
            n_mpi=3,  # Uneven division of 4 meshes
            n_threads=cpu_count + 2,  # Exceeds cores
        )

        # Should get warnings from multiple validators
        assert len(all_warnings) >= 2

        # Check we got warnings from different validators
        has_mpi_warning = any("mesh" in w.lower() for w in all_warnings)
        has_thread_warning = any("thread" in w.lower() for w in all_warnings)

        assert has_mpi_warning
        assert has_thread_warning

    def test_validate_all_optimal_config(
        self, validator: ParallelValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test validate_all with optimal configuration."""
        # Use recommended configuration
        config = validator.recommend_configuration(multi_mesh_sim)

        all_warnings = validator.validate_all(
            multi_mesh_sim,
            n_mpi=config["n_mpi"],
            n_threads=config["n_threads"],
        )

        # Optimal config may still have info messages, but at most one warning
        # (on systems with limited cores relative to mesh count)
        critical_warnings = [w for w in all_warnings if "WARNING" in w]
        assert len(critical_warnings) <= 1
