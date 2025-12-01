"""
Unit tests for parallel execution validation.
"""

import os

import pytest

from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Mesh
from pyfds.validation import ExecutionValidator


class TestExecutionValidator:
    """Test parallel configuration validation."""

    @pytest.fixture
    def validator(self) -> ExecutionValidator:
        """Create validator instance."""
        return ExecutionValidator()

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


class TestMPIMeshValidation(TestExecutionValidator):
    """Test MPI process count vs mesh count validation."""

    def test_mpi_matches_mesh_count(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test no warnings when MPI count matches mesh count."""
        result = validator.validate(multi_mesh_sim, n_mpi=4)
        assert len(result.issues) == 0

    def test_mpi_exceeds_mesh_count(
        self, validator: ExecutionValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test warning when MPI processes exceed mesh count."""
        result = validator.validate(single_mesh_sim, n_mpi=4)

        assert len(result.issues) == 1
        issue_str = str(result.issues[0])
        assert "(4)" in issue_str  # 4 MPI processes
        assert "(1)" in issue_str  # 1 mesh
        assert "idle" in issue_str.lower()

    def test_mpi_less_than_mesh_count_uneven(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test info when MPI processes unevenly divide mesh count."""
        # 4 meshes with 3 MPI processes = uneven division
        result = validator.validate(multi_mesh_sim, n_mpi=3)

        assert len(result.issues) == 1
        issue_str = str(result.issues[0])
        assert "4" in issue_str  # 4 meshes
        assert "3" in issue_str  # 3 MPI processes
        assert "unbalanced" in issue_str.lower()

    def test_mpi_less_than_mesh_count_even(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test no warning when MPI processes evenly divide mesh count."""
        # 4 meshes with 2 MPI processes = even division
        result = validator.validate(multi_mesh_sim, n_mpi=2)
        assert len(result.issues) == 0

    def test_single_mesh_single_mpi_ok(
        self, validator: ExecutionValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test single mesh with single MPI is fine."""
        result = validator.validate(single_mesh_sim, n_mpi=1)
        assert len(result.issues) == 0


class TestThreadCountValidation(TestExecutionValidator):
    """Test OpenMP thread count validation."""

    def test_thread_count_within_limits(
        self, validator: ExecutionValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test no warnings for reasonable thread count."""
        cpu_count = os.cpu_count() or 4

        # Use 25% of cores - should be fine
        result = validator.validate(single_mesh_sim, n_threads=max(1, cpu_count // 4))
        assert len(result.issues) == 0

    def test_thread_count_exceeds_cores(
        self, validator: ExecutionValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test warning when threads exceed available cores."""
        cpu_count = os.cpu_count() or 4

        result = validator.validate(single_mesh_sim, n_threads=cpu_count + 4)

        assert len(result.issues) >= 1
        issue_str = str(result.issues[0])
        assert "oversubscription" in issue_str.lower()
        assert str(cpu_count + 4) in issue_str

    def test_thread_count_over_50_percent(
        self, validator: ExecutionValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test info message when using >50% of cores."""
        cpu_count = os.cpu_count() or 4

        # Use 75% of cores
        n_threads = max(1, int(cpu_count * 0.75))
        if n_threads <= cpu_count // 2:
            pytest.skip("System doesn't have enough cores for this test")

        result = validator.validate(single_mesh_sim, n_threads=n_threads)

        assert len(result.issues) >= 1
        issue_str = str(result.issues[0])
        assert "50%" in issue_str
        assert "responsiveness" in issue_str.lower()


class TestCombinedParallelismValidation(TestExecutionValidator):
    """Test combined MPI + OpenMP validation."""

    def test_combined_within_limits(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test no warnings for reasonable combined parallelism."""
        cpu_count = os.cpu_count() or 8

        # Use 50% of cores total
        n_mpi = 2
        n_threads = max(1, cpu_count // 4)

        result = validator.validate(multi_mesh_sim, n_mpi=n_mpi, n_threads=n_threads)

        # May get info message about high OpenMP threads, but not error
        for issue in result.issues:
            assert "oversubscription" not in str(issue).lower()

    def test_combined_exceeds_cores(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test warning when combined parallelism exceeds cores."""
        cpu_count = os.cpu_count() or 8

        result = validator.validate(multi_mesh_sim, n_mpi=cpu_count, n_threads=2)

        assert len(result.issues) >= 1
        # Should warn about exceeding CPU count
        exceeds_warning = any("exceeds" in str(issue).lower() for issue in result.issues)
        assert exceeds_warning

    def test_high_openmp_with_mpi(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test info message about inefficient high OpenMP with MPI."""
        result = validator.validate(multi_mesh_sim, n_mpi=4, n_threads=8)

        # Should get info about OpenMP inefficiency beyond ~4 threads
        openmp_info = any(
            "OpenMP" in str(issue) and "diminishing" in str(issue) for issue in result.issues
        )
        assert openmp_info


class TestConfigurationRecommendations(TestExecutionValidator):
    """Test optimal configuration recommendations."""

    def test_recommend_for_single_mesh(
        self, validator: ExecutionValidator, single_mesh_sim: Simulation
    ) -> None:
        """Test recommendation for single mesh simulation."""
        config = validator.suggest_config(single_mesh_sim)

        assert config["n_mpi"] == 1
        assert config["n_threads"] >= 1  # Should use at least 1 thread
        assert "OpenMP" in config["rationale"]
        assert "Single mesh" in config["rationale"]

    def test_recommend_for_multi_mesh(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test recommendation for multi-mesh simulation."""
        config = validator.suggest_config(multi_mesh_sim)

        assert config["n_mpi"] >= 2  # Should use MPI
        assert "MPI" in config["rationale"]
        assert "Multi-mesh" in config["rationale"]

    def test_recommend_with_limited_cores(self, validator: ExecutionValidator) -> None:
        """Test recommendation adapts to available cores."""
        sim = Simulation(chid="test")

        # Create more meshes than likely available cores
        for i in range(100):
            sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(i, i + 1, 0, 1, 0, 1)))

        config = validator.suggest_config(sim)

        cpu_count = os.cpu_count() or 1
        # Should not recommend more MPI processes than available cores
        assert config["n_mpi"] <= cpu_count

    def test_recommend_with_no_meshes(self, validator: ExecutionValidator) -> None:
        """Test recommendation when no meshes defined."""
        sim = Simulation(chid="test")

        config = validator.suggest_config(sim)

        assert config["n_mpi"] == 1
        assert config["n_threads"] == 1
        assert "No meshes" in config["rationale"]


class TestValidateAll(TestExecutionValidator):
    """Test comprehensive validation."""

    def test_validate_combines_checks(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test validate runs all validation checks."""
        cpu_count = os.cpu_count() or 8

        # Configuration that should trigger multiple warnings
        # Use 3 MPI for 4 meshes (uneven) and exceed CPU count for threads
        result = validator.validate(
            multi_mesh_sim,
            n_mpi=3,  # Uneven division of 4 meshes
            n_threads=cpu_count + 2,  # Exceeds cores
        )

        # Should get warnings from multiple validators
        assert len(result.issues) >= 2

        # Check we got issues from different validators
        issue_strs = [str(issue) for issue in result.issues]
        has_mpi_warning = any("mesh" in w.lower() for w in issue_strs)
        has_thread_warning = any("thread" in w.lower() for w in issue_strs)

        assert has_mpi_warning
        assert has_thread_warning

    def test_validate_optimal_config(
        self, validator: ExecutionValidator, multi_mesh_sim: Simulation
    ) -> None:
        """Test validate with optimal configuration."""
        # Use recommended configuration
        config = validator.suggest_config(multi_mesh_sim)

        result = validator.validate(
            multi_mesh_sim,
            n_mpi=config["n_mpi"],
            n_threads=config["n_threads"],
        )

        # Optimal config may still have info messages, but at most one warning
        # (on systems with limited cores relative to mesh count)
        from pyfds.core.enums import Severity

        critical_warnings = [i for i in result.issues if i.severity == Severity.WARNING]
        assert len(critical_warnings) <= 1
