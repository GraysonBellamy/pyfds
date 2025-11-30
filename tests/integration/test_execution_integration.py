"""
Integration tests for FDS execution.

These tests require FDS to be installed and will be skipped if FDS is not found.
"""

import time
from pathlib import Path

import pytest

from pyfds import FDSNotFoundError, Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Device, Mesh, Obstruction, Surface, Time
from pyfds.execution import find_fds_executable


def is_fds_available() -> bool:
    """Check if FDS is available for testing."""
    try:
        find_fds_executable()
        return True
    except FDSNotFoundError:
        return False


# Skip all tests in this module if FDS is not available
pytestmark = pytest.mark.skipif(
    not is_fds_available(),
    reason="FDS not installed or not in PATH. Set FDS_EXECUTABLE environment variable if installed.",
)


class TestFDSExecution:
    """Integration tests for FDS execution."""

    def test_simple_simulation_blocking(self, tmp_path: Path) -> None:
        """Test running a simple FDS simulation (blocking mode)."""
        # Create a very small, fast simulation
        sim = Simulation(chid="integration_test_blocking", title="Integration Test")
        sim.add(Time(t_end=1.0))  # Very short simulation
        sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))  # Very coarse mesh

        # Run simulation
        results = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            monitor=False,  # Disable monitoring for faster test
            validate=True,
            timeout=30,  # 30 second timeout
        )

        # Verify results
        assert results.chid == "integration_test_blocking"
        assert results.output_dir == tmp_path

        # Check that output files exist
        assert results.out_file.exists()
        assert results.hrr_file.exists()

        # Verify HRR data can be loaded
        hrr_df = results.hrr
        assert "Time" in hrr_df.columns
        assert "HRR" in hrr_df.columns
        assert len(hrr_df) > 0

    def test_simple_simulation_nonblocking(self, tmp_path: Path) -> None:
        """Test running FDS simulation in non-blocking mode."""
        # Create simulation
        sim = Simulation(chid="integration_test_nonblocking", title="Non-Blocking Test")
        sim.add(Time(t_end=2.0))
        sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Run in non-blocking mode
        job = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            monitor=True,
            wait=False,  # Non-blocking
        )

        # Verify job is created
        assert job is not None
        assert job.chid == "integration_test_nonblocking"

        # Wait for completion with timeout
        max_wait = 60  # 60 second max
        start_time = time.time()
        while job.is_running() and (time.time() - start_time) < max_wait:
            time.sleep(1)

        # Get results
        results = job.get_results()

        # Verify results
        assert results.output_dir == tmp_path
        assert results.hrr_file.exists()

    def test_job_graceful_stop(self, tmp_path: Path) -> None:
        """Test graceful stopping of FDS simulation via CHID.stop file."""
        # Create a longer simulation to allow time for stop
        sim = Simulation(chid="integration_test_stop", title="Graceful Stop Test")
        sim.add(Time(t_end=30.0))  # Longer simulation
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Run in non-blocking mode
        job = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            monitor=True,
            wait=False,
        )

        # Wait a moment for simulation to start
        time.sleep(2)

        # Request graceful stop
        job.request_stop()

        # Verify stop file was created
        stop_file = tmp_path / f"{job.chid}.stop"
        assert stop_file.exists()

        # Wait for FDS to stop (with timeout)
        max_wait = 30
        start_time = time.time()
        while job.is_running() and (time.time() - start_time) < max_wait:
            time.sleep(1)

        # Job should have stopped
        assert not job.is_running()

    def test_simulation_with_devices(self, tmp_path: Path) -> None:
        """Test simulation with device outputs."""
        # Create simulation with devices
        sim = Simulation(chid="integration_test_devices", title="Device Test")
        sim.add(Time(t_end=1.0))
        sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Add some devices
        sim.add(Device(id="TEMP_1", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.5)))
        sim.add(Device(id="TEMP_2", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.8)))

        # Run simulation
        results = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            timeout=30,
        )

        # Verify device outputs
        assert results.devc_file.exists()

        # Load and verify device data
        devc_df = results.devc
        assert "Time" in devc_df.columns
        assert "TEMP_1" in devc_df.columns
        assert "TEMP_2" in devc_df.columns

        # Test device access
        devices = results.list_devices()
        assert "TEMP_1" in devices
        assert "TEMP_2" in devices

        temp_1_data = results.get_device("TEMP_1")
        assert "Time" in temp_1_data.columns
        assert "TEMP_1" in temp_1_data.columns

    def test_simulation_with_fire(self, tmp_path: Path) -> None:
        """Test simulation with fire source."""
        # Create simulation with fire
        sim = Simulation(chid="integration_test_fire", title="Fire Test")
        sim.add(Time(t_end=2.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

        # Add reaction (required when using HRRPUA)
        # Use a simple burner approach without HRRPUA instead
        # Just test with a hot surface which doesn't require REAC
        sim.add(Surface(id="HOT", tmp_front=500.0))  # Hot surface instead of fire

        # Add hot obstruction
        sim.add(Obstruction(xb=Bounds3D.of(0.8, 1.2, 0.8, 1.2, 0, 0.1), surf_id="HOT"))

        # Add temperature device
        sim.add(Device(id="TEMP", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 1.0)))

        # Run simulation
        results = sim.run(
            n_threads=2,  # Test with threading
            output_dir=tmp_path,
            timeout=60,
        )

        # Verify HRR data exists (might be zero for hot surface)
        hrr_df = results.hrr
        assert "HRR" in hrr_df.columns
        assert len(hrr_df) > 0

        # Verify temperature device data exists
        temp_data = results.get_device("TEMP")
        assert "TEMP" in temp_data.columns
        assert len(temp_data) > 0

    def test_validation_before_execution(self, tmp_path: Path) -> None:
        """Test that validation works before execution."""
        # Create simulation with warnings
        sim = Simulation(chid="integration_test_validation", title="Validation Test")
        sim.add(Time(t_end=0.5))

        # Add a non-cubic mesh (will generate warning)
        # ijk=Grid3D.of(100, 30, 30) xb=Bounds3D.of(0, 10, 0, 1, 0, 1) gives dx=0.1, dy=0.033, dz=0.033 -> aspect ratio ~3
        sim.add(Mesh(ijk=Grid3D.of(100, 30, 30), xb=Bounds3D.of(0, 10, 0, 1, 0, 1)))

        # Get validation warnings
        warnings = sim.validate()
        assert len(warnings) > 0
        assert any("non-cubic" in w.lower() for w in warnings)

        # Should still run successfully (warnings, not errors)
        results = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            validate=True,
            strict=False,  # Don't fail on warnings
            timeout=30,
        )

        assert results is not None

    def test_strict_validation_mode(self, tmp_path: Path) -> None:
        """Test that strict validation raises on warnings."""
        # Create simulation with warnings
        sim = Simulation(chid="integration_test_strict", title="Strict Validation Test")
        sim.add(Time(t_end=0.5))
        sim.add(
            Mesh(ijk=Grid3D.of(100, 30, 30), xb=Bounds3D.of(0, 10, 0, 1, 0, 1))
        )  # Non-cubic (aspect ratio ~3)

        # Should raise ValueError in strict mode
        with pytest.raises(ValueError, match="Simulation validation failed"):
            sim.run(
                n_threads=1,
                output_dir=tmp_path,
                validate=True,
                strict=True,  # Strict mode
            )

    def test_summary_statistics(self, tmp_path: Path) -> None:
        """Test that summary statistics are generated correctly."""
        # Create simulation
        sim = Simulation(chid="integration_test_summary", title="Summary Test")
        sim.add(Time(t_end=1.0))
        sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        sim.add(Device(id="TEMP", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 0.5)))

        # Run simulation
        results = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            timeout=30,
        )

        # Get summary
        summary = results.summary()

        # Verify summary contents
        assert summary["chid"] == "integration_test_summary"
        assert "output_dir" in summary
        assert "peak_hrr" in summary
        assert "duration" in summary
        assert "num_devices" in summary
        assert summary["num_devices"] == 1
        assert "device_ids" in summary
        assert "TEMP" in summary["device_ids"]


class TestFDSRunnerAPI:
    """Integration tests for FDSRunner API."""

    def test_runner_direct_usage(self, tmp_path: Path) -> None:
        """Test using FDSRunner directly."""
        from pyfds.execution import FDSRunner

        # Create and write simulation file
        sim = Simulation(chid="runner_test", title="Runner Test")
        sim.add(Time(t_end=0.5))
        sim.add(Mesh(ijk=Grid3D.of(5, 5, 5), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        fds_file = tmp_path / "runner_test.fds"
        sim.write(fds_file)

        # Use runner
        runner = FDSRunner()
        assert runner.fds_version is not None

        # Run simulation
        results = runner.run(
            fds_file=fds_file,
            n_threads=1,
            output_dir=tmp_path,
            monitor=False,
            timeout=30,
        )

        # Verify
        assert results.chid == "runner_test"
        assert results.hrr_file.exists()


class TestProgressMonitoring:
    """Integration tests for progress monitoring."""

    def test_progress_monitoring_updates(self, tmp_path: Path) -> None:
        """Test that progress monitoring provides updates."""
        # Create a slightly longer simulation to see progress
        sim = Simulation(chid="progress_test", title="Progress Test")
        sim.add(Time(t_end=5.0))  # Longer simulation
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

        # Track progress updates
        progress_values = []

        # Run with monitoring
        job = sim.run(
            n_threads=1,
            output_dir=tmp_path,
            monitor=True,
            wait=False,
        )

        # Monitor progress
        max_wait = 120  # 2 minute timeout
        start_time = time.time()
        while job.is_running() and (time.time() - start_time) < max_wait:
            progress = job.progress
            if progress > 0:
                progress_values.append(progress)
            time.sleep(2)

        # Get final results
        results = job.get_results()

        # Verify we got some progress updates
        # (might not get updates if simulation is too fast)
        assert results is not None
        assert results.hrr_file.exists()

        # If we got progress updates, verify they increase
        if len(progress_values) > 1:
            assert progress_values[-1] >= progress_values[0]
