"""
Unit tests for execution module.
"""

import os
from pathlib import Path

import pytest

from pyfds.execution import FDSNotFoundError
from pyfds.execution.process import (
    build_fds_command,
    find_fds_executable,
    get_environment_for_execution,
)


class TestFDSExecutableDiscovery:
    """Test FDS executable discovery."""

    def test_find_fds_from_path(self) -> None:
        """Test finding FDS in system PATH."""
        # This test will only pass if FDS is actually in PATH
        try:
            fds_path = find_fds_executable()
            assert fds_path.exists()
            assert fds_path.is_file()
        except FDSNotFoundError:
            pytest.skip("FDS not found in PATH")

    def test_find_fds_with_env_var(self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
        """Test finding FDS via environment variable."""
        # Create a fake FDS executable
        fake_fds = tmp_path / "fds"
        fake_fds.write_text("#!/bin/bash\necho 'FDS Version 6.9.0'")
        fake_fds.chmod(0o755)

        # Set environment variable
        monkeypatch.setenv("FDS_EXECUTABLE", str(fake_fds))

        fds_path = find_fds_executable()
        assert fds_path == fake_fds


class TestCommandBuilder:
    """Test FDS command building."""

    def test_build_basic_command(self, tmp_path: Path) -> None:
        """Test building basic FDS command."""
        fds_exe = tmp_path / "fds"
        fds_file = tmp_path / "test.fds"

        cmd = build_fds_command(
            fds_file=fds_file,
            fds_executable=fds_exe,
            n_threads=1,
            n_mpi=1,
        )

        assert str(fds_exe) in cmd
        assert str(fds_file) in cmd
        assert len(cmd) == 2  # Just fds and input file

    def test_build_command_with_openmp(self, tmp_path: Path) -> None:
        """Test building command with OpenMP threads."""
        fds_exe = tmp_path / "fds"
        fds_file = tmp_path / "test.fds"

        cmd = build_fds_command(
            fds_file=fds_file,
            fds_executable=fds_exe,
            n_threads=4,
            n_mpi=1,
        )

        # OpenMP is controlled by OMP_NUM_THREADS environment variable, not command-line flags
        assert str(fds_exe) in cmd
        assert str(fds_file) in cmd
        assert len(cmd) == 2  # Just [fds_exe, fds_file]

    def test_build_command_with_mpi(self, tmp_path: Path) -> None:
        """Test building command with MPI."""
        fds_exe = tmp_path / "fds"
        fds_file = tmp_path / "test.fds"

        cmd = build_fds_command(
            fds_file=fds_file,
            fds_executable=fds_exe,
            n_threads=1,
            n_mpi=2,
            mpiexec_path="mpiexec",
        )

        assert "mpiexec" in cmd
        assert "-n" in cmd
        assert "2" in cmd

    def test_build_command_with_mpi_and_openmp(self, tmp_path: Path) -> None:
        """Test building command with both MPI and OpenMP."""
        fds_exe = tmp_path / "fds"
        fds_file = tmp_path / "test.fds"

        cmd = build_fds_command(
            fds_file=fds_file,
            fds_executable=fds_exe,
            n_threads=4,
            n_mpi=2,
        )

        # MPI is in command, OpenMP is via OMP_NUM_THREADS environment variable
        assert "mpiexec" in cmd
        assert "-n" in cmd
        assert "2" in cmd
        assert str(fds_exe) in cmd
        assert str(fds_file) in cmd


class TestEnvironmentSetup:
    """Test environment variable setup."""

    def test_get_environment_basic(self) -> None:
        """Test basic environment setup."""
        env = get_environment_for_execution(n_threads=1)

        assert "OMP_NUM_THREADS" in env
        assert env["OMP_NUM_THREADS"] == "1"

    def test_get_environment_with_threads(self) -> None:
        """Test environment with multiple threads."""
        env = get_environment_for_execution(n_threads=8)

        assert env["OMP_NUM_THREADS"] == "8"

    def test_get_environment_preserves_existing(self) -> None:
        """Test that existing environment is preserved."""
        # Get current PATH
        current_path = os.environ.get("PATH", "")

        env = get_environment_for_execution(n_threads=4)

        # PATH should be preserved
        assert "PATH" in env
        assert env["PATH"] == current_path
