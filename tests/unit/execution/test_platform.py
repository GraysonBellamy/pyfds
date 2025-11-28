"""
Unit tests for platform-specific execution handling.
"""

import platform
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from pyfds.execution.platform import PlatformExecutor


class TestPlatformExecutor:
    """Test platform-specific FDS execution."""

    @pytest.fixture
    def temp_fds_exe(self, tmp_path: Path) -> Path:
        """Create temporary FDS executable."""
        fds_exe = tmp_path / "fds"
        fds_exe.write_text("#!/bin/bash\necho 'FDS'")
        fds_exe.chmod(0o755)
        return fds_exe


class TestPlatformDetection(TestPlatformExecutor):
    """Test platform detection and FDS command discovery."""

    def test_explicit_fds_executable(self, temp_fds_exe: Path) -> None:
        """Test using explicitly provided FDS executable."""
        executor = PlatformExecutor(fds_executable=temp_fds_exe)

        assert executor.fds_command == temp_fds_exe
        assert executor.platform == platform.system()

    @patch("shutil.which")
    @patch("pyfds.execution.platform.platform.system")
    def test_windows_finds_fds_local(
        self,
        mock_platform: MagicMock,
        mock_which: MagicMock,
        tmp_path: Path,
    ) -> None:
        """Test Windows preferentially finds fds_local wrapper."""
        mock_platform.return_value = "Windows"

        fds_local = tmp_path / "fds_local.bat"
        fds_local.write_text("@echo off\nfds.exe %*")

        def which_side_effect(cmd: str):
            if cmd == "fds_local":
                return str(fds_local)
            return None

        mock_which.side_effect = which_side_effect

        executor = PlatformExecutor()

        assert executor.uses_wrapper is True
        assert executor.fds_command == fds_local

    @patch("shutil.which")
    @patch("pyfds.execution.platform.platform.system")
    def test_linux_uses_standard_fds(
        self,
        mock_platform: MagicMock,
        mock_which: MagicMock,
        temp_fds_exe: Path,
    ) -> None:
        """Test Linux uses standard FDS executable."""
        mock_platform.return_value = "Linux"

        def which_side_effect(cmd: str):
            if cmd == "fds":
                return str(temp_fds_exe)
            return None

        mock_which.side_effect = which_side_effect

        executor = PlatformExecutor()

        assert executor.uses_wrapper is False
        assert executor.fds_command == temp_fds_exe

    def test_is_fds_local_wrapper(self, tmp_path: Path) -> None:
        """Test detection of fds_local wrapper."""
        executor = PlatformExecutor(fds_executable=tmp_path / "fds")

        # Test various wrapper names
        assert executor._is_fds_local_wrapper(Path("fds_local"))
        assert executor._is_fds_local_wrapper(Path("fds_local.bat"))
        assert executor._is_fds_local_wrapper(Path("/usr/bin/fds_local"))

        # Standard fds should not be detected as wrapper
        assert not executor._is_fds_local_wrapper(Path("fds"))
        assert not executor._is_fds_local_wrapper(Path("/usr/bin/fds"))


class TestCommandBuilding(TestPlatformExecutor):
    """Test platform-appropriate command building."""

    @patch("pyfds.execution.platform.platform.system")
    def test_windows_wrapper_command(self, mock_platform: MagicMock, tmp_path: Path) -> None:
        """Test Windows fds_local wrapper command building."""
        mock_platform.return_value = "Windows"

        fds_local = tmp_path / "fds_local.bat"
        fds_local.write_text("@echo off")
        fds_file = tmp_path / "test.fds"

        executor = PlatformExecutor(fds_executable=fds_local)
        executor.uses_wrapper = True  # Force wrapper mode

        cmd = executor.build_command(
            fds_file=fds_file,
            n_mpi=4,
            n_threads=2,
        )

        assert str(fds_local) in cmd
        assert "-p" in cmd
        assert "4" in cmd  # MPI processes
        assert "-o" in cmd
        assert "2" in cmd  # OpenMP threads
        assert str(fds_file) in cmd

    @patch("pyfds.execution.platform.platform.system")
    def test_windows_wrapper_single_process(self, mock_platform: MagicMock, tmp_path: Path) -> None:
        """Test Windows wrapper with single process (no -p flag)."""
        mock_platform.return_value = "Windows"

        fds_local = tmp_path / "fds_local.bat"
        fds_local.write_text("@echo off")
        fds_file = tmp_path / "test.fds"

        executor = PlatformExecutor(fds_executable=fds_local)
        executor.uses_wrapper = True

        cmd = executor.build_command(
            fds_file=fds_file,
            n_mpi=1,
            n_threads=1,
        )

        # Should not have -p or -o for single process/thread
        assert "-p" not in cmd
        assert "-o" not in cmd
        assert str(fds_file) in cmd

    def test_standard_mpi_command(self, temp_fds_exe: Path, tmp_path: Path) -> None:
        """Test standard mpiexec command building (Linux/macOS)."""
        fds_file = tmp_path / "test.fds"

        executor = PlatformExecutor(fds_executable=temp_fds_exe)
        executor.uses_wrapper = False  # Force standard mode

        cmd = executor.build_command(
            fds_file=fds_file,
            n_mpi=4,
            n_threads=2,
            mpiexec_path="mpiexec",
        )

        assert "mpiexec" in cmd
        assert "-n" in cmd
        assert "4" in cmd
        assert str(temp_fds_exe) in cmd
        assert str(fds_file) in cmd

        # OpenMP controlled by environment variable, not command-line
        assert "-o" not in cmd

    def test_standard_command_single_process(self, temp_fds_exe: Path, tmp_path: Path) -> None:
        """Test standard command with single MPI process."""
        fds_file = tmp_path / "test.fds"

        executor = PlatformExecutor(fds_executable=temp_fds_exe)
        executor.uses_wrapper = False

        cmd = executor.build_command(
            fds_file=fds_file,
            n_mpi=1,
            n_threads=4,
        )

        # Should not have mpiexec for single process
        assert "mpiexec" not in cmd
        assert str(temp_fds_exe) in cmd
        assert str(fds_file) in cmd

    def test_command_absolute_paths(self, temp_fds_exe: Path, tmp_path: Path) -> None:
        """Test that command uses absolute paths."""
        fds_file = tmp_path / "subdir" / "test.fds"
        fds_file.parent.mkdir()
        fds_file.touch()

        executor = PlatformExecutor(fds_executable=temp_fds_exe)

        cmd = executor.build_command(fds_file=fds_file, n_mpi=1, n_threads=1)

        # All paths should be absolute
        assert str(fds_file.absolute()) in " ".join(cmd)


class TestGetInfo(TestPlatformExecutor):
    """Test platform executor information retrieval."""

    def test_get_info_standard(self, temp_fds_exe: Path) -> None:
        """Test get_info with standard FDS executable."""
        executor = PlatformExecutor(fds_executable=temp_fds_exe)
        executor.uses_wrapper = False

        info = executor.get_info()

        assert "platform" in info
        assert "fds_command" in info
        assert "uses_wrapper" in info
        assert "wrapper_type" in info

        assert info["fds_command"] == str(temp_fds_exe)
        assert info["uses_wrapper"] is False
        assert info["wrapper_type"] == "none"

    def test_get_info_wrapper(self, tmp_path: Path) -> None:
        """Test get_info with fds_local wrapper."""
        fds_local = tmp_path / "fds_local"
        fds_local.write_text("#!/bin/bash\nfds $@")
        fds_local.chmod(0o755)

        executor = PlatformExecutor(fds_executable=fds_local)

        info = executor.get_info()

        assert info["uses_wrapper"] is True
        assert info["wrapper_type"] == "fds_local"


class TestWindowsSpecificBehavior(TestPlatformExecutor):
    """Test Windows-specific FDS execution behavior."""

    @patch("pyfds.execution.platform.platform.system")
    @patch("shutil.which")
    def test_windows_warns_if_no_fds_local(
        self,
        mock_which: MagicMock,
        mock_platform: MagicMock,
        temp_fds_exe: Path,
        caplog,
    ) -> None:
        """Test warning on Windows when fds_local not found."""
        mock_platform.return_value = "Windows"

        def which_side_effect(cmd: str):
            if cmd in ("fds_local", "fds_local.bat"):
                return None
            if cmd == "fds":
                return str(temp_fds_exe)
            return None

        mock_which.side_effect = which_side_effect

        with patch.object(Path, "exists", return_value=True):
            executor = PlatformExecutor()

        # Should have logged warning about missing fds_local
        assert executor.uses_wrapper is False

    @patch("pyfds.execution.platform.platform.system")
    def test_windows_wrapper_syntax_order(self, mock_platform: MagicMock, tmp_path: Path) -> None:
        """Test Windows wrapper command syntax order."""
        mock_platform.return_value = "Windows"

        fds_local = tmp_path / "fds_local.bat"
        fds_local.write_text("@echo off")
        fds_file = tmp_path / "test.fds"

        executor = PlatformExecutor(fds_executable=fds_local)
        executor.uses_wrapper = True

        cmd = executor.build_command(
            fds_file=fds_file,
            n_mpi=4,
            n_threads=2,
        )

        # Check command order: fds_local -p 4 -o 2 test.fds
        p_index = cmd.index("-p")
        o_index = cmd.index("-o")
        file_index = cmd.index(str(fds_file))

        assert p_index < o_index < file_index
