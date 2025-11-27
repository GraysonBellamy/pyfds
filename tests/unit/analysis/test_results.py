"""
Unit tests for Results class.
"""

from pathlib import Path

import pytest

from pyfds.analysis import Results


@pytest.fixture
def sample_output_dir(tmp_path: Path) -> Path:
    """Create sample FDS output directory with files."""
    output_dir = tmp_path / "outputs"
    output_dir.mkdir()

    # Create sample devc file
    devc_file = output_dir / "test_devc.csv"
    devc_content = """Time,TEMP_1,TEMP_2,VELOCITY_1
s,C,C,m/s
0.0,20.0,20.0,0.0
1.0,25.0,22.0,0.5
2.0,30.0,25.0,1.0
3.0,35.0,28.0,1.5
4.0,40.0,32.0,2.0
"""
    devc_file.write_text(devc_content)

    # Create sample HRR file
    hrr_file = output_dir / "test_hrr.csv"
    hrr_content = """Time,HRR,Q_RADI
s,kW,kW
0.0,0.0,0.0
1.0,100.0,20.0
2.0,250.0,50.0
3.0,400.0,80.0
4.0,500.0,100.0
"""
    hrr_file.write_text(hrr_content)

    # Create sample .out file
    out_file = output_dir / "test.out"
    out_content = """
FDS Version 6.9.0
Compilation Date: -01-01

Job TITLE        : Test Simulation
Job ID string    : test

&HEAD CHID='test', TITLE='Test Simulation' /
&TIME T_END=10.0 /
T_END=10.0

Time Step    1  January 01,   12:00:00
T= 1.0 s, dt= 0.1 s

FDS completed successfully.
"""
    out_file.write_text(out_content)

    return output_dir


class TestResults:
    """Test Results class functionality."""

    def test_basic_creation(self, sample_output_dir: Path) -> None:
        """Test basic Results creation."""
        results = Results("test", output_dir=sample_output_dir)
        assert results.chid == "test"
        assert results.output_dir == sample_output_dir

    def test_devc_property(self, sample_output_dir: Path) -> None:
        """Test accessing device data."""
        results = Results("test", output_dir=sample_output_dir)
        df = results.devc

        assert "Time" in df.columns
        assert "TEMP_1" in df.columns
        assert "TEMP_2" in df.columns
        assert len(df) > 0

    def test_hrr_property(self, sample_output_dir: Path) -> None:
        """Test accessing HRR data."""
        results = Results("test", output_dir=sample_output_dir)
        df = results.hrr

        assert "Time" in df.columns
        assert "HRR" in df.columns
        peak_hrr = df["HRR"].max()
        assert peak_hrr == 500.0

    def test_devices_dict(self, sample_output_dir: Path) -> None:
        """Test accessing devices as dictionary."""
        results = Results("test", output_dir=sample_output_dir)
        devices = results.devices

        assert isinstance(devices, dict)
        assert "TEMP_1" in devices
        assert "TEMP_2" in devices
        assert "VELOCITY_1" in devices

        temp_df = devices["TEMP_1"]
        assert "Time" in temp_df.columns
        assert "TEMP_1" in temp_df.columns

    def test_get_device(self, sample_output_dir: Path) -> None:
        """Test getting specific device."""
        results = Results("test", output_dir=sample_output_dir)
        temp_df = results.get_device("TEMP_1")

        assert "Time" in temp_df.columns
        assert "TEMP_1" in temp_df.columns
        assert len(temp_df.columns) == 2

    def test_list_devices(self, sample_output_dir: Path) -> None:
        """Test listing available devices."""
        results = Results("test", output_dir=sample_output_dir)
        devices = results.list_devices()

        assert "TEMP_1" in devices
        assert "TEMP_2" in devices
        assert "VELOCITY_1" in devices
        assert "Time" not in devices

    def test_summary(self, sample_output_dir: Path) -> None:
        """Test summary statistics."""
        results = Results("test", output_dir=sample_output_dir)
        summary = results.summary()

        assert summary["chid"] == "test"
        assert "peak_hrr" in summary
        assert summary["peak_hrr"] == 500.0
        assert "duration" in summary
        assert "num_devices" in summary
        assert summary["num_devices"] == 3
        assert "device_ids" in summary

    def test_missing_devc_file(self, tmp_path: Path) -> None:
        """Test error when device file is missing."""
        results = Results("missing", output_dir=tmp_path)

        with pytest.raises(FileNotFoundError, match="Device CSV file not found"):
            _ = results.devc

    def test_missing_hrr_file(self, tmp_path: Path) -> None:
        """Test error when HRR file is missing."""
        results = Results("missing", output_dir=tmp_path)

        with pytest.raises(FileNotFoundError, match="HRR CSV file not found"):
            _ = results.hrr

    def test_file_paths(self, sample_output_dir: Path) -> None:
        """Test file path properties."""
        results = Results("test", output_dir=sample_output_dir)

        assert results.devc_file == sample_output_dir / "test_devc.csv"
        assert results.hrr_file == sample_output_dir / "test_hrr.csv"
        assert results.out_file == sample_output_dir / "test.out"

    def test_repr(self, sample_output_dir: Path) -> None:
        """Test string representation."""
        results = Results("test", output_dir=sample_output_dir)
        repr_str = repr(results)

        assert "test" in repr_str
        assert "Results" in repr_str
