"""
Unit tests for CSV parser.
"""

from pathlib import Path

import polars as pl
import pytest

from pyfds.io.parsers import CSVParser


@pytest.fixture
def temp_csv_dir(tmp_path: Path) -> Path:
    """Create a temporary directory for CSV files."""
    return tmp_path


@pytest.fixture
def sample_devc_csv(temp_csv_dir: Path) -> Path:
    """Create a sample device CSV file."""
    csv_file = temp_csv_dir / "test_devc.csv"
    content = """Time,TEMP_1,TEMP_2,TEMP_3
s,C,C,C
0.0,20.0,20.0,20.0
1.0,25.0,22.0,21.0
2.0,30.0,25.0,23.0
3.0,35.0,28.0,26.0
4.0,40.0,32.0,29.0
5.0,45.0,35.0,32.0
"""
    csv_file.write_text(content)
    return csv_file


@pytest.fixture
def sample_hrr_csv(temp_csv_dir: Path) -> Path:
    """Create a sample HRR CSV file."""
    csv_file = temp_csv_dir / "test_hrr.csv"
    content = """Time,HRR,Q_RADI
s,kW,kW
0.0,0.0,0.0
1.0,100.0,20.0
2.0,250.0,50.0
3.0,400.0,80.0
4.0,500.0,100.0
5.0,450.0,90.0
"""
    csv_file.write_text(content)
    return csv_file


class TestCSVParser:
    """Test CSV parser functionality."""

    def test_parse_basic(self, sample_devc_csv: Path) -> None:
        """Test basic CSV parsing."""
        parser = CSVParser()
        df = parser.parse(sample_devc_csv)

        assert isinstance(df, pl.DataFrame)
        assert "Time" in df.columns
        assert "TEMP_1" in df.columns
        assert len(df) == 6  # Header + units + 6 data rows - units row

    def test_parse_devc(self, sample_devc_csv: Path) -> None:
        """Test device CSV parsing."""
        parser = CSVParser()
        df = parser.parse_devc(sample_devc_csv)

        assert isinstance(df, pl.DataFrame)
        assert "Time" in df.columns
        assert "TEMP_1" in df.columns
        assert "TEMP_2" in df.columns
        assert "TEMP_3" in df.columns

        # Check data values
        assert df["Time"][0] == 0.0
        assert df["TEMP_1"][0] == 20.0

    def test_parse_hrr(self, sample_hrr_csv: Path) -> None:
        """Test HRR CSV parsing."""
        parser = CSVParser()
        df = parser.parse_hrr(sample_hrr_csv)

        assert isinstance(df, pl.DataFrame)
        assert "Time" in df.columns
        assert "HRR" in df.columns

        # Check peak HRR
        peak_hrr = df["HRR"].max()
        assert peak_hrr == 500.0

    def test_get_device_data(self, sample_devc_csv: Path) -> None:
        """Test extracting specific device data."""
        parser = CSVParser()
        df = parser.parse_devc(sample_devc_csv)

        device_df = parser.get_device_data(df, "TEMP_1")
        assert "Time" in device_df.columns
        assert "TEMP_1" in device_df.columns
        assert len(device_df.columns) == 2

    def test_get_device_data_invalid(self, sample_devc_csv: Path) -> None:
        """Test extracting non-existent device."""
        parser = CSVParser()
        df = parser.parse_devc(sample_devc_csv)

        with pytest.raises(ValueError, match="Device 'INVALID' not found"):
            parser.get_device_data(df, "INVALID")

    def test_list_devices(self, sample_devc_csv: Path) -> None:
        """Test listing available devices."""
        parser = CSVParser()
        df = parser.parse_devc(sample_devc_csv)

        devices = parser.list_devices(df)
        assert "TEMP_1" in devices
        assert "TEMP_2" in devices
        assert "TEMP_3" in devices
        assert "Time" not in devices

    def test_parse_missing_file(self, temp_csv_dir: Path) -> None:
        """Test parsing non-existent file."""
        parser = CSVParser()
        missing_file = temp_csv_dir / "missing.csv"

        with pytest.raises(FileNotFoundError):
            parser.parse(missing_file)

    def test_parse_empty_csv(self, temp_csv_dir: Path) -> None:
        """Test parsing empty CSV file."""
        empty_csv = temp_csv_dir / "empty.csv"
        empty_csv.write_text("")

        parser = CSVParser()
        with pytest.raises(ValueError):
            parser.parse(empty_csv)

    def test_parse_no_time_column(self, temp_csv_dir: Path) -> None:
        """Test parsing CSV without Time column."""
        csv_file = temp_csv_dir / "no_time.csv"
        content = """Value1,Value2
1.0,2.0
3.0,4.0
"""
        csv_file.write_text(content)

        parser = CSVParser()
        with pytest.raises(ValueError, match="No Time column found"):
            parser.parse_devc(csv_file)
