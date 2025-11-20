"""Unit tests for validator file validation functions."""

from pathlib import Path

import pytest

from pyfds.core.validator import ValidationError, validate_fds_file


def test_validate_fds_file_valid(temp_fds_file):
    """Test validate_fds_file with a valid FDS file."""
    assert validate_fds_file(temp_fds_file) is True


def test_validate_fds_file_missing():
    """Test validate_fds_file with a missing file."""
    nonexistent = Path("/nonexistent/path/test.fds")
    with pytest.raises(ValidationError, match="File not found"):
        validate_fds_file(nonexistent)


def test_validate_fds_file_wrong_extension(tmp_path):
    """Test validate_fds_file with wrong file extension."""
    wrong_file = tmp_path / "test.txt"
    wrong_file.write_text("&HEAD CHID='test' /\n&TAIL /")

    with pytest.raises(ValidationError, match=r"must have \.fds extension"):
        validate_fds_file(wrong_file)


def test_validate_fds_file_missing_head(tmp_path):
    """Test validate_fds_file with missing &HEAD namelist."""
    fds_file = tmp_path / "test.fds"
    fds_file.write_text("&TIME T_END=100.0 /\n&TAIL /")

    with pytest.raises(ValidationError, match="Missing &HEAD"):
        validate_fds_file(fds_file)


def test_validate_fds_file_missing_tail(tmp_path):
    """Test validate_fds_file with missing &TAIL namelist."""
    fds_file = tmp_path / "test.fds"
    fds_file.write_text("&HEAD CHID='test' /\n&TIME T_END=100.0 /")

    with pytest.raises(ValidationError, match="Missing &TAIL"):
        validate_fds_file(fds_file)


def test_validate_fds_file_unbalanced_namelists(tmp_path):
    """Test validate_fds_file with unbalanced namelists."""
    fds_file = tmp_path / "test.fds"
    fds_content = """&HEAD CHID='test' /
&TIME T_END=100.0
&MESH IJK=10,10,10, XB=0,1,0,1,0,1 /
&TAIL /
"""
    fds_file.write_text(fds_content)

    with pytest.raises(ValidationError, match="Unbalanced namelists"):
        validate_fds_file(fds_file)


def test_validate_fds_file_complete(tmp_path):
    """Test validate_fds_file with a complete valid file."""
    fds_file = tmp_path / "complete.fds"
    fds_content = """&HEAD CHID='complete_test', TITLE='Complete Test' /
&TIME T_END=600.0, DT=0.1 /
&MESH IJK=50,50,25, XB=0,5,0,5,0,2.5 /
&SURF ID='FIRE', HRRPUA=1000.0, COLOR='RED' /
&OBST XB=2,3,2,3,0,0.1, SURF_ID='FIRE' /
&DEVC ID='TEMP1', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,1.0 /
&TAIL /
"""
    fds_file.write_text(fds_content)

    assert validate_fds_file(fds_file) is True


def test_validate_fds_file_empty(tmp_path):
    """Test validate_fds_file with empty file."""
    fds_file = tmp_path / "empty.fds"
    fds_file.write_text("")

    with pytest.raises(ValidationError, match="Missing &HEAD"):
        validate_fds_file(fds_file)


def test_validate_fds_file_case_insensitive(tmp_path):
    """Test that namelist matching is case insensitive."""
    fds_file = tmp_path / "case_test.fds"
    fds_content = """&head chid='test' /
&time t_end=100.0 /
&mesh ijk=10,10,10, xb=0,1,0,1,0,1 /
&tail /
"""
    fds_file.write_text(fds_content)

    # Should pass - FDS is case insensitive
    assert validate_fds_file(fds_file) is True
