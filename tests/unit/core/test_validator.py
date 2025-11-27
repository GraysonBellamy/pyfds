"""Tests for input validation and sanitization."""

from pathlib import Path

import pytest

from pyfds.core.validator import ValidationError as CoreValidationError
from pyfds.core.validator import validate_fds_file
from pyfds.utils.validation import (
    ValidationError,
    safe_read_text,
    validate_chid,
    validate_file_size,
    validate_non_negative_number,
    validate_path,
    validate_positive_number,
)


class TestValidateChid:
    """Tests for CHID validation."""

    def test_valid_chid(self):
        """Test valid CHID passes validation."""
        assert validate_chid("test_case") == "test_case"
        assert validate_chid("test-case") == "test-case"
        assert validate_chid("TestCase123") == "TestCase123"
        assert validate_chid("a") == "a"
        assert validate_chid("A1_B2-C3") == "A1_B2-C3"

    def test_empty_chid(self):
        """Test empty CHID raises error."""
        with pytest.raises(ValidationError, match="cannot be empty"):
            validate_chid("")

    def test_chid_with_path_separators(self):
        """Test CHID with path separators raises error."""
        with pytest.raises(ValidationError, match="path separators"):
            validate_chid("test/case")

        with pytest.raises(ValidationError, match="path separators"):
            validate_chid("test\\case")

        with pytest.raises(ValidationError, match="path separators"):
            validate_chid("../test")

    def test_chid_too_long(self):
        """Test CHID exceeding max length raises error."""
        long_chid = "a" * 61
        with pytest.raises(ValidationError, match="60 characters or less"):
            validate_chid(long_chid)

    def test_chid_with_invalid_characters(self):
        """Test CHID with invalid characters raises error."""
        with pytest.raises(ValidationError, match="only contain"):
            validate_chid("test case")  # space

        with pytest.raises(ValidationError, match="only contain"):
            validate_chid("test@case")  # @

        with pytest.raises(ValidationError, match="only contain"):
            validate_chid("test.case")  # .

    def test_chid_not_string(self):
        """Test non-string CHID raises error."""
        with pytest.raises(ValidationError, match="must be a string"):
            validate_chid(123)  # type: ignore

        # None is falsy, so it triggers "cannot be empty" check first
        with pytest.raises(ValidationError):
            validate_chid(None)  # type: ignore


class TestValidatePath:
    """Tests for path validation."""

    def test_valid_path(self, tmp_path):
        """Test valid path passes validation."""
        test_file = tmp_path / "test.txt"
        test_file.write_text("test")

        result = validate_path(test_file, must_exist=True)
        assert result.is_absolute()
        assert result.exists()

    def test_path_must_exist(self, tmp_path):
        """Test must_exist validation."""
        nonexistent = tmp_path / "does_not_exist.txt"

        with pytest.raises(ValidationError, match="does not exist"):
            validate_path(nonexistent, must_exist=True)

    def test_path_must_be_file(self, tmp_path):
        """Test must_be_file validation."""
        test_file = tmp_path / "test.txt"
        test_file.write_text("test")

        # Should pass
        validate_path(test_file, must_be_file=True)

        # Directory should fail
        with pytest.raises(ValidationError, match="not a file"):
            validate_path(tmp_path, must_be_file=True)

    def test_path_must_be_dir(self, tmp_path):
        """Test must_be_dir validation."""
        # Should pass
        validate_path(tmp_path, must_be_dir=True)

        # File should fail
        test_file = tmp_path / "test.txt"
        test_file.write_text("test")
        with pytest.raises(ValidationError, match="not a directory"):
            validate_path(test_file, must_be_dir=True)

    def test_path_allow_create(self, tmp_path):
        """Test allow_create validation."""
        new_file = tmp_path / "new.txt"

        # Should pass with allow_create=True
        result = validate_path(new_file, allow_create=True)
        assert result.is_absolute()

        # Should fail with allow_create=False
        with pytest.raises(ValidationError, match="does not exist"):
            validate_path(new_file, allow_create=False)

    def test_path_parent_must_exist(self, tmp_path):
        """Test parent directory must exist for new files."""
        nonexistent_parent = tmp_path / "does_not_exist" / "file.txt"

        with pytest.raises(ValidationError, match="Parent directory does not exist"):
            validate_path(nonexistent_parent, allow_create=True)


class TestValidateFileSize:
    """Tests for file size validation."""

    def test_small_file_passes(self, tmp_path):
        """Test small file passes validation."""
        test_file = tmp_path / "small.txt"
        test_file.write_text("small content")

        result = validate_file_size(test_file)
        assert result == test_file

    def test_large_file_fails(self, tmp_path):
        """Test file exceeding max size fails."""
        test_file = tmp_path / "large.txt"
        # Create a file larger than 1KB for testing
        test_file.write_bytes(b"x" * 2000)

        with pytest.raises(ValidationError, match="too large"):
            validate_file_size(test_file, max_size=1000)

    def test_nonexistent_file(self, tmp_path):
        """Test nonexistent file raises error."""
        nonexistent = tmp_path / "does_not_exist.txt"

        with pytest.raises(ValidationError, match="does not exist"):
            validate_file_size(nonexistent)

    def test_directory_fails(self, tmp_path):
        """Test directory raises error."""
        with pytest.raises(ValidationError, match="not a file"):
            validate_file_size(tmp_path)


class TestValidatePositiveNumber:
    """Tests for positive number validation."""

    def test_positive_integers(self):
        """Test positive integers pass."""
        assert validate_positive_number(1) == 1
        assert validate_positive_number(100) == 100

    def test_positive_floats(self):
        """Test positive floats pass."""
        assert validate_positive_number(0.1) == 0.1
        assert validate_positive_number(3.14) == 3.14

    def test_zero_fails(self):
        """Test zero fails validation."""
        with pytest.raises(ValidationError, match="must be positive"):
            validate_positive_number(0)

    def test_negative_fails(self):
        """Test negative numbers fail."""
        with pytest.raises(ValidationError, match="must be positive"):
            validate_positive_number(-1)

        with pytest.raises(ValidationError, match="must be positive"):
            validate_positive_number(-0.5)

    def test_non_number_fails(self):
        """Test non-numbers fail."""
        with pytest.raises(ValidationError, match="must be a number"):
            validate_positive_number("5")  # type: ignore

    def test_custom_name_in_error(self):
        """Test custom parameter name appears in error."""
        with pytest.raises(ValidationError, match="count must be positive"):
            validate_positive_number(-1, "count")


class TestValidateNonNegativeNumber:
    """Tests for non-negative number validation."""

    def test_positive_numbers(self):
        """Test positive numbers pass."""
        assert validate_non_negative_number(1) == 1
        assert validate_non_negative_number(0.5) == 0.5

    def test_zero_passes(self):
        """Test zero passes validation."""
        assert validate_non_negative_number(0) == 0
        assert validate_non_negative_number(0.0) == 0.0

    def test_negative_fails(self):
        """Test negative numbers fail."""
        with pytest.raises(ValidationError, match="must be non-negative"):
            validate_non_negative_number(-1)

        with pytest.raises(ValidationError, match="must be non-negative"):
            validate_non_negative_number(-0.1)


class TestSafeReadText:
    """Tests for safe text file reading."""

    def test_read_small_file(self, tmp_path):
        """Test reading small text file."""
        test_file = tmp_path / "test.txt"
        content = "Test content\nLine 2"
        test_file.write_text(content)

        result = safe_read_text(test_file)
        assert result == content

    def test_read_large_file_fails(self, tmp_path):
        """Test reading large file fails."""
        test_file = tmp_path / "large.txt"
        test_file.write_bytes(b"x" * 2000)

        with pytest.raises(ValidationError, match="too large"):
            safe_read_text(test_file, max_size=1000)

    def test_read_nonexistent_file(self, tmp_path):
        """Test reading nonexistent file fails."""
        nonexistent = tmp_path / "does_not_exist.txt"

        with pytest.raises(ValidationError, match="does not exist"):
            safe_read_text(nonexistent)

    def test_read_non_utf8_file(self, tmp_path):
        """Test reading non-UTF8 file fails with helpful error."""
        test_file = tmp_path / "binary.bin"
        test_file.write_bytes(b"\x80\x81\x82\x83")

        with pytest.raises(ValidationError, match="not valid utf-8 text"):
            safe_read_text(test_file)

    def test_custom_encoding(self, tmp_path):
        """Test reading with custom encoding."""
        test_file = tmp_path / "latin1.txt"
        content = "Café"
        test_file.write_text(content, encoding="latin-1")

        result = safe_read_text(test_file, encoding="latin-1")
        assert result == content


class TestValidateFDSFile:
    """Tests for FDS file validation."""

    def test_validate_fds_file_valid(self, temp_fds_file):
        """Test validate_fds_file with a valid FDS file."""
        assert validate_fds_file(temp_fds_file) is True

    def test_validate_fds_file_missing(self):
        """Test validate_fds_file with a missing file."""
        nonexistent = Path("/nonexistent/path/test.fds")
        with pytest.raises(CoreValidationError, match="File not found"):
            validate_fds_file(nonexistent)

    def test_validate_fds_file_wrong_extension(self, tmp_path):
        """Test validate_fds_file with wrong file extension."""
        wrong_file = tmp_path / "test.txt"
        wrong_file.write_text("&HEAD CHID='test' /\n&TAIL /")

        with pytest.raises(CoreValidationError, match=r"must have \.fds extension"):
            validate_fds_file(wrong_file)

    def test_validate_fds_file_missing_head(self, tmp_path):
        """Test validate_fds_file with missing &HEAD namelist."""
        fds_file = tmp_path / "test.fds"
        fds_file.write_text("&TIME T_END=100.0 /\n&TAIL /")

        with pytest.raises(CoreValidationError, match="Missing &HEAD"):
            validate_fds_file(fds_file)

    def test_validate_fds_file_missing_tail(self, tmp_path):
        """Test validate_fds_file with missing &TAIL namelist."""
        fds_file = tmp_path / "test.fds"
        fds_file.write_text("&HEAD CHID='test' /\n&TIME T_END=100.0 /")

        with pytest.raises(CoreValidationError, match="Missing &TAIL"):
            validate_fds_file(fds_file)

    def test_validate_fds_file_unbalanced_namelists(self, tmp_path):
        """Test validate_fds_file with unbalanced namelists."""
        fds_file = tmp_path / "test.fds"
        fds_content = """&HEAD CHID='test' /
&TIME T_END=100.0
&MESH IJK=10,10,10, XB=0,1,0,1,0,1 /
&TAIL /
"""
        fds_file.write_text(fds_content)

        with pytest.raises(CoreValidationError, match="Unbalanced namelists"):
            validate_fds_file(fds_file)

    def test_validate_fds_file_complete(self, tmp_path):
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

    def test_validate_fds_file_empty(self, tmp_path):
        """Test validate_fds_file with empty file."""
        fds_file = tmp_path / "empty.fds"
        fds_file.write_text("")

        with pytest.raises(CoreValidationError, match="Missing &HEAD"):
            validate_fds_file(fds_file)

    def test_validate_fds_file_case_insensitive(self, tmp_path):
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
