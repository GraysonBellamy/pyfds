"""Unit tests for HEAD namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Head


class TestHead:
    """Tests for Head namelist."""

    def test_basic_creation(self):
        """Test basic Head creation."""
        head = Head(chid="test_case")
        assert head.chid == "test_case"
        assert head.title is None

    def test_with_title(self):
        """Test Head with title."""
        head = Head(chid="test_case", title="Test Simulation")
        assert head.chid == "test_case"
        assert head.title == "Test Simulation"

    def test_to_fds_basic(self):
        """Test FDS output format."""
        head = Head(chid="test_case")
        fds_str = head.to_fds()
        assert "&HEAD" in fds_str
        assert "CHID='test_case'" in fds_str
        assert fds_str.endswith("/\n")

    def test_to_fds_with_title(self):
        """Test FDS output with title."""
        head = Head(chid="test_case", title="Test Simulation")
        fds_str = head.to_fds()
        assert "TITLE='Test Simulation'" in fds_str

    def test_chid_validation_empty(self):
        """Test CHID cannot be empty."""
        with pytest.raises(ValidationError):
            Head(chid="")

    def test_chid_validation_spaces(self):
        """Test CHID cannot contain spaces."""
        with pytest.raises(ValidationError):
            Head(chid="test case")

    def test_chid_validation_periods(self):
        """Test CHID cannot contain periods (per FDS User Guide)."""
        with pytest.raises(ValidationError):
            Head(chid="test.case")

    def test_chid_validation_length(self):
        """Test CHID length limit (50 characters per FDS User Guide)."""
        # 50 characters should be valid
        head = Head(chid="a" * 50)
        assert len(head.chid) == 50

        # 51 characters should fail
        with pytest.raises(ValidationError):
            Head(chid="a" * 51)

    def test_title_validation_length(self):
        """Test TITLE length limit (256 characters per FDS User Guide)."""
        # 256 characters should be valid
        head = Head(chid="test", title="a" * 256)
        assert len(head.title) == 256

        # 257 characters should fail
        with pytest.raises(ValidationError):
            Head(chid="test", title="a" * 257)
