"""Unit tests for RAMP namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Ramp


class TestRamp:
    """Tests for Ramp namelist."""

    def test_basic_creation(self):
        """Test basic ramp creation."""
        ramp = Ramp(id="TEST_RAMP", points=[(0, 0), (100, 1000)])
        assert ramp.id == "TEST_RAMP"
        assert len(ramp.points) == 2

    def test_ramp_sorting(self):
        """Test that points are automatically sorted."""
        ramp = Ramp(id="TEST", points=[(100, 1), (50, 0.5), (0, 0)])
        assert ramp.points[0] == (0, 0)
        assert ramp.points[1] == (50, 0.5)
        assert ramp.points[2] == (100, 1)

    def test_ramp_validation_min_points(self):
        """Test that ramp requires at least 2 points."""
        with pytest.raises(ValidationError, match="at least 2 points"):
            Ramp(id="TEST", points=[(0, 0)])

    def test_ramp_validation_duplicate_t(self):
        """Test that duplicate T values are rejected."""
        with pytest.raises(ValidationError, match="duplicate T values"):
            Ramp(id="TEST", points=[(0, 0), (0, 1), (100, 1000)])

    def test_ramp_evaluate(self):
        """Test ramp evaluation with linear interpolation."""
        ramp = Ramp(id="TEST", points=[(0, 0), (100, 1000)])
        assert ramp.evaluate(0) == 0
        assert ramp.evaluate(50) == 500
        assert ramp.evaluate(100) == 1000

    def test_ramp_to_fds(self):
        """Test FDS output format."""
        ramp = Ramp(id="HRR_RAMP", points=[(0, 0), (300, 1000)])
        fds_str = ramp.to_fds()
        assert "&RAMP ID='HRR_RAMP'" in fds_str
        assert "T=0" in fds_str
        assert "F=0" in fds_str
