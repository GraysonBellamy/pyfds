"""Unit tests for TIME namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Time


class TestTime:
    """Tests for Time namelist."""

    def test_basic_creation(self):
        """Test basic Time creation."""
        time = Time(t_end=600.0)
        assert time.t_end == 600.0

    def test_with_optional_params(self):
        """Test Time with optional parameters."""
        time = Time(t_end=600.0, dt=0.1, t_begin=10.0)
        assert time.t_end == 600.0
        assert time.dt == 0.1
        assert time.t_begin == 10.0

    def test_to_fds_basic(self):
        """Test FDS output format."""
        time = Time(t_end=600.0)
        fds_str = time.to_fds()
        assert "&TIME" in fds_str
        assert "T_END=600.0" in fds_str

    def test_to_fds_with_dt(self):
        """Test FDS output with time step."""
        time = Time(t_end=600.0, dt=0.1)
        fds_str = time.to_fds()
        assert "DT=0.1" in fds_str

    def test_t_end_validation_positive(self):
        """Test T_END must be positive."""
        with pytest.raises(ValidationError):
            Time(t_end=-100.0)

    def test_t_begin_validation_negative(self):
        """Test T_BEGIN cannot be negative."""
        with pytest.raises(ValidationError):
            Time(t_end=100.0, t_begin=-10.0)
