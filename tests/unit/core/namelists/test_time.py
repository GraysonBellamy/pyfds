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

    def test_time_step_control_params(self):
        """Test time step control parameters."""
        time = Time(
            t_end=600.0,
            lock_time_step=True,
            restrict_time_step=False,
            limiting_dt_ratio=0.001,
        )
        assert time.lock_time_step is True
        assert time.restrict_time_step is False
        assert time.limiting_dt_ratio == 0.001

    def test_to_fds_with_lock_time_step(self):
        """Test FDS output with LOCK_TIME_STEP."""
        time = Time(t_end=600.0, dt=0.05, lock_time_step=True)
        fds_str = time.to_fds()
        assert "LOCK_TIME_STEP=.TRUE." in fds_str

    def test_to_fds_with_restrict_time_step(self):
        """Test FDS output with RESTRICT_TIME_STEP."""
        time = Time(t_end=600.0, restrict_time_step=False)
        fds_str = time.to_fds()
        assert "RESTRICT_TIME_STEP=.FALSE." in fds_str

    def test_ramp_time(self):
        """Test RAMP_TIME parameter."""
        time = Time(t_end=10.0, ramp_time="time-ramp")
        assert time.ramp_time == "time-ramp"
        fds_str = time.to_fds()
        assert "RAMP_TIME='time-ramp'" in fds_str

    def test_time_shrink_factor(self):
        """Test TIME_SHRINK_FACTOR for steady-state applications."""
        time = Time(t_end=600.0, time_shrink_factor=10.0)
        assert time.time_shrink_factor == 10.0
        fds_str = time.to_fds()
        assert "TIME_SHRINK_FACTOR=10.0" in fds_str

    def test_wall_increment(self):
        """Test WALL_INCREMENT parameter."""
        time = Time(t_end=600.0, wall_increment=1)
        assert time.wall_increment == 1
        fds_str = time.to_fds()
        assert "WALL_INCREMENT=1" in fds_str

    def test_wall_increment_validation(self):
        """Test WALL_INCREMENT must be >= 1."""
        with pytest.raises(ValidationError):
            Time(t_end=600.0, wall_increment=0)

    def test_external_control_params(self):
        """Test external control parameters."""
        time = Time(
            t_end=600.0,
            dt_external=1.0,
            dt_external_heartbeat=5.0,
            external_heartbeat_filename="heartbeat.txt",
            heartbeat_fail=True,
        )
        assert time.dt_external == 1.0
        assert time.dt_external_heartbeat == 5.0
        assert time.external_heartbeat_filename == "heartbeat.txt"
        assert time.heartbeat_fail is True

    def test_dt_end_params(self):
        """Test DT_END_FILL and DT_END_MINIMUM parameters."""
        time = Time(t_end=600.0, dt_end_fill=1e-5, dt_end_minimum=1e-10)
        assert time.dt_end_fill == 1e-5
        assert time.dt_end_minimum == 1e-10
