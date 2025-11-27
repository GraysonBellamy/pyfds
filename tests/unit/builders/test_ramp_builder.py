"""Tests for RampBuilder class."""

import pytest

from pyfds.builders import RampBuilder


class TestRampBuilder:
    """Test RampBuilder functionality."""

    def test_linear_ramp(self):
        """Test linear ramp creation."""
        ramp = RampBuilder("LINEAR").linear(0, 300, 0, 1000).build()

        assert ramp.id == "LINEAR"
        assert len(ramp.points) == 2
        assert ramp.points[0] == (0, 0)
        assert ramp.points[1] == (300, 1000)
        assert ramp.evaluate(150) == 500  # Midpoint

    def test_step_function(self):
        """Test step function creation."""
        ramp = RampBuilder("STEP").step(60, 0, 1).build()

        assert ramp.id == "STEP"
        assert len(ramp.points) == 3
        assert ramp.evaluate(0) == 0
        assert ramp.evaluate(59) == pytest.approx(0, abs=0.01)
        assert ramp.evaluate(61) == 1

    def test_t_squared_medium(self):
        """Test medium t-squared fire growth."""
        ramp = RampBuilder("FIRE").t_squared("MEDIUM", 2500, 300).build()

        assert ramp.id == "FIRE"
        assert len(ramp.points) > 10  # Multiple points for smooth curve
        assert ramp.evaluate(300) == 2500  # Peak value
        assert ramp.evaluate(0) == 0  # Start at zero

    def test_t_squared_fast(self):
        """Test fast t-squared fire growth."""
        ramp = RampBuilder("FIRE_FAST").t_squared("FAST", 3000, 150).build()

        assert ramp.id == "FIRE_FAST"
        assert ramp.evaluate(150) == 3000
        # Growth should be faster than medium
        alpha_fast = 1055 / (150**2)
        expected_at_75 = alpha_fast * 75**2
        assert ramp.evaluate(75) == pytest.approx(expected_at_75, rel=0.1)

    def test_t_squared_custom_alpha(self):
        """Test t-squared with custom growth rate."""
        alpha = 0.05  # kW/s²
        ramp = RampBuilder("CUSTOM").t_squared(alpha, 1000, 200).build()

        assert ramp.evaluate(100) == pytest.approx(alpha * 100**2, rel=0.1)

    def test_t_squared_invalid_rate(self):
        """Test t-squared with invalid growth rate."""
        with pytest.raises(ValueError, match="Unknown growth rate"):
            RampBuilder("FIRE").t_squared("INVALID", 1000, 100).build()

    def test_temperature_table(self):
        """Test temperature-dependent property table."""
        table = {20: 45.8, 100: 43.3, 200: 40.7, 400: 36.4}
        ramp = RampBuilder("STEEL_K").temperature_table(table).build()

        assert ramp.id == "STEEL_K"
        assert len(ramp.points) == 4
        assert ramp.evaluate(20) == 45.8
        assert ramp.evaluate(400) == 36.4
        # Test interpolation
        assert 40 < ramp.evaluate(150) < 45

    def test_exponential_growth(self):
        """Test exponential growth curve."""
        ramp = RampBuilder("EXP").exponential(0, 300, 1, 1000, n_points=20).build()

        assert len(ramp.points) == 20
        assert ramp.evaluate(0) == pytest.approx(1, abs=1)
        assert ramp.evaluate(300) == pytest.approx(1000, abs=1)
        # Should be curved
        midpoint = ramp.evaluate(150)
        assert midpoint > 1 and midpoint < 500  # Less than linear midpoint

    def test_sine_wave(self):
        """Test sinusoidal variation."""
        ramp = (
            RampBuilder("SINE")
            .sine_wave(t_start=0, t_end=120, amplitude=50, period=60, offset=100)
            .build()
        )

        assert len(ramp.points) == 50  # Default n_points
        assert ramp.evaluate(0) == pytest.approx(100, abs=1)  # At offset
        assert ramp.evaluate(15) == pytest.approx(150, abs=2)  # At peak
        assert ramp.evaluate(45) == pytest.approx(50, abs=2)  # At trough

    def test_add_single_point(self):
        """Test adding single points."""
        ramp = (
            RampBuilder("CUSTOM").add_point(0, 0).add_point(100, 500).add_point(200, 1000).build()
        )

        assert len(ramp.points) == 3
        assert ramp.evaluate(100) == 500

    def test_add_multiple_points(self):
        """Test adding multiple points at once."""
        points = [(0, 0), (100, 300), (200, 800), (300, 1200)]
        ramp = RampBuilder("MULTI").add_points(points).build()

        assert len(ramp.points) == 4
        assert ramp.evaluate(100) == 300

    def test_no_points_error(self):
        """Test error when no points defined."""
        with pytest.raises(ValueError, match="No points defined"):
            RampBuilder("EMPTY").build()

    def test_builder_reuse_error(self):
        """Test that builder cannot be reused."""
        builder = RampBuilder("TEST").linear(0, 100, 0, 1)
        builder.build()

        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()

    def test_fds_output_format(self):
        """Test FDS output format."""
        ramp = RampBuilder("TEST").add_point(0, 0).add_point(100, 1).build()

        fds_output = ramp.to_fds()
        assert "&RAMP ID='TEST'" in fds_output
        assert "T=0" in fds_output or "T=0.0" in fds_output
        assert "F=0" in fds_output or "F=0.0" in fds_output
        assert "T=100" in fds_output
        assert "F=1" in fds_output

    def test_points_sorted(self):
        """Test that points are automatically sorted."""
        ramp = (
            RampBuilder("UNSORTED").add_point(100, 500).add_point(0, 0).add_point(50, 250).build()
        )

        # Points should be sorted by T value
        assert ramp.points[0][0] < ramp.points[1][0] < ramp.points[2][0]
