"""Unit tests for RAMP namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Ramp


class TestRampBasic:
    """Tests for basic Ramp creation and functionality."""

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

    def test_add_point(self):
        """Test adding points to ramp."""
        ramp = Ramp(id="TEST", points=[(0, 0), (100, 1)])
        ramp.add_point(50, 0.5)
        assert len(ramp.points) == 3
        assert ramp.points[1] == (50, 0.5)  # Should be sorted

    def test_add_point_duplicate_error(self):
        """Test that adding duplicate T value raises error."""
        ramp = Ramp(id="TEST", points=[(0, 0), (100, 1)])
        with pytest.raises(ValueError, match="duplicate T values"):
            ramp.add_point(0, 0.5)


class TestRampDeviceControl:
    """Tests for device/control-based ramps."""

    def test_devc_id_parameter(self):
        """Test DEVC_ID for device-controlled independent variable."""
        ramp = Ramp(
            id="BLOWER_RAMP",
            points=[(20, 0), (100, 0.5), (200, 1.0)],
            devc_id="TEMP_SENSOR",
        )
        assert ramp.devc_id == "TEMP_SENSOR"
        fds_str = ramp.to_fds()
        assert "DEVC_ID='TEMP_SENSOR'" in fds_str
        # DEVC_ID should only appear on first line
        assert fds_str.count("DEVC_ID") == 1

    def test_ctrl_id_parameter(self):
        """Test CTRL_ID for control-based independent variable."""
        ramp = Ramp(
            id="CTRL_RAMP",
            points=[(0, 0), (50, 0.5), (100, 1.0)],
            ctrl_id="MY_CTRL",
        )
        assert ramp.ctrl_id == "MY_CTRL"
        fds_str = ramp.to_fds()
        assert "CTRL_ID='MY_CTRL'" in fds_str

    def test_devc_id_dep_parameter(self):
        """Test DEVC_ID_DEP for dependent variable replacement."""
        ramp = Ramp(
            id="DEP_RAMP",
            devc_id_dep="OUTPUT_DEVC",
        )
        assert ramp.devc_id_dep == "OUTPUT_DEVC"
        fds_str = ramp.to_fds()
        assert "DEVC_ID_DEP='OUTPUT_DEVC'" in fds_str
        # No T/F values when using dependent variable replacement
        assert "T=" not in fds_str
        assert "F=" not in fds_str

    def test_ctrl_id_dep_parameter(self):
        """Test CTRL_ID_DEP for control-based dependent variable."""
        ramp = Ramp(
            id="CTRL_DEP_RAMP",
            ctrl_id_dep="SIN_CTRL",
        )
        assert ramp.ctrl_id_dep == "SIN_CTRL"
        fds_str = ramp.to_fds()
        assert "CTRL_ID_DEP='SIN_CTRL'" in fds_str


class TestRampExternalControl:
    """Tests for external file control."""

    def test_external_file_parameter(self):
        """Test EXTERNAL_FILE parameter."""
        ramp = Ramp(
            id="EXT_RAMP",
            external_file=True,
            initial_value=0.5,
        )
        assert ramp.external_file is True
        assert ramp.initial_value == 0.5
        fds_str = ramp.to_fds()
        assert "EXTERNAL_FILE=.TRUE." in fds_str
        assert "INITIAL_VALUE=0.5" in fds_str

    def test_external_file_with_points(self):
        """Test EXTERNAL_FILE with initial points."""
        ramp = Ramp(
            id="EXT_RAMP",
            points=[(0, 0), (100, 1)],
            external_file=True,
            initial_value=0.0,
        )
        fds_str = ramp.to_fds()
        # EXTERNAL_FILE should appear on first line
        assert "EXTERNAL_FILE=.TRUE." in fds_str
        assert fds_str.count("EXTERNAL_FILE") == 1

    def test_initial_value_parameter(self):
        """Test INITIAL_VALUE for external control."""
        ramp = Ramp(
            id="INIT_RAMP",
            external_file=True,
            initial_value=0.081633,
        )
        fds_str = ramp.to_fds()
        assert "0.081633" in fds_str


class TestRampInterpolation:
    """Tests for interpolation parameters."""

    def test_number_interpolation_points(self):
        """Test NUMBER_INTERPOLATION_POINTS parameter."""
        ramp = Ramp(
            id="INTERP_RAMP",
            points=[(0, 0), (100, 1)],
            number_interpolation_points=10000,
        )
        assert ramp.number_interpolation_points == 10000
        fds_str = ramp.to_fds()
        assert "NUMBER_INTERPOLATION_POINTS=10000" in fds_str


class TestRampSpatialVariation:
    """Tests for spatially-varying ramps (X and Z coordinates)."""

    def test_x_values_for_gravity_ramp(self):
        """Test X values for spatially-varying gravity ramps."""
        ramp = Ramp(
            id="GRAV_RAMP",
            points=[(0, -9.81), (50, -9.81), (51, -9.80), (100, -9.80)],
            x_values=[0.0, 50.0, 51.0, 100.0],
        )
        assert ramp.x_values == [0.0, 50.0, 51.0, 100.0]
        fds_str = ramp.to_fds()
        # Should use X instead of T
        assert "X=0" in fds_str
        assert "X=50" in fds_str
        assert "T=" not in fds_str

    def test_z_values_for_wind_ramp(self):
        """Test Z values for height-varying wind ramps."""
        ramp = Ramp(
            id="WIND_RAMP",
            points=[(0, 1.0), (200, 1.5), (500, 1.8)],
            z_values=[0.0, 200.0, 500.0],
        )
        assert ramp.z_values == [0.0, 200.0, 500.0]
        fds_str = ramp.to_fds()
        # Should use Z instead of T
        assert "Z=0" in fds_str
        assert "Z=200" in fds_str
        assert "T=" not in fds_str

    def test_x_values_length_mismatch(self):
        """Test that x_values must match points length."""
        with pytest.raises(ValidationError, match="x_values length"):
            Ramp(
                id="BAD_RAMP",
                points=[(0, 0), (100, 1)],
                x_values=[0.0, 50.0, 100.0],  # 3 values, 2 points
            )

    def test_z_values_length_mismatch(self):
        """Test that z_values must match points length."""
        with pytest.raises(ValidationError, match="z_values length"):
            Ramp(
                id="BAD_RAMP",
                points=[(0, 0), (100, 1)],
                z_values=[0.0],  # 1 value, 2 points
            )


class TestRampComplexScenarios:
    """Tests for complex ramp scenarios from FDS User Guide."""

    def test_blower_temperature_control(self):
        """Test blower ramp controlled by temperature device."""
        # From FDS User Guide section on RAMPDEVC
        ramp = Ramp(
            id="BLOWER RAMP",
            points=[(20, 0.0), (100, 0.5), (200, 1.0)],
            devc_id="TEMP DEVC",
        )
        fds_str = ramp.to_fds()
        assert "BLOWER RAMP" in fds_str
        assert "DEVC_ID='TEMP DEVC'" in fds_str
        assert "T=20" in fds_str
        assert "F=0" in fds_str

    def test_tunnel_gravity_variation(self):
        """Test gravity ramp varying with tunnel position."""
        # 5% slope starting at x=50m
        ramp = Ramp(
            id="z-ramp",
            points=[(0, -9.81), (50, -9.81), (51, -9.80), (100, -9.80)],
            x_values=[0.0, 50.0, 51.0, 100.0],
        )
        fds_str = ramp.to_fds()
        assert "X=0" in fds_str
        assert "X=51" in fds_str
        assert "F=-9.81" in fds_str or "F=-9.8" in fds_str

    def test_wind_height_profile(self):
        """Test wind speed multiplier varying with height."""
        ramp = Ramp(
            id="spd",
            points=[(0, 1.0), (200, 1.5), (500, 1.8)],
            z_values=[0.0, 200.0, 500.0],
        )
        fds_str = ramp.to_fds()
        assert "Z=0" in fds_str
        assert "Z=500" in fds_str
        assert "F=1.8" in fds_str

    def test_sine_function_with_ctrl_dep(self):
        """Test sine function ramp using CTRL_ID_DEP."""
        # From FDS User Guide: sin(2*pi*(t-t0)/10)
        ramp = Ramp(
            id="ramp sin",
            ctrl_id_dep="sin(2*pi*(t-t0)/10)",
        )
        fds_str = ramp.to_fds()
        assert "ID='ramp sin'" in fds_str
        assert "CTRL_ID_DEP='sin(2*pi*(t-t0)/10)'" in fds_str

    def test_fire_with_freeze_control(self):
        """Test fire ramp that freezes based on device."""
        # From hrr_freeze example
        ramp = Ramp(
            id="FRAMP",
            points=[(0, 0), (50, 1)],
            devc_id="FREEZE TIME",
        )
        fds_str = ramp.to_fds()
        assert "DEVC_ID='FREEZE TIME'" in fds_str
        assert "T=0" in fds_str
        assert "T=50" in fds_str


class TestRampOutput:
    """Tests for FDS output formatting."""

    def test_output_multiple_lines(self):
        """Test that each point generates a separate RAMP line."""
        ramp = Ramp(id="TEST", points=[(0, 0), (50, 0.5), (100, 1)])
        fds_str = ramp.to_fds()
        lines = [line for line in fds_str.strip().split("\n") if line.startswith("&RAMP")]
        assert len(lines) == 3

    def test_output_order_preserved(self):
        """Test that output preserves sorted point order."""
        ramp = Ramp(id="TEST", points=[(100, 1), (0, 0), (50, 0.5)])
        fds_str = ramp.to_fds()
        lines = fds_str.strip().split("\n")
        # First line should have T=0
        assert "T=0" in lines[0]
        # Last line should have T=100
        assert "T=100" in lines[2]

    def test_ctrl_id_only_on_first_line(self):
        """Test that CTRL_ID only appears on first RAMP line."""
        ramp = Ramp(
            id="TEST",
            points=[(0, 0), (50, 0.5), (100, 1)],
            ctrl_id="MY_CTRL",
        )
        fds_str = ramp.to_fds()
        # CTRL_ID should only appear once
        assert fds_str.count("CTRL_ID") == 1
        # And it should be on the first line
        first_line = fds_str.split("\n")[0]
        assert "CTRL_ID" in first_line

    def test_interpolation_points_only_on_first_line(self):
        """Test that NUMBER_INTERPOLATION_POINTS only appears on first line."""
        ramp = Ramp(
            id="TEST",
            points=[(0, 0), (100, 1)],
            number_interpolation_points=10000,
        )
        fds_str = ramp.to_fds()
        assert fds_str.count("NUMBER_INTERPOLATION_POINTS") == 1
