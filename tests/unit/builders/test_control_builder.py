"""Tests for ControlBuilder class."""

import pytest

from pyfds.builders.control import ControlBuilder
from pyfds.core.enums import ControlFunction


class TestControlBuilder:
    """Test ControlBuilder functionality."""

    def test_any_logic(self):
        """Test ANY (OR) logic control."""
        ctrl = ControlBuilder("ALARM").any(["SD_1", "SD_2", "SD_3"]).build()

        assert ctrl.id == "ALARM"
        assert ctrl.function_type == ControlFunction.ANY
        assert ctrl.input_id == ["SD_1", "SD_2", "SD_3"]

    def test_all_logic(self):
        """Test ALL (AND) logic control."""
        ctrl = ControlBuilder("DUAL_COND").all(["TEMP_HIGH", "SMOKE"]).build()

        assert ctrl.id == "DUAL_COND"
        assert ctrl.function_type == ControlFunction.ALL
        assert ctrl.input_id == ["TEMP_HIGH", "SMOKE"]

    def test_only_passthrough(self):
        """Test ONLY (passthrough) control."""
        ctrl = ControlBuilder("PASS").only("DETECTOR_1").build()

        assert ctrl.function_type == ControlFunction.ONLY
        assert ctrl.input_id == "DETECTOR_1"

    def test_time_delay(self):
        """Test time-delayed control."""
        ctrl = ControlBuilder("DELAYED").time_delay("DETECTOR", delay=10.0).build()

        assert ctrl.function_type == ControlFunction.TIME_DELAY
        assert ctrl.input_id == "DETECTOR"
        assert ctrl.delay == 10.0

    def test_custom_function(self):
        """Test custom control function."""
        ctrl = ControlBuilder("CUSTOM").custom("INPUT_1").build()

        assert ctrl.function_type == ControlFunction.CUSTOM
        assert ctrl.input_id == "INPUT_1"

    def test_kill_function(self):
        """Test KILL function."""
        ctrl = ControlBuilder("KILL_SIM").kill().build()

        assert ctrl.function_type == ControlFunction.KILL

    def test_restart_function(self):
        """Test RESTART function."""
        ctrl = ControlBuilder("RESTART_SIM").restart().build()

        assert ctrl.function_type == ControlFunction.RESTART

    def test_with_delay_modifier(self):
        """Test adding delay to control."""
        ctrl = ControlBuilder("ALARM").any(["SD_1", "SD_2"]).with_delay(5.0).build()

        assert ctrl.delay == 5.0

    def test_with_initial_state(self):
        """Test setting initial state."""
        ctrl = ControlBuilder("CTRL").any(["A", "B"]).with_initial_state(True).build()

        assert ctrl.initial_state is True

    def test_with_latch(self):
        """Test setting latch behavior."""
        ctrl = ControlBuilder("LATCH").any(["A", "B"]).with_latch(False).build()

        assert ctrl.latch is False

    def test_complete_control(self):
        """Test building complete control with all modifiers."""
        ctrl = (
            ControlBuilder("COMPLEX")
            .any(["SD_1", "SD_2", "SD_3"])
            .with_delay(3.0)
            .with_initial_state(False)
            .with_latch(True)
            .build()
        )

        assert ctrl.function_type == ControlFunction.ANY
        assert ctrl.delay == 3.0
        assert ctrl.initial_state is False
        assert ctrl.latch is True

    def test_no_function_error(self):
        """Test error when no function type specified."""
        with pytest.raises(ValueError, match="No function type specified"):
            ControlBuilder("TEST").build()

    def test_builder_reuse_error(self):
        """Test that builder cannot be reused."""
        builder = ControlBuilder("TEST").any(["A", "B"])
        builder.build()

        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()

    def test_fds_output_format(self):
        """Test FDS output format."""
        ctrl = ControlBuilder("TEST").any(["A", "B"]).build()

        fds_output = ctrl.to_fds()
        assert "&CTRL" in fds_output
        assert "ID='TEST'" in fds_output
        assert "FUNCTION_TYPE='ANY'" in fds_output

    def test_default_values(self):
        """Test that default values are correct."""
        ctrl = ControlBuilder("TEST").any(["A"]).build()

        assert ctrl.delay == 0.0
        assert ctrl.initial_state is False
        assert ctrl.latch is True
