"""Unit tests for CTRL namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.enums import ControlFunction
from pyfds.core.namelists import Ctrl


class TestCtrl:
    """Tests for Ctrl namelist."""

    def test_any_control(self):
        """Test ANY control logic."""
        ctrl = Ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD_1", "SD_2"])
        assert ctrl.function_type == ControlFunction.ANY
        assert len(ctrl.input_id) == 2

    def test_ctrl_validation_any_requires_list(self):
        """Test that ANY function requires multiple inputs."""
        with pytest.raises(ValidationError, match="multiple INPUT_ID"):
            Ctrl(id="BAD", function_type=ControlFunction.ANY, input_id="SD_1")

    def test_ctrl_to_fds(self):
        """Test FDS output format."""
        ctrl = Ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD_1", "SD_2"])
        fds_str = ctrl.to_fds()
        assert "&CTRL ID='ALARM'" in fds_str
        assert "FUNCTION_TYPE='ANY'" in fds_str
