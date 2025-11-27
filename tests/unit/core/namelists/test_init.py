"""Unit tests for INIT namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Init


class TestInit:
    """Tests for Init namelist."""

    def test_basic_creation(self):
        """Test init with region bounds."""
        init = Init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500)
        assert init.xb == Bounds3D(0, 10, 0, 10, 0, 0.1)
        assert init.temperature == 500

    def test_init_validation_requires_xb_or_xyz(self):
        """Test that either XB or XYZ is required."""
        with pytest.raises(ValidationError, match="XB or XYZ"):
            Init(temperature=500)

    def test_init_to_fds(self):
        """Test FDS output format."""
        init = Init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500)
        fds_str = init.to_fds()
        assert "&INIT" in fds_str
        assert "XB=" in fds_str
        assert "TEMPERATURE=500" in fds_str
