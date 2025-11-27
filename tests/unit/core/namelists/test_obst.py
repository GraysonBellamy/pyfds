"""Unit tests for OBST namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Obstruction


class TestObstruction:
    """Tests for Obstruction namelist."""

    def test_basic_creation(self):
        """Test basic Obstruction creation."""
        obst = Obstruction(xb=(0, 1, 0, 1, 0, 1))
        assert obst.xb == Bounds3D(0, 1, 0, 1, 0, 1)

    def test_with_surf_id(self):
        """Test Obstruction with surface ID."""
        obst = Obstruction(xb=(0, 1, 0, 1, 0, 1), surf_id="FIRE")
        assert obst.surf_id == "FIRE"

    def test_to_fds(self):
        """Test FDS output format."""
        obst = Obstruction(xb=(0, 1, 0, 1, 0, 1), surf_id="FIRE")
        fds_str = obst.to_fds()
        assert "&OBST" in fds_str
        assert "XB=" in fds_str
        assert "SURF_ID='FIRE'" in fds_str

    def test_thin_obstruction(self):
        """Test thin obstruction (equal bounds allowed)."""
        obst = Obstruction(xb=(0, 0, 0, 1, 0, 1))
        assert obst.xb == Bounds3D(0, 0, 0, 1, 0, 1)

    def test_xb_validation(self):
        """Test XB validation."""
        with pytest.raises(ValidationError):
            Obstruction(xb=(1, 0, 0, 1, 0, 1))
