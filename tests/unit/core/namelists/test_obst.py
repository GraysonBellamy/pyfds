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

    def test_burn_away_basic(self):
        """Test basic burn-away functionality."""
        obst = Obstruction(xb=(0, 1, 0, 1, 0, 1), burn_away=True)
        assert obst.burn_away is True
        fds_str = obst.to_fds()
        assert "BURN_AWAY=.TRUE." in fds_str

    def test_bulk_density(self):
        """Test bulk density parameter."""
        obst = Obstruction(xb=(0, 1, 0, 1, 0, 1), bulk_density=500.0)
        assert obst.bulk_density == 500.0
        fds_str = obst.to_fds()
        assert "BULK_DENSITY=500.0" in fds_str

    def test_surf_id_interior(self):
        """Test surface ID for interior."""
        obst = Obstruction(xb=(0, 1, 0, 1, 0, 1), surf_id_interior="CHAR")
        assert obst.surf_id_interior == "CHAR"
        fds_str = obst.to_fds()
        assert "SURF_ID_INTERIOR='CHAR'" in fds_str

    def test_bulk_density_validation(self):
        """Test bulk density validation (must be positive)."""
        with pytest.raises(ValidationError):
            Obstruction(xb=(0, 1, 0, 1, 0, 1), bulk_density=-100)
