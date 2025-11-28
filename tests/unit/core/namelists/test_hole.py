"""Unit tests for HOLE namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Hole


class TestHole:
    """Tests for Hole namelist."""

    def test_basic_creation(self):
        """Test basic Hole creation."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1))
        assert hole.xb == Bounds3D(5, 5.1, 2, 4, 0, 2.1)

    def test_with_id(self):
        """Test Hole with identifier."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1), id="DOOR")
        assert hole.id == "DOOR"

    def test_to_fds_basic(self):
        """Test basic FDS output format."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1))
        fds_str = hole.to_fds()
        assert "&HOLE" in fds_str
        assert "XB=5,5.1,2,4,0,2.1" in fds_str

    def test_to_fds_with_id(self):
        """Test FDS output with ID."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1), id="DOOR")
        fds_str = hole.to_fds()
        assert "ID='DOOR'" in fds_str

    def test_controlled_hole(self):
        """Test hole with control logic."""
        hole = Hole(
            xb=(5, 5.1, 2, 4, 0, 2.1), id="WINDOW", ctrl_id="WINDOW_CTRL", devc_id="WINDOW_DEVC"
        )
        assert hole.ctrl_id == "WINDOW_CTRL"
        assert hole.devc_id == "WINDOW_DEVC"
        fds_str = hole.to_fds()
        assert "CTRL_ID='WINDOW_CTRL'" in fds_str
        assert "DEVC_ID='WINDOW_DEVC'" in fds_str

    def test_visualization_parameters(self):
        """Test visualization parameters."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1), color="GRAY", rgb=(128, 128, 128), transparency=0.5)
        assert hole.color == "GRAY"
        assert hole.rgb == (128, 128, 128)
        assert hole.transparency == 0.5
        fds_str = hole.to_fds()
        assert "COLOR='GRAY'" in fds_str
        assert "RGB=128,128,128" in fds_str
        assert "TRANSPARENCY=0.5" in fds_str

    def test_multiplier(self):
        """Test multiplier ID for array replication."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1), mult_id="DOOR_ARRAY")
        assert hole.mult_id == "DOOR_ARRAY"
        fds_str = hole.to_fds()
        assert "MULT_ID='DOOR_ARRAY'" in fds_str

    def test_xb_validation(self):
        """Test XB validation."""
        # Valid hole
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1))
        assert hole.xb == Bounds3D(5, 5.1, 2, 4, 0, 2.1)

        # Invalid XB (wrong order)
        with pytest.raises(ValidationError):
            Hole(xb=(5.1, 5, 2, 4, 0, 2.1))

    def test_rgb_validation(self):
        """Test RGB color validation."""
        # Valid RGB
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1), rgb=(255, 0, 128))
        assert hole.rgb == (255, 0, 128)

        # Invalid RGB - negative value
        with pytest.raises(ValidationError):
            Hole(xb=(5, 5.1, 2, 4, 0, 2.1), rgb=(-1, 0, 128))

        # Invalid RGB - value too high
        with pytest.raises(ValidationError):
            Hole(xb=(5, 5.1, 2, 4, 0, 2.1), rgb=(256, 0, 128))

        # Invalid RGB - wrong number of components
        with pytest.raises(ValidationError):
            Hole(xb=(5, 5.1, 2, 4, 0, 2.1), rgb=(255, 0))

    def test_transparency_validation(self):
        """Test transparency validation."""
        # Valid transparency
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1), transparency=0.8)
        assert hole.transparency == 0.8

        # Invalid transparency - negative
        with pytest.raises(ValidationError):
            Hole(xb=(5, 5.1, 2, 4, 0, 2.1), transparency=-0.1)

        # Invalid transparency - greater than 1
        with pytest.raises(ValidationError):
            Hole(xb=(5, 5.1, 2, 4, 0, 2.1), transparency=1.5)

    def test_bounds3d_input(self):
        """Test Bounds3D input."""
        bounds = Bounds3D(5, 5.1, 2, 4, 0, 2.1)
        hole = Hole(xb=bounds)
        assert hole.xb == bounds

    def test_default_transparency(self):
        """Test default transparency value."""
        hole = Hole(xb=(5, 5.1, 2, 4, 0, 2.1))
        assert hole.transparency == 1.0
        fds_str = hole.to_fds()
        assert "TRANSPARENCY" not in fds_str  # Should not output default value

    def test_complete_hole(self):
        """Test hole with all parameters."""
        hole = Hole(
            xb=(5, 5.1, 2, 4, 0, 2.1),
            id="TEST_HOLE",
            ctrl_id="CTRL",
            devc_id="DEVC",
            color="RED",
            rgb=(255, 0, 0),
            transparency=0.9,
            mult_id="MULT",
        )
        fds_str = hole.to_fds()
        expected_parts = [
            "&HOLE",
            "XB=5,5.1,2,4,0,2.1",
            "ID='TEST_HOLE'",
            "CTRL_ID='CTRL'",
            "DEVC_ID='DEVC'",
            "COLOR='RED'",
            "RGB=255,0,0",
            "TRANSPARENCY=0.9",
            "MULT_ID='MULT'",
            "/",
        ]
        for part in expected_parts:
            assert part in fds_str
