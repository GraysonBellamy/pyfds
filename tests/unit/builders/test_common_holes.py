"""Tests for CommonHoles factory class."""

from pyfds.builders.libraries import CommonHoles
from pyfds.core.geometry import Bounds3D


class TestCommonHoles:
    """Test CommonHoles factory methods."""

    def test_door_basic_tuple(self):
        """Test basic door creation with tuple."""
        hole = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1))
        assert hole.xb is not None
        xb_tuple = hole.xb.as_tuple()
        assert xb_tuple[0] == 5.0  # x_min
        assert xb_tuple[1] == 5.0  # x_max (thin wall)
        assert xb_tuple[2] == 2  # y_min
        assert xb_tuple[3] == 4  # y_max
        assert xb_tuple[4] == 0  # z_min
        assert xb_tuple[5] == 2.1  # z_max

    def test_door_basic_bounds3d(self):
        """Test basic door creation with Bounds3D."""
        hole = CommonHoles.door(xb=Bounds3D.of(5, 5, 2, 4, 0, 2.1))
        assert hole.xb is not None
        xb_tuple = hole.xb.as_tuple()
        assert xb_tuple[0] == 5.0
        assert xb_tuple[5] == 2.1

    def test_door_with_id(self):
        """Test door with custom ID."""
        hole = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1), id="MAIN_DOOR")
        assert hole.id == "MAIN_DOOR"

    def test_door_default_color(self):
        """Test door has default brown color."""
        hole = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1))
        assert hole.color == "BROWN"

    def test_door_with_control(self):
        """Test door with control ID."""
        hole = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1), ctrl_id="DOOR_CTRL")
        assert hole.ctrl_id == "DOOR_CTRL"

    def test_window_basic_tuple(self):
        """Test basic window creation with tuple."""
        hole = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2))
        assert hole.xb is not None
        xb_tuple = hole.xb.as_tuple()
        assert xb_tuple[0] == 0.0
        assert xb_tuple[1] == 0.0
        assert xb_tuple[4] == 1  # z_min elevated
        assert xb_tuple[5] == 2  # z_max

    def test_window_with_id(self):
        """Test window with custom ID."""
        hole = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2), id="MAIN_WINDOW")
        assert hole.id == "MAIN_WINDOW"

    def test_window_default_color(self):
        """Test window has default cyan color."""
        hole = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2))
        assert hole.color == "CYAN"

    def test_window_default_transparency(self):
        """Test window has default transparency."""
        hole = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2))
        assert hole.transparency == 0.5

    def test_window_custom_transparency(self):
        """Test window with custom transparency."""
        hole = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2), transparency=0.8)
        assert hole.transparency == 0.8

    def test_fds_output(self):
        """Test FDS output format."""
        hole = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1), id="DOOR1")
        fds_output = hole.to_fds()
        assert "&HOLE" in fds_output
        assert "ID='DOOR1'" in fds_output
        assert "XB=" in fds_output
