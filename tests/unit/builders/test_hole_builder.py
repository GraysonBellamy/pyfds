"""Unit tests for HoleBuilder."""

import pytest

from pyfds.builders import HoleBuilder
from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Hole


class TestHoleBuilderBasicUsage:
    """Test basic HoleBuilder usage."""

    def test_simple_build(self):
        """Test building a simple hole."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).build()
        assert isinstance(hole, Hole)
        assert hole.xb == Bounds3D(0, 1, 0, 1, 0, 1)

    def test_build_with_bounds3d(self):
        """Test building with Bounds3D object."""
        xb = Bounds3D(0, 1, 0, 1, 0, 1)
        hole = HoleBuilder(xb).build()
        assert hole.xb == xb

    def test_invalid_bounds_tuple(self):
        """Test invalid bounds tuple length."""
        with pytest.raises(ValueError, match="must have exactly 6 elements"):
            HoleBuilder((0, 1, 0, 1, 0))

    def test_builder_cannot_be_reused(self):
        """Test that builder cannot be used twice."""
        builder = HoleBuilder((0, 1, 0, 1, 0, 1))
        builder.build()
        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()


class TestHoleBuilderIdentification:
    """Test identification methods."""

    def test_with_id(self):
        """Test with_id method."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_id("DOOR").build()
        assert hole.id == "DOOR"


class TestHoleBuilderControl:
    """Test control methods."""

    def test_with_control_ctrl_id(self):
        """Test with_control method with ctrl_id."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_control(ctrl_id="CTRL1").build()
        assert hole.ctrl_id == "CTRL1"
        assert hole.devc_id is None

    def test_with_control_devc_id(self):
        """Test with_control method with devc_id."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_control(devc_id="DEVC1").build()
        assert hole.devc_id == "DEVC1"
        assert hole.ctrl_id is None

    def test_with_control_both(self):
        """Test with_control method with both ctrl_id and devc_id."""
        hole = (
            HoleBuilder((0, 1, 0, 1, 0, 1)).with_control(ctrl_id="CTRL1", devc_id="DEVC1").build()
        )
        assert hole.ctrl_id == "CTRL1"
        assert hole.devc_id == "DEVC1"


class TestHoleBuilderVisualization:
    """Test visualization methods."""

    def test_with_visualization_color(self):
        """Test with_visualization method with color."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_visualization(color="GRAY").build()
        assert hole.color == "GRAY"

    def test_with_visualization_rgb(self):
        """Test with_visualization method with rgb."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_visualization(rgb=(128, 128, 128)).build()
        assert hole.rgb == (128, 128, 128)

    def test_with_visualization_transparency(self):
        """Test with_visualization method with transparency."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_visualization(transparency=0.5).build()
        assert hole.transparency == 0.5

    def test_with_visualization_all(self):
        """Test with_visualization method with all parameters."""
        hole = (
            HoleBuilder((0, 1, 0, 1, 0, 1))
            .with_visualization(color="BLUE", rgb=(0, 0, 255), transparency=0.8)
            .build()
        )
        assert hole.color == "BLUE"
        assert hole.rgb == (0, 0, 255)
        assert hole.transparency == 0.8


class TestHoleBuilderMultiplier:
    """Test multiplier methods."""

    def test_with_multiplier(self):
        """Test with_multiplier method."""
        hole = HoleBuilder((0, 1, 0, 1, 0, 1)).with_multiplier("MULT1").build()
        assert hole.mult_id == "MULT1"


class TestHoleBuilderFactoryMethods:
    """Test factory methods."""

    def test_door_factory(self):
        """Test door factory method."""
        hole = HoleBuilder.door(wall_x=5, y_min=2, y_max=4, z_min=0, z_max=2.1)
        assert isinstance(hole, Hole)
        assert hole.xb == Bounds3D(5, 5.1, 2, 4, 0, 2.1)
        assert hole.id == "DOOR"

    def test_door_factory_custom_id(self):
        """Test door factory method with custom ID."""
        hole = HoleBuilder.door(wall_x=5, y_min=2, y_max=4, z_min=0, z_max=2.1, id="MAIN_DOOR")
        assert hole.id == "MAIN_DOOR"

    def test_window_factory(self):
        """Test window factory method."""
        hole = HoleBuilder.window(wall_x=5, y_min=2, y_max=4, z_min=0, z_max=2.1)
        assert isinstance(hole, Hole)
        assert hole.xb == Bounds3D(5, 5.1, 2, 4, 0, 2.1)
        assert hole.id == "WINDOW"

    def test_window_factory_custom_id(self):
        """Test window factory method with custom ID."""
        hole = HoleBuilder.window(wall_x=5, y_min=2, y_max=4, z_min=0, z_max=2.1, id="MAIN_WINDOW")
        assert hole.id == "MAIN_WINDOW"

    def test_factory_custom_thickness(self):
        """Test factory methods with custom thickness."""
        hole = HoleBuilder.door(wall_x=5, y_min=2, y_max=4, z_min=0, z_max=2.1, thickness=0.2)
        assert hole.xb == Bounds3D(5, 5.2, 2, 4, 0, 2.1)


class TestHoleBuilderComplexUsage:
    """Test complex HoleBuilder usage."""

    def test_complex_hole(self):
        """Test building a complex hole with multiple parameters."""
        hole = (
            HoleBuilder((5, 5.1, 2, 4, 0, 2.1))
            .with_id("WINDOW")
            .with_control(ctrl_id="WINDOW_CTRL", devc_id="WINDOW_DEVC")
            .with_visualization(color="GRAY", transparency=0.9)
            .with_multiplier("WINDOW_MULT")
            .build()
        )

        assert hole.id == "WINDOW"
        assert hole.ctrl_id == "WINDOW_CTRL"
        assert hole.devc_id == "WINDOW_DEVC"
        assert hole.color == "GRAY"
        assert hole.transparency == 0.9
        assert hole.mult_id == "WINDOW_MULT"

    def test_fds_output(self):
        """Test FDS output generation."""
        hole = (
            HoleBuilder((5, 5.1, 2, 4, 0, 2.1))
            .with_id("WINDOW")
            .with_control(ctrl_id="WINDOW_CTRL")
            .with_visualization(color="GRAY")
            .build()
        )

        fds_str = hole.to_fds()
        assert "&HOLE" in fds_str
        assert "XB=5,5.1,2,4,0,2.1" in fds_str
        assert "ID='WINDOW'" in fds_str
        assert "CTRL_ID='WINDOW_CTRL'" in fds_str
        assert "COLOR='GRAY'" in fds_str
