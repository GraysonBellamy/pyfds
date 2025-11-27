"""Tests for VentBuilder class."""

import pytest

from pyfds.builders import VentBuilder
from pyfds.core.geometry import Bounds3D, Point3D


class TestVentBuilder:
    """Test VentBuilder functionality."""

    def test_opening(self):
        """Test creating opening to ambient."""
        vent = VentBuilder.opening(xb=(5, 5, 2, 4, 0, 2.1), id="DOOR")

        assert vent.id == "DOOR"
        assert vent.xb == Bounds3D(5, 5, 2, 4, 0, 2.1)
        assert vent.surf_id == "OPEN"

    def test_opening_no_id(self):
        """Test opening without ID."""
        vent = VentBuilder.opening(xb=(5, 5, 2, 4, 0, 2.1))

        assert vent.id is None
        assert vent.surf_id == "OPEN"

    def test_hvac_supply(self):
        """Test HVAC supply vent."""
        vent = VentBuilder.hvac_supply(xb=(5, 6, 5, 6, 3, 3), volume_flow=0.5, id="SUPPLY")

        assert vent.id == "SUPPLY"
        assert vent.surf_id == "HVAC"
        assert vent.volume_flow == 0.5

    def test_hvac_exhaust(self):
        """Test HVAC exhaust vent."""
        vent = VentBuilder.hvac_exhaust(xb=(0, 1, 0, 1, 3, 3), volume_flow=0.3, id="EXHAUST")

        assert vent.surf_id == "HVAC"
        assert vent.volume_flow == -0.3  # Should be negative

    def test_hvac_exhaust_negative_input(self):
        """Test HVAC exhaust with already negative flow."""
        vent = VentBuilder.hvac_exhaust(xb=(0, 1, 0, 1, 3, 3), volume_flow=-0.3, id="EXHAUST")

        assert vent.volume_flow == -0.3  # Should stay negative

    def test_circular_burner(self):
        """Test circular burner vent."""
        vent = VentBuilder.circular_burner(
            center=(0, 0, 0), radius=0.5, surf_id="FIRE", id="BURNER"
        )

        assert vent.id == "BURNER"
        assert vent.xyz == Point3D(0, 0, 0)
        assert vent.radius == 0.5
        assert vent.surf_id == "FIRE"
    # Check bounding box is correct
    assert vent.xb == Bounds3D(xmin=-0.5, xmax=0.5, ymin=-0.5, ymax=0.5, zmin=0, zmax=0)    def test_annular_burner(self):
        """Test annular (ring) burner vent."""
        vent = VentBuilder.annular_burner(
            center=(0, 0, 0),
            radius=0.5,
            radius_inner=0.3,
            surf_id="FIRE",
            id="RING_BURNER",
        )

        assert vent.xyz == Point3D(0, 0, 0)
        assert vent.radius == 0.5
        assert vent.radius_inner == 0.3
        assert vent.surf_id == "FIRE"

    def test_mesh_boundary_open(self):
        """Test open mesh boundary."""
        vent = VentBuilder.mesh_boundary(mb="XMIN", surf_id="OPEN")

        assert vent.mb == "XMIN"
        assert vent.surf_id == "OPEN"

    def test_mesh_boundary_periodic(self):
        """Test periodic mesh boundary."""
        vent = VentBuilder.mesh_boundary(mb="XMAX", surf_id="PERIODIC")

        assert vent.mb == "XMAX"
        assert vent.surf_id == "PERIODIC"

    def test_mesh_boundary_with_mesh_id(self):
        """Test mesh boundary with mesh ID."""
        vent = VentBuilder.mesh_boundary(mb="YMIN", surf_id="OPEN", mesh_id="MESH_1")

        assert vent.mesh_id == "MESH_1"

    def test_door(self):
        """Test standard door opening."""
        vent = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id="DOOR_1")

        assert vent.id == "DOOR_1"
        assert vent.surf_id == "OPEN"
        assert vent.xb == Bounds3D(xmin=5.0, xmax=5.0, ymin=2.0, ymax=3.0, zmin=0.0, zmax=2.1)

    def test_door_custom_height(self):
        """Test door with custom height."""
        vent = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, z_min=0.1, z_max=2.5, id="TALL_DOOR")

        assert vent.xb == Bounds3D(xmin=5.0, xmax=5.0, ymin=2.0, ymax=3.0, zmin=0.1, zmax=2.5)

    def test_window(self):
        """Test window opening."""
        vent = VentBuilder.window(x=0.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5, id="WINDOW_1")

        assert vent.id == "WINDOW_1"
        assert vent.surf_id == "OPEN"
        assert vent.xb == Bounds3D(xmin=0.0, xmax=0.0, ymin=1.0, ymax=2.0, zmin=1.0, zmax=1.5)

    def test_fds_output_format_opening(self):
        """Test FDS output format for opening."""
        vent = VentBuilder.opening(xb=(5, 5, 2, 4, 0, 2.1), id="DOOR")

        fds_output = vent.to_fds()
        assert "&VENT" in fds_output
        assert "ID='DOOR'" in fds_output
        assert "SURF_ID='OPEN'" in fds_output

    def test_fds_output_format_hvac(self):
        """Test FDS output format for HVAC."""
        vent = VentBuilder.hvac_supply(xb=(5, 6, 5, 6, 3, 3), volume_flow=0.5, id="SUPPLY")

        fds_output = vent.to_fds()
        assert "SURF_ID='HVAC'" in fds_output
        assert "VOLUME_FLOW=0.5" in fds_output

    def test_fds_output_format_circular(self):
        """Test FDS output format for circular vent."""
        vent = VentBuilder.circular_burner(center=(0, 0, 0), radius=0.5, surf_id="FIRE")

        fds_output = vent.to_fds()
        assert "RADIUS=0.5" in fds_output
        assert "XYZ=0" in fds_output or "XYZ=0.0" in fds_output

    def test_build_raises_error(self):
        """Test that build() method raises NotImplementedError."""
        builder = VentBuilder()
        with pytest.raises(NotImplementedError, match="factory methods"):
            builder.build()
