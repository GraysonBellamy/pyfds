"""Tests for VentBuilder class."""

import pytest

from pyfds.builders import VentBuilder
from pyfds.core.geometry import Bounds3D, Point3D


class TestVentBuilder:
    """Test VentBuilder functionality."""

    def test_opening(self):
        """Test creating opening to ambient."""
        vent = VentBuilder("DOOR").opening(xb=(5, 5, 2, 4, 0, 2.1)).build()

        assert vent.id == "DOOR"
        assert vent.xb == Bounds3D(5, 5, 2, 4, 0, 2.1)
        assert vent.surf_id == "OPEN"

    def test_opening_no_id(self):
        """Test opening without ID."""
        vent = VentBuilder().opening(xb=(5, 5, 2, 4, 0, 2.1)).build()

        assert vent.id is None
        assert vent.surf_id == "OPEN"

    def test_circular_burner(self):
        """Test circular burner vent."""
        vent = (
            VentBuilder("BURNER")
            .circular_burner(center=(0, 0, 0), radius=0.5, surf_id="FIRE")
            .build()
        )

        assert vent.id == "BURNER"
        assert vent.xyz == Point3D(0, 0, 0)
        assert vent.radius == 0.5
        assert vent.surf_id == "FIRE"

        # Check bounding box is correct
        assert vent.xb == Bounds3D(xmin=-0.5, xmax=0.5, ymin=-0.5, ymax=0.5, zmin=0, zmax=0)

    def test_annular_burner(self):
        """Test annular (ring) burner vent."""
        vent = (
            VentBuilder("RING_BURNER")
            .annular_burner(center=(0, 0, 0), radius=0.5, radius_inner=0.3, surf_id="FIRE")
            .build()
        )
        # Check bounding box is correct
        assert vent.xb == Bounds3D(xmin=-0.5, xmax=0.5, ymin=-0.5, ymax=0.5, zmin=0, zmax=0)

        assert vent.xyz == Point3D(0, 0, 0)
        assert vent.radius == 0.5
        assert vent.radius_inner == 0.3
        assert vent.surf_id == "FIRE"

    def test_mesh_boundary_open(self):
        """Test open mesh boundary."""
        vent = VentBuilder("BOUNDARY").mesh_boundary(mb="XMIN", surf_id="OPEN").build()

        assert vent.id == "BOUNDARY"
        assert vent.mb == "XMIN"
        assert vent.surf_id == "OPEN"

    def test_mesh_boundary_periodic(self):
        """Test periodic mesh boundary."""
        vent = VentBuilder().mesh_boundary(mb="XMAX", surf_id="PERIODIC").build()

        assert vent.mb == "XMAX"
        assert vent.surf_id == "PERIODIC"

    def test_mesh_boundary_with_mesh_id(self):
        """Test mesh boundary with mesh ID."""
        vent = (
            VentBuilder("BOUNDARY")
            .mesh_boundary(mb="YMIN", surf_id="OPEN", mesh_id="MESH_1")
            .build()
        )

        assert vent.mesh_id == "MESH_1"

    def test_door(self):
        """Test standard door opening."""
        vent = VentBuilder("DOOR_1").door(x=5.0, y_min=2.0, y_max=3.0).build()

        assert vent.id == "DOOR_1"
        assert vent.surf_id == "OPEN"
        assert vent.xb == Bounds3D(xmin=5.0, xmax=5.0, ymin=2.0, ymax=3.0, zmin=0.0, zmax=2.1)

    def test_door_custom_height(self):
        """Test door with custom height."""
        vent = (
            VentBuilder("TALL_DOOR").door(x=5.0, y_min=2.0, y_max=3.0, z_min=0.1, z_max=2.5).build()
        )

        assert vent.xb == Bounds3D(xmin=5.0, xmax=5.0, ymin=2.0, ymax=3.0, zmin=0.1, zmax=2.5)

    def test_window(self):
        """Test window opening."""
        vent = (
            VentBuilder("WINDOW_1")
            .window(x=0.0, y_min=1.0, y_max=2.0, z_min=1.0, z_max=1.5)
            .build()
        )

        assert vent.id == "WINDOW_1"
        assert vent.surf_id == "OPEN"
        assert vent.xb == Bounds3D(xmin=0.0, xmax=0.0, ymin=1.0, ymax=2.0, zmin=1.0, zmax=1.5)

    def test_fds_output_format_opening(self):
        """Test FDS output format for opening."""
        vent = VentBuilder("DOOR").opening(xb=(5, 5, 2, 4, 0, 2.1)).build()

        fds_output = vent.to_fds()
        assert "&VENT" in fds_output
        assert "ID='DOOR'" in fds_output
        assert "SURF_ID='OPEN'" in fds_output

    def test_fds_output_format_circular(self):
        """Test FDS output format for circular vent."""
        vent = VentBuilder().circular_burner(center=(0, 0, 0), radius=0.5, surf_id="FIRE").build()

        fds_output = vent.to_fds()
        assert "RADIUS=0.5" in fds_output
        assert "XYZ=0" in fds_output or "XYZ=0.0" in fds_output

    def test_builder_cannot_be_reused(self):
        """Test that builder cannot be used twice."""
        builder = VentBuilder("TEST").opening(xb=(0, 0, 0, 1, 0, 1))  # Plane at x=0
        builder.build()
        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()


class TestVentBuilderPhase4Enhancements:
    """Test Phase 4 VENT enhancements."""

    def test_with_geometry_params(self):
        """Test geometry parameters."""
        vent = (
            VentBuilder("GEO_VENT")
            .opening(xb=(0, 0, 0, 1, 0, 1))  # Plane at x=0
            .with_geometry_params(db="XMIN", pbx=5.0, pby=10.0, pbz=15.0, ior=1)
            .build()
        )
        assert vent.db == "XMIN"
        assert vent.pbx == 5.0
        assert vent.pby == 10.0
        assert vent.pbz == 15.0
        assert vent.ior == 1

    def test_with_control_params(self):
        """Test control parameters."""
        vent = (
            VentBuilder("CTRL_VENT")
            .opening(xb=(0, 0, 0, 1, 0, 1))  # Plane at x=0
            .with_control_params(outline=True, mult_id="VENT_ARRAY", obst_id="WALL_1")
            .build()
        )
        assert vent.outline is True
        assert vent.mult_id == "VENT_ARRAY"
        assert vent.obst_id == "WALL_1"

    def test_with_texture(self):
        """Test texture origin."""
        vent = (
            VentBuilder("TEXTURE_VENT")
            .opening(xb=(0, 0, 0, 1, 0, 1))  # Plane at x=0
            .with_texture(texture_origin=(1.0, 2.0, 3.0))
            .build()
        )
        assert vent.texture_origin == (1.0, 2.0, 3.0)

    def test_with_fire_spread(self):
        """Test fire spread rate."""
        vent = (
            VentBuilder("SPREAD_VENT")
            .circular_burner(center=(0, 0, 0), radius=0.5, surf_id="FIRE")
            .with_fire_spread(spread_rate=0.02)
            .build()
        )
        assert vent.spread_rate == 0.02

    def test_with_open_boundary_ramps(self):
        """Test open boundary ramps."""
        vent = (
            VentBuilder("RAMP_VENT")
            .mesh_boundary(mb="XMIN", surf_id="OPEN")
            .with_open_boundary_ramps(tmp_exterior_ramp="TEMP_RAMP", pressure_ramp="PRESSURE_RAMP")
            .build()
        )
        assert vent.tmp_exterior_ramp == "TEMP_RAMP"
        assert vent.pressure_ramp == "PRESSURE_RAMP"

    def test_with_synthetic_turbulence(self):
        """Test synthetic turbulence parameters."""
        vent = (
            VentBuilder("TURB_VENT")
            .mesh_boundary(mb="XMIN", surf_id="OPEN")
            .with_synthetic_turbulence(
                n_eddy=100,
                l_eddy=0.1,
                l_eddy_ij=[[0.1, 0.0, 0.0], [0.0, 0.1, 0.0], [0.0, 0.0, 0.1]],
                vel_rms=0.5,
                reynolds_stress=[[0.1, 0.0, 0.0], [0.0, 0.1, 0.0], [0.0, 0.0, 0.1]],
                uvw=(1.0, 0.5, 0.2),
            )
            .build()
        )
        assert vent.n_eddy == 100
        assert vent.l_eddy == 0.1
        assert vent.l_eddy_ij == [[0.1, 0.0, 0.0], [0.0, 0.1, 0.0], [0.0, 0.0, 0.1]]
        assert vent.vel_rms == 0.5
        assert vent.reynolds_stress == [[0.1, 0.0, 0.0], [0.0, 0.1, 0.0], [0.0, 0.0, 0.1]]
        assert vent.uvw == (1.0, 0.5, 0.2)

    def test_phase4_fds_output(self):
        """Test Phase 4 parameters in FDS output."""
        vent = (
            VentBuilder("PHASE4_VENT")
            .opening(xb=(0, 0, 0, 1, 0, 1))  # Plane at x=0
            .with_geometry_params(db="XMIN", ior=-2)
            .with_control_params(outline=True)
            .with_texture(texture_origin=(0.5, 0.5, 0.0))
            .with_synthetic_turbulence(n_eddy=50, l_eddy=0.05, vel_rms=0.3)
            .build()
        )
        fds_output = vent.to_fds()
        assert "&VENT" in fds_output
        assert "ID='PHASE4_VENT'" in fds_output
        assert "DB='XMIN'" in fds_output
        assert "IOR=-2" in fds_output
        assert "OUTLINE=.TRUE." in fds_output
        assert "TEXTURE_ORIGIN=0.5,0.5,0.0" in fds_output
        assert "N_EDDY=50" in fds_output
        assert "L_EDDY=0.05" in fds_output
        assert "VEL_RMS=0.3" in fds_output

    def test_method_chaining(self):
        """Test fluent API method chaining."""
        vent = (
            VentBuilder("CHAINED_VENT")
            .circular_burner(center=(0, 0, 0), radius=0.5, surf_id="FIRE")
            .with_fire_spread(spread_rate=0.01)
            .with_geometry_params(ior=3)
            .with_control_params(outline=True)
            .with_synthetic_turbulence(n_eddy=25)
            .build()
        )
        assert vent.id == "CHAINED_VENT"
        assert vent.radius == 0.5
        assert vent.spread_rate == 0.01
        assert vent.ior == 3
        assert vent.outline is True
        assert vent.n_eddy == 25
