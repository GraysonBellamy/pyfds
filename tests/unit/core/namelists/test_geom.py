"""Tests for Geometry namelist."""

import pytest

from pyfds.builders.geom import GeomBuilder
from pyfds.core.namelists.geom import Geometry


class TestGeom:
    """Test Geometry namelist functionality."""

    def test_basic_geom(self):
        """Test basic geometry creation."""
        geom = Geometry(id="TEST_GEOM", surf_id="STEEL", verts=[0, 0, 0], faces=[1, 1, 1, 1])
        assert geom.id == "TEST_GEOM"
        assert geom.surf_id == "STEEL"

    def test_triangulated_surface(self):
        """Test triangulated surface geometry."""
        geom = Geometry(
            id="TRIANGLE", surf_id="INERT", verts=[0, 0, 0, 1, 0, 0, 0.5, 1, 0], faces=[1, 2, 3, 1]
        )
        assert geom.id == "TRIANGLE"
        assert geom.verts == [0, 0, 0, 1, 0, 0, 0.5, 1, 0]
        assert geom.faces == [1, 2, 3, 1]

    def test_sphere_geometry(self):
        """Test sphere geometry."""
        geom = Geometry(
            id="BALL", surf_id="STEEL", sphere_origin=(5, 5, 1), sphere_radius=0.5, n_levels=3
        )
        assert geom.id == "BALL"
        assert geom.sphere_origin == (5, 5, 1)
        assert geom.sphere_radius == 0.5
        assert geom.n_levels == 3

    def test_cylinder_geometry(self):
        """Test cylinder geometry."""
        geom = Geometry(
            id="PIPE",
            surf_id="STEEL",
            cylinder_origin=(0, 0, 0),
            cylinder_axis=(0, 0, 1),
            cylinder_length=2.0,
            cylinder_radius=0.1,
        )
        assert geom.id == "PIPE"
        assert geom.cylinder_origin == (0, 0, 0)
        assert geom.cylinder_axis == (0, 0, 1)
        assert geom.cylinder_length == 2.0
        assert geom.cylinder_radius == 0.1

    def test_terrain_geometry(self):
        """Test terrain geometry."""
        geom = Geometry(
            id="LAND", surf_id="DIRT", zvals=[0, 1, 2, 1, 0], is_terrain=True, extend_terrain=True
        )
        assert geom.id == "LAND"
        assert geom.zvals == [0, 1, 2, 1, 0]
        assert geom.is_terrain is True
        assert geom.extend_terrain is True

    def test_sphere_with_type(self):
        """Test sphere geometry with sphere_type parameter."""
        geom = Geometry(
            id="BALL",
            surf_id="STEEL",
            sphere_origin=(5, 5, 1),
            sphere_radius=0.5,
            sphere_type=1,
            n_levels=3,
        )
        assert geom.sphere_type == 1
        assert geom.n_levels == 3

    def test_sphere_with_lat_long(self):
        """Test sphere geometry with n_lat and n_long parameters."""
        geom = Geometry(
            id="BALL",
            surf_id="STEEL",
            sphere_origin=(5, 5, 1),
            sphere_radius=0.5,
            n_lat=12,
            n_long=24,
        )
        assert geom.n_lat == 12
        assert geom.n_long == 24

    def test_bndf_geom_parameter(self):
        """Test BNDF_GEOM parameter."""
        geom = Geometry(
            id="TEST",
            surf_id="STEEL",
            verts=[0, 0, 0],
            faces=[1, 1, 1, 1],
            bndf_geom=True,
        )
        assert geom.bndf_geom is True
        fds_output = geom.to_fds()
        assert "BNDF_GEOM=.TRUE." in fds_output

    def test_cell_block_orientation(self):
        """Test CELL_BLOCK_ORIENTATION parameter for thin geometry."""
        geom = Geometry(
            id="THIN",
            surf_id="STEEL",
            verts=[0, 0, 0],
            faces=[1, 1, 1, 1],
            cell_block_ior=3,
            cell_block_orientation=(0.0, 0.0, 1.0),
        )
        assert geom.cell_block_ior == 3
        assert geom.cell_block_orientation == (0.0, 0.0, 1.0)

    def test_geom_validation_multiple_definitions(self):
        """Test validation prevents multiple geometry definitions."""
        with pytest.raises(ValueError, match="Cannot specify multiple geometry definitions"):
            Geometry(
                id="INVALID",
                verts=[0, 0, 0, 1, 0, 0, 0.5, 1, 0],
                faces=[1, 2, 3, 1],
                sphere_origin=(0, 0, 0),
                sphere_radius=1.0,
            )

    def test_geom_validation_no_definition(self):
        """Test validation requires geometry definition."""
        with pytest.raises(ValueError, match="Must specify geometry via"):
            Geometry(id="INVALID", surf_id="STEEL")

    def test_geom_to_fds_basic(self):
        """Test basic FDS output."""
        geom = Geometry(id="BASIC", surf_id="STEEL", verts=[0, 0, 0], faces=[1, 1, 1, 1])
        fds_output = geom.to_fds()
        assert "&GEOM" in fds_output
        assert "ID='BASIC'" in fds_output
        assert "SURF_ID='STEEL'" in fds_output
        assert "/" in fds_output

    def test_geom_to_fds_sphere(self):
        """Test sphere FDS output."""
        geom = Geometry(id="BALL", surf_id="STEEL", sphere_origin=(5, 5, 1), sphere_radius=0.5)
        fds_output = geom.to_fds()
        assert "SPHERE_ORIGIN=5.0,5.0,1.0" in fds_output
        assert "SPHERE_RADIUS=0.5" in fds_output

    def test_geom_to_fds_terrain(self):
        """Test terrain FDS output."""
        geom = Geometry(id="LAND", surf_id="DIRT", zvals=[0, 1, 2], is_terrain=True)
        fds_output = geom.to_fds()
        assert "ZVALS=0.0,1.0,2.0" in fds_output
        assert "IS_TERRAIN=.TRUE." in fds_output


class TestGeomBuilder:
    """Test GeomBuilder functionality."""

    def test_basic_builder(self):
        """Test basic builder usage."""
        geom = (
            GeomBuilder("TEST")
            .with_surface("STEEL")
            .with_vertices([(0, 0, 0), (1, 0, 0), (0.5, 1, 0)])
            .with_faces([(1, 2, 3, 1)])
            .build()
        )
        assert geom.id == "TEST"
        assert geom.surf_id == "STEEL"

    def test_triangulated_builder(self):
        """Test triangulated surface builder."""
        geom = (
            GeomBuilder("TRIANGLE")
            .with_surface("INERT")
            .with_vertices([(0, 0, 0), (1, 0, 0), (0.5, 1, 0)])
            .with_faces([(1, 2, 3, 1)])
            .build()
        )
        assert geom.id == "TRIANGLE"
        assert geom.verts == [0, 0, 0, 1, 0, 0, 0.5, 1, 0]
        assert geom.faces == [1, 2, 3, 1]

    def test_sphere_builder(self):
        """Test sphere builder."""
        geom = (
            GeomBuilder("BALL")
            .with_surface("STEEL")
            .sphere(center=(5, 5, 1), radius=0.5, subdivisions=3)
            .build()
        )
        assert geom.id == "BALL"
        assert geom.sphere_origin == (5, 5, 1)
        assert geom.sphere_radius == 0.5
        assert geom.n_levels == 3

    def test_cylinder_builder(self):
        """Test cylinder builder."""
        geom = (
            GeomBuilder("PIPE")
            .with_surface("STEEL")
            .cylinder(origin=(0, 0, 0), axis=(0, 0, 1), length=2.0, radius=0.1)
            .build()
        )
        assert geom.id == "PIPE"
        assert geom.cylinder_origin == (0, 0, 0)
        assert geom.cylinder_axis == (0, 0, 1)
        assert geom.cylinder_length == 2.0
        assert geom.cylinder_radius == 0.1

    def test_terrain_builder(self):
        """Test terrain builder."""
        geom = (
            GeomBuilder("LAND")
            .with_surface("DIRT")
            .terrain(elevations=[0, 1, 2, 1, 0], extend_to_domain=True)
            .build()
        )
        assert geom.id == "LAND"
        assert geom.zvals == [0, 1, 2, 1, 0]
        assert geom.is_terrain is True
        assert geom.extend_terrain is True

    def test_texture_builder(self):
        """Test texture mapping builder."""
        geom = (
            GeomBuilder("TEXTURED")
            .with_surface("STEEL")
            .with_vertices([(0, 0, 0), (1, 0, 0), (0.5, 1, 0)])
            .with_faces([(1, 2, 3, 1)])
            .with_texture(mapping="SPHERICAL", origin=(0, 0, 0), scale=2.0)
            .build()
        )
        assert geom.id == "TEXTURED"
        assert geom.texture_mapping == "SPHERICAL"
        assert geom.texture_origin == (0, 0, 0)
        assert geom.texture_scale == 2.0

    def test_builder_cannot_be_reused(self):
        """Test that builder cannot be used twice."""
        builder = (
            GeomBuilder("TEST")
            .with_surface("STEEL")
            .with_vertices([(0, 0, 0), (1, 0, 0), (0.5, 1, 0)])
            .with_faces([(1, 2, 3, 1)])
        )
        builder.build()
        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()
