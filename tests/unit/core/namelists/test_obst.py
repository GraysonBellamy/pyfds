"""Unit tests for OBST namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Obstruction


class TestObstructionBasics:
    """Tests for Obstruction namelist basic functionality."""

    def test_basic_creation(self):
        """Test basic Obstruction creation."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        assert obst.xb == Bounds3D.of(0, 1, 0, 1, 0, 1)

    def test_with_surf_id(self):
        """Test Obstruction with surface ID."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id="FIRE")
        assert obst.surf_id == "FIRE"

    def test_to_fds(self):
        """Test FDS output format."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id="FIRE")
        fds_str = obst.to_fds()
        assert "&OBST" in fds_str
        assert "XB=" in fds_str
        assert "SURF_ID='FIRE'" in fds_str

    def test_thin_obstruction(self):
        """Test thin obstruction (equal bounds allowed)."""
        obst = Obstruction(xb=Bounds3D.of(0, 0, 0, 1, 0, 1))
        assert obst.xb == Bounds3D.of(0, 0, 0, 1, 0, 1)

    def test_xb_validation(self):
        """Test XB validation."""
        with pytest.raises(ValueError):
            Obstruction(xb=Bounds3D.of(1, 0, 0, 1, 0, 1))

    def test_id_parameter(self):
        """Test ID parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), id="WALL_1")
        assert obst.id == "WALL_1"
        fds_str = obst.to_fds()
        assert "ID='WALL_1'" in fds_str


class TestObstructionSurfaces:
    """Tests for surface-related parameters."""

    def test_surf_ids_parameter(self):
        """Test SURF_IDS for top, sides, bottom."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            surf_ids=("CEILING", "WALL", "FLOOR"),
        )
        assert obst.surf_ids == ("CEILING", "WALL", "FLOOR")
        fds_str = obst.to_fds()
        assert "SURF_IDS=" in fds_str

    def test_surf_id6_parameter(self):
        """Test SURF_ID6 for all 6 faces."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            surf_id6=("SIDE1", "SIDE2", "SIDE3", "SIDE4", "BOTTOM", "TOP"),
        )
        assert obst.surf_id6 == ("SIDE1", "SIDE2", "SIDE3", "SIDE4", "BOTTOM", "TOP")
        fds_str = obst.to_fds()
        assert "SURF_ID6=" in fds_str

    def test_surf_id_interior(self):
        """Test surface ID for interior."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id_interior="CHAR")
        assert obst.surf_id_interior == "CHAR"
        fds_str = obst.to_fds()
        assert "SURF_ID_INTERIOR='CHAR'" in fds_str

    def test_surf_id_conflict_validation(self):
        """Test that only one of SURF_ID, SURF_IDS, SURF_ID6 can be specified."""
        with pytest.raises(ValidationError, match="Only one of"):
            Obstruction(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                surf_id="WALL",
                surf_ids=("TOP", "SIDE", "BOTTOM"),
            )

    def test_surf_ids_validation(self):
        """Test SURF_IDS must have 3 elements."""
        with pytest.raises(ValidationError):
            Obstruction(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                surf_ids=("TOP", "SIDE"),  # Only 2 elements
            )

    def test_surf_id6_validation(self):
        """Test SURF_ID6 must have 6 elements."""
        with pytest.raises(ValidationError):
            Obstruction(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                surf_id6=("S1", "S2", "S3", "S4", "S5"),  # Only 5 elements
            )


class TestObstructionAppearance:
    """Tests for appearance parameters."""

    def test_rgb_parameter(self):
        """Test RGB color parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), rgb=(255, 0, 0))
        assert obst.rgb == (255, 0, 0)
        fds_str = obst.to_fds()
        assert "RGB=255,0,0" in fds_str

    def test_rgb_validation(self):
        """Test RGB validation (must be 0-255)."""
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), rgb=(256, 0, 0))
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), rgb=(-1, 0, 0))

    def test_transparency_parameter(self):
        """Test transparency parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), transparency=0.5)
        assert obst.transparency == 0.5
        fds_str = obst.to_fds()
        assert "TRANSPARENCY=0.5" in fds_str

    def test_transparency_validation(self):
        """Test transparency validation (must be 0-1)."""
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), transparency=1.5)
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), transparency=-0.1)

    def test_outline_parameter(self):
        """Test outline parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), outline=True)
        assert obst.outline is True
        fds_str = obst.to_fds()
        assert "OUTLINE=.TRUE." in fds_str

    def test_color_parameter(self):
        """Test color parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), color="RED")
        assert obst.color == "RED"
        fds_str = obst.to_fds()
        assert "COLOR='RED'" in fds_str

    def test_texture_origin_parameter(self):
        """Test texture origin parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), texture_origin=(1.0, 2.0, 3.0))
        assert obst.texture_origin == (1.0, 2.0, 3.0)
        fds_str = obst.to_fds()
        assert "TEXTURE_ORIGIN=1.0,2.0,3.0" in fds_str


class TestObstructionGeometryControl:
    """Tests for geometry control parameters."""

    def test_geometry_control_parameters(self):
        """Test geometry control parameters."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            thicken=True,
            overlay=False,
            permit_hole=False,
            removable=False,
            allow_vent=False,
        )
        assert obst.thicken is True
        assert obst.overlay is False
        assert obst.permit_hole is False
        assert obst.removable is False
        assert obst.allow_vent is False
        fds_str = obst.to_fds()
        assert "THICKEN=.TRUE." in fds_str
        assert "OVERLAY=.FALSE." in fds_str
        assert "PERMIT_HOLE=.FALSE." in fds_str
        assert "REMOVABLE=.FALSE." in fds_str
        assert "ALLOW_VENT=.FALSE." in fds_str


class TestObstructionControl:
    """Tests for control/activation parameters."""

    def test_control_parameters(self):
        """Test control and device parameters."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), ctrl_id="CTRL1", devc_id="DEVC1")
        assert obst.ctrl_id == "CTRL1"
        assert obst.devc_id == "DEVC1"
        fds_str = obst.to_fds()
        assert "CTRL_ID='CTRL1'" in fds_str
        assert "DEVC_ID='DEVC1'" in fds_str

    def test_mult_id_parameter(self):
        """Test multiplier ID parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), mult_id="MULT1")
        assert obst.mult_id == "MULT1"
        fds_str = obst.to_fds()
        assert "MULT_ID='MULT1'" in fds_str


class TestObstructionBndf:
    """Tests for boundary file output parameters."""

    def test_bndf_obst_parameter(self):
        """Test BNDF_OBST parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), bndf_obst=False)
        assert obst.bndf_obst is False
        fds_str = obst.to_fds()
        assert "BNDF_OBST=.FALSE." in fds_str

    def test_bndf_face_parameter(self):
        """Test BNDF_FACE parameter."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            bndf_face=(True, True, False, False, True, True, True),
        )
        assert obst.bndf_face == (True, True, False, False, True, True, True)

    def test_bndf_face_validation(self):
        """Test BNDF_FACE must have 7 elements."""
        with pytest.raises(ValidationError):
            Obstruction(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                bndf_face=(True, True, False),  # Only 3 elements
            )


class TestObstructionHeatTransfer:
    """Tests for heat transfer parameters."""

    def test_matl_id_single(self):
        """Test single material ID."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), matl_id="CONCRETE")
        assert obst.matl_id == "CONCRETE"
        fds_str = obst.to_fds()
        assert "MATL_ID='CONCRETE'" in fds_str

    def test_matl_id_multiple(self):
        """Test multiple material IDs."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            matl_id=["MAT1", "MAT2"],
            matl_mass_fraction=[0.7, 0.3],
        )
        assert obst.matl_id == ["MAT1", "MAT2"]
        assert obst.matl_mass_fraction == [0.7, 0.3]
        fds_str = obst.to_fds()
        assert "'MAT1','MAT2'" in fds_str
        assert "MATL_MASS_FRACTION=0.7,0.3" in fds_str

    def test_ht3d_parameters(self):
        """Test HT3D heat transfer parameters."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            matl_id="CONCRETE",
            cell_size=0.05,
            cell_size_factor=2.0,
            stretch_factor=1.5,
            n_layer_cells_max=10,
            internal_heat_source=1000.0,
            ramp_ihs="HEAT_RAMP",
        )
        assert obst.matl_id == "CONCRETE"
        assert obst.cell_size == 0.05
        assert obst.cell_size_factor == 2.0
        assert obst.stretch_factor == 1.5
        assert obst.n_layer_cells_max == 10
        assert obst.internal_heat_source == 1000.0
        assert obst.ramp_ihs == "HEAT_RAMP"
        fds_str = obst.to_fds()
        assert "MATL_ID='CONCRETE'" in fds_str
        assert "CELL_SIZE=0.05" in fds_str
        assert "CELL_SIZE_FACTOR=2.0" in fds_str
        assert "STRETCH_FACTOR=1.5" in fds_str
        assert "N_LAYER_CELLS_MAX=10" in fds_str
        assert "INTERNAL_HEAT_SOURCE=1000.0" in fds_str
        assert "RAMP_IHS='HEAT_RAMP'" in fds_str

    def test_bulk_density(self):
        """Test bulk density parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), bulk_density=500.0)
        assert obst.bulk_density == 500.0
        fds_str = obst.to_fds()
        assert "BULK_DENSITY=500.0" in fds_str

    def test_bulk_density_validation(self):
        """Test bulk density validation (must be positive)."""
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), bulk_density=-100)


class TestObstructionShape:
    """Tests for shape-based geometry parameters."""

    def test_shape_parameters(self):
        """Test shape-based geometry parameters."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            shape="SPHERE",
            xyz=(0.5, 0.5, 0.5),
            radius=0.4,
            height=1.0,
            length=2.0,
            width=1.5,
            orientation=(0.0, 0.0, 1.0),
            theta=45.0,
        )
        assert obst.shape == "SPHERE"
        assert obst.xyz == (0.5, 0.5, 0.5)
        assert obst.radius == 0.4
        assert obst.height == 1.0
        assert obst.length == 2.0
        assert obst.width == 1.5
        assert obst.orientation == (0.0, 0.0, 1.0)
        assert obst.theta == 45.0
        fds_str = obst.to_fds()
        assert "SHAPE='SPHERE'" in fds_str
        assert "XYZ=0.5,0.5,0.5" in fds_str
        assert "RADIUS=0.4" in fds_str
        assert "HEIGHT=1.0" in fds_str
        assert "LENGTH=2.0" in fds_str
        assert "WIDTH=1.5" in fds_str
        assert "THETA=45.0" in fds_str

    def test_shape_validation(self):
        """Test shape parameter validation."""
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), shape="INVALID_SHAPE")

    def test_shape_requires_xyz(self):
        """Test that SHAPE requires XYZ."""
        with pytest.raises(ValidationError, match="SHAPE requires XYZ"):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), shape="SPHERE")


class TestObstructionComplex:
    """Tests for complex obstruction scenarios."""

    def test_complex_obstruction(self):
        """Test complex obstruction with multiple parameters."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 2, 0, 1, 0, 0.5),
            id="COMPLEX_WALL",
            surf_id="CONCRETE",
            rgb=(128, 128, 128),
            transparency=0.8,
            outline=True,
            matl_id="CONCRETE",
            cell_size=0.1,
            bulk_density=2400.0,
            surf_id_interior="CHAR",
        )
        fds_str = obst.to_fds()
        expected_parts = [
            "&OBST",
            "XB=0,2,0,1,0,0.5",
            "ID='COMPLEX_WALL'",
            "SURF_ID='CONCRETE'",
            "RGB=128,128,128",
            "TRANSPARENCY=0.8",
            "OUTLINE=.TRUE.",
            "MATL_ID='CONCRETE'",
            "CELL_SIZE=0.1",
            "BULK_DENSITY=2400.0",
            "SURF_ID_INTERIOR='CHAR'",
            "/",
        ]
        for part in expected_parts:
            assert part in fds_str

    def test_defaults_not_output(self):
        """Test that default values are not written to FDS."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        fds_str = obst.to_fds()
        # Default values should not appear
        assert "OVERLAY=.TRUE." not in fds_str
        assert "PERMIT_HOLE=.TRUE." not in fds_str
        assert "REMOVABLE=.TRUE." not in fds_str
        assert "ALLOW_VENT=.TRUE." not in fds_str
        assert "BNDF_OBST=.TRUE." not in fds_str
        assert "TRANSPARENCY=1" not in fds_str
        assert "OUTLINE=.FALSE." not in fds_str
        assert "THICKEN=.FALSE." not in fds_str
