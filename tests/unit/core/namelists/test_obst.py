"""Unit tests for OBST namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Obstruction


class TestObstruction:
    """Tests for Obstruction namelist."""

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

    def test_burn_away_basic(self):
        """Test basic burn-away functionality."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), burn_away=True)
        assert obst.burn_away is True
        fds_str = obst.to_fds()
        assert "BURN_AWAY=.TRUE." in fds_str

    def test_bulk_density(self):
        """Test bulk density parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), bulk_density=500.0)
        assert obst.bulk_density == 500.0
        fds_str = obst.to_fds()
        assert "BULK_DENSITY=500.0" in fds_str

    def test_surf_id_interior(self):
        """Test surface ID for interior."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id_interior="CHAR")
        assert obst.surf_id_interior == "CHAR"
        fds_str = obst.to_fds()
        assert "SURF_ID_INTERIOR='CHAR'" in fds_str

    def test_bulk_density_validation(self):
        """Test bulk density validation (must be positive)."""
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), bulk_density=-100)

    def test_id_parameter(self):
        """Test ID parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), id="WALL_1")
        assert obst.id == "WALL_1"
        fds_str = obst.to_fds()
        assert "ID='WALL_1'" in fds_str

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

    def test_texture_origin_parameter(self):
        """Test texture origin parameter."""
        obst = Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), texture_origin=(1.0, 2.0, 3.0))
        assert obst.texture_origin == (1.0, 2.0, 3.0)
        fds_str = obst.to_fds()
        assert "TEXTURE_ORIGIN=1.0,2.0,3.0" in fds_str

    def test_ht3d_parameters(self):
        """Test HT3D heat transfer parameters."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            ht3d=True,
            matl_id="CONCRETE",
            cell_size=0.05,
            cell_size_factor=2.0,
            stretch_factor=1.5,
            n_layer_cells_max=10,
            internal_heat_source=1000.0,
            ramp_ihs="HEAT_RAMP",
        )
        assert obst.ht3d is True
        assert obst.matl_id == "CONCRETE"
        assert obst.cell_size == 0.05
        assert obst.cell_size_factor == 2.0
        assert obst.stretch_factor == 1.5
        assert obst.n_layer_cells_max == 10
        assert obst.internal_heat_source == 1000.0
        assert obst.ramp_ihs == "HEAT_RAMP"
        fds_str = obst.to_fds()
        assert "HT3D=.TRUE." in fds_str
        assert "MATL_ID='CONCRETE'" in fds_str
        assert "CELL_SIZE=0.05" in fds_str
        assert "CELL_SIZE_FACTOR=2.0" in fds_str
        assert "STRETCH_FACTOR=1.5" in fds_str
        assert "N_LAYER_CELLS_MAX=10" in fds_str
        assert "INTERNAL_HEAT_SOURCE=1000.0" in fds_str
        assert "RAMP_IHS='HEAT_RAMP'" in fds_str

    def test_matl_mass_fraction(self):
        """Test material mass fraction parameter."""
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
            orientation=(0, 0, 1),
            theta=45.0,
        )
        assert obst.shape == "SPHERE"
        assert obst.xyz == (0.5, 0.5, 0.5)
        assert obst.radius == 0.4
        assert obst.height == 1.0
        assert obst.length == 2.0
        assert obst.width == 1.5
        assert obst.orientation == (0, 0, 1)
        assert obst.theta == 45.0
        fds_str = obst.to_fds()
        assert "SHAPE='SPHERE'" in fds_str
        assert "XYZ=0.5,0.5,0.5" in fds_str
        assert "RADIUS=0.4" in fds_str
        assert "HEIGHT=1.0" in fds_str
        assert "LENGTH=2.0" in fds_str
        assert "WIDTH=1.5" in fds_str
        assert "ORIENTATION=0,0,1" in fds_str
        assert "THETA=45.0" in fds_str

    def test_shape_validation(self):
        """Test shape parameter validation."""
        with pytest.raises(ValidationError):
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), shape="INVALID_SHAPE")

    def test_surf_id_individual_faces(self):
        """Test individual surface IDs for faces."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            surf_id_top="CEILING",
            surf_id_bottom="FLOOR",
            surf_id_sides="WALL",
        )
        assert obst.surf_id_top == "CEILING"
        assert obst.surf_id_bottom == "FLOOR"
        assert obst.surf_id_sides == "WALL"
        fds_str = obst.to_fds()
        assert "SURF_ID_TOP='CEILING'" in fds_str
        assert "SURF_ID_BOTTOM='FLOOR'" in fds_str
        assert "SURF_ID_SIDES='WALL'" in fds_str

    def test_complex_obstruction(self):
        """Test complex obstruction with multiple parameters."""
        obst = Obstruction(
            xb=Bounds3D.of(0, 2, 0, 1, 0, 0.5),
            id="COMPLEX_WALL",
            surf_id="CONCRETE",
            rgb=(128, 128, 128),
            transparency=0.8,
            outline=True,
            ht3d=True,
            matl_id="CONCRETE",
            cell_size=0.1,
            bulk_density=2400.0,
            burn_away=True,
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
            "HT3D=.TRUE.",
            "MATL_ID='CONCRETE'",
            "CELL_SIZE=0.1",
            "BULK_DENSITY=2400.0",
            "BURN_AWAY=.TRUE.",
            "SURF_ID_INTERIOR='CHAR'",
            "/",
        ]
        for part in expected_parts:
            assert part in fds_str
