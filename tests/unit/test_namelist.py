"""
Unit tests for namelist classes.
"""

import numpy as np
import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    ControlFunction,
    Ctrl,
    Device,
    Head,
    Init,
    Material,
    Mesh,
    Misc,
    Obstruction,
    Prop,
    Ramp,
    Reaction,
    Surface,
    Time,
    TurbulenceModel,
    Vent,
    VentShape,
    VentType,
)


class TestHead:
    """Tests for Head namelist."""

    def test_basic_creation(self):
        """Test basic Head creation."""
        head = Head(chid="test_case")
        assert head.chid == "test_case"
        assert head.title is None

    def test_with_title(self):
        """Test Head with title."""
        head = Head(chid="test_case", title="Test Simulation")
        assert head.chid == "test_case"
        assert head.title == "Test Simulation"

    def test_to_fds_basic(self):
        """Test FDS output format."""
        head = Head(chid="test_case")
        fds_str = head.to_fds()
        assert "&HEAD" in fds_str
        assert "CHID='test_case'" in fds_str
        assert fds_str.endswith("/\n")

    def test_to_fds_with_title(self):
        """Test FDS output with title."""
        head = Head(chid="test_case", title="Test Simulation")
        fds_str = head.to_fds()
        assert "TITLE='Test Simulation'" in fds_str

    def test_chid_validation_empty(self):
        """Test CHID cannot be empty."""
        with pytest.raises(ValidationError):
            Head(chid="")

    def test_chid_validation_spaces(self):
        """Test CHID cannot contain spaces."""
        with pytest.raises(ValidationError):
            Head(chid="test case")

    def test_chid_validation_length(self):
        """Test CHID length limit."""
        with pytest.raises(ValidationError):
            Head(chid="a" * 61)


class TestTime:
    """Tests for Time namelist."""

    def test_basic_creation(self):
        """Test basic Time creation."""
        time = Time(t_end=600.0)
        assert time.t_end == 600.0

    def test_with_optional_params(self):
        """Test Time with optional parameters."""
        time = Time(t_end=600.0, dt=0.1, t_begin=10.0)
        assert time.t_end == 600.0
        assert time.dt == 0.1
        assert time.t_begin == 10.0

    def test_to_fds_basic(self):
        """Test FDS output format."""
        time = Time(t_end=600.0)
        fds_str = time.to_fds()
        assert "&TIME" in fds_str
        assert "T_END=600.0" in fds_str

    def test_to_fds_with_dt(self):
        """Test FDS output with time step."""
        time = Time(t_end=600.0, dt=0.1)
        fds_str = time.to_fds()
        assert "DT=0.1" in fds_str

    def test_t_end_validation_positive(self):
        """Test T_END must be positive."""
        with pytest.raises(ValidationError):
            Time(t_end=-100.0)

    def test_t_begin_validation_negative(self):
        """Test T_BEGIN cannot be negative."""
        with pytest.raises(ValidationError):
            Time(t_end=100.0, t_begin=-10.0)


class TestMesh:
    """Tests for Mesh namelist."""

    def test_basic_creation(self):
        """Test basic Mesh creation."""
        mesh = Mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        assert mesh.ijk == Grid3D(10, 10, 10)
        assert mesh.xb == Bounds3D(0, 1, 0, 1, 0, 1)

    def test_to_fds(self):
        """Test FDS output format."""
        mesh = Mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        fds_str = mesh.to_fds()
        assert "&MESH" in fds_str
        assert "IJK=10,10,10" in fds_str
        assert "XB=" in fds_str
        assert "0" in fds_str and "1" in fds_str

    def test_ijk_validation_length(self):
        """Test IJK must have 3 values."""
        with pytest.raises(ValidationError):
            Mesh(ijk=(10, 10), xb=(0, 1, 0, 1, 0, 1))

    def test_ijk_validation_positive(self):
        """Test IJK values must be positive."""
        with pytest.raises(ValidationError):
            Mesh(ijk=(-10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

    def test_xb_validation_length(self):
        """Test XB must have 6 values."""
        with pytest.raises(ValidationError):
            Mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1))

    def test_xb_validation_bounds(self):
        """Test XB bounds must be valid."""
        with pytest.raises(ValidationError):
            Mesh(ijk=(10, 10, 10), xb=(1, 0, 0, 1, 0, 1))

    def test_get_cell_size(self):
        """Test cell size calculation."""
        mesh = Mesh(ijk=(10, 20, 5), xb=(0, 10, 0, 10, 0, 5))
        dx, dy, dz = mesh.get_cell_size()
        assert dx == pytest.approx(1.0)
        assert dy == pytest.approx(0.5)
        assert dz == pytest.approx(1.0)


class TestSurface:
    """Tests for Surface namelist."""

    def test_basic_creation(self):
        """Test basic Surface creation."""
        surf = Surface(id="FIRE")
        assert surf.id == "FIRE"

    def test_with_hrrpua(self):
        """Test Surface with heat release rate."""
        surf = Surface(id="FIRE", hrrpua=1000.0, color="RED")
        assert surf.hrrpua == 1000.0
        assert surf.color == "RED"

    def test_to_fds(self):
        """Test FDS output format."""
        surf = Surface(id="FIRE", hrrpua=1000.0, color="RED")
        fds_str = surf.to_fds()
        assert "&SURF" in fds_str
        assert "ID='FIRE'" in fds_str
        assert "HRRPUA=1000.0" in fds_str
        assert "COLOR='RED'" in fds_str

    def test_rgb_validation(self):
        """Test RGB validation."""
        surf = Surface(id="TEST", rgb=(255, 128, 0))
        assert surf.rgb == (255, 128, 0)

        with pytest.raises(ValidationError):
            Surface(id="TEST", rgb=(256, 0, 0))

        with pytest.raises(ValidationError):
            Surface(id="TEST", rgb=(-1, 0, 0))

    def test_hrrpua_validation(self):
        """Test HRRPUA must be non-negative."""
        with pytest.raises(ValidationError):
            Surface(id="FIRE", hrrpua=-100.0)


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


class TestDevice:
    """Tests for Device namelist."""

    def test_basic_creation_xyz(self):
        """Test basic Device creation with XYZ."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=(1.0, 1.0, 2.0))
        assert dev.id == "TEMP1"
        assert dev.quantity == "TEMPERATURE"
        assert dev.xyz == Point3D(1.0, 1.0, 2.0)

    def test_basic_creation_xb(self):
        """Test basic Device creation with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=(0, 1, 0, 1, 0, 1))
        assert dev.xb == Bounds3D(0, 1, 0, 1, 0, 1)

    def test_to_fds_xyz(self):
        """Test FDS output format with XYZ."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=(1.0, 1.0, 2.0))
        fds_str = dev.to_fds()
        assert "&DEVC" in fds_str
        assert "ID='TEMP1'" in fds_str
        assert "QUANTITY='TEMPERATURE'" in fds_str
        assert "XYZ=1.0,1.0,2.0" in fds_str

    def test_to_fds_xb(self):
        """Test FDS output format with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=(0, 1, 0, 1, 0, 1))
        fds_str = dev.to_fds()
        assert "XB=" in fds_str
        assert "0" in fds_str and "1" in fds_str


class TestRamp:
    """Tests for Ramp namelist."""

    def test_basic_creation(self):
        """Test basic ramp creation."""
        ramp = Ramp(id="TEST_RAMP", points=[(0, 0), (100, 1000)])
        assert ramp.id == "TEST_RAMP"
        assert len(ramp.points) == 2

    def test_ramp_sorting(self):
        """Test that points are automatically sorted."""
        ramp = Ramp(id="TEST", points=[(100, 1), (50, 0.5), (0, 0)])
        assert ramp.points[0] == (0, 0)
        assert ramp.points[1] == (50, 0.5)
        assert ramp.points[2] == (100, 1)

    def test_ramp_validation_min_points(self):
        """Test that ramp requires at least 2 points."""
        with pytest.raises(ValidationError, match="at least 2 points"):
            Ramp(id="TEST", points=[(0, 0)])

    def test_ramp_validation_duplicate_t(self):
        """Test that duplicate T values are rejected."""
        with pytest.raises(ValidationError, match="duplicate T values"):
            Ramp(id="TEST", points=[(0, 0), (0, 1), (100, 1000)])

    def test_ramp_evaluate(self):
        """Test ramp evaluation with linear interpolation."""
        ramp = Ramp(id="TEST", points=[(0, 0), (100, 1000)])
        assert ramp.evaluate(0) == 0
        assert ramp.evaluate(50) == 500
        assert ramp.evaluate(100) == 1000

    def test_ramp_to_fds(self):
        """Test FDS output format."""
        ramp = Ramp(id="HRR_RAMP", points=[(0, 0), (300, 1000)])
        fds_str = ramp.to_fds()
        assert "&RAMP ID='HRR_RAMP'" in fds_str
        assert "T=0" in fds_str
        assert "F=0" in fds_str


class TestMaterial:
    """Tests for Material namelist."""

    def test_basic_creation(self):
        """Test basic material creation."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        assert mat.id == "WOOD"
        assert mat.density == 500.0

    def test_material_with_ramp(self):
        """Test material with temperature-dependent properties."""
        mat = Material(id="STEEL", density=7850.0, conductivity_ramp="STEEL_K", specific_heat=0.46)
        assert mat.conductivity_ramp == "STEEL_K"

    def test_material_validation_no_conductivity(self):
        """Test that conductivity is required."""
        with pytest.raises(ValidationError, match="CONDUCTIVITY"):
            Material(id="BAD", density=500.0, specific_heat=2.5)

    def test_material_validation_density_range(self):
        """Test density range validation."""
        with pytest.raises(ValidationError, match="DENSITY"):
            Material(id="BAD", density=0.5, conductivity=0.13, specific_heat=2.5)

    def test_material_to_fds(self):
        """Test FDS output format."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        fds_str = mat.to_fds()
        assert "&MATL ID='WOOD'" in fds_str
        assert "DENSITY=500" in fds_str


class TestReaction:
    """Tests for Reaction namelist."""

    def test_basic_creation(self):
        """Test basic reaction creation."""
        reac = Reaction(fuel="PROPANE")
        assert reac.fuel == "PROPANE"

    def test_custom_reaction(self):
        """Test custom fuel composition."""
        reac = Reaction(c=7, h=16, heat_of_combustion=44600)
        assert reac.c == 7
        assert reac.h == 16

    def test_reaction_validation_yields(self):
        """Test that yields cannot exceed 1.0."""
        with pytest.raises(ValidationError, match=r"yields.*exceeds 1.0"):
            Reaction(fuel="PROPANE", soot_yield=0.6, co_yield=0.5)

    def test_reaction_to_fds(self):
        """Test FDS output format."""
        reac = Reaction(fuel="PROPANE")
        fds_str = reac.to_fds()
        assert "&REAC" in fds_str
        assert "FUEL='PROPANE'" in fds_str


class TestProp:
    """Tests for Prop namelist."""

    def test_sprinkler_prop(self):
        """Test sprinkler property."""
        prop = Prop(id="SPRINKLER", activation_temperature=68, rti=50)
        assert prop.id == "SPRINKLER"
        assert prop.activation_temperature == 68

    def test_prop_to_fds(self):
        """Test FDS output format."""
        prop = Prop(id="SPRINKLER", activation_temperature=68)
        fds_str = prop.to_fds()
        assert "&PROP ID='SPRINKLER'" in fds_str
        assert "ACTIVATION_TEMPERATURE=68" in fds_str


class TestCtrl:
    """Tests for Ctrl namelist."""

    def test_any_control(self):
        """Test ANY control logic."""
        ctrl = Ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD_1", "SD_2"])
        assert ctrl.function_type == ControlFunction.ANY
        assert len(ctrl.input_id) == 2

    def test_ctrl_validation_any_requires_list(self):
        """Test that ANY function requires multiple inputs."""
        with pytest.raises(ValidationError, match="multiple INPUT_ID"):
            Ctrl(id="BAD", function_type=ControlFunction.ANY, input_id="SD_1")

    def test_ctrl_to_fds(self):
        """Test FDS output format."""
        ctrl = Ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD_1", "SD_2"])
        fds_str = ctrl.to_fds()
        assert "&CTRL ID='ALARM'" in fds_str
        assert "FUNCTION_TYPE='ANY'" in fds_str


class TestInit:
    """Tests for Init namelist."""

    def test_basic_creation(self):
        """Test init with region bounds."""
        init = Init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500)
        assert init.xb == Bounds3D(0, 10, 0, 10, 0, 0.1)
        assert init.temperature == 500

    def test_init_validation_requires_xb_or_xyz(self):
        """Test that either XB or XYZ is required."""
        with pytest.raises(ValidationError, match="XB or XYZ"):
            Init(temperature=500)

    def test_init_to_fds(self):
        """Test FDS output format."""
        init = Init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500)
        fds_str = init.to_fds()
        assert "&INIT" in fds_str
        assert "XB=" in fds_str
        assert "TEMPERATURE=500" in fds_str


class TestVent:
    """Tests for Vent namelist."""

    def test_basic_open_vent(self):
        """Test basic rectangular opening."""
        vent = Vent(xb=(5, 5, 2, 4, 0, 3), surf_id="OPEN")
        assert vent.surf_id == "OPEN"
        assert vent.get_vent_type() == VentType.OPEN
        assert vent.get_shape() == VentShape.RECTANGULAR
        area = vent.get_area()
        assert area == pytest.approx(6.0)  # 2m x 3m

    def test_hvac_vent(self):
        """Test HVAC vent (flow parameters defined on SURF, not VENT)."""
        vent = Vent(xb=(5, 6, 5, 6, 3, 3), surf_id="HVAC")
        assert vent.surf_id == "HVAC"
        assert vent.get_vent_type() == VentType.HVAC
        area = vent.get_area()
        assert area == pytest.approx(1.0)

    def test_circular_vent(self):
        """Test circular vent creation."""
        vent = Vent(xb=(-1, 1, -1, 1, 0, 0), surf_id="FIRE", xyz=(0, 0, 0), radius=0.5)
        assert vent.get_shape() == VentShape.CIRCULAR
        area = vent.get_area()
        assert area == pytest.approx(np.pi * 0.25)

    def test_annular_vent(self):
        """Test annular (ring-shaped) vent."""
        vent = Vent(
            xb=(-2, 2, -2, 2, 0, 0), surf_id="BURNER", xyz=(0, 0, 0), radius=1.0, radius_inner=0.5
        )
        assert vent.get_shape() == VentShape.ANNULAR
        area = vent.get_area()
        expected = np.pi * (1.0**2 - 0.5**2)
        assert area == pytest.approx(expected)

    def test_mesh_boundary_vent(self):
        """Test vent on mesh boundary."""
        vent = Vent(mb="XMIN", surf_id="OPEN")
        assert vent.mb == "XMIN"
        assert vent.xb is None
        assert vent.get_vent_type() == VentType.OPEN

    def test_vent_requires_xb_or_mb(self):
        """Test that vent requires either XB or MB."""
        with pytest.raises(ValidationError, match="must have either XB or MB"):
            Vent(surf_id="OPEN")

    def test_vent_must_be_planar(self):
        """Test that XB must define a plane."""
        with pytest.raises(ValidationError, match="must be a plane"):
            Vent(xb=(0, 1, 0, 1, 0, 1), surf_id="OPEN")

    def test_planar_vents_all_directions(self):
        """Test planar vents in all three directions."""
        # X direction
        vent_x = Vent(xb=(5, 5, 0, 2, 0, 3), surf_id="OPEN")
        assert vent_x.get_area() == pytest.approx(6.0)

        # Y direction
        vent_y = Vent(xb=(0, 2, 5, 5, 0, 3), surf_id="OPEN")
        assert vent_y.get_area() == pytest.approx(6.0)

        # Z direction
        vent_z = Vent(xb=(0, 2, 0, 3, 0, 0), surf_id="OPEN")
        assert vent_z.get_area() == pytest.approx(6.0)

    def test_circular_requires_xyz(self):
        """Test circular vent requires XYZ."""
        with pytest.raises(ValidationError, match="requires both XYZ and RADIUS"):
            Vent(xb=(-1, 1, -1, 1, 0, 0), surf_id="FIRE", radius=0.5)

    def test_annular_inner_less_than_outer(self):
        """Test that inner radius must be less than outer radius."""
        with pytest.raises(ValidationError, match="must be less than RADIUS"):
            Vent(
                xb=(-2, 2, -2, 2, 0, 0),
                surf_id="BURNER",
                xyz=(0, 0, 0),
                radius=0.5,
                radius_inner=1.0,
            )

    def test_mb_valid_values(self):
        """Test MB must be valid boundary name."""
        with pytest.raises(ValidationError, match="MB must be one of"):
            Vent(mb="INVALID", surf_id="OPEN")

    def test_vent_type_detection(self):
        """Test vent type is correctly determined."""
        assert Vent(xb=(0, 0, 0, 1, 0, 1), surf_id="OPEN").get_vent_type() == VentType.OPEN
        assert Vent(xb=(0, 0, 0, 1, 0, 1), surf_id="HVAC").get_vent_type() == VentType.HVAC
        assert Vent(xb=(0, 0, 0, 1, 0, 1), surf_id="MIRROR").get_vent_type() == VentType.MIRROR
        assert Vent(xb=(0, 0, 0, 1, 0, 1), surf_id="PERIODIC").get_vent_type() == VentType.PERIODIC
        assert Vent(xb=(0, 0, 0, 1, 0, 1), surf_id="CUSTOM").get_vent_type() == VentType.SURFACE

    def test_vent_to_fds_basic(self):
        """Test FDS output for simple open vent."""
        vent = Vent(xb=(5, 5, 2, 4, 0, 3), surf_id="OPEN")
        fds_str = vent.to_fds()
        assert "&VENT" in fds_str
        assert "XB=" in fds_str
        assert "SURF_ID='OPEN'" in fds_str
        assert "/" in fds_str

    def test_vent_to_fds_hvac(self):
        """Test FDS output for HVAC vent."""
        vent = Vent(xb=(5, 6, 5, 6, 3, 3), surf_id="HVAC")
        fds_str = vent.to_fds()
        assert "SURF_ID='HVAC'" in fds_str

    def test_vent_to_fds_circular(self):
        """Test FDS output for circular vent."""
        vent = Vent(xb=(-1, 1, -1, 1, 0, 0), surf_id="FIRE", xyz=(0, 0, 0), radius=0.5)
        fds_str = vent.to_fds()
        assert "XYZ=" in fds_str
        assert "RADIUS=0.5" in fds_str

    def test_vent_with_control(self):
        """Test vent with control activation."""
        vent = Vent(xb=(5, 5, 2, 4, 0, 3), surf_id="OPEN", devc_id="TEMP_SENSOR", delay=30.0)
        assert vent.devc_id == "TEMP_SENSOR"
        assert vent.delay == 30.0


class TestMisc:
    """Tests for Misc namelist."""

    def test_basic_creation_defaults(self):
        """Test Misc creation with default values."""
        misc = Misc()
        assert misc.tmpa == 20.0
        assert misc.humidity == 40.0
        assert misc.turbulence_model == TurbulenceModel.DEARDORFF

    def test_ambient_conditions(self):
        """Test setting ambient conditions."""
        misc = Misc(tmpa=25.0, humidity=70.0)
        assert misc.tmpa == 25.0
        assert misc.humidity == 70.0

    def test_turbulence_model(self):
        """Test setting turbulence model."""
        misc = Misc(turbulence_model=TurbulenceModel.DYNAMIC_SMAGORINSKY, c_smagorinsky=0.18)
        assert misc.turbulence_model == TurbulenceModel.DYNAMIC_SMAGORINSKY
        assert misc.c_smagorinsky == 0.18

    def test_solid_phase_only(self):
        """Test solid phase only mode."""
        misc = Misc(solid_phase_only=True)
        assert misc.solid_phase_only is True

    def test_wildfire_mode(self):
        """Test wildfire simulation mode."""
        misc = Misc(level_set_mode=1, tmpa=35.0, humidity=15.0)
        assert misc.level_set_mode == 1
        assert misc.tmpa == 35.0
        assert misc.humidity == 15.0

    def test_restart_configuration(self):
        """Test restart configuration."""
        misc = Misc(restart=True, restart_chid="previous_run")
        assert misc.restart is True
        assert misc.restart_chid == "previous_run"

    def test_cfl_parameters(self):
        """Test CFL number settings."""
        misc = Misc(cfl_min=0.9, cfl_max=1.1)
        assert misc.cfl_min == 0.9
        assert misc.cfl_max == 1.1

    def test_temperature_validation(self):
        """Test temperature range validation."""
        with pytest.raises(ValidationError, match="outside reasonable range"):
            Misc(tmpa=-300.0)

        with pytest.raises(ValidationError, match="outside reasonable range"):
            Misc(tmpa=2500.0)

    def test_cfl_validation(self):
        """Test CFL validation."""
        with pytest.raises(ValidationError, match=r"CFL_MIN.*must be less than"):
            Misc(cfl_min=1.5, cfl_max=1.0)

    def test_mode_conflicts(self):
        """Test that solid_phase_only and isothermal cannot both be true."""
        with pytest.raises(ValidationError, match="Cannot use both"):
            Misc(solid_phase_only=True, isothermal=True)

    def test_humidity_range(self):
        """Test humidity must be 0-100%."""
        misc = Misc(humidity=0.0)
        assert misc.humidity == 0.0

        misc = Misc(humidity=100.0)
        assert misc.humidity == 100.0

        with pytest.raises(ValidationError):
            Misc(humidity=-10.0)

        with pytest.raises(ValidationError):
            Misc(humidity=110.0)

    def test_to_fds_defaults_omitted(self):
        """Test that default values are not written to FDS."""
        misc = Misc()
        fds_str = misc.to_fds()
        # Default values should not appear
        assert "TMPA=20" not in fds_str
        assert "HUMIDITY=40" not in fds_str

    def test_to_fds_non_defaults(self):
        """Test that non-default values are written to FDS."""
        misc = Misc(tmpa=25.0, humidity=70.0, solid_phase_only=True)
        fds_str = misc.to_fds()
        assert "&MISC" in fds_str
        assert "TMPA=25" in fds_str
        assert "HUMIDITY=70" in fds_str
        assert "SOLID_PHASE_ONLY=" in fds_str

    def test_to_fds_turbulence_model(self):
        """Test turbulence model in FDS output."""
        misc = Misc(turbulence_model=TurbulenceModel.VREMAN)
        fds_str = misc.to_fds()
        assert "TURBULENCE_MODEL='VREMAN'" in fds_str

    def test_to_fds_cfl(self):
        """Test CFL parameters in FDS output."""
        misc = Misc(cfl_min=0.9, cfl_max=1.1)
        fds_str = misc.to_fds()
        assert "CFL_MIN=0.9" in fds_str
        assert "CFL_MAX=1.1" in fds_str

    def test_pressure_validation(self):
        """Test pressure must be positive."""
        with pytest.raises(ValidationError):
            Misc(p_inf=-1000.0)

    def test_level_set_mode_range(self):
        """Test level_set_mode must be 0, 1, or 2."""
        misc = Misc(level_set_mode=0)
        assert misc.level_set_mode == 0

        misc = Misc(level_set_mode=2)
        assert misc.level_set_mode == 2

        with pytest.raises(ValidationError):
            Misc(level_set_mode=3)
