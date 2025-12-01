"""Unit tests for VENT namelist."""

import numpy as np
import pytest
from pydantic import ValidationError

from pyfds.core.enums import VentShape, VentType
from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists import Vent


class TestVent:
    """Tests for Vent namelist."""

    def test_basic_open_vent(self):
        """Test basic rectangular opening."""
        vent = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id="OPEN")
        assert vent.surf_id == "OPEN"
        assert vent.get_vent_type() == VentType.OPEN
        assert vent.get_shape() == VentShape.RECTANGULAR
        area = vent.get_area()
        assert area == pytest.approx(6.0)  # 2m x 3m

    def test_hvac_vent(self):
        """Test HVAC vent (flow parameters defined on SURF, not VENT)."""
        vent = Vent(xb=Bounds3D.of(5, 6, 5, 6, 3, 3), surf_id="HVAC")
        assert vent.surf_id == "HVAC"
        assert vent.get_vent_type() == VentType.HVAC
        area = vent.get_area()
        assert area == pytest.approx(1.0)

    def test_circular_vent(self):
        """Test circular vent creation."""
        vent = Vent(
            xb=Bounds3D.of(-1, 1, -1, 1, 0, 0), surf_id="FIRE", xyz=Point3D.of(0, 0, 0), radius=0.5
        )
        assert vent.get_shape() == VentShape.CIRCULAR
        area = vent.get_area()
        assert area == pytest.approx(np.pi * 0.25)

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
            Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id="OPEN")

    def test_planar_vents_all_directions(self):
        """Test planar vents in all three directions."""
        # X direction
        vent_x = Vent(xb=Bounds3D.of(5, 5, 0, 2, 0, 3), surf_id="OPEN")
        assert vent_x.get_area() == pytest.approx(6.0)

        # Y direction
        vent_y = Vent(xb=Bounds3D.of(0, 2, 5, 5, 0, 3), surf_id="OPEN")
        assert vent_y.get_area() == pytest.approx(6.0)

        # Z direction
        vent_z = Vent(xb=Bounds3D.of(0, 2, 0, 3, 0, 0), surf_id="OPEN")
        assert vent_z.get_area() == pytest.approx(6.0)

    def test_circular_requires_xyz(self):
        """Test circular vent requires XYZ."""
        with pytest.raises(ValidationError, match="requires both XYZ and RADIUS"):
            Vent(xb=Bounds3D.of(-1, 1, -1, 1, 0, 0), surf_id="FIRE", radius=0.5)

    def test_mb_valid_values(self):
        """Test MB must be valid boundary name."""
        with pytest.raises(ValidationError, match="MB must be one of"):
            Vent(mb="INVALID", surf_id="OPEN")

    def test_vent_type_detection(self):
        """Test vent type is correctly determined."""
        assert (
            Vent(xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="OPEN").get_vent_type() == VentType.OPEN
        )
        assert (
            Vent(xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="HVAC").get_vent_type() == VentType.HVAC
        )
        assert (
            Vent(xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="MIRROR").get_vent_type()
            == VentType.MIRROR
        )
        assert (
            Vent(xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="PERIODIC").get_vent_type()
            == VentType.PERIODIC
        )
        assert (
            Vent(xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="CUSTOM").get_vent_type()
            == VentType.SURFACE
        )

    def test_vent_to_fds_basic(self):
        """Test FDS output for simple open vent."""
        vent = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id="OPEN")
        fds_str = vent.to_fds()
        assert "&VENT" in fds_str
        assert "XB=" in fds_str
        assert "SURF_ID='OPEN'" in fds_str
        assert "/" in fds_str

    def test_vent_to_fds_hvac(self):
        """Test FDS output for HVAC vent."""
        vent = Vent(xb=Bounds3D.of(5, 6, 5, 6, 3, 3), surf_id="HVAC")
        fds_str = vent.to_fds()
        assert "SURF_ID='HVAC'" in fds_str

    def test_vent_to_fds_circular(self):
        """Test FDS output for circular vent."""
        vent = Vent(
            xb=Bounds3D.of(-1, 1, -1, 1, 0, 0), surf_id="FIRE", xyz=Point3D.of(0, 0, 0), radius=0.5
        )
        fds_str = vent.to_fds()
        assert "XYZ=" in fds_str
        assert "RADIUS=0.5" in fds_str

    def test_vent_with_control(self):
        """Test vent with control activation."""
        vent = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id="OPEN", devc_id="TEMP_SENSOR")
        assert vent.devc_id == "TEMP_SENSOR"

    def test_dynamic_pressure(self):
        """Test dynamic pressure is a float with default 0.0."""
        # Default value
        vent = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id="OPEN")
        assert vent.dynamic_pressure == 0.0

        # Custom value
        vent2 = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id="OPEN", dynamic_pressure=100.0)
        assert vent2.dynamic_pressure == 100.0
        fds_str = vent2.to_fds()
        assert "DYNAMIC_PRESSURE=100.0" in fds_str

    def test_area_adjust(self):
        """Test area adjust parameter for spreading fire."""
        # Default is False
        vent = Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 0), surf_id="FIRE")
        assert vent.area_adjust is False

        # Set to True
        vent2 = Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 0), surf_id="FIRE", area_adjust=True)
        assert vent2.area_adjust is True
        fds_str = vent2.to_fds()
        assert "AREA_ADJUST=.TRUE." in fds_str

    def test_geom_level_set(self):
        """Test geom flag for level set fire spread."""
        # Default is False
        vent = Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 0), surf_id="FIRE")
        assert vent.geom is False

        # Set to True
        vent2 = Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 0), surf_id="FIRE", geom=True)
        assert vent2.geom is True
        fds_str = vent2.to_fds()
        assert "GEOM=.TRUE." in fds_str

    def test_spread_rate(self):
        """Test spread rate parameter."""
        # Default value
        vent = Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 0), surf_id="FIRE")
        assert vent.spread_rate == 0.05

        # Custom value
        vent2 = Vent(xb=Bounds3D.of(0, 1, 0, 1, 0, 0), surf_id="FIRE", spread_rate=0.1)
        assert vent2.spread_rate == 0.1
        fds_str = vent2.to_fds()
        assert "SPREAD_RATE=0.1" in fds_str

    def test_turbulence_parameters(self):
        """Test synthetic turbulence parameters."""
        vent = Vent(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 0),
            surf_id="INERT",
            n_eddy=100,
            l_eddy=0.1,
            vel_rms=0.5,
            uvw=(1.0, 0.0, 0.0),
        )
        assert vent.n_eddy == 100
        assert vent.l_eddy == 0.1
        assert vent.vel_rms == 0.5
        assert vent.uvw == (1.0, 0.0, 0.0)

        fds_str = vent.to_fds()
        assert "N_EDDY=100" in fds_str
        assert "L_EDDY=0.1" in fds_str
        assert "VEL_RMS=0.5" in fds_str
        assert "UVW=" in fds_str

    def test_reynolds_stress_3x3(self):
        """Test reynolds stress must be 3x3 matrix."""
        vent = Vent(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 0),
            surf_id="INERT",
            reynolds_stress=[[1, 0, 0], [0, 1, 0], [0, 0, 1]],
        )
        assert vent.reynolds_stress == [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

        with pytest.raises(ValidationError, match="REYNOLDS_STRESS must be a 3x3 matrix"):
            Vent(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 0),
                surf_id="INERT",
                reynolds_stress=[[1, 0], [0, 1]],
            )

    def test_l_eddy_ij_3x3(self):
        """Test l_eddy_ij must be 3x3 matrix."""
        vent = Vent(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 0),
            surf_id="INERT",
            l_eddy_ij=[[0.1, 0, 0], [0, 0.1, 0], [0, 0, 0.1]],
        )
        assert vent.l_eddy_ij == [[0.1, 0, 0], [0, 0.1, 0], [0, 0, 0.1]]

        with pytest.raises(ValidationError, match="L_EDDY_IJ must be a 3x3 matrix"):
            Vent(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 0),
                surf_id="INERT",
                l_eddy_ij=[[0.1, 0], [0, 0.1]],
            )

    def test_pressure_ramp(self):
        """Test pressure ramp for dynamic pressure."""
        vent = Vent(
            xb=Bounds3D.of(5, 5, 2, 4, 0, 3),
            surf_id="OPEN",
            dynamic_pressure=50.0,
            pressure_ramp="WIND_RAMP",
        )
        assert vent.pressure_ramp == "WIND_RAMP"
        fds_str = vent.to_fds()
        assert "PRESSURE_RAMP='WIND_RAMP'" in fds_str

    def test_tmp_exterior(self):
        """Test exterior temperature for OPEN boundary."""
        vent = Vent(
            xb=Bounds3D.of(5, 5, 2, 4, 0, 3),
            surf_id="OPEN",
            tmp_exterior=35.0,
            tmp_exterior_ramp="TEMP_RAMP",
        )
        assert vent.tmp_exterior == 35.0
        assert vent.tmp_exterior_ramp == "TEMP_RAMP"
        fds_str = vent.to_fds()
        assert "TMP_EXTERIOR=35.0" in fds_str
        assert "TMP_EXTERIOR_RAMP='TEMP_RAMP'" in fds_str
