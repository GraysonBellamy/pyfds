"""Unit tests for VENT namelist."""

import numpy as np
import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Vent, VentShape, VentType


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
