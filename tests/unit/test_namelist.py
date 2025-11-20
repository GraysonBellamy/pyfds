"""
Unit tests for namelist classes.
"""

import pytest
from pydantic import ValidationError

from pyfds.core.namelist import Device, Head, Mesh, Obstruction, Surface, Time


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
        assert mesh.ijk == (10, 10, 10)
        assert mesh.xb == (0, 1, 0, 1, 0, 1)

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
        assert obst.xb == (0, 1, 0, 1, 0, 1)

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
        assert obst.xb == (0, 0, 0, 1, 0, 1)

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
        assert dev.xyz == (1.0, 1.0, 2.0)

    def test_basic_creation_xb(self):
        """Test basic Device creation with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=(0, 1, 0, 1, 0, 1))
        assert dev.xb == (0, 1, 0, 1, 0, 1)

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
