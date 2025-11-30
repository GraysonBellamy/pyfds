"""Unit tests for DEVC namelist."""

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists import Device


class TestDevice:
    """Tests for Device namelist."""

    def test_basic_creation_xyz(self):
        """Test basic Device creation with XYZ."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 2.0))
        assert dev.id == "TEMP1"
        assert dev.quantity == "TEMPERATURE"
        assert dev.xyz == Point3D.of(1.0, 1.0, 2.0)

    def test_basic_creation_xb(self):
        """Test basic Device creation with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        assert dev.xb == Bounds3D.of(0, 1, 0, 1, 0, 1)

    def test_to_fds_xyz(self):
        """Test FDS output format with XYZ."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 2.0))
        fds_str = dev.to_fds()
        assert "&DEVC" in fds_str
        assert "ID='TEMP1'" in fds_str
        assert "QUANTITY='TEMPERATURE'" in fds_str
        assert "XYZ=1.0,1.0,2.0" in fds_str

    def test_to_fds_xb(self):
        """Test FDS output format with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        fds_str = dev.to_fds()
        assert "XB=" in fds_str
        assert "0" in fds_str and "1" in fds_str
