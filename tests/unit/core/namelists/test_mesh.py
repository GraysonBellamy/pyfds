"""Unit tests for MESH namelist."""

import pytest

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Mesh


class TestMesh:
    """Tests for Mesh namelist."""

    def test_basic_creation(self):
        """Test basic Mesh creation."""
        mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        assert mesh.ijk == Grid3D.of(10, 10, 10)
        assert mesh.xb == Bounds3D.of(0, 1, 0, 1, 0, 1)

    def test_to_fds(self):
        """Test FDS output format."""
        mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        fds_str = mesh.to_fds()
        assert "&MESH" in fds_str
        assert "IJK=10,10,10" in fds_str
        assert "XB=" in fds_str
        assert "0" in fds_str and "1" in fds_str

    def test_ijk_validation_length(self):
        """Test IJK must have 3 values."""
        with pytest.raises(TypeError):
            Mesh(ijk=Grid3D.of(10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))

    def test_ijk_validation_positive(self):
        """Test IJK values must be positive."""
        with pytest.raises(ValueError):
            Mesh(ijk=Grid3D.of(-10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))

    def test_xb_validation_length(self):
        """Test XB must have 6 values."""
        with pytest.raises(TypeError):
            Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1))

    def test_xb_validation_bounds(self):
        """Test XB bounds must be valid."""
        with pytest.raises(ValueError):
            Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(1, 0, 0, 1, 0, 1))

    def test_get_cell_size(self):
        """Test cell size calculation."""
        mesh = Mesh(ijk=Grid3D.of(10, 20, 5), xb=Bounds3D.of(0, 10, 0, 10, 0, 5))
        dx, dy, dz = mesh.get_cell_size()
        assert dx == pytest.approx(1.0)
        assert dy == pytest.approx(0.5)
        assert dz == pytest.approx(1.0)

    def test_no_vn_cfl_parameters(self):
        """Test that MESH has VN/CFL parameters as None by default."""
        mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        # These attributes exist but are None by default
        assert hasattr(mesh, "vn_max")
        assert hasattr(mesh, "cfl_max")
        assert mesh.vn_max is None
        assert mesh.cfl_max is None

    def test_no_vn_cfl_in_output(self):
        """Test that MESH output does not contain VN or CFL parameters."""
        mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        fds_str = mesh.to_fds()
        # VN and CFL parameters should not appear in MESH output
        assert "VN_MAX" not in fds_str
        assert "VN_MIN" not in fds_str
        assert "CFL_MAX" not in fds_str
        assert "CFL_MIN" not in fds_str
        # But the MESH should still be valid
        assert "&MESH" in fds_str
