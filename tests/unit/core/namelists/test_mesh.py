"""Unit tests for MESH namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Mesh


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
