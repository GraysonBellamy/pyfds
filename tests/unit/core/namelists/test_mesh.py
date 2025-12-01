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
        """Test that VN/CFL parameters are NOT on MESH (they belong to MISC)."""
        mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        # VN/CFL parameters should NOT be on MESH - they belong to MISC
        assert not hasattr(mesh, "vn_max")
        assert not hasattr(mesh, "vn_min")
        assert not hasattr(mesh, "cfl_max")
        assert not hasattr(mesh, "cfl_min")
        assert not hasattr(mesh, "check_vn")

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


class TestMeshNewParameters:
    """Tests for new MESH parameters from FDS User Guide."""

    def test_bndf_mesh_default(self):
        """Test BNDF_MESH default is True."""
        mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        assert mesh.bndf_mesh is True
        # Should not appear in output when True (default)
        fds_str = mesh.to_fds()
        assert "BNDF_MESH" not in fds_str

    def test_bndf_mesh_false(self):
        """Test BNDF_MESH can be set to False."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            bndf_mesh=False,
        )
        assert mesh.bndf_mesh is False
        fds_str = mesh.to_fds()
        assert "BNDF_MESH=.FALSE." in fds_str

    def test_check_mesh_alignment(self):
        """Test CHECK_MESH_ALIGNMENT parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            check_mesh_alignment=True,
        )
        assert mesh.check_mesh_alignment is True
        fds_str = mesh.to_fds()
        assert "CHECK_MESH_ALIGNMENT=.TRUE." in fds_str

    def test_color(self):
        """Test COLOR parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            color="RED",
        )
        assert mesh.color == "RED"
        fds_str = mesh.to_fds()
        assert "COLOR='RED'" in fds_str

    def test_rgb(self):
        """Test RGB parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            rgb=(255, 128, 0),
        )
        assert mesh.rgb == (255, 128, 0)
        fds_str = mesh.to_fds()
        assert "RGB=255,128,0" in fds_str

    def test_rgb_validation(self):
        """Test RGB values must be in range 0-255."""
        from pydantic import ValidationError

        with pytest.raises(ValidationError, match="RGB"):
            Mesh(
                ijk=Grid3D.of(10, 10, 10),
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                rgb=(256, 0, 0),
            )

    def test_trnx_id(self):
        """Test TRNX_ID parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            trnx_id="X_TRANSFORM",
        )
        assert mesh.trnx_id == "X_TRANSFORM"
        fds_str = mesh.to_fds()
        assert "TRNX_ID='X_TRANSFORM'" in fds_str

    def test_trny_id(self):
        """Test TRNY_ID parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            trny_id="Y_TRANSFORM",
        )
        assert mesh.trny_id == "Y_TRANSFORM"
        fds_str = mesh.to_fds()
        assert "TRNY_ID='Y_TRANSFORM'" in fds_str

    def test_trnz_id(self):
        """Test TRNZ_ID parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            trnz_id="Z_TRANSFORM",
        )
        assert mesh.trnz_id == "Z_TRANSFORM"
        fds_str = mesh.to_fds()
        assert "TRNZ_ID='Z_TRANSFORM'" in fds_str

    def test_cylindrical(self):
        """Test CYLINDRICAL parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            cylindrical=True,
        )
        assert mesh.cylindrical is True
        fds_str = mesh.to_fds()
        assert "CYLINDRICAL=.TRUE." in fds_str

    def test_mpi_process(self):
        """Test MPI_PROCESS parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            mpi_process=2,
        )
        assert mesh.mpi_process == 2
        fds_str = mesh.to_fds()
        assert "MPI_PROCESS=2" in fds_str

    def test_mult_id(self):
        """Test MULT_ID parameter."""
        mesh = Mesh(
            ijk=Grid3D.of(10, 10, 10),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            mult_id="MESH_ARRAY",
        )
        assert mesh.mult_id == "MESH_ARRAY"
        fds_str = mesh.to_fds()
        assert "MULT_ID='MESH_ARRAY'" in fds_str
