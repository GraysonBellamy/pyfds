"""
Unit tests for GeometryManager.
"""

from pyfds.core.geometry import Grid3D
from pyfds.core.managers.geometry import GeometryManager
from pyfds.core.simulation import Simulation


class TestGeometryManager:
    """Tests for GeometryManager class."""

    def test_initialization(self):
        """Test manager initialization."""
        mgr = GeometryManager()
        assert mgr.meshes == []
        assert mgr.obstructions == []
        assert mgr.vents == []

    def test_add_mesh_via_simulation(self):
        """Test adding meshes through simulation integration."""
        sim = Simulation(chid="test")
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Access through manager
        assert len(sim.geometry.meshes) == 1
        assert sim.geometry.meshes[0].ijk == Grid3D(nx=10, ny=10, nz=10)

    def test_add_multiple_meshes_via_simulation(self):
        """Test adding multiple meshes."""
        sim = Simulation(chid="test")
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.mesh(ijk=(20, 20, 10), xb=(1, 3, 0, 2, 0, 1))

        assert len(sim.geometry.meshes) == 2

    def test_add_obstruction_via_simulation(self):
        """Test adding an obstruction."""
        sim = Simulation(chid="test")
        sim.obstruction(xb=(0, 1, 0, 1, 0, 0.1), surf_id="FIRE")

        assert len(sim.geometry.obstructions) == 1
        assert sim.geometry.obstructions[0].surf_id == "FIRE"

    def test_add_vent_via_simulation(self):
        """Test adding a vent."""
        sim = Simulation(chid="test")
        sim.vent(xb=(0, 0, 0, 1, 0, 1), surf_id="OPEN")

        assert len(sim.geometry.vents) == 1
        assert sim.geometry.vents[0].surf_id == "OPEN"

    def test_validate_no_meshes(self):
        """Test validation fails when no meshes are defined."""
        mgr = GeometryManager()
        warnings = mgr.validate()

        assert len(warnings) == 1
        assert "No meshes defined" in warnings[0]

    def test_validate_with_mesh(self):
        """Test validation passes with valid mesh."""
        sim = Simulation(chid="test")
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        warnings = sim.geometry.validate()
        assert len(warnings) == 0

    def test_validate_non_cubic_cells(self):
        """Test validation warns about non-cubic cells."""
        sim = Simulation(chid="test")
        # Create mesh with non-cubic cells: dx=0.1, dy=0.3, dz=0.3 (aspect ratio 3.0)
        sim.mesh(ijk=(100, 10, 10), xb=(0, 10, 0, 3, 0, 3))

        warnings = sim.geometry.validate()
        # Should warn about aspect ratio
        aspect_warnings = [w for w in warnings if "aspect ratio" in w.lower()]
        assert len(aspect_warnings) > 0
