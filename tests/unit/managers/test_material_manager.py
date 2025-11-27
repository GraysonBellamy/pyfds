"""
Unit tests for MaterialManager.
"""

from pyfds.core.managers.material import MaterialManager
from pyfds.core.simulation import Simulation


class TestMaterialManager:
    """Tests for MaterialManager class."""

    def test_initialization(self):
        """Test MaterialManager initializes with empty lists."""
        mgr = MaterialManager()
        assert mgr.materials == []
        assert mgr.surfaces == []

    def test_add_surface_via_simulation(self):
        """Test adding a surface."""
        sim = Simulation(chid="test")
        sim.surface(id="FIRE", hrrpua=1000.0, color="RED")

        assert len(sim.material_mgr.surfaces) == 1
        assert sim.material_mgr.surfaces[0].id == "FIRE"

    def test_add_multiple_surfaces(self):
        """Test adding multiple surfaces."""
        sim = Simulation(chid="test")
        sim.surface(id="FIRE", hrrpua=1000.0)
        sim.surface(id="WALL", tmp_front=20.0)

        assert len(sim.material_mgr.surfaces) == 2

    def test_validate_surface_references(self):
        """Test surface reference validation."""
        mgr = MaterialManager()
        # No surfaces defined
        warnings = mgr.validate_surface_references({"FIRE", "WALL"})

        # Both should be flagged as undefined
        assert len(warnings) == 2

    def test_validate_surface_references_with_predefined(self):
        """Test predefined surfaces don't generate warnings."""
        mgr = MaterialManager()
        warnings = mgr.validate_surface_references({"INERT", "OPEN", "MIRROR"})

        # Predefined surfaces should not generate warnings
        assert len(warnings) == 0
