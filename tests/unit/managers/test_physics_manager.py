"""
Unit tests for PhysicsManager.
"""

from pyfds.core.managers.physics import PhysicsManager
from pyfds.core.namelists import Misc, Reaction
from pyfds.core.simulation import Simulation


class TestPhysicsManager:
    """Tests for PhysicsManager class."""

    def test_initialization(self):
        """Test manager initialization."""
        mgr = PhysicsManager()
        assert mgr.reactions == []
        assert mgr.misc_params is None

    def test_add_reaction_directly(self):
        """Test adding reaction directly to manager."""
        mgr = PhysicsManager()
        reaction = Reaction(fuel="PROPANE")
        mgr.add_reaction(reaction)

        assert len(mgr.reactions) == 1
        assert mgr.reactions[0].fuel == "PROPANE"

    def test_add_reaction_via_simulation_builder(self):
        """Test adding reaction through simulation builder method."""
        sim = Simulation(chid="test")
        sim.reaction(fuel="METHANE", soot_yield=0.015)

        assert len(sim.physics.reactions) == 1
        assert sim.physics.reactions[0].fuel == "METHANE"
        assert sim.physics.reactions[0].soot_yield == 0.015

    def test_add_reaction_via_simulation_add_method(self):
        """Test adding reaction through simulation add_reaction method."""
        sim = Simulation(chid="test")
        reaction = Reaction(fuel="PROPANE", co_yield=0.05)
        sim.add_reaction(reaction)

        assert len(sim.physics.reactions) == 1
        assert sim.physics.reactions[0].co_yield == 0.05

    def test_add_multiple_reactions(self):
        """Test adding multiple reactions."""
        sim = Simulation(chid="test")
        sim.reaction(fuel="PROPANE")
        sim.reaction(fuel="METHANE")

        assert len(sim.physics.reactions) == 2

    def test_set_misc_with_object(self):
        """Test setting MISC parameters with Misc object."""
        mgr = PhysicsManager()
        misc = Misc(tmpa=25.0, humidity=50.0)
        mgr.set_misc(misc)

        assert mgr.misc_params is not None
        assert mgr.misc_params.tmpa == 25.0
        assert mgr.misc_params.humidity == 50.0

    def test_set_misc_with_kwargs(self):
        """Test setting MISC parameters with keyword arguments."""
        mgr = PhysicsManager()
        mgr.set_misc(tmpa=30.0)

        assert mgr.misc_params is not None
        assert mgr.misc_params.tmpa == 30.0

    def test_set_misc_via_simulation(self):
        """Test setting MISC through simulation."""
        sim = Simulation(chid="test")
        sim.set_misc(tmpa=20.0, solid_phase_only=True)

        assert sim.physics.misc_params is not None
        assert sim.physics.misc_params.tmpa == 20.0
        assert sim.physics.misc_params.solid_phase_only is True

    def test_set_misc_overwrites_previous(self):
        """Test that setting MISC overwrites previous value."""
        mgr = PhysicsManager()
        mgr.set_misc(tmpa=20.0)
        mgr.set_misc(tmpa=30.0)

        assert mgr.misc_params.tmpa == 30.0

    def test_validate_no_warnings_when_empty(self):
        """Test validation passes with no reactions or misc."""
        mgr = PhysicsManager()
        warnings = mgr.validate()
        assert len(warnings) == 0

    def test_validate_with_reaction(self):
        """Test validation with reaction."""
        sim = Simulation(chid="test")
        sim.reaction(fuel="PROPANE")

        warnings = sim.physics.validate()
        assert len(warnings) == 0

    def test_validate_with_misc(self):
        """Test validation with misc parameters."""
        sim = Simulation(chid="test")
        sim.set_misc(tmpa=25.0)

        warnings = sim.physics.validate()
        assert len(warnings) == 0
