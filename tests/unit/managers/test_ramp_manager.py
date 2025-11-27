"""Tests for RampManager."""

from pyfds.core.managers import RampManager
from pyfds.core.namelists import Ramp
from pyfds.core.simulation import Simulation


class TestRampManager:
    """Test RampManager functionality."""

    def test_initialization(self):
        """Test RampManager initializes with empty ramp list."""
        manager = RampManager()
        assert len(manager.ramps) == 0

    def test_add_ramp(self):
        """Test adding ramps through manager."""
        manager = RampManager()
        ramp = Ramp(id="TEST_RAMP", points=[(0, 0), (100, 1000)])
        manager.add_ramp(ramp)

        assert len(manager.ramps) == 1
        assert manager.ramps[0].id == "TEST_RAMP"

    def test_add_ramp_via_simulation(self):
        """Test adding ramps through simulation convenience method."""
        sim = Simulation(chid="test")
        ramp = Ramp(id="HRR_RAMP", points=[(0, 0), (60, 500)])
        sim.add_ramp(ramp)

        assert len(sim.ramps.ramps) == 1
        assert sim.ramps.ramps[0].id == "HRR_RAMP"

    def test_validate_no_duplicates(self):
        """Test validation passes with unique IDs."""
        manager = RampManager()
        manager.add_ramp(Ramp(id="RAMP1", points=[(0, 0), (100, 100)]))
        manager.add_ramp(Ramp(id="RAMP2", points=[(0, 0), (100, 200)]))

        warnings = manager.validate()
        assert len(warnings) == 0

    def test_validate_duplicate_ids(self):
        """Test validation detects duplicate ramp IDs."""
        manager = RampManager()
        manager.add_ramp(Ramp(id="DUPLICATE", points=[(0, 0), (100, 100)]))
        manager.add_ramp(Ramp(id="DUPLICATE", points=[(0, 0), (100, 200)]))

        warnings = manager.validate()
        assert len(warnings) == 1
        assert "Duplicate RAMP ID 'DUPLICATE'" in warnings[0]

    def test_validate_ramp_references_valid(self):
        """Test validation of ramp references when all exist."""
        manager = RampManager()
        manager.add_ramp(Ramp(id="RAMP_A", points=[(0, 0), (100, 100)]))
        manager.add_ramp(Ramp(id="RAMP_B", points=[(0, 0), (100, 200)]))

        referenced = {"RAMP_A", "RAMP_B"}
        warnings = manager.validate_ramp_references(referenced)
        assert len(warnings) == 0

    def test_validate_ramp_references_missing(self):
        """Test validation detects missing ramp references."""
        manager = RampManager()
        manager.add_ramp(Ramp(id="RAMP_A", points=[(0, 0), (100, 100)]))

        referenced = {"RAMP_A", "MISSING_RAMP"}
        warnings = manager.validate_ramp_references(referenced)
        assert len(warnings) == 1
        assert "Referenced undefined RAMP 'MISSING_RAMP'" in warnings[0]

    def test_ramp_in_fds_output(self):
        """Test ramps appear in FDS output."""
        sim = Simulation(chid="test")
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.add_ramp(Ramp(id="FIRE_RAMP", points=[(0, 0), (60, 1000)]))

        fds_output = sim.to_fds()
        assert "&RAMP ID='FIRE_RAMP'" in fds_output
        assert "T=0" in fds_output
        assert "F=0" in fds_output
        assert "T=60" in fds_output
        assert "F=1000" in fds_output
