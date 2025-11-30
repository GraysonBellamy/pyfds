"""Tests for cross-reference validation."""

from pyfds import Simulation
from pyfds.core.enums import Severity
from pyfds.core.namelists import Material, Surface
from pyfds.validation import CrossReferenceValidator


class TestCrossReferenceValidator:
    """Test cross-namelist reference validation."""

    def test_missing_matl_reference(self):
        """Test detection of missing MATL reference in SURF."""
        sim = Simulation(chid="test", title="Test")

        # Add surface referencing non-existent material
        sim.add(Surface(id="WALL", matl_id="MISSING_MATL", thickness=0.1))

        validator = CrossReferenceValidator(sim)
        issues = validator.validate()

        errors = [i for i in issues if i.severity == Severity.ERROR]
        assert len(errors) == 1
        assert "MISSING_MATL" in str(errors[0])

    def test_valid_matl_reference(self):
        """Test valid MATL reference passes."""
        sim = Simulation(chid="test", title="Test")

        matl = Material(id="CONCRETE", density=2400, conductivity=1.6, specific_heat=0.88)
        surf = Surface(id="WALL", matl_id="CONCRETE", thickness=0.1)

        sim.add(matl)
        sim.add(surf)

        validator = CrossReferenceValidator(sim)
        issues = validator.validate()

        errors = [i for i in issues if i.severity == Severity.ERROR]
        assert len(errors) == 0

    def test_missing_ramp_warning(self):
        """Test warning for missing RAMP reference."""
        sim = Simulation(chid="test", title="Test")

        sim.add(Surface(id="FIRE", hrrpua=500, ramp_q="MISSING_RAMP"))

        validator = CrossReferenceValidator(sim)
        issues = validator.validate()

        warnings = [i for i in issues if i.severity == Severity.WARNING]
        assert len(warnings) == 1
        assert "MISSING_RAMP" in str(warnings[0])

    def test_ht3d_on_vent_error(self):
        """Test error when HT3D surface used on VENT."""
        # This test would require more setup - placeholder for now
        pass

    def test_burn_away_without_bulk_density(self):
        """Test warning for BURN_AWAY without BULK_DENSITY."""
        # This test would require more setup - placeholder for now
        pass
