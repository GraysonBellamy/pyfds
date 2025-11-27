"""Unit tests for SPEC namelist (Priority 1)."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Species


class TestSpecBasic:
    """Test basic SPEC functionality."""

    def test_predefined_species(self):
        """Test predefined species definition."""
        spec = Species(id="OXYGEN", mass_fraction_0=0.23)
        assert spec.id == "OXYGEN"
        assert spec.mass_fraction_0 == 0.23
        fds_output = spec.to_fds()
        assert "ID=" in fds_output or "id=" in fds_output.lower()
        assert "MASS_FRACTION_0" in fds_output or "mass_fraction_0" in fds_output.lower()

    def test_user_defined_fuel(self):
        """Test user-defined fuel with composition."""
        spec = Species(fuel="MY_FUEL", c=7, h=16)
        assert spec.fuel == "MY_FUEL"
        assert spec.c == 7
        assert spec.h == 16
        fds_output = spec.to_fds()
        assert "FUEL=" in fds_output or "fuel=" in fds_output.lower()

    def test_lumped_component(self):
        """Test lumped component species."""
        spec = Species(id="TOLUENE", lumped_component_only=True)
        assert spec.lumped_component_only is True
        fds_output = spec.to_fds()
        assert (
            "LUMPED_COMPONENT_ONLY" in fds_output or "lumped_component_only" in fds_output.lower()
        )

    def test_requires_id_or_fuel(self):
        """Test that either ID or FUEL must be specified."""
        with pytest.raises(ValidationError, match="Either ID or FUEL must be specified"):
            Species(c=7, h=16)


class TestSpecMixture:
    """Test mixture species functionality."""

    def test_mixture_with_mass_fraction(self):
        """Test mixture species with mass fractions."""
        spec = Species(id="FUEL_MIX", spec_id=["PROPANE", "ETHANE"], mass_fraction=[0.7, 0.3])
        assert len(spec.spec_id) == 2
        assert len(spec.mass_fraction) == 2
        assert sum(spec.mass_fraction) == pytest.approx(1.0)

    def test_mixture_with_volume_fraction(self):
        """Test mixture species with volume fractions."""
        spec = Species(id="GAS_MIX", spec_id=["METHANE", "ETHANE"], volume_fraction=[0.6, 0.4])
        assert len(spec.spec_id) == 2
        assert len(spec.volume_fraction) == 2
        assert sum(spec.volume_fraction) == pytest.approx(1.0)

    def test_mixture_length_mismatch(self):
        """Test validation of mismatched array lengths."""
        with pytest.raises(ValidationError, match="must have same length"):
            Species(id="BAD_MIX", spec_id=["A", "B"], mass_fraction=[0.5, 0.3, 0.2])

    def test_mixture_fractions_sum_validation(self):
        """Test that fractions must sum to 1.0."""
        with pytest.raises(ValidationError, match=r"must sum to 1.0"):
            Species(id="BAD_MIX", spec_id=["A", "B"], mass_fraction=[0.5, 0.3])

    def test_cannot_specify_both_fractions(self):
        """Test that mass and volume fractions are mutually exclusive."""
        with pytest.raises(ValidationError, match="Cannot specify both"):
            Species(
                id="BAD_MIX",
                spec_id=["A", "B"],
                mass_fraction=[0.5, 0.5],
                volume_fraction=[0.5, 0.5],
            )


class TestSpecAerosol:
    """Test aerosol species functionality."""

    def test_aerosol_requires_density(self):
        """Test that aerosol species requires density_solid."""
        with pytest.raises(ValidationError, match="DENSITY_SOLID must be specified"):
            Species(id="SOOT", aerosol=True)

    def test_aerosol_with_properties(self):
        """Test aerosol with full properties."""
        spec = Species(id="SOOT", aerosol=True, density_solid=1800.0, mean_diameter=1e-6)
        assert spec.aerosol is True
        assert spec.density_solid == 1800.0
        assert spec.mean_diameter == 1e-6


class TestSpecThermophysical:
    """Test thermophysical property overrides."""

    def test_custom_properties(self):
        """Test custom thermophysical properties."""
        spec = Species(
            id="CUSTOM", mw=44.0, specific_heat=1.0, conductivity=0.025, viscosity=1.8e-5
        )
        assert spec.mw == 44.0
        assert spec.specific_heat == 1.0
        assert spec.conductivity == 0.025
        assert spec.viscosity == 1.8e-5

    def test_negative_properties_invalid(self):
        """Test that negative properties raise errors."""
        with pytest.raises(ValidationError):
            Species(id="BAD", mw=-10.0)


class TestSpecAmbient:
    """Test ambient composition parameters."""

    def test_mass_fraction_0_range(self):
        """Test mass_fraction_0 range validation."""
        # Valid
        spec = Species(id="O2", mass_fraction_0=0.23)
        assert spec.mass_fraction_0 == 0.23

        # Invalid - too high
        with pytest.raises(ValidationError):
            Species(id="BAD", mass_fraction_0=1.5)

        # Invalid - negative
        with pytest.raises(ValidationError):
            Species(id="BAD", mass_fraction_0=-0.1)

    def test_volume_fraction_0_range(self):
        """Test volume_fraction_0 range validation."""
        spec = Species(id="N2", volume_fraction_0=0.79)
        assert spec.volume_fraction_0 == 0.79

        with pytest.raises(ValidationError):
            Species(id="BAD", volume_fraction_0=2.0)

    def test_mutual_exclusivity_ambient(self):
        """Test that mass and volume fractions are mutually exclusive for ambient."""
        with pytest.raises(ValidationError, match="Cannot specify both"):
            Species(id="BAD", mass_fraction_0=0.2, volume_fraction_0=0.2)


class TestSpecFDSOutput:
    """Test FDS output generation."""

    def test_minimal_spec(self):
        """Test minimal SPEC output."""
        spec = Species(id="TEST")
        output = spec.to_fds()
        assert output.startswith("&SPEC")
        assert output.strip().endswith("/")
        assert "ID=" in output or "id=" in output.lower()

    def test_fuel_spec_output(self):
        """Test FUEL-based SPEC output."""
        spec = Species(fuel="MY_FUEL", c=1, h=4)
        output = spec.to_fds()
        assert "FUEL=" in output or "fuel=" in output.lower()
        assert "C=1" in output or "c=1" in output.lower()
        assert "H=4" in output or "h=4" in output.lower()

    def test_mixture_spec_output(self):
        """Test mixture SPEC output."""
        spec = Species(id="MIX", spec_id=["A", "B"], mass_fraction=[0.6, 0.4])
        output = spec.to_fds()
        assert "SPEC_ID=" in output or "spec_id=" in output.lower()
        assert "MASS_FRACTION=" in output or "mass_fraction=" in output.lower()
