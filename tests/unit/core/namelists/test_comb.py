"""Tests for Combustion namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists.comb import Combustion


class TestCombustion:
    """Combustion namelist tests."""

    def test_default_values(self):
        """Test default combustion values."""
        comb = Combustion()
        assert comb.suppression is True
        assert comb.initial_unmixed_fraction == 1.0

    def test_extinction_model_validation(self):
        """Test extinction model validation."""
        comb = Combustion(extinction_model="EXTINCTION 2")
        assert comb.extinction_model == "EXTINCTION 2"

        with pytest.raises(ValueError):
            Combustion(extinction_model="INVALID")

    def test_tau_bounds_validation(self):
        """Test tau_chem <= tau_flame validation."""
        with pytest.raises(ValueError, match="TAU_CHEM must be <= TAU_FLAME"):
            Combustion(tau_chem=1.0, tau_flame=0.5)

    def test_to_fds(self):
        """Test FDS output."""
        comb = Combustion(
            extinction_model="EXTINCTION 2",
            initial_unmixed_fraction=0.5,
        )
        fds = comb.to_fds()
        assert "&COMB" in fds
        assert "EXTINCTION_MODEL='EXTINCTION 2'" in fds
        assert "INITIAL_UNMIXED_FRACTION=0.5" in fds

    def test_suppression_parameter(self):
        """Test suppression parameter."""
        comb = Combustion(suppression=False)
        fds = comb.to_fds()
        assert "SUPPRESSION=.FALSE." in fds

    def test_mixing_parameters(self):
        """Test mixing time parameters."""
        comb = Combustion(
            tau_chem=0.1,
            tau_flame=1.0,
            fixed_mix_time=0.5,
        )
        fds = comb.to_fds()
        assert "TAU_CHEM=0.1" in fds
        assert "TAU_FLAME=1.0" in fds
        assert "FIXED_MIX_TIME=0.5" in fds

    def test_ramp_parameters(self):
        """Test ramp parameters."""
        comb = Combustion(ramp_zeta_0="MIXING_RAMP")
        fds = comb.to_fds()
        assert "RAMP_ZETA_0='MIXING_RAMP'" in fds

    def test_species_thresholds(self):
        """Test species threshold parameters."""
        comb = Combustion(
            zz_min_global=1e-8,
            finite_rate_min_temp=500.0,
        )
        fds = comb.to_fds()
        assert "ZZ_MIN_GLOBAL=1e-08" in fds
        assert "FINITE_RATE_MIN_TEMP=500.0" in fds

    def test_diagnostics(self):
        """Test diagnostic parameters."""
        comb = Combustion(compute_adiabatic_flame_temperature=True)
        fds = comb.to_fds()
        assert "COMPUTE_ADIABATIC_FLAME_TEMPERATURE=.TRUE." in fds

    def test_parameter_ranges(self):
        """Test parameter range validation."""
        # Valid ranges
        Combustion(initial_unmixed_fraction=0.0)
        Combustion(initial_unmixed_fraction=1.0)

        # Invalid ranges
        with pytest.raises(ValidationError):
            Combustion(initial_unmixed_fraction=-0.1)
        with pytest.raises(ValidationError):
            Combustion(initial_unmixed_fraction=1.1)

    def test_minimal_output(self):
        """Test minimal FDS output with defaults."""
        comb = Combustion()
        fds = comb.to_fds()
        # Should only output non-default values
        lines = [
            line.strip() for line in fds.split("\n") if line.strip() and not line.startswith("!")
        ]
        # Should have &COMB and / only (since all values are defaults)
        assert len(lines) == 2
        assert lines[0] == "&COMB"
        assert lines[1] == "/"

    def test_case_insensitive_extinction_model(self):
        """Test case insensitive extinction model."""
        comb = Combustion(extinction_model="extinction 1")
        assert comb.extinction_model == "EXTINCTION 1"
        fds = comb.to_fds()
        assert "EXTINCTION_MODEL='EXTINCTION 1'" in fds
