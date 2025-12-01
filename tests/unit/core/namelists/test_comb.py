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

    def test_ode_solver_parameters(self):
        """Test ODE solver configuration parameters."""
        comb = Combustion(
            ode_solver="CVODE",
            ode_min_atol=1e-10,
            ode_rel_error=1e-6,
            max_chemistry_substeps=50,
            n_fixed_chemistry_substeps=10,
        )
        fds = comb.to_fds()
        assert "ODE_SOLVER='CVODE'" in fds
        assert "ODE_MIN_ATOL=1e-10" in fds
        assert "ODE_REL_ERROR=1e-06" in fds
        assert "MAX_CHEMISTRY_SUBSTEPS=50" in fds
        assert "N_FIXED_CHEMISTRY_SUBSTEPS=10" in fds

    def test_ode_solver_validation(self):
        """Test ODE solver validation."""
        # Valid solvers
        Combustion(ode_solver="EXPLICIT EULER")
        Combustion(ode_solver="RK2")
        Combustion(ode_solver="RK2 RICHARDSON")
        Combustion(ode_solver="RK3")
        Combustion(ode_solver="CVODE")

        # Invalid solver
        with pytest.raises(ValueError, match="ODE_SOLVER must be one of"):
            Combustion(ode_solver="INVALID_SOLVER")

    def test_chemistry_integration_parameters(self):
        """Test chemistry integration parameters."""
        comb = Combustion(
            equiv_ratio_check=False,
            min_equiv_ratio=0.5,
            max_equiv_ratio=5.0,
            do_chem_load_balance=True,
        )
        fds = comb.to_fds()
        assert "EQUIV_RATIO_CHECK=.FALSE." in fds
        assert "MIN_EQUIV_RATIO=0.5" in fds
        assert "MAX_EQUIV_RATIO=5.0" in fds
        assert "DO_CHEM_LOAD_BALANCE=.TRUE." in fds

    def test_equiv_ratio_validation(self):
        """Test equivalence ratio validation."""
        # Valid: min <= max
        Combustion(min_equiv_ratio=0.5, max_equiv_ratio=10.0)

        # Invalid: min > max
        with pytest.raises(ValueError, match="MIN_EQUIV_RATIO must be <= MAX_EQUIV_RATIO"):
            Combustion(min_equiv_ratio=10.0, max_equiv_ratio=5.0)

    def test_free_burn_temperature(self):
        """Test free burn temperature parameter."""
        comb = Combustion(free_burn_temperature=700.0)
        fds = comb.to_fds()
        assert "FREE_BURN_TEMPERATURE=700.0" in fds

    def test_check_realizability(self):
        """Test check realizability parameter."""
        comb = Combustion(check_realizability=True)
        fds = comb.to_fds()
        assert "CHECK_REALIZABILITY=.TRUE." in fds

    def test_cvode_configuration(self):
        """Test CVODE solver with full configuration."""
        comb = Combustion(
            ode_solver="CVODE",
            ode_min_atol=1e-12,
            ode_rel_error=1e-6,
            equiv_ratio_check=True,
            min_equiv_ratio=0.0,
            max_equiv_ratio=20.0,
            do_chem_load_balance=True,
            finite_rate_min_temp=300.0,
        )
        assert comb.ode_solver == "CVODE"
        fds = comb.to_fds()
        assert "ODE_SOLVER='CVODE'" in fds
        assert "DO_CHEM_LOAD_BALANCE=.TRUE." in fds
