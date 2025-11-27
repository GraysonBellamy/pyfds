"""Unit tests for REAC namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Reaction


class TestReaction:
    """Tests for Reaction namelist."""

    def test_basic_creation(self):
        """Test basic reaction creation."""
        reac = Reaction(fuel="PROPANE")
        assert reac.fuel == "PROPANE"

    def test_custom_reaction(self):
        """Test custom fuel composition."""
        reac = Reaction(c=7, h=16, heat_of_combustion=44600)
        assert reac.c == 7
        assert reac.h == 16

    def test_reaction_validation_yields(self):
        """Test that yields cannot exceed 1.0."""
        with pytest.raises(ValidationError, match=r"yields.*exceeds 1.0"):
            Reaction(fuel="PROPANE", soot_yield=0.6, co_yield=0.5)

    def test_reaction_to_fds(self):
        """Test FDS output format."""
        reac = Reaction(fuel="PROPANE")
        fds_str = reac.to_fds()
        assert "&REAC" in fds_str
        assert "FUEL='PROPANE'" in fds_str

    # Phase 3 tests
    def test_reaction_with_id(self):
        """Test reaction with identifier."""
        reac = Reaction(id="MAIN_REACTION", fuel="PROPANE")
        assert reac.id == "MAIN_REACTION"
        fds_str = reac.to_fds()
        assert "ID='MAIN_REACTION'" in fds_str

    def test_hcn_yield(self):
        """Test HCN yield parameter."""
        reac = Reaction(fuel="WOOD", hcn_yield=0.001)
        assert reac.hcn_yield == 0.001
        fds_str = reac.to_fds()
        assert "HCN_YIELD=0.001" in fds_str

    def test_energy_per_o2(self):
        """Test energy per unit mass O2."""
        reac = Reaction(fuel="PROPANE", epumo2=13100)
        assert reac.epumo2 == 13100
        fds_str = reac.to_fds()
        assert "EPUMO2=13100" in fds_str

    def test_complete_heat_of_combustion(self):
        """Test complete heat of combustion."""
        reac = Reaction(fuel="METHANE", hoc_complete=55600)
        assert reac.hoc_complete == 55600
        fds_str = reac.to_fds()
        assert "HOC_COMPLETE=55600" in fds_str

    def test_two_step_chemistry(self):
        """Test two-step chemistry parameters."""
        reac = Reaction(
            fuel="WOOD",
            n_simple_chemistry_reactions=2,
            fuel_c_to_co_fraction=0.1,
            fuel_n_to_hcn_fraction=0.001,
            fuel_h_to_h2_fraction=0.05,
        )
        assert reac.n_simple_chemistry_reactions == 2
        assert reac.fuel_c_to_co_fraction == 0.1
        assert reac.fuel_n_to_hcn_fraction == 0.001
        assert reac.fuel_h_to_h2_fraction == 0.05

        fds_str = reac.to_fds()
        assert "N_SIMPLE_CHEMISTRY_REACTIONS=2" in fds_str
        assert "FUEL_C_TO_CO_FRACTION=0.1" in fds_str
        assert "FUEL_N_TO_HCN_FRACTION=0.001" in fds_str
        assert "FUEL_H_TO_H2_FRACTION=0.05" in fds_str

    def test_yield_validation_with_hcn(self):
        """Test yield validation includes HCN."""
        with pytest.raises(ValidationError, match=r"yields.*exceeds 1.0"):
            Reaction(fuel="PROPANE", soot_yield=0.5, co_yield=0.3, hcn_yield=0.3)

    def test_lower_oxygen_limit(self):
        """Test lower oxygen limit for extinction."""
        reac = Reaction(fuel="PROPANE", lower_oxygen_limit=0.12)
        assert reac.lower_oxygen_limit == 0.12
        fds_str = reac.to_fds()
        assert "LOWER_OXYGEN_LIMIT=0.12" in fds_str

    def test_auto_ignition_exclusion_zone(self):
        """Test auto-ignition exclusion zone."""
        zone_bounds = (0, 1, 0, 1, 0, 1)
        reac = Reaction(
            fuel="PROPANE",
            ait_exclusion_zone=zone_bounds,
            ait_exclusion_zone_temperature=300.0,
            ait_exclusion_zone_devc_id="THERMOCOUPLE",
            ait_exclusion_zone_ctrl_id="CONTROL_LOGIC",
        )
        assert reac.ait_exclusion_zone == zone_bounds
        assert reac.ait_exclusion_zone_temperature == 300.0
        assert reac.ait_exclusion_zone_devc_id == "THERMOCOUPLE"
        assert reac.ait_exclusion_zone_ctrl_id == "CONTROL_LOGIC"

        fds_str = reac.to_fds()
        assert "AIT_EXCLUSION_ZONE=0.0,1.0,0.0,1.0,0.0,1.0" in fds_str
        assert "AIT_EXCLUSION_ZONE_TEMPERATURE=300.0" in fds_str
        assert "AIT_EXCLUSION_ZONE_DEVC_ID='THERMOCOUPLE'" in fds_str
        assert "AIT_EXCLUSION_ZONE_CTRL_ID='CONTROL_LOGIC'" in fds_str

    def test_auto_ignition_exclusion_zone_validation(self):
        """Test auto-ignition exclusion zone validation."""
        with pytest.raises(ValidationError):
            Reaction(fuel="PROPANE", ait_exclusion_zone=(0, 1, 0, 1))  # Wrong length

    def test_atom_balance_check(self):
        """Test atom balance check parameter."""
        reac = Reaction(fuel="PROPANE", check_atom_balance=False)
        assert reac.check_atom_balance is False
        fds_str = reac.to_fds()
        assert "CHECK_ATOM_BALANCE=.FALSE." in fds_str

    def test_validation_tolerances(self):
        """Test validation tolerance parameters."""
        reac = Reaction(fuel="PROPANE", reac_atom_error=1e-3, reac_mass_error=1e-3)
        assert reac.reac_atom_error == 1e-3
        assert reac.reac_mass_error == 1e-3

        fds_str = reac.to_fds()
        assert "REAC_ATOM_ERROR=0.001" in fds_str
        assert "REAC_MASS_ERROR=0.001" in fds_str
