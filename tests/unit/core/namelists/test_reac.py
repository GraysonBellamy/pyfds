"""Unit tests for REAC namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Reaction


class TestReactionBasic:
    """Tests for basic Reaction creation."""

    def test_basic_creation(self):
        """Test basic reaction creation."""
        reac = Reaction(fuel="PROPANE")
        assert reac.fuel == "PROPANE"

    def test_reaction_with_id(self):
        """Test reaction with identifier."""
        reac = Reaction(id="MAIN_REACTION", fuel="PROPANE")
        assert reac.id == "MAIN_REACTION"
        fds_str = reac.to_fds()
        assert "ID='MAIN_REACTION'" in fds_str

    def test_reaction_to_fds(self):
        """Test FDS output format."""
        reac = Reaction(fuel="PROPANE")
        fds_str = reac.to_fds()
        assert "&REAC" in fds_str
        assert "FUEL='PROPANE'" in fds_str


class TestReactionFuelComposition:
    """Tests for fuel composition parameters (C, H, O, N)."""

    def test_carbon_atoms(self):
        """Test carbon atoms parameter."""
        reac = Reaction(fuel="MY_FUEL", c=7, h=16)
        assert reac.c == 7
        fds_str = reac.to_fds()
        assert "C=7" in fds_str

    def test_hydrogen_atoms(self):
        """Test hydrogen atoms parameter."""
        reac = Reaction(fuel="MY_FUEL", c=7, h=16)
        assert reac.h == 16
        fds_str = reac.to_fds()
        assert "H=16" in fds_str

    def test_oxygen_atoms(self):
        """Test oxygen atoms parameter."""
        reac = Reaction(fuel="MY_FUEL", c=3, h=6, o=2)
        assert reac.o == 2
        fds_str = reac.to_fds()
        assert "O=2" in fds_str

    def test_nitrogen_atoms(self):
        """Test nitrogen atoms parameter."""
        reac = Reaction(fuel="MY_FUEL", c=3, h=5, n=1)
        assert reac.n == 1
        fds_str = reac.to_fds()
        assert "N=1" in fds_str

    def test_full_fuel_composition(self):
        """Test fuel with all composition parameters."""
        reac = Reaction(
            fuel="COMPLEX_FUEL",
            c=3.52,
            h=5.48,
            o=0.88,
            n=0.32,
            heat_of_combustion=23200,
        )
        assert reac.c == 3.52
        assert reac.h == 5.48
        assert reac.o == 0.88
        assert reac.n == 0.32
        fds_str = reac.to_fds()
        assert "C=3.52" in fds_str
        assert "H=5.48" in fds_str
        assert "O=0.88" in fds_str
        assert "N=0.32" in fds_str

    def test_hydrogen_fuel_no_carbon(self):
        """Test hydrogen fuel (no carbon)."""
        # Hydrogen has no carbon, only hydrogen
        reac = Reaction(fuel="HYDROGEN", h=2)
        assert reac.h == 2
        assert reac.c is None
        fds_str = reac.to_fds()
        assert "H=2" in fds_str
        assert "C=" not in fds_str


class TestReactionEnergy:
    """Tests for energy parameters."""

    def test_heat_of_combustion(self):
        """Test heat of combustion parameter."""
        reac = Reaction(fuel="CUSTOM_FUEL", heat_of_combustion=44600)
        assert reac.heat_of_combustion == 44600
        fds_str = reac.to_fds()
        assert "HEAT_OF_COMBUSTION=44600" in fds_str

    def test_complete_heat_of_combustion(self):
        """Test complete heat of combustion."""
        reac = Reaction(fuel="METHANE", hoc_complete=55600)
        assert reac.hoc_complete == 55600
        fds_str = reac.to_fds()
        assert "HOC_COMPLETE=55600" in fds_str

    def test_energy_per_o2(self):
        """Test energy per unit mass O2."""
        reac = Reaction(fuel="PROPANE", epumo2=13100)
        assert reac.epumo2 == 13100
        fds_str = reac.to_fds()
        assert "EPUMO2=13100" in fds_str

    def test_radiative_fraction(self):
        """Test radiative fraction parameter."""
        reac = Reaction(fuel="PROPANE", radiative_fraction=0.35)
        assert reac.radiative_fraction == 0.35
        fds_str = reac.to_fds()
        assert "RADIATIVE_FRACTION=0.35" in fds_str

    def test_ramp_chi_r(self):
        """Test RAMP_CHI_R for time-varying radiative fraction."""
        reac = Reaction(fuel="PROPANE", ramp_chi_r="CHI_R_RAMP")
        assert reac.ramp_chi_r == "CHI_R_RAMP"
        fds_str = reac.to_fds()
        assert "RAMP_CHI_R='CHI_R_RAMP'" in fds_str

    def test_ideal_parameter(self):
        """Test IDEAL heat of combustion parameter."""
        reac = Reaction(fuel="PROPANE", ideal=True)
        assert reac.ideal is True
        fds_str = reac.to_fds()
        assert "IDEAL=.TRUE." in fds_str


class TestReactionYields:
    """Tests for product yield parameters."""

    def test_soot_yield(self):
        """Test soot yield parameter."""
        reac = Reaction(fuel="PROPANE", soot_yield=0.015)
        assert reac.soot_yield == 0.015
        fds_str = reac.to_fds()
        assert "SOOT_YIELD=0.015" in fds_str

    def test_co_yield(self):
        """Test CO yield parameter."""
        reac = Reaction(fuel="PROPANE", co_yield=0.05)
        assert reac.co_yield == 0.05
        fds_str = reac.to_fds()
        assert "CO_YIELD=0.05" in fds_str

    def test_hcn_yield(self):
        """Test HCN yield parameter."""
        reac = Reaction(fuel="WOOD", hcn_yield=0.001)
        assert reac.hcn_yield == 0.001
        fds_str = reac.to_fds()
        assert "HCN_YIELD=0.001" in fds_str

    def test_reaction_validation_yields(self):
        """Test that yields cannot exceed 1.0."""
        with pytest.raises(ValidationError, match=r"yields.*exceeds 1.0"):
            Reaction(fuel="PROPANE", soot_yield=0.6, co_yield=0.5)

    def test_yield_validation_with_hcn(self):
        """Test yield validation includes HCN."""
        with pytest.raises(ValidationError, match=r"yields.*exceeds 1.0"):
            Reaction(fuel="PROPANE", soot_yield=0.5, co_yield=0.3, hcn_yield=0.3)


class TestReactionTwoStepChemistry:
    """Tests for two-step chemistry parameters."""

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

    def test_fuel_radcal_id(self):
        """Test FUEL_RADCAL_ID for radiation absorption."""
        reac = Reaction(fuel="CUSTOM", fuel_radcal_id="METHANE")
        assert reac.fuel_radcal_id == "METHANE"
        fds_str = reac.to_fds()
        assert "FUEL_RADCAL_ID='METHANE'" in fds_str


class TestReactionIgnition:
    """Tests for auto-ignition parameters."""

    def test_auto_ignition_temperature(self):
        """Test auto-ignition temperature."""
        reac = Reaction(fuel="PROPANE", auto_ignition_temperature=450.0)
        assert reac.auto_ignition_temperature == 450.0
        fds_str = reac.to_fds()
        assert "AUTO_IGNITION_TEMPERATURE=450" in fds_str

    def test_auto_ignition_exclusion_zone(self):
        """Test auto-ignition exclusion zone."""
        zone = [(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)]
        reac = Reaction(
            fuel="PROPANE",
            ait_exclusion_zone=zone,
            ait_exclusion_zone_temperature=[300.0],
            ait_exclusion_zone_devc_id=["THERMOCOUPLE"],
            ait_exclusion_zone_ctrl_id=["CONTROL_LOGIC"],
        )
        assert reac.ait_exclusion_zone == zone
        assert reac.ait_exclusion_zone_temperature == [300.0]
        assert reac.ait_exclusion_zone_devc_id == ["THERMOCOUPLE"]
        assert reac.ait_exclusion_zone_ctrl_id == ["CONTROL_LOGIC"]

        fds_str = reac.to_fds()
        assert "AIT_EXCLUSION_ZONE" in fds_str
        assert "AIT_EXCLUSION_ZONE_TEMPERATURE" in fds_str
        assert "AIT_EXCLUSION_ZONE_DEVC_ID" in fds_str
        assert "AIT_EXCLUSION_ZONE_CTRL_ID" in fds_str

    def test_multiple_exclusion_zones(self):
        """Test multiple auto-ignition exclusion zones."""
        zones = [
            (0.0, 1.0, 0.0, 1.0, 0.0, 1.0),
            (2.0, 3.0, 0.0, 1.0, 0.0, 1.0),
        ]
        reac = Reaction(
            fuel="PROPANE",
            ait_exclusion_zone=zones,
            ait_exclusion_zone_temperature=[300.0, 350.0],
        )
        assert len(reac.ait_exclusion_zone) == 2


class TestReactionExtinction:
    """Tests for extinction parameters."""

    def test_critical_flame_temperature(self):
        """Test critical flame temperature."""
        reac = Reaction(fuel="PROPANE", critical_flame_temperature=1427.0)
        assert reac.critical_flame_temperature == 1427.0
        fds_str = reac.to_fds()
        assert "CRITICAL_FLAME_TEMPERATURE=1427" in fds_str

    def test_lower_oxygen_limit(self):
        """Test lower oxygen limit for extinction."""
        reac = Reaction(fuel="PROPANE", lower_oxygen_limit=0.12)
        assert reac.lower_oxygen_limit == 0.12
        fds_str = reac.to_fds()
        assert "LOWER_OXYGEN_LIMIT=0.12" in fds_str


class TestReactionValidation:
    """Tests for validation parameters."""

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


class TestReactionSpeciesStoichiometry:
    """Tests for species stoichiometry parameters."""

    def test_spec_id_nu_and_nu(self):
        """Test species stoichiometry arrays."""
        reac = Reaction(
            fuel="PROPANE",
            spec_id_nu=["CO2", "H2O", "SOOT"],
            nu=[3.0, 4.0, 0.01],
        )
        assert reac.spec_id_nu == ["CO2", "H2O", "SOOT"]
        assert reac.nu == [3.0, 4.0, 0.01]

        fds_str = reac.to_fds()
        assert "SPEC_ID_NU" in fds_str
        assert "NU" in fds_str

    def test_stoichiometry_array_mismatch(self):
        """Test that mismatched arrays are rejected."""
        with pytest.raises(ValidationError, match="same length"):
            Reaction(
                fuel="PROPANE",
                spec_id_nu=["CO2", "H2O"],
                nu=[3.0, 4.0, 0.01],
            )


class TestReactionKinetics:
    """Tests for finite-rate kinetics parameters."""

    def test_arrhenius_parameters(self):
        """Test Arrhenius rate parameters."""
        reac = Reaction(
            fuel="PROPANE",
            a=8.6e11,
            e=125520,
            n_t=0.5,
        )
        assert reac.a == 8.6e11
        assert reac.e == 125520
        assert reac.n_t == 0.5

        fds_str = reac.to_fds()
        assert "A=" in fds_str
        assert "E=125520" in fds_str
        assert "N_T=0.5" in fds_str

    def test_concentration_exponents(self):
        """Test species concentration exponents."""
        reac = Reaction(
            fuel="PROPANE",
            spec_id_n_s=["PROPANE", "OXYGEN"],
            n_s=[0.1, 1.65],
        )
        assert reac.spec_id_n_s == ["PROPANE", "OXYGEN"]
        assert reac.n_s == [0.1, 1.65]

        fds_str = reac.to_fds()
        assert "SPEC_ID_N_S" in fds_str
        assert "N_S" in fds_str

    def test_concentration_exponent_mismatch(self):
        """Test that mismatched N_S arrays are rejected."""
        with pytest.raises(ValidationError, match="same length"):
            Reaction(
                fuel="PROPANE",
                spec_id_n_s=["PROPANE"],
                n_s=[0.1, 1.65],
            )

    def test_equation_parameter(self):
        """Test reaction equation string."""
        reac = Reaction(
            fuel="METHANE",
            equation="CH4 + 2*O2 = CO2 + 2*H2O",
        )
        assert "=" in reac.equation
        fds_str = reac.to_fds()
        assert "EQUATION='CH4 + 2*O2 = CO2 + 2*H2O'" in fds_str

    def test_equation_validation(self):
        """Test equation must contain = sign."""
        with pytest.raises(ValidationError, match="EQUATION must contain"):
            Reaction(fuel="PROPANE", equation="invalid equation")

    def test_reactype_parameter(self):
        """Test REACTYPE parameter."""
        reac = Reaction(fuel="PROPANE", reactype="ARRHENIUS-TYPE")
        assert reac.reactype == "ARRHENIUS-TYPE"
        fds_str = reac.to_fds()
        assert "REACTYPE='ARRHENIUS-TYPE'" in fds_str

    def test_priority_parameter(self):
        """Test reaction priority."""
        reac = Reaction(fuel="PROPANE", priority=2)
        assert reac.priority == 2
        fds_str = reac.to_fds()
        assert "PRIORITY=2" in fds_str

    def test_reverse_parameter(self):
        """Test reversible reaction."""
        reac = Reaction(fuel="PROPANE", reverse=True)
        assert reac.reverse is True
        fds_str = reac.to_fds()
        assert "REVERSE=.TRUE." in fds_str


class TestReactionFalloff:
    """Tests for falloff reaction parameters."""

    def test_low_pressure_parameters(self):
        """Test low-pressure limit parameters for falloff."""
        reac = Reaction(
            fuel="H2",
            a_low_pr=1.2e17,
            e_low_pr=45500,
        )
        assert reac.a_low_pr == 1.2e17
        assert reac.e_low_pr == 45500

        fds_str = reac.to_fds()
        assert "A_LOW_PR" in fds_str
        assert "E_LOW_PR" in fds_str

    def test_troe_parameters(self):
        """Test Troe falloff parameters."""
        reac = Reaction(
            fuel="H2",
            a_troe=0.5,
            t1_troe=1000.0,
            t2_troe=2000.0,
            t3_troe=100.0,
        )
        assert reac.a_troe == 0.5
        assert reac.t1_troe == 1000.0
        assert reac.t2_troe == 2000.0
        assert reac.t3_troe == 100.0

        fds_str = reac.to_fds()
        assert "A_TROE=0.5" in fds_str
        assert "T1_TROE=1000" in fds_str
        assert "T2_TROE=2000" in fds_str
        assert "T3_TROE=100" in fds_str


class TestReactionThirdBody:
    """Tests for third body reaction parameters."""

    def test_third_body_parameter(self):
        """Test THIRD_BODY enable flag."""
        reac = Reaction(fuel="H2", third_body=True)
        assert reac.third_body is True
        fds_str = reac.to_fds()
        assert "THIRD_BODY=.TRUE." in fds_str

    def test_third_body_efficiencies(self):
        """Test third body efficiency arrays."""
        reac = Reaction(
            fuel="H2",
            third_body=True,
            third_eff=[2.5, 1.0, 12.0],
            third_eff_id=["H2", "O2", "H2O"],
        )
        assert reac.third_eff == [2.5, 1.0, 12.0]
        assert reac.third_eff_id == ["H2", "O2", "H2O"]

        fds_str = reac.to_fds()
        assert "THIRD_EFF" in fds_str
        assert "THIRD_EFF_ID" in fds_str

    def test_third_body_array_mismatch(self):
        """Test that mismatched third body arrays are rejected."""
        with pytest.raises(ValidationError, match="same length"):
            Reaction(
                fuel="H2",
                third_eff=[2.5, 1.0],
                third_eff_id=["H2", "O2", "H2O"],
            )


class TestReactionComplexScenarios:
    """Tests for complex reaction scenarios."""

    def test_finite_rate_kinetics(self):
        """Test complete finite-rate kinetics setup."""
        reac = Reaction(
            id="H2_O2",
            fuel="HYDROGEN",
            a=1.8e13,
            e=113400,
            n_t=0.0,
            spec_id_n_s=["HYDROGEN", "OXYGEN"],
            n_s=[1.0, 0.5],
            third_body=True,
            third_eff=[2.5, 1.0, 12.0],
            third_eff_id=["H2", "O2", "H2O"],
        )
        fds_str = reac.to_fds()
        assert "ID='H2_O2'" in fds_str
        assert "FUEL='HYDROGEN'" in fds_str
        assert "A=" in fds_str
        assert "E=113400" in fds_str
        assert "THIRD_BODY=.TRUE." in fds_str

    def test_two_step_with_yields(self):
        """Test two-step chemistry with product yields."""
        reac = Reaction(
            fuel="WOOD",
            n_simple_chemistry_reactions=2,
            fuel_c_to_co_fraction=0.15,
            soot_yield=0.02,
            co_yield=0.05,
            hcn_yield=0.001,
            radiative_fraction=0.30,
        )
        fds_str = reac.to_fds()
        assert "N_SIMPLE_CHEMISTRY_REACTIONS=2" in fds_str
        assert "FUEL_C_TO_CO_FRACTION=0.15" in fds_str
        assert "SOOT_YIELD=0.02" in fds_str
        assert "CO_YIELD=0.05" in fds_str
        assert "HCN_YIELD=0.001" in fds_str
        assert "RADIATIVE_FRACTION=0.3" in fds_str
