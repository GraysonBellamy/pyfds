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

    def test_formula_and_elements_mutual_exclusive(self):
        """Test that FORMULA and elemental composition are mutually exclusive."""
        # Valid: formula only
        spec = Species(id="METHANOL", formula="CH3OH", mw=32.0)
        assert spec.formula == "CH3OH"
        assert spec.c is None

        # Valid: elements only
        spec = Species(id="ETHANOL", c=2, h=6, o=1, mw=46.0)
        assert spec.c == 2
        assert spec.formula is None

        # Invalid: both formula and elements
        with pytest.raises(
            ValidationError, match="Cannot specify both FORMULA and elemental composition"
        ):
            Species(id="BAD", formula="CH4", c=1, h=4)

    def test_lumped_component(self):
        """Test lumped component species."""
        spec = Species(id="TOLUENE", lumped_component_only=True)
        assert spec.lumped_component_only is True
        fds_output = spec.to_fds()
        assert (
            "LUMPED_COMPONENT_ONLY" in fds_output or "lumped_component_only" in fds_output.lower()
        )

    def test_requires_id(self):
        """Test that ID is required."""
        with pytest.raises(ValidationError, match="Field required"):
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

    def test_mass_fraction_cond_0(self):
        """Test mass_fraction_cond_0 parameter."""
        spec = Species(id="WATER VAPOR", mass_fraction_cond_0=0.001)
        assert spec.mass_fraction_cond_0 == 0.001


class TestSpecFDSOutput:
    """Test FDS output generation."""

    def test_minimal_spec(self):
        """Test minimal SPEC output."""
        spec = Species(id="TEST")
        output = spec.to_fds()
        assert output.startswith("&SPEC")
        assert output.strip().endswith("/")
        assert "ID=" in output or "id=" in output.lower()

    def test_formula_spec_output(self):
        """Test formula-based SPEC output."""
        spec = Species(id="MY_FUEL", formula="C3H8", mw=44.0)
        output = spec.to_fds()
        assert "ID=" in output or "id=" in output.lower()
        assert "FORMULA=" in output or "formula=" in output.lower()

    def test_mixture_spec_output(self):
        """Test mixture SPEC output."""
        spec = Species(id="MIX", spec_id=["A", "B"], mass_fraction=[0.6, 0.4])
        output = spec.to_fds()
        assert "SPEC_ID=" in output or "spec_id=" in output.lower()
        assert "MASS_FRACTION=" in output or "mass_fraction=" in output.lower()


class TestSpecPolynomials:
    """Test NASA polynomial functionality."""

    def test_nasa7_polynomials(self):
        """Test NASA7 polynomial specification."""
        spec = Species(
            id="OH",
            polynomial="NASA7",
            polynomial_coeff=[
                [3.99, -0.002, 4.61e-06, -3.88e-09, 1.36e-12, 3615.08, -0.104],
                [3.09, 0.0005, 1.26e-07, -8.79e-11, 1.17e-14, 3858.65, 4.47],
            ],
            polynomial_temp=[200.0, 1000.0, 5000.0],
        )
        assert spec.polynomial == "NASA7"
        assert len(spec.polynomial_coeff) == 2
        assert len(spec.polynomial_coeff[0]) == 7

    def test_nasa9_polynomials(self):
        """Test NASA9 polynomial specification."""
        spec = Species(
            id="NAOH",
            polynomial="NASA9",
            polynomial_coeff=[
                [
                    34420.36,
                    -792.32,
                    8.99,
                    -0.004,
                    3.06e-06,
                    -5.11e-10,
                    -1.54e-13,
                    -20869.51,
                    -25.10,
                ],
                [
                    875378.77,
                    -2342.51,
                    7.97,
                    0.0001,
                    -6.26e-08,
                    1.02e-11,
                    -5.71e-16,
                    -9509.90,
                    -22.02,
                ],
            ],
            polynomial_temp=[200.0, 1000.0, 5000.0],
        )
        assert spec.polynomial == "NASA9"
        assert len(spec.polynomial_coeff) == 2
        assert len(spec.polynomial_coeff[0]) == 9

    def test_polynomial_requires_type(self):
        """Test that polynomial type is required with coefficients."""
        with pytest.raises(ValidationError, match="POLYNOMIAL type must be specified"):
            Species(
                id="BAD",
                polynomial_coeff=[
                    [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0],
                    [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0],
                ],
            )

    def test_polynomial_wrong_coefficient_count(self):
        """Test that wrong coefficient count raises error."""
        with pytest.raises(ValidationError, match="requires 7 coefficients"):
            Species(
                id="BAD",
                polynomial="NASA7",
                polynomial_coeff=[
                    [1.0, 2.0, 3.0, 4.0, 5.0],  # Only 5 coefficients
                    [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0],
                ],
            )

    def test_polynomial_temp_requires_three_values(self):
        """Test that polynomial temp requires 3 values."""
        with pytest.raises(ValidationError, match="requires 3 temperature values"):
            Species(id="BAD", polynomial_temp=[200.0, 1000.0])


class TestSpecToxicity:
    """Test FED/FIC toxicity parameters."""

    def test_fic_concentration(self):
        """Test FIC concentration parameter."""
        spec = Species(id="HCN", fic_concentration=150.0)
        assert spec.fic_concentration == 150.0

    def test_fld_lethal_dose(self):
        """Test FLD lethal dose parameter."""
        spec = Species(id="CO", fld_lethal_dose=35000.0)
        assert spec.fld_lethal_dose == 35000.0


class TestSpecCondensation:
    """Test condensation and refractive index parameters."""

    def test_refractive_indices(self):
        """Test refractive index parameters."""
        spec = Species(
            id="WATER VAPOR",
            real_refractive_index=1.33,
            complex_refractive_index=0.01,
        )
        assert spec.real_refractive_index == 1.33
        assert spec.complex_refractive_index == 0.01


class TestSpecAgglomeration:
    """Test particle agglomeration parameters."""

    def test_agglomeration_parameters(self):
        """Test agglomeration parameter specification."""
        spec = Species(
            id="SOOT",
            aerosol=True,
            density_solid=1800.0,
            min_diameter=1e-9,
            max_diameter=1e-6,
            n_bins=10,
        )
        assert spec.min_diameter == 1e-9
        assert spec.max_diameter == 1e-6
        assert spec.n_bins == 10

    def test_agglomeration_requires_min_max_diameter(self):
        """Test that n_bins requires min and max diameter."""
        with pytest.raises(
            ValidationError, match="MIN_DIAMETER and MAX_DIAMETER must be specified"
        ):
            Species(id="BAD", n_bins=10)

    def test_min_diameter_less_than_max(self):
        """Test that min diameter must be less than max."""
        with pytest.raises(ValidationError, match="MIN_DIAMETER must be less than MAX_DIAMETER"):
            Species(id="BAD", min_diameter=1e-6, max_diameter=1e-9, n_bins=10)


class TestSpecODE:
    """Test ODE solver parameters for detailed chemistry."""

    def test_ode_error_parameters(self):
        """Test ODE error tolerance parameters."""
        spec = Species(
            id="OH",
            ode_abs_error=1e-10,
            ode_rel_error=1e-8,
        )
        assert spec.ode_abs_error == 1e-10
        assert spec.ode_rel_error == 1e-8


class TestSpecLiquid:
    """Test liquid phase parameters."""

    def test_liquid_properties(self):
        """Test liquid phase property specification."""
        spec = Species(
            id="WATER",
            density_liquid=1000.0,
            specific_heat_liquid=4.18,
            conductivity_liquid=0.6,
            viscosity_liquid=1e-3,
            vaporization_temperature=100.0,
            heat_of_vaporization=2257.0,
            h_v_reference_temperature=25.0,
            beta_liquid=2.1e-4,
            melting_temperature=0.0,
        )
        assert spec.density_liquid == 1000.0
        assert spec.specific_heat_liquid == 4.18
        assert spec.conductivity_liquid == 0.6
        assert spec.viscosity_liquid == 1e-3
        assert spec.vaporization_temperature == 100.0
        assert spec.heat_of_vaporization == 2257.0
        assert spec.beta_liquid == 2.1e-4

    def test_ramp_cp_l(self):
        """Test liquid specific heat ramp."""
        spec = Species(id="WATER", ramp_cp_l="CP_L_RAMP")
        assert spec.ramp_cp_l == "CP_L_RAMP"


class TestSpecRamps:
    """Test temperature-dependent property ramps."""

    def test_all_ramps(self):
        """Test all property ramps."""
        spec = Species(
            id="CUSTOM",
            ramp_cp="CP_RAMP",
            ramp_k="K_RAMP",
            ramp_d="D_RAMP",
            ramp_mu="MU_RAMP",
            ramp_g_f="GF_RAMP",
        )
        assert spec.ramp_cp == "CP_RAMP"
        assert spec.ramp_k == "K_RAMP"
        assert spec.ramp_d == "D_RAMP"
        assert spec.ramp_mu == "MU_RAMP"
        assert spec.ramp_g_f == "GF_RAMP"


class TestSpecTransport:
    """Test transport property parameters."""

    def test_transport_properties(self):
        """Test transport property specification."""
        spec = Species(
            id="CUSTOM",
            pr_gas=0.7,
            turbulent_schmidt_number=0.5,
            sigmalj=3.5,
            epsilonklj=150.0,
        )
        assert spec.pr_gas == 0.7
        assert spec.turbulent_schmidt_number == 0.5
        assert spec.sigmalj == 3.5
        assert spec.epsilonklj == 150.0


class TestSpecRadiation:
    """Test radiation property parameters."""

    def test_radcal_id(self):
        """Test RadCal surrogate species."""
        spec = Species(id="ETHANOL", radcal_id="METHANOL")
        assert spec.radcal_id == "METHANOL"

    def test_mass_extinction_coefficient(self):
        """Test mass extinction coefficient."""
        spec = Species(
            id="SOOT", aerosol=True, density_solid=1800.0, mass_extinction_coefficient=8700.0
        )
        assert spec.mass_extinction_coefficient == 8700.0


class TestSpecAerosolExtended:
    """Extended aerosol tests."""

    def test_thermophoretic_diameter(self):
        """Test thermophoretic diameter parameter."""
        spec = Species(
            id="SOOT",
            aerosol=True,
            density_solid=1800.0,
            thermophoretic_diameter=3e-8,
        )
        assert spec.thermophoretic_diameter == 3e-8

    def test_conductivity_solid(self):
        """Test solid conductivity for aerosols."""
        spec = Species(
            id="SOOT",
            aerosol=True,
            density_solid=1800.0,
            conductivity_solid=0.26,
        )
        assert spec.conductivity_solid == 0.26
