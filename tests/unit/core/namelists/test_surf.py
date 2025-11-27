"""Unit tests for SURF namelist functionality."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Surface


class TestSurfHeatTransferParameters:
    """Test heat transfer parameter additions."""

    def test_mlrpua_parameter(self):
        """Test mass flux per unit area parameter."""
        surf = Surface(id="TEST", mlrpua=0.01)
        assert surf.mlrpua == 0.01
        assert "MLRPUA" in surf.to_fds() or "mlrpua" in surf.to_fds().lower()

    def test_mlrpua_negative_validation(self):
        """Test that negative mass flux raises validation error."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", mlrpua=-0.01)

    def test_mass_flux_total_parameter(self):
        """Test total mass flux parameter."""
        surf = Surface(id="TEST", mass_flux_total=0.5)
        assert surf.mass_flux_total == 0.5

    def test_convective_heat_flux_parameter(self):
        """Test convective heat flux parameter."""
        surf = Surface(id="TEST", convective_heat_flux=100.0)
        assert surf.convective_heat_flux == 100.0

    def test_net_heat_flux_parameter(self):
        """Test net heat flux parameter."""
        surf = Surface(id="TEST", net_heat_flux=50.0)
        assert surf.net_heat_flux == 50.0

    def test_external_flux_parameter(self):
        """Test external radiative flux parameter."""
        surf = Surface(id="TEST", external_flux=25.0)
        assert surf.external_flux == 25.0


class TestSurfPyrolysisControl:
    """Test pyrolysis control parameter additions."""

    def test_heat_of_combustion_parameter(self):
        """Test heat of combustion parameter."""
        surf = Surface(id="TEST", heat_of_combustion=15000.0)
        assert surf.heat_of_combustion == 15000.0

    def test_heat_of_combustion_negative_validation(self):
        """Test that negative heat of combustion raises validation error."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", heat_of_combustion=-100.0)

    def test_ignition_temperature_parameter(self):
        """Test ignition temperature parameter."""
        surf = Surface(id="TEST", ignition_temperature=300.0)
        assert surf.ignition_temperature == 300.0

    def test_burn_away_parameter(self):
        """Test burn away boolean parameter."""
        surf = Surface(id="TEST", burn_away=True)
        assert surf.burn_away is True

    def test_burn_away_default_false(self):
        """Test burn away defaults to False."""
        surf = Surface(id="TEST")
        assert surf.burn_away is False

    def test_backing_void(self):
        """Test backing=VOID parameter."""
        surf = Surface(id="TEST", backing="VOID")
        assert surf.backing == "VOID"

    def test_backing_insulated(self):
        """Test backing=INSULATED parameter."""
        surf = Surface(id="TEST", backing="INSULATED")
        assert surf.backing == "INSULATED"

    def test_backing_exposed(self):
        """Test backing=EXPOSED parameter."""
        surf = Surface(id="TEST", backing="EXPOSED")
        assert surf.backing == "EXPOSED"

    def test_backing_case_insensitive(self):
        """Test backing parameter is case-insensitive."""
        surf = Surface(id="TEST", backing="void")
        assert surf.backing == "VOID"

    def test_backing_invalid_value(self):
        """Test invalid backing value raises error."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", backing="INVALID")


class TestSurfRadiationProperties:
    """Test radiation property additions."""

    def test_emissivity_parameter(self):
        """Test emissivity parameter."""
        surf = Surface(id="TEST", emissivity=0.9)
        assert surf.emissivity == 0.9

    def test_emissivity_bounds_validation(self):
        """Test emissivity must be between 0 and 1."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", emissivity=1.5)
        with pytest.raises(ValidationError):
            Surface(id="TEST", emissivity=-0.1)

    def test_absorptivity_parameter(self):
        """Test absorptivity parameter."""
        surf = Surface(id="TEST", absorptivity=0.8)
        assert surf.absorptivity == 0.8

    def test_absorptivity_bounds_validation(self):
        """Test absorptivity must be between 0 and 1."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", absorptivity=1.2)
        with pytest.raises(ValidationError):
            Surface(id="TEST", absorptivity=-0.5)


class TestSurfTimeDependentProperties:
    """Test time-dependent property additions."""

    def test_ramp_q_parameter(self):
        """Test RAMP_Q parameter for heat flux."""
        surf = Surface(id="TEST", hrrpua=500.0, ramp_q="fire_ramp")
        assert surf.ramp_q == "fire_ramp"

    def test_ramp_mf_parameter(self):
        """Test RAMP_MF parameter for mass flux."""
        surf = Surface(id="TEST", mlrpua=0.01, ramp_mf="fuel_ramp")
        assert surf.ramp_mf == "fuel_ramp"

    def test_tau_q_parameter(self):
        """Test TAU_Q parameter."""
        surf = Surface(id="TEST", hrrpua=500.0, tau_q=10.0)
        assert surf.tau_q == 10.0

    def test_tau_q_negative_validation(self):
        """Test that negative tau_q raises validation error."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", tau_q=-5.0)

    def test_tau_mf_parameter(self):
        """Test TAU_MF parameter."""
        surf = Surface(id="TEST", mlrpua=0.01, tau_mf=15.0)
        assert surf.tau_mf == 15.0

    def test_tau_mf_negative_validation(self):
        """Test that negative tau_mf raises validation error."""
        with pytest.raises(ValidationError):
            Surface(id="TEST", tau_mf=-10.0)


class TestSurfMutuallyExclusiveHeatSources:
    """Test validation of mutually exclusive heat source specifications."""

    def test_single_heat_source_hrrpua_valid(self):
        """Test that single heat source (HRRPUA) is valid."""
        surf = Surface(id="TEST", hrrpua=500.0)
        assert surf.hrrpua == 500.0

    def test_single_heat_source_mlrpua_valid(self):
        """Test that single heat source (MLRPUA) is valid."""
        surf = Surface(id="TEST", mlrpua=0.01)
        assert surf.mlrpua == 0.01

    def test_hrrpua_and_mlrpua_invalid(self):
        """Test that HRRPUA and MLRPUA together raise validation error."""
        with pytest.raises(ValidationError, match="Only one heat source"):
            Surface(id="TEST", hrrpua=500.0, mlrpua=0.01)

    def test_hrrpua_and_mass_flux_total_invalid(self):
        """Test that HRRPUA and MASS_FLUX_TOTAL together raise validation error."""
        with pytest.raises(ValidationError, match="Only one heat source"):
            Surface(id="TEST", hrrpua=500.0, mass_flux_total=0.5)

    def test_hrrpua_and_convective_heat_flux_invalid(self):
        """Test that HRRPUA and CONVECTIVE_HEAT_FLUX together raise validation error."""
        with pytest.raises(ValidationError, match="Only one heat source"):
            Surface(id="TEST", hrrpua=500.0, convective_heat_flux=100.0)

    def test_mlrpua_and_net_heat_flux_invalid(self):
        """Test that MLRPUA and NET_HEAT_FLUX together raise validation error."""
        with pytest.raises(ValidationError, match="Only one heat source"):
            Surface(id="TEST", mlrpua=0.01, net_heat_flux=50.0)

    def test_three_heat_sources_invalid(self):
        """Test that three heat sources together raise validation error."""
        with pytest.raises(ValidationError, match="Only one heat source"):
            Surface(id="TEST", hrrpua=500.0, mlrpua=0.01, convective_heat_flux=100.0)


class TestSurfFireSpread:
    """Test fire spread parameters."""

    def test_spread_rate_parameter(self):
        """Test SPREAD_RATE parameter."""
        surf = Surface(id="FIRE", hrrpua=500.0, spread_rate=0.03)
        assert surf.spread_rate == 0.03
        output = surf.to_fds()
        assert "SPREAD_RATE" in output or "spread_rate" in output.lower()

    def test_xyz_parameter(self):
        """Test XYZ ignition point parameter."""
        surf = Surface(id="FIRE", hrrpua=500.0, xyz=(1.5, 2.0, 0.0))
        assert surf.xyz == (1.5, 2.0, 0.0)
        output = surf.to_fds()
        assert "XYZ=" in output or "xyz=" in output.lower()

    def test_spread_rate_requires_heat_source(self):
        """Test that SPREAD_RATE requires HRRPUA or MLRPUA."""
        with pytest.raises(ValidationError, match="SPREAD_RATE requires"):
            Surface(id="BAD", spread_rate=0.03)

    def test_area_multiplier(self):
        """Test AREA_MULTIPLIER parameter."""
        surf = Surface(id="TEST", hrrpua=1000.0, area_multiplier=1.5)
        assert surf.area_multiplier == 1.5
        output = surf.to_fds()
        assert "AREA_MULTIPLIER" in output or "area_multiplier" in output.lower()

    def test_area_multiplier_default(self):
        """Test AREA_MULTIPLIER defaults to 1.0."""
        surf = Surface(id="TEST", hrrpua=1000.0)
        assert surf.area_multiplier == 1.0
        # Should not appear in output when default
        output = surf.to_fds()
        assert "AREA_MULTIPLIER" not in output


class TestSurfSpeciesControl:
    """Test species control parameters."""

    def test_single_spec_id(self):
        """Test single SPEC_ID."""
        surf = Surface(id="FIRE", hrrpua=1000.0, spec_id="PROPANE")
        assert surf.spec_id == "PROPANE"
        output = surf.to_fds()
        assert "SPEC_ID=" in output or "spec_id=" in output.lower()

    def test_multiple_spec_id_with_mass_fraction(self):
        """Test multiple SPEC_ID with MASS_FRACTION."""
        surf = Surface(
            id="FIRE", hrrpua=1000.0, spec_id=["PROPANE", "ETHANE"], mass_fraction=[0.7, 0.3]
        )
        assert len(surf.spec_id) == 2
        assert len(surf.mass_fraction) == 2
        assert sum(surf.mass_fraction) == pytest.approx(1.0)

    def test_mass_fraction_requires_spec_id(self):
        """Test that MASS_FRACTION requires SPEC_ID."""
        with pytest.raises(ValidationError, match="MASS_FRACTION requires SPEC_ID"):
            Surface(id="BAD", hrrpua=1000.0, mass_fraction=[0.5, 0.5])

    def test_mass_fraction_requires_list_spec_id(self):
        """Test that MASS_FRACTION requires SPEC_ID to be a list."""
        with pytest.raises(ValidationError, match="SPEC_ID to be a list"):
            Surface(id="BAD", hrrpua=1000.0, spec_id="PROPANE", mass_fraction=[1.0])

    def test_mass_fraction_length_match(self):
        """Test that SPEC_ID and MASS_FRACTION lengths must match."""
        with pytest.raises(ValidationError, match="must have same length"):
            Surface(id="BAD", hrrpua=1000.0, spec_id=["A", "B"], mass_fraction=[0.5, 0.3, 0.2])

    def test_mass_fraction_sum_validation(self):
        """Test that MASS_FRACTION values must sum to 1.0."""
        with pytest.raises(ValidationError, match=r"must sum to 1.0"):
            Surface(id="BAD", hrrpua=1000.0, spec_id=["A", "B"], mass_fraction=[0.5, 0.3])


class TestSurfThermallyThickBurning:
    """Test thermally-thick specified burning parameters."""

    def test_heat_of_vaporization(self):
        """Test HEAT_OF_VAPORIZATION parameter."""
        surf = Surface(
            id="SOLID", hrrpua=1000.0, ignition_temperature=500.0, heat_of_vaporization=1000.0
        )
        assert surf.heat_of_vaporization == 1000.0
        output = surf.to_fds()
        assert "HEAT_OF_VAPORIZATION" in output or "heat_of_vaporization" in output.lower()

    def test_extinction_temperature(self):
        """Test EXTINCTION_TEMPERATURE parameter."""
        surf = Surface(
            id="SOLID", hrrpua=1000.0, ignition_temperature=500.0, extinction_temperature=450.0
        )
        assert surf.extinction_temperature == 450.0

    def test_extinction_below_ignition(self):
        """Test that EXTINCTION_TEMPERATURE must be <= IGNITION_TEMPERATURE."""
        with pytest.raises(ValidationError, match="must be <="):
            Surface(
                id="BAD", hrrpua=1000.0, ignition_temperature=400.0, extinction_temperature=500.0
            )

    def test_burn_duration(self):
        """Test BURN_DURATION parameter."""
        surf = Surface(id="FIRE", hrrpua=1000.0, burn_duration=300.0)
        assert surf.burn_duration == 300.0
        output = surf.to_fds()
        assert "BURN_DURATION" in output or "burn_duration" in output.lower()


class TestSurfSPyroModel:
    """Test SPyro (Scaling Pyrolysis) model parameters."""

    def test_inert_q_ref(self):
        """Test INERT_Q_REF flag."""
        surf = Surface(id="TEST", hrrpua=1.0, inert_q_ref=True)
        assert surf.inert_q_ref is True
        output = surf.to_fds()
        assert "INERT_Q_REF" in output or "inert_q_ref" in output.lower()

    def test_reference_heat_flux_single(self):
        """Test single REFERENCE_HEAT_FLUX."""
        surf = Surface(
            id="TEST",
            hrrpua=1.0,
            ignition_temperature=300.0,
            ramp_q="test_ramp",
            reference_heat_flux=50.0,
        )
        assert surf.reference_heat_flux == 50.0

    def test_reference_heat_flux_array(self):
        """Test array of REFERENCE_HEAT_FLUX."""
        surf = Surface(
            id="TEST",
            hrrpua=1.0,
            ignition_temperature=300.0,
            ramp_q="test_ramp",
            reference_heat_flux=[25.0, 50.0, 75.0],
        )
        assert len(surf.reference_heat_flux) == 3

    def test_reference_thickness_single(self):
        """Test single REFERENCE_THICKNESS."""
        surf = Surface(id="TEST", hrrpua=1.0, reference_thickness=0.01)
        assert surf.reference_thickness == 0.01

    def test_reference_thickness_array(self):
        """Test array of REFERENCE_THICKNESS."""
        surf = Surface(
            id="TEST",
            hrrpua=1.0,
            ignition_temperature=300.0,
            ramp_q="test_data",
            reference_heat_flux=[25.0, 50.0],
            reference_thickness=[0.01, 0.02],
        )
        assert len(surf.reference_thickness) == 2

    def test_reference_arrays_length_match(self):
        """Test that REFERENCE_HEAT_FLUX and REFERENCE_THICKNESS arrays must match."""
        with pytest.raises(ValidationError, match="must have same length"):
            Surface(
                id="BAD",
                hrrpua=1.0,
                ignition_temperature=300.0,
                ramp_q="test_data",
                reference_heat_flux=[25.0, 50.0, 75.0],
                reference_thickness=[0.01, 0.02],
            )

    def test_maximum_scaling_heat_flux(self):
        """Test MAXIMUM_SCALING_HEAT_FLUX parameter."""
        surf = Surface(id="TEST", hrrpua=1.0, maximum_scaling_heat_flux=2000.0)
        assert surf.maximum_scaling_heat_flux == 2000.0
        output = surf.to_fds()
        assert (
            "MAXIMUM_SCALING_HEAT_FLUX" in output or "maximum_scaling_heat_flux" in output.lower()
        )

    def test_minimum_scaling_heat_flux(self):
        """Test MINIMUM_SCALING_HEAT_FLUX parameter."""
        surf = Surface(id="TEST", hrrpua=1.0, minimum_scaling_heat_flux=10.0)
        assert surf.minimum_scaling_heat_flux == 10.0

    def test_reference_heat_flux_time_interval(self):
        """Test REFERENCE_HEAT_FLUX_TIME_INTERVAL parameter."""
        surf = Surface(id="TEST", hrrpua=1.0, reference_heat_flux_time_interval=2.0)
        assert surf.reference_heat_flux_time_interval == 2.0


class TestSurfLayerDivide:
    """Test solid phase gas transport parameters."""

    def test_layer_divide(self):
        """Test LAYER_DIVIDE parameter."""
        surf = Surface(id="TEST", matl_id="WOOD", thickness=0.01, layer_divide=1.5)
        assert surf.layer_divide == 1.5
        output = surf.to_fds()
        assert "LAYER_DIVIDE" in output or "layer_divide" in output.lower()


class TestSurfTGAAnalysis:
    """Test TGA analysis parameters."""

    def test_tga_analysis_flag(self):
        """Test TGA_ANALYSIS flag."""
        surf = Surface(id="TEST", matl_id="WOOD", thickness=0.01, tga_analysis=True)
        assert surf.tga_analysis is True
        output = surf.to_fds()
        assert "TGA_ANALYSIS" in output or "tga_analysis" in output.lower()

    def test_tga_analysis_requires_matl(self):
        """Test that TGA_ANALYSIS requires MATL_ID."""
        with pytest.raises(ValidationError, match="TGA_ANALYSIS requires MATL_ID"):
            Surface(id="BAD", tga_analysis=True)

    def test_tga_heating_rate(self):
        """Test TGA_HEATING_RATE parameter."""
        surf = Surface(
            id="TEST", matl_id="WOOD", thickness=0.01, tga_analysis=True, tga_heating_rate=10.0
        )
        assert surf.tga_heating_rate == 10.0

    def test_tga_heating_rate_default(self):
        """Test TGA_HEATING_RATE defaults to 5.0."""
        surf = Surface(id="TEST", matl_id="WOOD", thickness=0.01, tga_analysis=True)
        assert surf.tga_heating_rate == 5.0

    def test_tga_final_temperature(self):
        """Test TGA_FINAL_TEMPERATURE parameter."""
        surf = Surface(
            id="TEST",
            matl_id="WOOD",
            thickness=0.01,
            tga_analysis=True,
            tga_final_temperature=1000.0,
        )
        assert surf.tga_final_temperature == 1000.0

    def test_tga_conversion_factors(self):
        """Test TGA/MCC/DSC conversion factors."""
        surf = Surface(
            id="TEST",
            matl_id="WOOD",
            thickness=0.01,
            tga_conversion_factor=2.0,
            mcc_conversion_factor=-1.0,
            dsc_conversion_factor=0.5,
        )
        assert surf.tga_conversion_factor == 2.0
        assert surf.mcc_conversion_factor == -1.0
        assert surf.dsc_conversion_factor == 0.5


class TestSurfLiquidEvaporation:
    """Test liquid evaporation parameters."""

    def test_mass_transfer_coefficient(self):
        """Test MASS_TRANSFER_COEFFICIENT parameter."""
        surf = Surface(id="POOL", matl_id="ETHANOL", mass_transfer_coefficient=0.01)
        assert surf.mass_transfer_coefficient == 0.01
        output = surf.to_fds()
        assert (
            "MASS_TRANSFER_COEFFICIENT" in output or "mass_transfer_coefficient" in output.lower()
        )


class TestSurfFDSOutput:
    """Test FDS output generation for new parameters."""

    def test_fds_output_with_mlrpua(self):
        """Test FDS output includes MLRPUA."""
        surf = Surface(id="BURNER", mlrpua=0.01)
        fds_output = surf.to_fds()
        assert "MLRPUA" in fds_output or "mlrpua" in fds_output.lower()
        assert "0.01" in fds_output

    def test_fds_output_with_ignition(self):
        """Test FDS output includes ignition parameters."""
        surf = Surface(id="WOOD", ignition_temperature=300.0, burn_away=True)
        fds_output = surf.to_fds()
        assert "300" in fds_output or "300.0" in fds_output
        assert "BURN_AWAY" in fds_output or "burn_away" in fds_output.lower()

    def test_fds_output_with_radiation(self):
        """Test FDS output includes radiation properties."""
        surf = Surface(id="STEEL", emissivity=0.7, absorptivity=0.6)
        fds_output = surf.to_fds()
        assert "0.7" in fds_output
        assert "0.6" in fds_output

    def test_fds_output_with_ramp(self):
        """Test FDS output includes ramp references."""
        surf = Surface(id="FIRE", hrrpua=1000.0, ramp_q="t2_fast")
        fds_output = surf.to_fds()
        assert "t2_fast" in fds_output
        assert "RAMP" in fds_output or "ramp" in fds_output.lower()

    def test_fds_output_comprehensive(self):
        """Test FDS output with multiple new parameters."""
        surf = Surface(
            id="COMPLEX",
            hrrpua=500.0,
            ramp_q="fire_ramp",
            emissivity=0.9,
            ignition_temperature=250.0,
            backing="INSULATED",
        )
        fds_output = surf.to_fds()
        assert "COMPLEX" in fds_output
        assert "500" in fds_output or "500.0" in fds_output
        assert "fire_ramp" in fds_output
        assert "0.9" in fds_output
        assert "250" in fds_output or "250.0" in fds_output
        assert "INSULATED" in fds_output

    def test_fire_spread_output(self):
        """Test FDS output with fire spread parameters."""
        surf = Surface(
            id="SPREADING_FIRE",
            hrrpua=500.0,
            ramp_q="fire_ramp",
            spread_rate=0.03,
            xyz=(1.5, 4.0, 0.0),
        )
        output = surf.to_fds()
        assert "HRRPUA" in output or "hrrpua" in output.lower()
        assert "SPREAD_RATE" in output or "spread_rate" in output.lower()
        assert "XYZ=" in output or "xyz=" in output.lower()

    def test_spyro_model_output(self):
        """Test FDS output with SPyro model parameters."""
        surf = Surface(
            id="SPYRO_TEST",
            hrrpua=1.0,
            ignition_temperature=300.0,
            ramp_q="cone_data",
            reference_heat_flux=[25.0, 50.0, 75.0],
            reference_thickness=[0.01, 0.01, 0.01],
        )
        output = surf.to_fds()
        assert "REFERENCE_HEAT_FLUX" in output or "reference_heat_flux" in output.lower()
        assert "REFERENCE_THICKNESS" in output or "reference_thickness" in output.lower()


class TestSurfBackwardCompatibility:
    """Test backward compatibility with existing code."""

    def test_simple_fire_surface_still_works(self):
        """Test that existing simple fire surfaces still work."""
        surf = Surface(id="FIRE", hrrpua=1000.0, color="RED")
        assert surf.id == "FIRE"
        assert surf.hrrpua == 1000.0
        assert surf.color == "RED"

    def test_material_surface_still_works(self):
        """Test that existing material surfaces still work."""
        surf = Surface(id="WALL", matl_id="CONCRETE", thickness=0.2)
        assert surf.matl_id == "CONCRETE"
        assert surf.thickness == 0.2

    def test_all_new_parameters_optional(self):
        """Test that all new parameters are optional."""
        surf = Surface(id="SIMPLE")
        assert surf.mlrpua is None
        assert surf.mass_flux_total is None
        assert surf.convective_heat_flux is None
        assert surf.net_heat_flux is None
        assert surf.external_flux is None
        assert surf.heat_of_combustion is None
        assert surf.ignition_temperature is None
        assert surf.burn_away is False  # Default value
        assert surf.backing is None
        assert surf.emissivity is None
        assert surf.absorptivity is None
        assert surf.ramp_q is None
        assert surf.ramp_mf is None
        assert surf.tau_q is None
        assert surf.tau_mf is None
        assert surf.spread_rate is None
        assert surf.xyz is None
        assert surf.area_multiplier == 1.0  # Default value
        assert surf.spec_id is None
        assert surf.mass_fraction is None
        assert surf.heat_of_vaporization is None
        assert surf.extinction_temperature is None
        assert surf.burn_duration is None
        assert surf.inert_q_ref is False  # Default value
        assert surf.reference_heat_flux is None
        assert surf.reference_thickness is None
        assert surf.maximum_scaling_heat_flux == 1500.0  # Default value
        assert surf.minimum_scaling_heat_flux == 0.0  # Default value
        assert surf.reference_heat_flux_time_interval == 1.0  # Default value
        assert surf.layer_divide is None
        assert surf.tga_analysis is False  # Default value
        assert surf.tga_heating_rate == 5.0  # Default value
        assert surf.tga_final_temperature == 800.0  # Default value
        assert surf.tga_conversion_factor == 1.0  # Default value
        assert surf.mcc_conversion_factor == 1.0  # Default value
        assert surf.dsc_conversion_factor == 1.0  # Default value
        assert surf.mass_transfer_coefficient is None
