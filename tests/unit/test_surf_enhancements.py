"""Unit tests for SURF namelist Stage 1.1 enhancements."""

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
        assert surf.heat_of_combustion is None
        assert surf.ignition_temperature is None
        assert surf.burn_away is False
        assert surf.backing is None
        assert surf.emissivity is None
        assert surf.absorptivity is None
        assert surf.ramp_q is None
        assert surf.ramp_mf is None
        assert surf.tau_q is None
        assert surf.tau_mf is None
