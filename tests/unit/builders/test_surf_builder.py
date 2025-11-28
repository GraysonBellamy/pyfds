"""Unit tests for SurfBuilder."""

import pytest

from pyfds.builders import SurfBuilder
from pyfds.core.namelists import Surface


class TestSurfBuilderBasicUsage:
    """Test basic SurfBuilder usage."""

    def test_simple_build(self):
        """Test building a simple surface."""
        surf = SurfBuilder("TEST").build()
        assert isinstance(surf, Surface)
        assert surf.id == "TEST"

    def test_builder_cannot_be_reused(self):
        """Test that builder cannot be used twice."""
        builder = SurfBuilder("TEST")
        builder.build()
        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()


class TestSurfBuilderColorMethods:
    """Test color setting methods."""

    def test_with_color(self):
        """Test with_color method."""
        surf = SurfBuilder("FIRE").with_color("RED").build()
        assert surf.color == "RED"

    def test_with_rgb(self):
        """Test with_rgb method."""
        surf = SurfBuilder("CUSTOM").with_rgb(255, 128, 0).build()
        assert surf.rgb == (255, 128, 0)


class TestSurfBuilderHeatReleaseMethods:
    """Test heat release rate methods."""

    def test_with_heat_release(self):
        """Test with_heat_release method."""
        surf = SurfBuilder("FIRE").with_heat_release(500.0).build()
        assert surf.hrrpua == 500.0
        assert surf.ramp_q is None

    def test_with_heat_release_and_ramp(self):
        """Test with_heat_release with ramp."""
        surf = SurfBuilder("FIRE").with_heat_release(1000.0, ramp_id="fire_ramp").build()
        assert surf.hrrpua == 1000.0
        assert surf.ramp_q == "fire_ramp"


class TestSurfBuilderMassFluxMethods:
    """Test mass flux methods."""

    def test_with_mass_flux(self):
        """Test with_mass_flux method."""
        surf = SurfBuilder("BURNER").with_mass_flux(0.01).build()
        assert surf.mlrpua == 0.01
        assert surf.ramp_mf is None

    def test_with_mass_flux_and_ramp(self):
        """Test with_mass_flux with ramp."""
        surf = SurfBuilder("BURNER").with_mass_flux(0.02, ramp_id="fuel_ramp").build()
        assert surf.mlrpua == 0.02
        assert surf.ramp_mf == "fuel_ramp"


class TestSurfBuilderTemperatureMethods:
    """Test temperature methods."""

    def test_with_temperature(self):
        """Test with_temperature method."""
        surf = SurfBuilder("HOT_WALL").with_temperature(200.0).build()
        assert surf.tmp_front == 200.0


class TestSurfBuilderMaterialMethods:
    """Test material methods."""

    def test_with_material(self):
        """Test with_material method."""
        surf = SurfBuilder("WALL").with_material("CONCRETE", 0.2).build()
        assert surf.matl_id == "CONCRETE"
        assert surf.thickness == 0.2


class TestSurfBuilderIgnitionMethods:
    """Test ignition methods."""

    def test_with_ignition_no_burn_away(self):
        """Test with_ignition method without burn_away."""
        surf = SurfBuilder("WOOD").with_ignition(300.0).build()
        assert surf.ignition_temperature == 300.0
        assert surf.burn_away is False

    def test_with_ignition_with_burn_away(self):
        """Test with_ignition method with burn_away."""
        surf = SurfBuilder("PAPER").with_ignition(250.0, burn_away=True).build()
        assert surf.ignition_temperature == 250.0
        assert surf.burn_away is True


class TestSurfBuilderRadiationMethods:
    """Test radiation methods."""

    def test_with_radiation_emissivity_only(self):
        """Test with_radiation with emissivity only."""
        surf = SurfBuilder("STEEL").with_radiation(emissivity=0.7).build()
        assert surf.emissivity == 0.7
        assert surf.absorptivity is None

    def test_with_radiation_both_properties(self):
        """Test with_radiation with both emissivity and absorptivity."""
        surf = SurfBuilder("BLACK").with_radiation(emissivity=0.9, absorptivity=0.95).build()
        assert surf.emissivity == 0.9
        assert surf.absorptivity == 0.95


class TestSurfBuilderBackingMethods:
    """Test backing methods."""

    def test_with_backing(self):
        """Test with_backing method."""
        surf = SurfBuilder("WALL").with_backing("INSULATED").build()
        assert surf.backing == "INSULATED"


class TestSurfBuilderHeatOfCombustionMethods:
    """Test heat of combustion methods."""

    def test_with_heat_of_combustion(self):
        """Test with_heat_of_combustion method."""
        surf = SurfBuilder("FUEL").with_heat_of_combustion(15000.0).build()
        assert surf.heat_of_combustion == 15000.0


class TestSurfBuilderConvectiveHeatFluxMethods:
    """Test convective heat flux methods."""

    def test_with_convective_heat_flux(self):
        """Test with_convective_heat_flux method."""
        surf = SurfBuilder("HEATER").with_convective_heat_flux(100.0).build()
        assert surf.convective_heat_flux == 100.0


class TestSurfBuilderVelocityMethods:
    """Test velocity methods."""

    def test_with_velocity(self):
        """Test with_velocity method."""
        surf = SurfBuilder("VENT").with_velocity(2.0).build()
        assert surf.vel == 2.0


class TestSurfBuilderVolumeFlowMethods:
    """Test volume flow methods."""

    def test_with_volume_flow(self):
        """Test with_volume_flow method."""
        surf = SurfBuilder("SUPPLY").with_volume_flow(0.5).build()
        assert surf.volume_flow == 0.5


class TestSurfBuilderMethodChaining:
    """Test method chaining capabilities."""

    def test_chain_multiple_methods(self):
        """Test chaining multiple methods together."""
        surf = (
            SurfBuilder("COMPLEX")
            .with_heat_release(500.0, ramp_id="fire_ramp")
            .with_color("ORANGE")
            .with_radiation(emissivity=0.85)
            .with_ignition(280.0, burn_away=True)
            .build()
        )
        assert surf.id == "COMPLEX"
        assert surf.hrrpua == 500.0
        assert surf.ramp_q == "fire_ramp"
        assert surf.color == "ORANGE"
        assert surf.emissivity == 0.85
        assert surf.ignition_temperature == 280.0
        assert surf.burn_away is True

    def test_material_and_radiation_chain(self):
        """Test chaining material and radiation properties."""
        surf = (
            SurfBuilder("WALL")
            .with_material("CONCRETE", 0.15)
            .with_radiation(emissivity=0.9)
            .with_backing("INSULATED")
            .build()
        )
        assert surf.matl_id == "CONCRETE"
        assert surf.thickness == 0.15
        assert surf.emissivity == 0.9
        assert surf.backing == "INSULATED"


class TestSurfBuilderRealWorldScenarios:
    """Test real-world fire simulation scenarios."""

    def test_simple_fire_surface(self):
        """Test building a simple fire surface."""
        surf = SurfBuilder("FIRE").with_heat_release(1000.0).with_color("RED").build()
        assert surf.hrrpua == 1000.0
        assert surf.color == "RED"

    def test_growing_fire_with_ramp(self):
        """Test building a growing fire with ramp."""
        surf = (
            SurfBuilder("T2_FIRE")
            .with_heat_release(2500.0, ramp_id="t2_fast")
            .with_radiation(emissivity=0.9)
            .with_color("ORANGE")
            .build()
        )
        assert surf.hrrpua == 2500.0
        assert surf.ramp_q == "t2_fast"
        assert surf.emissivity == 0.9

    def test_burner_with_mass_flux(self):
        """Test building a burner with mass flux."""
        surf = (
            SurfBuilder("BURNER")
            .with_mass_flux(0.01, ramp_id="fuel_ramp")
            .with_heat_of_combustion(12000.0)
            .build()
        )
        assert surf.mlrpua == 0.01
        assert surf.ramp_mf == "fuel_ramp"
        assert surf.heat_of_combustion == 12000.0

    def test_combustible_material_with_ignition(self):
        """Test building a combustible material surface."""
        surf = (
            SurfBuilder("WOOD_WALL")
            .with_material("PINE", 0.02)
            .with_ignition(300.0, burn_away=True)
            .with_radiation(emissivity=0.9)
            .with_backing("EXPOSED")
            .build()
        )
        assert surf.matl_id == "PINE"
        assert surf.thickness == 0.02
        assert surf.ignition_temperature == 300.0
        assert surf.burn_away is True
        assert surf.emissivity == 0.9
        assert surf.backing == "EXPOSED"

    def test_insulated_wall(self):
        """Test building an insulated wall surface."""
        surf = (
            SurfBuilder("INSULATED_WALL")
            .with_material("INSULATION", 0.1)
            .with_backing("INSULATED")
            .with_radiation(emissivity=0.8)
            .build()
        )
        assert surf.matl_id == "INSULATION"
        assert surf.backing == "INSULATED"

    def test_hvac_supply_vent(self):
        """Test building an HVAC supply vent surface."""
        surf = SurfBuilder("SUPPLY").with_volume_flow(0.5).with_velocity(3.0).build()
        assert surf.volume_flow == 0.5
        assert surf.vel == 3.0


class TestSurfBuilderParticleGeneration:
    """Test particle generation methods (Stage 2.2)."""

    def test_with_particle_generation_basic(self):
        """Test basic particle generation."""
        surf = (
            SurfBuilder("SPRINKLER").with_particle_generation("WATER_DROP", mass_flux=0.01).build()
        )
        assert surf.part_id == "WATER_DROP"
        assert surf.particle_mass_flux == 0.01
        assert surf.nppc == 1

    def test_with_particle_generation_with_nppc(self):
        """Test particle generation with custom nppc."""
        surf = (
            SurfBuilder("SPRAY")
            .with_particle_generation("WATER_DROP", mass_flux=0.02, nppc=5)
            .build()
        )
        assert surf.part_id == "WATER_DROP"
        assert surf.particle_mass_flux == 0.02
        assert surf.nppc == 5

    def test_with_droplet_distribution_full(self):
        """Test droplet distribution with all parameters."""
        surf = (
            SurfBuilder("NOZZLE")
            .with_droplet_distribution(median_diameter=0.001, gamma_d=2.4, spray_pattern="GAUSSIAN")
            .build()
        )
        assert surf.median_diameter == 0.001
        assert surf.gamma_d == 2.4
        assert surf.spray_pattern == "GAUSSIAN"

    def test_with_droplet_distribution_minimal(self):
        """Test droplet distribution with only median diameter."""
        surf = SurfBuilder("SIMPLE").with_droplet_distribution(median_diameter=0.0005).build()
        assert surf.median_diameter == 0.0005
        assert surf.gamma_d is None
        assert surf.spray_pattern is None

    def test_with_particle_velocity_magnitude(self):
        """Test particle velocity with magnitude."""
        surf = SurfBuilder("SPRAY").with_particle_velocity(5.0).build()
        assert surf.vel_part == 5.0
        assert surf.particle_velocity is None

    def test_with_particle_velocity_vector(self):
        """Test particle velocity with vector."""
        surf = SurfBuilder("SPRAY").with_particle_velocity((1.0, 0.0, -2.0)).build()
        assert surf.particle_velocity == (1.0, 0.0, -2.0)
        assert surf.vel_part is None

    def test_as_sprinkler(self):
        """Test as_sprinkler convenience method."""
        surf = (
            SurfBuilder("SPRINKLER_HEAD")
            .as_sprinkler(
                part_id="WATER_DROP",
                mass_flux=0.01,
                median_diameter=0.001,
                velocity=5.0,
            )
            .build()
        )
        assert surf.part_id == "WATER_DROP"
        assert surf.particle_mass_flux == 0.01
        assert surf.median_diameter == 0.001
        assert surf.vel_part == 5.0


class TestSurfBuilderParticleChaining:
    """Test chaining particle generation methods."""

    def test_full_particle_chain(self):
        """Test full chain of particle methods."""
        surf = (
            SurfBuilder("SPRINKLER")
            .with_particle_generation("WATER_DROP", mass_flux=0.015, nppc=3)
            .with_droplet_distribution(median_diameter=0.001, gamma_d=2.4, spray_pattern="GAUSSIAN")
            .with_particle_velocity(6.0)
            .with_color("BLUE")
            .build()
        )
        assert surf.part_id == "WATER_DROP"
        assert surf.particle_mass_flux == 0.015
        assert surf.nppc == 3
        assert surf.median_diameter == 0.001
        assert surf.gamma_d == 2.4
        assert surf.spray_pattern == "GAUSSIAN"
        assert surf.vel_part == 6.0
        assert surf.color == "BLUE"

    def test_sprinkler_with_additional_config(self):
        """Test as_sprinkler with additional configuration."""
        surf = (
            SurfBuilder("ESFR_SPRINKLER")
            .as_sprinkler(
                part_id="WATER_DROP",
                mass_flux=0.02,
                median_diameter=0.0008,
                velocity=8.0,
            )
            .with_color("CYAN")
            .build()
        )
        assert surf.part_id == "WATER_DROP"
        assert surf.particle_mass_flux == 0.02
        assert surf.median_diameter == 0.0008
        assert surf.vel_part == 8.0
        assert surf.color == "CYAN"

    def test_sprinkler_with_override_diameter(self):
        """Test as_sprinkler with override of diameter."""
        surf = (
            SurfBuilder("CUSTOM_SPRINKLER")
            .as_sprinkler(
                part_id="WATER_DROP",
                mass_flux=0.02,
                median_diameter=0.0008,
                velocity=8.0,
            )
            .with_droplet_distribution(
                median_diameter=0.0012, gamma_d=2.5, spray_pattern="GAUSSIAN"
            )
            .build()
        )
        # The second call should override median_diameter
        assert surf.median_diameter == 0.0012
        assert surf.gamma_d == 2.5
        assert surf.spray_pattern == "GAUSSIAN"


class TestSurfBuilderFDSOutputIntegration:
    """Test that builder output generates valid FDS."""

    def test_builder_generates_valid_fds(self):
        """Test that SurfBuilder generates valid FDS output."""
        surf = (
            SurfBuilder("FIRE")
            .with_heat_release(500.0, ramp_id="fire_ramp")
            .with_color("RED")
            .build()
        )
        fds_output = surf.to_fds()
        assert "&SURF" in fds_output
        assert "ID='FIRE'" in fds_output or 'ID="FIRE"' in fds_output
        assert "500" in fds_output or "500.0" in fds_output
        assert "fire_ramp" in fds_output
        assert "/" in fds_output  # FDS namelist terminator

    def test_complex_surface_fds_output(self):
        """Test complex surface generates valid FDS."""
        surf = (
            SurfBuilder("COMPLEX")
            .with_material("WOOD", 0.02)
            .with_ignition(300.0, burn_away=True)
            .with_radiation(emissivity=0.85, absorptivity=0.8)
            .with_backing("EXPOSED")
            .build()
        )
        fds_output = surf.to_fds()
        assert "&SURF" in fds_output
        assert "COMPLEX" in fds_output
        assert "WOOD" in fds_output
        assert "0.02" in fds_output
        assert "300" in fds_output or "300.0" in fds_output
        assert "0.85" in fds_output
        assert "0.8" in fds_output
        assert "EXPOSED" in fds_output

    def test_particle_generation_fds_output(self):
        """Test particle generation FDS output."""
        surf = (
            SurfBuilder("SPRINKLER")
            .as_sprinkler(
                part_id="WATER_DROP",
                mass_flux=0.01,
                median_diameter=0.001,
                velocity=5.0,
            )
            .build()
        )
        fds_output = surf.to_fds()
        assert "&SURF" in fds_output
        assert "SPRINKLER" in fds_output
        assert "WATER_DROP" in fds_output
        assert "0.01" in fds_output
        assert "0.001" in fds_output
        assert "5" in fds_output or "5.0" in fds_output


class TestSurfBuilderPhase3ThermalEnhancements:
    """Test Phase 3 SURF thermal enhancements."""

    def test_with_initial_temperature(self):
        """Test temperature boundary conditions."""
        surf = (
            SurfBuilder("THERMAL")
            .with_initial_temperature(
                tmp_front_initial=300.0, tmp_inner=400.0, tmp_back=500.0, tmp_gas_back=350.0
            )
            .build()
        )
        assert surf.tmp_front_initial == 300.0
        assert surf.tmp_inner == 400.0
        assert surf.tmp_back == 500.0
        assert surf.tmp_gas_back == 350.0

    def test_with_temperature_ramps(self):
        """Test temperature ramps."""
        surf = (
            SurfBuilder("RAMPED")
            .with_temperature_ramps(
                ramp_t="TEMP_RAMP",
                ramp_tmp_back="BACK_RAMP",
                ramp_tmp_gas_front="FRONT_RAMP",
                ramp_tmp_gas_back="GAS_BACK_RAMP",
                ramp_t_i="INIT_RAMP",
            )
            .build()
        )
        assert surf.ramp_t == "TEMP_RAMP"
        assert surf.ramp_tmp_back == "BACK_RAMP"
        assert surf.ramp_tmp_gas_front == "FRONT_RAMP"
        assert surf.ramp_tmp_gas_back == "GAS_BACK_RAMP"
        assert surf.ramp_t_i == "INIT_RAMP"

    def test_with_adiabatic(self):
        """Test adiabatic surface."""
        surf = SurfBuilder("ADIABATIC").with_adiabatic().build()
        assert surf.adiabatic is True

    def test_with_heat_transfer_coefficient_back(self):
        """Test back side heat transfer coefficient."""
        surf = SurfBuilder("HTC_BACK").with_heat_transfer_coefficient_back(25.0, "HTC_RAMP").build()
        assert surf.heat_transfer_coefficient_back == 25.0
        assert surf.ramp_heat_transfer_coefficient_back == "HTC_RAMP"

    def test_with_heat_transfer_model(self):
        """Test heat transfer model configuration."""
        surf = (
            SurfBuilder("HT_MODEL")
            .with_heat_transfer_model(
                model="LOGLAW", length_scale=2.0, ramp_htc="HTC_RAMP", blowing=True
            )
            .build()
        )
        assert surf.heat_transfer_model == "LOGLAW"
        assert surf.convection_length_scale == 2.0
        assert surf.ramp_heat_transfer_coefficient == "HTC_RAMP"
        assert surf.blowing is True

    def test_with_nusselt_correlation(self):
        """Test custom Nusselt correlation."""
        surf = (
            SurfBuilder("NUSSELT").with_nusselt_correlation(c0=1.0, c1=2.0, c2=3.0, m=4.0).build()
        )
        assert surf.nusselt_c0 == 1.0
        assert surf.nusselt_c1 == 2.0
        assert surf.nusselt_c2 == 3.0
        assert surf.nusselt_m == 4.0

    def test_with_impinging_jet(self):
        """Test impinging jet configuration."""
        surf = SurfBuilder("JET").with_impinging_jet(sigma=0.5).build()
        assert surf.heat_transfer_coefficient_sigma == 0.5

    def test_with_emissivity_back(self):
        """Test back surface emissivity."""
        surf = SurfBuilder("EMISSIVE").with_emissivity_back(0.8).build()
        assert surf.emissivity_back == 0.8

    def test_with_geometry(self):
        """Test solid phase geometry."""
        surf = (
            SurfBuilder("GEOMETRY")
            .with_geometry(
                geometry="CYLINDRICAL",
                inner_radius=0.1,
                length=2.0,
                radius=0.5,
                width=1.0,
                horizontal=True,
            )
            .build()
        )
        assert surf.geometry == "CYLINDRICAL"
        assert surf.inner_radius == 0.1
        assert surf.length == 2.0
        assert surf.radius == 0.5
        assert surf.width == 1.0
        assert surf.horizontal is True

    def test_with_3d_heat_conduction(self):
        """Test 3D heat conduction."""
        surf = SurfBuilder("3D_HT").with_3d_heat_conduction(variable_thickness=True).build()
        assert surf.ht3d is True
        assert surf.variable_thickness is True

    def test_with_numerical_params(self):
        """Test numerical parameters."""
        surf = (
            SurfBuilder("NUMERICAL")
            .with_numerical_params(
                stretch_factor=[1.1, 1.2, 1.3],
                cell_size_factor=[0.1, 0.2, 0.3],
                cell_size=[0.01, 0.02, 0.03],
                n_layer_cells_max=[10, 20, 30],
                time_step_factor=15.0,
                delta_tmp_max=15.0,
                minimum_layer_thickness=[0.001, 0.002, 0.003],
                minimum_layer_mass_fraction=[0.01, 0.02, 0.03],
                remesh_ratio=0.2,
            )
            .build()
        )
        assert surf.stretch_factor == [1.1, 1.2, 1.3]
        assert surf.cell_size_factor == [0.1, 0.2, 0.3]
        assert surf.cell_size == [0.01, 0.02, 0.03]
        assert surf.n_layer_cells_max == [10, 20, 30]
        assert surf.time_step_factor == 15.0
        assert surf.delta_tmp_max == 15.0
        assert surf.minimum_layer_thickness == [0.001, 0.002, 0.003]
        assert surf.minimum_layer_mass_fraction == [0.01, 0.02, 0.03]
        assert surf.remesh_ratio == 0.2

    def test_with_internal_heat_source(self):
        """Test internal heat source."""
        surf = (
            SurfBuilder("HEAT_SOURCE")
            .with_internal_heat_source([1000.0, 2000.0, 3000.0], "IHS_RAMP")
            .build()
        )
        assert surf.internal_heat_source == [1000.0, 2000.0, 3000.0]
        assert surf.ramp_ihs == "IHS_RAMP"

    def test_as_default(self):
        """Test default surface flag."""
        surf = SurfBuilder("DEFAULT").as_default().build()
        assert surf.default is True

    def test_with_texture(self):
        """Test texture mapping."""
        surf = (
            SurfBuilder("TEXTURED")
            .with_texture(texture_map="wall.png", width=2.0, height=3.0, transparency=0.9)
            .build()
        )
        assert surf.texture_map == "wall.png"
        assert surf.texture_width == 2.0
        assert surf.texture_height == 3.0
        assert surf.transparency == 0.9

    def test_phase3_fds_output(self):
        """Test Phase 3 parameters in FDS output."""
        surf = (
            SurfBuilder("PHASE3_SURF")
            .with_initial_temperature(tmp_front_initial=300.0, tmp_back=400.0)
            .with_adiabatic()
            .with_geometry("CARTESIAN")
            .with_3d_heat_conduction()
            .with_internal_heat_source([500.0])
            .as_default()
            .build()
        )
        fds_output = surf.to_fds()
        assert "&SURF" in fds_output
        assert "TMP_FRONT_INITIAL=300.0" in fds_output
        assert "TMP_BACK=400.0" in fds_output
        assert "ADIABATIC=.TRUE." in fds_output
        assert "GEOMETRY='CARTESIAN'" in fds_output
        assert "HT3D=.TRUE." in fds_output
        assert "INTERNAL_HEAT_SOURCE=500.0" in fds_output
        assert "DEFAULT=.TRUE." in fds_output


class TestMultiLayerBuilder:
    """Test multi-layer surface building."""

    def test_simple_multi_layer(self):
        """Test simple multi-layer wall."""
        surf = (
            SurfBuilder("WALL")
            .with_multi_layer_material(
                layers=[
                    {"matl_id": "GYPSUM", "thickness": 0.013},
                    {"matl_id": "INSULATION", "thickness": 0.1},
                    {"matl_id": "GYPSUM", "thickness": 0.013},
                ]
            )
            .build()
        )

        assert surf.matl_id == ["GYPSUM", "INSULATION", "GYPSUM"]
        assert surf.thickness == [0.013, 0.1, 0.013]
        assert surf.backing == "EXPOSED"

    def test_multi_component_layer(self):
        """Test layer with multiple material components."""
        surf = (
            SurfBuilder("COMPOSITE")
            .with_multi_layer_material(
                layers=[
                    {
                        "matl_id": ["CALCIUM_SILICATE", "ITE"],
                        "thickness": 0.025,
                        "mass_fraction": [0.68, 0.32],
                    },
                ]
            )
            .build()
        )

        assert surf.matl_id == [["CALCIUM_SILICATE", "ITE"]]
        assert surf.matl_mass_fraction == [[0.68, 0.32]]

    def test_missing_thickness_error(self):
        """Test error when thickness is missing."""
        with pytest.raises(ValueError, match="thickness"):
            SurfBuilder("BAD").with_multi_layer_material(layers=[{"matl_id": "GYPSUM"}]).build()

    def test_mass_fraction_mismatch_error(self):
        """Test error when mass_fraction length mismatches."""
        with pytest.raises(ValueError, match="mass_fraction length"):
            SurfBuilder("BAD").with_multi_layer_material(
                layers=[
                    {
                        "matl_id": ["A", "B", "C"],
                        "thickness": 0.01,
                        "mass_fraction": [0.5, 0.5],  # Should be 3 values
                    }
                ]
            ).build()


class TestSpyroBuilder:
    """Test SPyro model building."""

    def test_simple_spyro(self):
        """Test basic SPyro configuration."""
        surf = (
            SurfBuilder("PLYWOOD")
            .with_spyro_model(reference_heat_flux=50.0, ramp_q="PLYWOOD_HRR")
            .build()
        )

        assert surf.reference_heat_flux == 50.0
        assert surf.ramp_q == "PLYWOOD_HRR"

    def test_multi_flux_spyro(self):
        """Test SPyro with multiple heat flux experiments."""
        surf = (
            SurfBuilder("MATERIAL")
            .with_spyro_model(
                reference_heat_flux=[35.0, 50.0, 75.0],
                ramp_q="MAT_HRR",
                reference_thickness=[0.01, 0.01, 0.01],
            )
            .build()
        )

        assert surf.reference_heat_flux == [35.0, 50.0, 75.0]
        assert surf.reference_thickness == [0.01, 0.01, 0.01]
