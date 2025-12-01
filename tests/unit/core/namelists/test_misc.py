"""Unit tests for MISC namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.enums import LESFilterType, SimulationMode, TurbulenceModel
from pyfds.core.namelists import Misc


class TestMiscBasics:
    """Tests for Misc namelist basic functionality."""

    def test_basic_creation_defaults(self):
        """Test Misc creation with default values."""
        misc = Misc()
        assert misc.tmpa == 20.0
        assert misc.humidity == 40.0
        assert misc.turbulence_model == TurbulenceModel.DEARDORFF

    def test_ambient_conditions(self):
        """Test setting ambient conditions."""
        misc = Misc(tmpa=25.0, humidity=70.0)
        assert misc.tmpa == 25.0
        assert misc.humidity == 70.0

    def test_solid_phase_only(self):
        """Test solid phase only mode."""
        misc = Misc(solid_phase_only=True)
        assert misc.solid_phase_only is True

    def test_restart_configuration(self):
        """Test restart configuration."""
        misc = Misc(restart=True, restart_chid="previous_run")
        assert misc.restart is True
        assert misc.restart_chid == "previous_run"


class TestMiscTurbulence:
    """Tests for turbulence model parameters."""

    def test_turbulence_model(self):
        """Test setting turbulence model."""
        misc = Misc(turbulence_model=TurbulenceModel.DYNAMIC_SMAGORINSKY, c_smagorinsky=0.18)
        assert misc.turbulence_model == TurbulenceModel.DYNAMIC_SMAGORINSKY
        assert misc.c_smagorinsky == 0.18

    def test_simulation_mode(self):
        """Test simulation mode setting."""
        misc = Misc(simulation_mode=SimulationMode.LES)
        assert misc.simulation_mode == SimulationMode.LES

    def test_les_filter_type(self):
        """Test LES filter type setting."""
        misc = Misc(les_filter_type=LESFilterType.MAX)
        assert misc.les_filter_type == LESFilterType.MAX

    def test_c_wale(self):
        """Test WALE constant."""
        misc = Misc(turbulence_model=TurbulenceModel.WALE, c_wale=0.55)
        assert misc.c_wale == 0.55

    def test_fixed_les_filter_width(self):
        """Test fixed LES filter width."""
        misc = Misc(fixed_les_filter_width=0.05)
        assert misc.fixed_les_filter_width == 0.05

    def test_turbulent_prandtl_schmidt(self):
        """Test turbulent Prandtl and Schmidt numbers."""
        misc = Misc(pr_t=0.6, sc_t=0.7)
        assert misc.pr_t == 0.6
        assert misc.sc_t == 0.7


class TestMiscNumerical:
    """Tests for numerical parameters."""

    def test_cfl_parameters(self):
        """Test CFL number settings."""
        misc = Misc(cfl_min=0.9, cfl_max=1.1)
        assert misc.cfl_min == 0.9
        assert misc.cfl_max == 1.1

    def test_vn_parameters(self):
        """Test Von Neumann number settings."""
        misc = Misc(vn_min=0.7, vn_max=0.9)
        assert misc.vn_min == 0.7
        assert misc.vn_max == 0.9

    def test_vn_defaults(self):
        """Test VN parameters have correct default values."""
        misc = Misc()
        assert misc.vn_max == 1.0
        assert misc.vn_min == 0.8

    def test_check_vn(self):
        """Test CHECK_VN parameter."""
        misc = Misc(check_vn=False)
        assert misc.check_vn is False

    def test_cfl_velocity_norm(self):
        """Test CFL velocity norm setting."""
        misc = Misc(cfl_velocity_norm=1)
        assert misc.cfl_velocity_norm == 1

    def test_flux_limiter(self):
        """Test flux limiter setting."""
        misc = Misc(flux_limiter=3)
        assert misc.flux_limiter == 3

    def test_particle_cfl_parameters(self):
        """Test particle CFL parameters."""
        misc = Misc(particle_cfl=True, particle_cfl_min=0.7, particle_cfl_max=1.2)
        assert misc.particle_cfl is True
        assert misc.particle_cfl_min == 0.7
        assert misc.particle_cfl_max == 1.2

    def test_check_fo(self):
        """Test CHECK_FO parameter."""
        misc = Misc(check_fo=True)
        assert misc.check_fo is True

    def test_check_ht(self):
        """Test CHECK_HT parameter."""
        misc = Misc(check_ht=True)
        assert misc.check_ht is True

    def test_alignment_tolerance(self):
        """Test alignment tolerance."""
        misc = Misc(alignment_tolerance=0.01)
        assert misc.alignment_tolerance == 0.01


class TestMiscSolver:
    """Tests for solver options."""

    def test_freeze_velocity(self):
        """Test freeze velocity settings."""
        misc = Misc(freeze_velocity=True, unfreeze_time=10.0)
        assert misc.freeze_velocity is True
        assert misc.unfreeze_time == 10.0

    def test_noise_settings(self):
        """Test noise settings."""
        misc = Misc(noise=False, noise_velocity=0.01)
        assert misc.noise is False
        assert misc.noise_velocity == 0.01

    def test_rnd_seed(self):
        """Test random seed setting."""
        misc = Misc(rnd_seed=12345)
        assert misc.rnd_seed == 12345


class TestMiscVisibility:
    """Tests for visibility parameters."""

    def test_visibility_parameters(self):
        """Test visibility settings."""
        misc = Misc(maximum_visibility=50.0, visibility_factor=8.0)
        assert misc.maximum_visibility == 50.0
        assert misc.visibility_factor == 8.0

    def test_smoke_albedo(self):
        """Test smoke albedo."""
        misc = Misc(smoke_albedo=0.5)
        assert misc.smoke_albedo == 0.5


class TestMiscDeposition:
    """Tests for deposition parameters."""

    def test_deposition_flags(self):
        """Test deposition flag settings."""
        misc = Misc(
            deposition=False,
            gravitational_deposition=False,
            thermophoretic_deposition=False,
            turbulent_deposition=False,
        )
        assert misc.deposition is False
        assert misc.gravitational_deposition is False
        assert misc.thermophoretic_deposition is False
        assert misc.turbulent_deposition is False

    def test_aerosol_settings(self):
        """Test aerosol settings."""
        misc = Misc(aerosol_al2o3=True, aerosol_scrubbing=True)
        assert misc.aerosol_al2o3 is True
        assert misc.aerosol_scrubbing is True

    def test_soot_parameters(self):
        """Test soot parameters."""
        misc = Misc(soot_density=2000.0, soot_oxidation=True)
        assert misc.soot_density == 2000.0
        assert misc.soot_oxidation is True

    def test_settling_flags(self):
        """Test settling flags."""
        misc = Misc(gravitational_settling=False, thermophoretic_settling=False)
        assert misc.gravitational_settling is False
        assert misc.thermophoretic_settling is False

    def test_cnf_cutoff(self):
        """Test CNF cutoff parameter."""
        misc = Misc(cnf_cutoff=0.01)
        assert misc.cnf_cutoff == 0.01

    def test_nucleation_sites(self):
        """Test nucleation sites parameter."""
        misc = Misc(nucleation_sites=1.0e8)
        assert misc.nucleation_sites == 1.0e8


class TestMiscHVAC:
    """Tests for HVAC parameters."""

    def test_hvac_parameters(self):
        """Test HVAC settings."""
        misc = Misc(
            hvac_local_pressure=False,
            hvac_pres_relax=0.5,
            hvac_qfan=True,
            hvac_mass_transport_cell_l=0.1,
        )
        assert misc.hvac_local_pressure is False
        assert misc.hvac_pres_relax == 0.5
        assert misc.hvac_qfan is True
        assert misc.hvac_mass_transport_cell_l == 0.1


class TestMiscWildfire:
    """Tests for wildfire/level set parameters."""

    def test_wildfire_mode(self):
        """Test wildfire simulation mode."""
        misc = Misc(level_set_mode=1, tmpa=35.0, humidity=15.0)
        assert misc.level_set_mode == 1
        assert misc.tmpa == 35.0
        assert misc.humidity == 15.0

    def test_level_set_ellipse(self):
        """Test level set ellipse setting."""
        misc = Misc(level_set_ellipse=False)
        assert misc.level_set_ellipse is False


class TestMiscOutput:
    """Tests for output control parameters."""

    def test_output_settings(self):
        """Test output settings."""
        misc = Misc(bndf_default=False, iblank_smv=False, overwrite=False, verbose=True)
        assert misc.bndf_default is False
        assert misc.iblank_smv is False
        assert misc.overwrite is False
        assert misc.verbose is True


class TestMiscGeographic:
    """Tests for geographic position parameters."""

    def test_geographic_position(self):
        """Test geographic position settings."""
        misc = Misc(origin_lat=40.7128, origin_lon=-74.0060, north_bearing=45.0)
        assert misc.origin_lat == 40.7128
        assert misc.origin_lon == -74.0060
        assert misc.north_bearing == 45.0


class TestMiscGeometry:
    """Tests for geometry and mesh parameters."""

    def test_thicken_obstructions(self):
        """Test thicken obstructions setting."""
        misc = Misc(thicken_obstructions=True)
        assert misc.thicken_obstructions is True

    def test_neighbor_separation_distance(self):
        """Test neighbor separation distance."""
        misc = Misc(neighbor_separation_distance=0.1)
        assert misc.neighbor_separation_distance == 0.1

    def test_zone_volume(self):
        """Test minimum zone volume."""
        misc = Misc(minimum_zone_volume=0.001)
        assert misc.minimum_zone_volume == 0.001

    def test_no_pressure_zones(self):
        """Test no pressure zones flag."""
        misc = Misc(no_pressure_zones=True)
        assert misc.no_pressure_zones is True

    def test_max_leak_paths(self):
        """Test max leak paths."""
        misc = Misc(max_leak_paths=500)
        assert misc.max_leak_paths == 500

    def test_texture_origin(self):
        """Test texture origin."""
        misc = Misc(texture_origin=(1.0, 2.0, 3.0))
        assert misc.texture_origin == (1.0, 2.0, 3.0)


class TestMiscRamps:
    """Tests for ramp-related parameters."""

    def test_max_ramps(self):
        """Test max ramps setting."""
        misc = Misc(max_ramps=200)
        assert misc.max_ramps == 200

    def test_tau_default(self):
        """Test tau default setting."""
        misc = Misc(tau_default=2.0)
        assert misc.tau_default == 2.0

    def test_gravity_ramps(self):
        """Test gravity ramp settings."""
        misc = Misc(ramp_gx="GRAVITY_X", ramp_gy="GRAVITY_Y", ramp_gz="GRAVITY_Z")
        assert misc.ramp_gx == "GRAVITY_X"
        assert misc.ramp_gy == "GRAVITY_Y"
        assert misc.ramp_gz == "GRAVITY_Z"

    def test_velocity_ramps(self):
        """Test velocity ramp settings."""
        misc = Misc(ramp_ux="VEL_X", ramp_vy="VEL_Y", ramp_wz="VEL_Z")
        assert misc.ramp_ux == "VEL_X"
        assert misc.ramp_vy == "VEL_Y"
        assert misc.ramp_wz == "VEL_Z"


class TestMiscAmbient:
    """Tests for ambient/thermodynamic parameters."""

    def test_gamma(self):
        """Test gamma (ratio of specific heats)."""
        misc = Misc(gamma=1.3)
        assert misc.gamma == 1.3

    def test_constant_specific_heat_ratio(self):
        """Test constant specific heat ratio flag."""
        misc = Misc(constant_specific_heat_ratio=True)
        assert misc.constant_specific_heat_ratio is True

    def test_h_f_reference_temperature(self):
        """Test reference temperature for enthalpy."""
        misc = Misc(h_f_reference_temperature=20.0)
        assert misc.h_f_reference_temperature == 20.0

    def test_ambient_mass_fractions(self):
        """Test ambient mass fractions."""
        misc = Misc(y_co2_infty=0.0004, y_o2_infty=0.23)
        assert misc.y_co2_infty == 0.0004
        assert misc.y_o2_infty == 0.23


class TestMiscOther:
    """Tests for other miscellaneous parameters."""

    def test_mpi_timeout(self):
        """Test MPI timeout setting."""
        misc = Misc(mpi_timeout=1200.0)
        assert misc.mpi_timeout == 1200.0

    def test_i_max_temp(self):
        """Test max temperature clip."""
        misc = Misc(i_max_temp=4000)
        assert misc.i_max_temp == 4000

    def test_external_filename(self):
        """Test external filename."""
        misc = Misc(external_filename="control.dat")
        assert misc.external_filename == "control.dat"

    def test_terrain_image(self):
        """Test terrain image."""
        misc = Misc(terrain_image="terrain.png")
        assert misc.terrain_image == "terrain.png"

    def test_gvec(self):
        """Test gravity vector."""
        misc = Misc(gvec=(0.0, 0.0, -10.0))
        assert misc.gvec == (0.0, 0.0, -10.0)


class TestMiscValidation:
    """Tests for MISC validation."""

    def test_temperature_validation(self):
        """Test temperature range validation."""
        with pytest.raises(ValidationError, match="outside reasonable range"):
            Misc(tmpa=-300.0)

        with pytest.raises(ValidationError, match="outside reasonable range"):
            Misc(tmpa=2500.0)

    def test_cfl_validation(self):
        """Test CFL validation."""
        with pytest.raises(ValidationError, match=r"CFL_MIN.*must be less than"):
            Misc(cfl_min=1.5, cfl_max=1.0)

    def test_vn_validation(self):
        """Test VN validation."""
        with pytest.raises(ValidationError, match=r"VN_MIN.*must be less than"):
            Misc(vn_min=1.5, vn_max=1.0)

    def test_particle_cfl_validation(self):
        """Test particle CFL validation."""
        with pytest.raises(ValidationError, match=r"PARTICLE_CFL_MIN.*must be less than"):
            Misc(particle_cfl_min=1.5, particle_cfl_max=1.0)

    def test_mode_conflicts(self):
        """Test that solid_phase_only and isothermal cannot both be true."""
        with pytest.raises(ValidationError, match="Cannot use both"):
            Misc(solid_phase_only=True, isothermal=True)

    def test_freeze_unfreeze_validation(self):
        """Test freeze/unfreeze consistency."""
        with pytest.raises(ValidationError, match="UNFREEZE_TIME requires FREEZE_VELOCITY"):
            Misc(unfreeze_time=10.0)  # freeze_velocity defaults to False

    def test_humidity_range(self):
        """Test humidity must be 0-100%."""
        misc = Misc(humidity=0.0)
        assert misc.humidity == 0.0

        misc = Misc(humidity=100.0)
        assert misc.humidity == 100.0

        with pytest.raises(ValidationError):
            Misc(humidity=-10.0)

        with pytest.raises(ValidationError):
            Misc(humidity=110.0)

    def test_pressure_validation(self):
        """Test pressure must be positive."""
        with pytest.raises(ValidationError):
            Misc(p_inf=-1000.0)

    def test_level_set_mode_range(self):
        """Test level_set_mode must be 0-4."""
        misc = Misc(level_set_mode=0)
        assert misc.level_set_mode == 0

        misc = Misc(level_set_mode=4)
        assert misc.level_set_mode == 4

        with pytest.raises(ValidationError):
            Misc(level_set_mode=5)

    def test_gvec_validation(self):
        """Test gravity vector validation."""
        with pytest.raises(ValidationError):
            Misc(gvec=(0.0, 0.0))  # Must be 3-element

    def test_texture_origin_validation(self):
        """Test texture origin validation."""
        with pytest.raises(ValidationError):
            Misc(texture_origin=(0.0, 0.0))  # Must be 3-element

    def test_latitude_range(self):
        """Test latitude range validation."""
        with pytest.raises(ValidationError):
            Misc(origin_lat=100.0)  # Must be -90 to 90

    def test_longitude_range(self):
        """Test longitude range validation."""
        with pytest.raises(ValidationError):
            Misc(origin_lon=200.0)  # Must be -180 to 180


class TestMiscFdsOutput:
    """Tests for FDS output generation."""

    def test_to_fds_defaults_omitted(self):
        """Test that default values are not written to FDS."""
        misc = Misc()
        fds_str = misc.to_fds()
        # Default values should not appear
        assert "TMPA=20" not in fds_str
        assert "HUMIDITY=40" not in fds_str

    def test_to_fds_non_defaults(self):
        """Test that non-default values are written to FDS."""
        misc = Misc(tmpa=25.0, humidity=70.0, solid_phase_only=True)
        fds_str = misc.to_fds()
        assert "&MISC" in fds_str
        assert "TMPA=25" in fds_str
        assert "HUMIDITY=70" in fds_str
        assert "SOLID_PHASE_ONLY=" in fds_str

    def test_to_fds_turbulence_model(self):
        """Test turbulence model in FDS output."""
        misc = Misc(turbulence_model=TurbulenceModel.VREMAN)
        fds_str = misc.to_fds()
        assert "TURBULENCE_MODEL='VREMAN'" in fds_str

    def test_to_fds_simulation_mode(self):
        """Test simulation mode in FDS output."""
        misc = Misc(simulation_mode=SimulationMode.DNS)
        fds_str = misc.to_fds()
        assert "SIMULATION_MODE='DNS'" in fds_str

    def test_to_fds_cfl(self):
        """Test CFL parameters in FDS output."""
        misc = Misc(cfl_min=0.9, cfl_max=1.1)
        fds_str = misc.to_fds()
        assert "CFL_MIN=0.9" in fds_str
        assert "CFL_MAX=1.1" in fds_str

    def test_to_fds_vn(self):
        """Test VN parameters in FDS output."""
        misc = Misc(vn_min=0.7, vn_max=0.9)
        fds_str = misc.to_fds()
        assert "VN_MIN=0.7" in fds_str
        assert "VN_MAX=0.9" in fds_str

    def test_to_fds_vn_defaults_omitted(self):
        """Test that default VN values are not written to FDS."""
        misc = Misc()
        fds_str = misc.to_fds()
        assert "VN_MAX" not in fds_str
        assert "VN_MIN" not in fds_str

    def test_to_fds_gvec(self):
        """Test GVEC in FDS output."""
        misc = Misc(gvec=(0.0, 0.0, -10.0))
        fds_str = misc.to_fds()
        assert "GVEC" in fds_str
        assert "-10" in fds_str

    def test_to_fds_check_vn(self):
        """Test CHECK_VN in FDS output."""
        misc = Misc(check_vn=False)
        fds_str = misc.to_fds()
        assert "CHECK_VN=" in fds_str

    def test_to_fds_deposition_flags(self):
        """Test deposition flags in FDS output."""
        misc = Misc(deposition=False, agglomeration=False)
        fds_str = misc.to_fds()
        assert "DEPOSITION=" in fds_str
        assert "AGGLOMERATION=" in fds_str
