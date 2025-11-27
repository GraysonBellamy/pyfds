"""Unit tests for MISC namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Misc, TurbulenceModel


class TestMisc:
    """Tests for Misc namelist."""

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

    def test_turbulence_model(self):
        """Test setting turbulence model."""
        misc = Misc(turbulence_model=TurbulenceModel.DYNAMIC_SMAGORINSKY, c_smagorinsky=0.18)
        assert misc.turbulence_model == TurbulenceModel.DYNAMIC_SMAGORINSKY
        assert misc.c_smagorinsky == 0.18

    def test_solid_phase_only(self):
        """Test solid phase only mode."""
        misc = Misc(solid_phase_only=True)
        assert misc.solid_phase_only is True

    def test_wildfire_mode(self):
        """Test wildfire simulation mode."""
        misc = Misc(level_set_mode=1, tmpa=35.0, humidity=15.0)
        assert misc.level_set_mode == 1
        assert misc.tmpa == 35.0
        assert misc.humidity == 15.0

    def test_restart_configuration(self):
        """Test restart configuration."""
        misc = Misc(restart=True, restart_chid="previous_run")
        assert misc.restart is True
        assert misc.restart_chid == "previous_run"

    def test_cfl_parameters(self):
        """Test CFL number settings."""
        misc = Misc(cfl_min=0.9, cfl_max=1.1)
        assert misc.cfl_min == 0.9
        assert misc.cfl_max == 1.1

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

    def test_mode_conflicts(self):
        """Test that solid_phase_only and isothermal cannot both be true."""
        with pytest.raises(ValidationError, match="Cannot use both"):
            Misc(solid_phase_only=True, isothermal=True)

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

    def test_to_fds_cfl(self):
        """Test CFL parameters in FDS output."""
        misc = Misc(cfl_min=0.9, cfl_max=1.1)
        fds_str = misc.to_fds()
        assert "CFL_MIN=0.9" in fds_str
        assert "CFL_MAX=1.1" in fds_str

    def test_pressure_validation(self):
        """Test pressure must be positive."""
        with pytest.raises(ValidationError):
            Misc(p_inf=-1000.0)

    def test_level_set_mode_range(self):
        """Test level_set_mode must be 0, 1, or 2."""
        misc = Misc(level_set_mode=0)
        assert misc.level_set_mode == 0

        misc = Misc(level_set_mode=2)
        assert misc.level_set_mode == 2

        with pytest.raises(ValidationError):
            Misc(level_set_mode=3)
