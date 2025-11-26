"""
FDS MISC namelist.

Miscellaneous simulation parameters for physics, numerics, and solver behavior.
"""

from enum import Enum
from typing import Any

from pydantic import Field, model_validator

from pyfds.core.namelists.base import NamelistBase


class TurbulenceModel(str, Enum):
    """LES turbulence models."""

    DEARDORFF = "DEARDORFF"
    DYNAMIC_SMAGORINSKY = "DYNAMIC SMAGORINSKY"
    VREMAN = "VREMAN"
    WALE = "WALE"


class Misc(NamelistBase):
    """
    FDS MISC namelist - miscellaneous parameters.

    Contains global simulation parameters that affect physics, numerics,
    and solver behavior. Only one MISC namelist is allowed per simulation.

    Ambient Conditions
    -----------------
    tmpa : float, optional
        Ambient temperature [°C], default: 20.0
    p_inf : float, optional
        Background pressure [Pa], default: 101325.0
    humidity : float, optional
        Relative humidity [%], default: 40.0
    gvec : tuple[float, float, float], optional
        Gravity vector [m/s²], default: (0.0, 0.0, -9.81)

    Turbulence Parameters
    --------------------
    turbulence_model : TurbulenceModel, optional
        LES turbulence model, default: DEARDORFF
    c_deardorff : float, optional
        Deardorff model constant, default: 0.1
    c_smagorinsky : float, optional
        Smagorinsky constant, default: 0.2
    c_vreman : float, optional
        Vreman model constant, default: 0.07

    Numerical Parameters
    -------------------
    cfl_max : float, optional
        Maximum CFL number, default: 1.0
    cfl_min : float, optional
        Minimum CFL number, default: 0.8

    Solver Options
    -------------
    solid_phase_only : bool, optional
        Only solve solid heat transfer, default: False
    isothermal : bool, optional
        Isothermal flow calculation, default: False
    radiation : bool, optional
        Include radiation, default: True
    stratification : bool, optional
        Include stratification, default: True

    Special Modes
    ------------
    level_set_mode : int, optional
        Wildfire spread mode (0, 1, or 2)
    particle_cfl : bool, optional
        Use particle CFL, default: True

    Restart
    -------
    restart : bool, optional
        Enable restart capability, default: False
    restart_chid : str, optional
        CHID for restart file

    Examples
    --------
    >>> # Standard ambient conditions
    >>> misc = Misc(tmpa=25.0, humidity=70.0)

    >>> # Solid phase only
    >>> misc = Misc(solid_phase_only=True)

    >>> # Wildfire simulation
    >>> misc = Misc(level_set_mode=1, tmpa=35.0, humidity=15.0)

    >>> # Restart configuration
    >>> misc = Misc(restart=True, restart_chid='previous_run')

    Notes
    -----
    - Only one MISC namelist allowed per simulation
    - Some parameters have major performance impacts
    - Cannot use both solid_phase_only and isothermal
    - Only non-default values are written to FDS file
    """

    # Ambient conditions
    tmpa: float = Field(20.0, description="Ambient temperature [°C]")
    p_inf: float = Field(101325.0, gt=0, description="Background pressure [Pa]")
    humidity: float = Field(40.0, ge=0, le=100, description="Relative humidity [%]")
    gvec: tuple[float, float, float] = Field((0.0, 0.0, -9.81), description="Gravity vector [m/s²]")

    # Turbulence model
    turbulence_model: TurbulenceModel = Field(
        TurbulenceModel.DEARDORFF, description="Turbulence model"
    )
    c_deardorff: float = Field(0.1, ge=0, le=1, description="Deardorff constant")
    c_smagorinsky: float = Field(0.2, ge=0, le=1, description="Smagorinsky constant")
    c_vreman: float = Field(0.07, ge=0, le=1, description="Vreman constant")

    # Numerical parameters
    cfl_max: float = Field(1.0, gt=0.1, le=10, description="Maximum CFL number")
    cfl_min: float = Field(0.8, gt=0.1, le=10, description="Minimum CFL number")

    # Solver options
    solid_phase_only: bool = Field(False, description="Solid phase only")
    isothermal: bool = Field(False, description="Isothermal flow")
    radiation: bool = Field(True, description="Include radiation")
    stratification: bool = Field(True, description="Include stratification")

    # Special modes
    level_set_mode: int | None = Field(None, ge=0, le=2, description="Wildfire mode")
    particle_cfl: bool = Field(True, description="Use particle CFL")

    # Restart
    restart: bool = Field(False, description="Enable restart")
    restart_chid: str | None = Field(None, description="Restart CHID")

    @model_validator(mode="after")
    def validate_misc(self) -> "Misc":
        """Validate MISC parameters."""
        # Temperature range check
        if not (-273.15 < self.tmpa < 2000):
            raise ValueError(f"TMPA ({self.tmpa}°C) outside reasonable range [-273.15, 2000]")

        # CFL validation
        if self.cfl_min > self.cfl_max:
            raise ValueError(
                f"CFL_MIN ({self.cfl_min}) must be less than or equal to CFL_MAX ({self.cfl_max})"
            )

        # Mode conflicts
        if self.solid_phase_only and self.isothermal:
            raise ValueError("Cannot use both SOLID_PHASE_ONLY and ISOTHERMAL")

        return self

    def to_fds(self) -> str:
        """
        Generate FDS MISC namelist.

        Only outputs non-default values to keep the file clean.
        """
        params: dict[str, Any] = {}

        # Ambient conditions (only if non-default)
        if self.tmpa != 20.0:
            params["tmpa"] = self.tmpa
        if self.p_inf != 101325.0:
            params["p_inf"] = self.p_inf
        if self.humidity != 40.0:
            params["humidity"] = self.humidity
        if self.gvec != (0.0, 0.0, -9.81):
            params["gvec"] = self.gvec

        # Turbulence model
        if self.turbulence_model != TurbulenceModel.DEARDORFF:
            params["turbulence_model"] = self.turbulence_model.value
        if self.c_deardorff != 0.1:
            params["c_deardorff"] = self.c_deardorff
        if self.c_smagorinsky != 0.2:
            params["c_smagorinsky"] = self.c_smagorinsky
        if self.c_vreman != 0.07:
            params["c_vreman"] = self.c_vreman

        # CFL parameters
        if self.cfl_max != 1.0:
            params["cfl_max"] = self.cfl_max
        if self.cfl_min != 0.8:
            params["cfl_min"] = self.cfl_min

        # Solver options
        if self.solid_phase_only:
            params["solid_phase_only"] = self.solid_phase_only
        if self.isothermal:
            params["isothermal"] = self.isothermal
        if not self.radiation:
            params["radiation"] = self.radiation
        if not self.stratification:
            params["stratification"] = self.stratification

        # Special modes
        if self.level_set_mode is not None:
            params["level_set_mode"] = self.level_set_mode
        if not self.particle_cfl:
            params["particle_cfl"] = self.particle_cfl

        # Restart
        if self.restart:
            params["restart"] = self.restart
            if self.restart_chid:
                params["restart_chid"] = self.restart_chid

        return self._build_namelist("MISC", params)
