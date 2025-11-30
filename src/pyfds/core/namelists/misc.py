"""
FDS MISC namelist.

Miscellaneous simulation parameters for physics, numerics, and solver behavior.
"""

from pydantic import model_validator

from pyfds.core.enums import TurbulenceModel
from pyfds.core.namelists.base import FdsField, NamelistBase


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
    vn_max : float, optional
        Maximum Von Neumann number, default: 1.0
    vn_min : float, optional
        Minimum Von Neumann number, default: 0.8

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
    tmpa: float = FdsField(20.0, description="Ambient temperature [°C]")
    p_inf: float = FdsField(101325.0, gt=0, description="Background pressure [Pa]")
    humidity: float = FdsField(40.0, ge=0, le=100, description="Relative humidity [%]")
    gvec: tuple[float, float, float] = FdsField(
        (0.0, 0.0, -9.81), description="Gravity vector [m/s²]"
    )

    # Turbulence model
    turbulence_model: TurbulenceModel = FdsField(
        TurbulenceModel.DEARDORFF, description="Turbulence model"
    )
    c_deardorff: float = FdsField(0.1, ge=0, le=1, description="Deardorff constant")
    c_smagorinsky: float = FdsField(0.2, ge=0, le=1, description="Smagorinsky constant")
    c_vreman: float = FdsField(0.07, ge=0, le=1, description="Vreman constant")

    # Numerical parameters
    cfl_max: float = FdsField(1.0, gt=0.1, le=10, description="Maximum CFL number")
    cfl_min: float = FdsField(0.8, gt=0.1, le=10, description="Minimum CFL number")
    vn_max: float = FdsField(1.0, gt=0.1, le=10, description="Maximum Von Neumann number")
    vn_min: float = FdsField(0.8, gt=0.1, le=10, description="Minimum Von Neumann number")

    # Solver options
    solid_phase_only: bool = FdsField(False, description="Solid phase only")
    isothermal: bool = FdsField(False, description="Isothermal flow")
    radiation: bool = FdsField(True, description="Include radiation")
    stratification: bool = FdsField(True, description="Include stratification")

    # Special modes
    level_set_mode: int | None = FdsField(None, ge=0, le=2, description="Wildfire mode")
    particle_cfl: bool = FdsField(True, description="Use particle CFL")

    # Restart
    restart: bool = FdsField(False, description="Enable restart")
    restart_chid: str | None = FdsField(None, description="Restart CHID")

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

        # VN validation
        if self.vn_min > self.vn_max:
            raise ValueError(
                f"VN_MIN ({self.vn_min}) must be less than or equal to VN_MAX ({self.vn_max})"
            )

        # Mode conflicts
        if self.solid_phase_only and self.isothermal:
            raise ValueError("Cannot use both SOLID_PHASE_ONLY and ISOTHERMAL")

        return self

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MISC"
