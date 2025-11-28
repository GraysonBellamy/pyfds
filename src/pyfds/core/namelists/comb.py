"""
FDS COMB namelist.

Combustion model parameters.
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.namelists.base import NamelistBase


class Combustion(NamelistBase):
    """
    FDS COMB namelist - combustion model parameters.

    Controls global combustion behavior including extinction models,
    turbulent combustion, and mixing parameters.

    Parameters
    ----------
    extinction_model : str, optional
        Extinction model: 'EXTINCTION 1' or 'EXTINCTION 2'
    suppression : bool, optional
        Enable flame suppression model, default: True
    initial_unmixed_fraction : float, optional
        Initial unmixed fraction (0-1), default: 1.0
    ramp_zeta_0 : str, optional
        Ramp ID for time-varying initial unmixed fraction
    fixed_mix_time : float, optional
        Fixed mixing time [s]
    tau_chem : float, optional
        Minimum bound for mixing time [s]
    tau_flame : float, optional
        Maximum bound for mixing time [s]
    zz_min_global : float, optional
        Minimum species mass fraction for reactions, default: 1e-10
    finite_rate_min_temp : float, optional
        Minimum temperature for finite-rate reactions [°C]
    compute_adiabatic_flame_temperature : bool, optional
        Compute and report adiabatic flame temperature, default: False

    Examples
    --------
    >>> # Enable extinction model 2
    >>> comb = Combustion(extinction_model='EXTINCTION 2')

    >>> # Premixed combustion
    >>> comb = Combustion(initial_unmixed_fraction=0.0)

    >>> # Finite-rate chemistry settings
    >>> comb = Combustion(
    ...     finite_rate_min_temp=100.0,
    ...     zz_min_global=1e-8
    ... )
    """

    # Extinction model
    extinction_model: str | None = Field(
        None, description="Extinction model: 'EXTINCTION 1' or 'EXTINCTION 2'"
    )

    @field_validator("extinction_model", mode="before")
    @classmethod
    def validate_extinction_model(cls, v: str | None) -> str | None:
        """Convert extinction model to uppercase."""
        if v is not None:
            return v.upper()
        return v

    suppression: bool = Field(True, description="Enable flame suppression model")

    # Turbulent combustion / mixing
    initial_unmixed_fraction: float = Field(
        1.0, ge=0.0, le=1.0, description="Initial unmixed fraction"
    )
    ramp_zeta_0: str | None = Field(
        None, description="Ramp ID for time-varying initial unmixed fraction"
    )
    fixed_mix_time: float | None = Field(None, gt=0, description="Fixed mixing time [s]")
    tau_chem: float | None = Field(None, gt=0, description="Minimum mixing time bound [s]")
    tau_flame: float | None = Field(None, gt=0, description="Maximum mixing time bound [s]")

    # Species/reaction thresholds
    zz_min_global: float = Field(
        1e-10, gt=0, description="Minimum species mass fraction for reactions"
    )
    finite_rate_min_temp: float | None = Field(
        None, description="Minimum temperature for finite-rate reactions [°C]"
    )

    # Diagnostics
    compute_adiabatic_flame_temperature: bool = Field(
        False, description="Compute and report adiabatic flame temperature"
    )

    @model_validator(mode="after")
    def validate_combustion(self) -> "Combustion":
        """Validate combustion parameters."""
        if self.extinction_model is not None:
            valid = ["EXTINCTION 1", "EXTINCTION 2"]
            if self.extinction_model.upper() not in valid:
                raise ValueError(f"EXTINCTION_MODEL must be one of {valid}")

        if (
            self.tau_chem is not None
            and self.tau_flame is not None
            and self.tau_chem > self.tau_flame
        ):
            raise ValueError("TAU_CHEM must be <= TAU_FLAME")

        return self

    def to_fds(self) -> str:
        """Generate FDS COMB namelist."""
        params: dict[str, Any] = {}

        if self.extinction_model is not None:
            params["extinction_model"] = self.extinction_model.upper()
        if not self.suppression:
            params["suppression"] = self.suppression
        if self.initial_unmixed_fraction != 1.0:
            params["initial_unmixed_fraction"] = self.initial_unmixed_fraction
        if self.ramp_zeta_0 is not None:
            params["ramp_zeta_0"] = self.ramp_zeta_0
        if self.fixed_mix_time is not None:
            params["fixed_mix_time"] = self.fixed_mix_time
        if self.tau_chem is not None:
            params["tau_chem"] = self.tau_chem
        if self.tau_flame is not None:
            params["tau_flame"] = self.tau_flame
        if self.zz_min_global != 1e-10:
            params["zz_min_global"] = self.zz_min_global
        if self.finite_rate_min_temp is not None:
            params["finite_rate_min_temp"] = self.finite_rate_min_temp
        if self.compute_adiabatic_flame_temperature:
            params["compute_adiabatic_flame_temperature"] = True

        if not params:
            return "&COMB\n/\n"

        return self._build_namelist("COMB", params)
