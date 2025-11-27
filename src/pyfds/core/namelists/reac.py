"""
FDS REAC namelist.

Combustion reaction definition.
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.namelists.base import NamelistBase


class Reaction(NamelistBase):
    """
    FDS REAC namelist - combustion reaction.

    Defines the combustion reaction including fuel composition, heat of combustion,
    and product yields. FDS supports only one reaction per simulation.

    Parameters
    ----------
    fuel : str, optional
        Fuel name for predefined fuels (e.g., 'PROPANE', 'METHANE')
    c : float, optional
        Number of carbon atoms in fuel molecule
    h : float, optional
        Number of hydrogen atoms in fuel molecule
    o : float, optional
        Number of oxygen atoms in fuel molecule
    n : float, optional
        Number of nitrogen atoms in fuel molecule
    heat_of_combustion : float, optional
        Heat of combustion [kJ/kg]
    soot_yield : float, optional
        Soot yield [kg soot/kg fuel], default: 0.01
    co_yield : float, optional
        CO yield [kg CO/kg fuel], default: 0.0
    radiative_fraction : float, optional
        Fraction of energy radiated, default: 0.35
    auto_ignition_temperature : float, optional
        Auto-ignition temperature [°C]

    Examples
    --------
    >>> # Use predefined fuel
    >>> reac = Reaction(fuel='PROPANE')

    >>> # Custom fuel composition
    >>> reac = Reaction(c=7, h=16, heat_of_combustion=44600, soot_yield=0.015)

    Notes
    -----
    - Only one REAC namelist allowed per simulation
    - Yields must sum to less than 1.0
    """

    fuel: str | None = Field(None, description="Fuel name")
    c: float | None = Field(None, gt=0, description="Carbon atoms")
    h: float | None = Field(None, gt=0, description="Hydrogen atoms")
    o: float | None = Field(None, ge=0, description="Oxygen atoms")
    n: float | None = Field(None, ge=0, description="Nitrogen atoms")
    heat_of_combustion: float | None = Field(None, gt=0, description="Heat of combustion [kJ/kg]")
    soot_yield: float = Field(0.01, ge=0, le=1, description="Soot yield")
    co_yield: float = Field(0.0, ge=0, le=1, description="CO yield")
    radiative_fraction: float | None = Field(None, ge=0, le=1, description="Radiative fraction")
    auto_ignition_temperature: float | None = Field(None, description="Auto-ignition temp [°C]")

    # Extinction Parameters (Stage 1.3)
    extinction_model: str | None = Field(
        None, description="Extinction model: EXTINCTION_1, EXTINCTION_2"
    )
    critical_flame_temperature: float | None = Field(
        None, gt=0, description="Critical flame temperature for extinction (K)"
    )

    # Suppression Parameters (Stage 1.3)
    suppression: bool = Field(False, description="Enable suppression model")
    k_suppression: float | None = Field(None, ge=0, description="Suppression rate constant")

    # Heat of Combustion Mode (Stage 1.3)
    ideal: bool = Field(True, description="Use ideal heat of combustion")

    # Species Tracking (Stage 1.3)
    spec_id_nu: list[str] = Field(default_factory=list, description="Species IDs for stoichiometry")
    nu: list[float] = Field(default_factory=list, description="Stoichiometric coefficients")

    # Advanced Parameters (Stage 1.3)
    fixed_mix_time: float | None = Field(None, gt=0, description="Fixed mixing time (s)")
    tau_chem: float | None = Field(None, gt=0, description="Chemical time scale (s)")
    tau_flame: float | None = Field(None, gt=0, description="Flame time scale (s)")

    @field_validator("extinction_model")
    @classmethod
    def validate_extinction_model(cls, v: str | None) -> str | None:
        """Validate extinction model."""
        if v is not None:
            valid_models = ["EXTINCTION_1", "EXTINCTION_2"]
            if v.upper() not in valid_models:
                raise ValueError(f"EXTINCTION_MODEL must be one of {valid_models}, got '{v}'")
            return v.upper()
        return v

    @model_validator(mode="after")
    def validate_reaction(self) -> "Reaction":
        """Validate reaction parameters."""
        # Check yields sum
        total_yield = self.soot_yield + self.co_yield
        if total_yield > 1.0:
            raise ValueError(f"Sum of product yields ({total_yield:.2f}) exceeds 1.0")

        # Validate species stoichiometry
        if len(self.spec_id_nu) != len(self.nu):
            raise ValueError("SPEC_ID_NU and NU must have same length")

        return self

    def to_fds(self) -> str:
        """Generate FDS REAC namelist."""
        params: dict[str, Any] = {}

        if self.fuel:
            params["fuel"] = self.fuel
        if self.c is not None:
            params["c"] = self.c
        if self.h is not None:
            params["h"] = self.h
        if self.o and self.o > 0:
            params["o"] = self.o
        if self.n and self.n > 0:
            params["n"] = self.n
        if self.heat_of_combustion:
            params["heat_of_combustion"] = self.heat_of_combustion
        if self.soot_yield != 0.01:
            params["soot_yield"] = self.soot_yield
        if self.co_yield > 0:
            params["co_yield"] = self.co_yield
        if self.radiative_fraction is not None:
            params["radiative_fraction"] = self.radiative_fraction
        if self.auto_ignition_temperature is not None:
            params["auto_ignition_temperature"] = self.auto_ignition_temperature

        # Extinction Parameters
        if self.extinction_model:
            params["extinction_model"] = self.extinction_model
        if self.critical_flame_temperature is not None:
            params["critical_flame_temperature"] = self.critical_flame_temperature

        # Suppression Parameters
        if self.suppression:
            params["suppression"] = self.suppression
        if self.k_suppression is not None:
            params["k_suppression"] = self.k_suppression

        # Heat of Combustion Mode
        if not self.ideal:  # Only output if False (default is True)
            params["ideal"] = self.ideal

        # Species Tracking
        if self.spec_id_nu:
            params["spec_id_nu"] = self.spec_id_nu
        if self.nu:
            params["nu"] = self.nu

        # Advanced Parameters
        if self.fixed_mix_time is not None:
            params["fixed_mix_time"] = self.fixed_mix_time
        if self.tau_chem is not None:
            params["tau_chem"] = self.tau_chem
        if self.tau_flame is not None:
            params["tau_flame"] = self.tau_flame

        return self._build_namelist("REAC", params)
