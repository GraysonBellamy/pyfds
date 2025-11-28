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
    id : str, optional
        Reaction identifier
    hcn_yield : float, optional
        HCN yield [kg/kg fuel], default: 0.0
    epumo2 : float, optional
        Energy per unit mass of O2 consumed [kJ/kg]
    hoc_complete : float, optional
        Complete heat of combustion [kJ/kg]
    n_simple_chemistry_reactions : int, optional
        Number of simple chemistry reactions (1 or 2), default: 1
    fuel_c_to_co_fraction : float, optional
        Fraction of fuel carbon converted to CO, default: 0.0
    fuel_n_to_hcn_fraction : float, optional
        Fraction of fuel nitrogen converted to HCN, default: 0.0
    fuel_h_to_h2_fraction : float, optional
        Fraction of fuel hydrogen converted to H2, default: 0.0
    check_atom_balance : bool, optional
        Check atom balance in reaction, default: True
    reac_atom_error : float, optional
        Atom balance error tolerance, default: 1e-4
    reac_mass_error : float, optional
        Mass balance error tolerance, default: 1e-4
    lower_oxygen_limit : float, optional
        Lower oxygen index for extinction, default: 0.0
    ait_exclusion_zone : tuple[float, ...], optional
        XB bounds for auto-ignition exclusion zone
    ait_exclusion_zone_temperature : float, optional
        Temperature above which ignition is allowed [°C]
    ait_exclusion_zone_devc_id : str, optional
        Device to control exclusion zone
    ait_exclusion_zone_ctrl_id : str, optional
        Control logic for exclusion zone

    # Finite-rate kinetics parameters
    a : float, optional
        Pre-exponential factor [(mol/cm³)^(1-n)/s]
    e : float, optional
        Activation energy [J/mol]
    n_t : float, optional
        Temperature exponent in rate equation, default: 0.0
    spec_id_n_s : list[str], optional
        Species IDs for concentration exponents
    n_s : list[float], optional
        Concentration exponents for each species
    equation : str, optional
        Reaction equation in text form (e.g., 'CH4+2*O2=CO2+2*H2O')
    priority : int, optional
        Reaction priority for multi-step schemes
    reverse : bool, optional
        Enable reversible reaction, default: False
    fuel_radcal_id : str, optional
        RadCal species for fuel radiation absorption
    tau_chem : float, optional
        Minimum mixing time bound [s]
    tau_flame : float, optional
        Maximum mixing time bound [s]

    Examples
    --------
    >>> # Use predefined fuel
    >>> reac = Reaction(fuel='PROPANE')

    >>> # Custom fuel composition
    >>> reac = Reaction(c=7, h=16, heat_of_combustion=44600, soot_yield=0.015)

    >>> # Two-step chemistry with CO production
    >>> reac = Reaction(
    ...     fuel='WOOD',
    ...     n_simple_chemistry_reactions=2,
    ...     fuel_c_to_co_fraction=0.1,
    ...     hcn_yield=0.001
    ... )

    >>> # Finite-rate kinetics
    >>> reac = Reaction(
    ...     fuel='PROPANE',
    ...     a=8.6e11,
    ...     e=125520,
    ...     spec_id_n_s=['PROPANE', 'OXYGEN'],
    ...     n_s=[0.1, 1.65],
    ...     equation='C3H8 + 5*O2 = 3*CO2 + 4*H2O'
    ... )

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

    # Reaction identification (Phase 3)
    id: str | None = Field(None, description="Reaction identifier")

    # Product yields (Phase 3)
    hcn_yield: float = Field(0.0, ge=0, le=1, description="HCN yield [kg/kg fuel]")

    # Energy parameters (Phase 3)
    epumo2: float | None = Field(
        None, gt=0, description="Energy per unit mass of O2 consumed [kJ/kg]"
    )
    hoc_complete: float | None = Field(
        None, gt=0, description="Complete heat of combustion [kJ/kg]"
    )

    # Two-step chemistry (Phase 3)
    n_simple_chemistry_reactions: int = Field(
        1, ge=1, le=2, description="Number of simple chemistry reactions (1 or 2)"
    )

    # Product fractions for incomplete combustion (Phase 3)
    fuel_c_to_co_fraction: float = Field(
        0.0, ge=0, le=1, description="Fraction of fuel carbon converted to CO"
    )
    fuel_n_to_hcn_fraction: float = Field(
        0.0, ge=0, le=1, description="Fraction of fuel nitrogen converted to HCN"
    )
    fuel_h_to_h2_fraction: float = Field(
        0.0, ge=0, le=1, description="Fraction of fuel hydrogen converted to H2"
    )

    # Validation options (Phase 3)
    check_atom_balance: bool = Field(True, description="Check atom balance in reaction")
    reac_atom_error: float = Field(1e-4, gt=0, description="Atom balance error tolerance")
    reac_mass_error: float = Field(1e-4, gt=0, description="Mass balance error tolerance")

    # Oxygen limit (Phase 3)
    lower_oxygen_limit: float = Field(
        0.0, ge=0, le=1, description="Lower oxygen index for extinction"
    )

    # Auto-ignition exclusion zones (Phase 3)
    ait_exclusion_zone: tuple[float, ...] | None = Field(
        None, description="XB bounds for auto-ignition exclusion zone"
    )
    ait_exclusion_zone_temperature: float | None = Field(
        None, description="Temperature above which ignition is allowed [°C]"
    )
    ait_exclusion_zone_devc_id: str | None = Field(
        None, description="Device to control exclusion zone"
    )
    ait_exclusion_zone_ctrl_id: str | None = Field(
        None, description="Control logic for exclusion zone"
    )

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

    # Finite-rate kinetics parameters
    a: float | None = Field(None, gt=0, description="Pre-exponential factor [(mol/cm³)^(1-n)/s]")
    e: float | None = Field(None, ge=0, description="Activation energy [J/mol]")
    n_t: float = Field(0.0, description="Temperature exponent in rate equation")

    # Concentration exponents
    spec_id_n_s: list[str] | None = Field(
        None, description="Species IDs for concentration exponents"
    )
    n_s: list[float] | None = Field(None, description="Concentration exponents for each species")

    # Reaction specification
    equation: str | None = Field(None, description="Reaction equation in text form")
    priority: int | None = Field(None, ge=1, description="Reaction priority for multi-step schemes")
    reverse: bool = Field(False, description="Enable reversible reaction")

    # Radiation
    fuel_radcal_id: str | None = Field(
        None, description="RadCal species for fuel radiation absorption"
    )

    # Mixing time bounds (COMB parameters, but can be set on REAC for convenience)
    tau_chem: float | None = Field(None, gt=0, description="Minimum mixing time bound [s]")
    tau_flame: float | None = Field(None, gt=0, description="Maximum mixing time bound [s]")

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
        # Check yields sum (include new HCN yield)
        total_yield = self.soot_yield + self.co_yield + self.hcn_yield
        if total_yield > 1.0:
            raise ValueError(f"Sum of product yields ({total_yield:.2f}) exceeds 1.0")

        # Validate species stoichiometry
        if len(self.spec_id_nu) != len(self.nu):
            raise ValueError("SPEC_ID_NU and NU must have same length")

        # Validate auto-ignition exclusion zone
        if self.ait_exclusion_zone is not None and len(self.ait_exclusion_zone) != 6:
            raise ValueError("AIT_EXCLUSION_ZONE must have exactly 6 values (XB bounds)")

        # Finite-rate validation
        if (
            self.spec_id_n_s is not None
            and self.n_s is not None
            and len(self.spec_id_n_s) != len(self.n_s)
        ):
            raise ValueError("SPEC_ID_N_S and N_S must have same length")

        # Equation format validation
        if self.equation is not None and "=" not in self.equation:
            raise ValueError("EQUATION must contain '=' separating reactants and products")

        # Validate mixing time bounds
        if (
            self.tau_chem is not None
            and self.tau_flame is not None
            and self.tau_chem > self.tau_flame
        ):
            raise ValueError("TAU_CHEM must be <= TAU_FLAME")

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

        # Extinction and suppression parameters
        if self.extinction_model is not None:
            params["extinction_model"] = self.extinction_model
        if self.critical_flame_temperature is not None:
            params["critical_flame_temperature"] = self.critical_flame_temperature
        if self.suppression:
            params["suppression"] = self.suppression
        if self.k_suppression is not None:
            params["k_suppression"] = self.k_suppression
        if not self.ideal:
            params["ideal"] = self.ideal

        # New Phase 3 parameters
        if self.id is not None:
            params["id"] = self.id
        if self.hcn_yield > 0:
            params["hcn_yield"] = self.hcn_yield
        if self.epumo2 is not None:
            params["epumo2"] = self.epumo2
        if self.hoc_complete is not None:
            params["hoc_complete"] = self.hoc_complete
        if self.n_simple_chemistry_reactions != 1:
            params["n_simple_chemistry_reactions"] = self.n_simple_chemistry_reactions
        if self.fuel_c_to_co_fraction > 0:
            params["fuel_c_to_co_fraction"] = self.fuel_c_to_co_fraction
        if self.fuel_n_to_hcn_fraction > 0:
            params["fuel_n_to_hcn_fraction"] = self.fuel_n_to_hcn_fraction
        if self.fuel_h_to_h2_fraction > 0:
            params["fuel_h_to_h2_fraction"] = self.fuel_h_to_h2_fraction
        if not self.check_atom_balance:
            params["check_atom_balance"] = self.check_atom_balance
        if self.reac_atom_error != 1e-4:
            params["reac_atom_error"] = self.reac_atom_error
        if self.reac_mass_error != 1e-4:
            params["reac_mass_error"] = self.reac_mass_error
        if self.lower_oxygen_limit > 0:
            params["lower_oxygen_limit"] = self.lower_oxygen_limit

        # Auto-ignition exclusion zone parameters
        if self.ait_exclusion_zone is not None:
            params["ait_exclusion_zone"] = list(self.ait_exclusion_zone)
        if self.ait_exclusion_zone_temperature is not None:
            params["ait_exclusion_zone_temperature"] = self.ait_exclusion_zone_temperature
        if self.ait_exclusion_zone_devc_id is not None:
            params["ait_exclusion_zone_devc_id"] = self.ait_exclusion_zone_devc_id
        if self.ait_exclusion_zone_ctrl_id is not None:
            params["ait_exclusion_zone_ctrl_id"] = self.ait_exclusion_zone_ctrl_id

        # Finite-rate kinetics parameters
        if self.a is not None:
            params["a"] = self.a
        if self.e is not None:
            params["e"] = self.e
        if self.n_t != 0.0:
            params["n_t"] = self.n_t
        if self.spec_id_n_s is not None:
            params["spec_id_n_s"] = self.spec_id_n_s
        if self.n_s is not None:
            params["n_s"] = self.n_s

        # Reaction specification
        if self.equation is not None:
            params["equation"] = self.equation
        if self.priority is not None:
            params["priority"] = self.priority
        if self.reverse:
            params["reverse"] = self.reverse

        # Radiation
        if self.fuel_radcal_id is not None:
            params["fuel_radcal_id"] = self.fuel_radcal_id

        # Mixing time bounds
        if self.tau_chem is not None:
            params["tau_chem"] = self.tau_chem
        if self.tau_flame is not None:
            params["tau_flame"] = self.tau_flame

        return self._build_namelist("REAC", params)
