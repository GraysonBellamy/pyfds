"""
FDS REAC namelist.

Combustion reaction definition.
"""

from pydantic import field_validator, model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase


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

    fuel: str | None = FdsField(None, description="Fuel name")
    c: float | None = FdsField(None, gt=0, description="Carbon atoms")
    h: float | None = FdsField(None, gt=0, description="Hydrogen atoms")
    o: float | None = FdsField(None, ge=0, exclude_if=0, description="Oxygen atoms")
    n: float | None = FdsField(None, ge=0, exclude_if=0, description="Nitrogen atoms")
    heat_of_combustion: float | None = FdsField(
        None, gt=0, description="Heat of combustion [kJ/kg]"
    )
    soot_yield: float = FdsField(0.01, ge=0, le=1, exclude_if=0.01, description="Soot yield")
    co_yield: float = FdsField(0.0, ge=0, le=1, exclude_if=0.0, description="CO yield")
    radiative_fraction: float | None = FdsField(None, ge=0, le=1, description="Radiative fraction")
    auto_ignition_temperature: float | None = FdsField(None, description="Auto-ignition temp [°C]")

    # Reaction identification
    id: str | None = FdsField(None, description="Reaction identifier")

    # Product yields
    hcn_yield: float = FdsField(
        0.0, ge=0, le=1, exclude_if=0.0, description="HCN yield [kg/kg fuel]"
    )

    # Energy parameters
    epumo2: float | None = FdsField(
        None, gt=0, description="Energy per unit mass of O2 consumed [kJ/kg]"
    )
    hoc_complete: float | None = FdsField(
        None, gt=0, description="Complete heat of combustion [kJ/kg]"
    )

    # Two-step chemistry
    n_simple_chemistry_reactions: int = FdsField(
        1, ge=1, le=2, exclude_if=1, description="Number of simple chemistry reactions (1 or 2)"
    )

    # Product fractions for incomplete combustion
    fuel_c_to_co_fraction: float = FdsField(
        0.0, ge=0, le=1, exclude_if=0.0, description="Fraction of fuel carbon converted to CO"
    )
    fuel_n_to_hcn_fraction: float = FdsField(
        0.0, ge=0, le=1, exclude_if=0.0, description="Fraction of fuel nitrogen converted to HCN"
    )
    fuel_h_to_h2_fraction: float = FdsField(
        0.0, ge=0, le=1, exclude_if=0.0, description="Fraction of fuel hydrogen converted to H2"
    )

    # Validation options
    check_atom_balance: bool = FdsField(
        True, exclude_if=True, description="Check atom balance in reaction"
    )
    reac_atom_error: float = FdsField(
        1e-4, gt=0, exclude_if=1e-4, description="Atom balance error tolerance"
    )
    reac_mass_error: float = FdsField(
        1e-4, gt=0, exclude_if=1e-4, description="Mass balance error tolerance"
    )

    # Oxygen limit
    lower_oxygen_limit: float = FdsField(
        0.0, ge=0, le=1, exclude_if=0.0, description="Lower oxygen index for extinction"
    )

    # Auto-ignition exclusion zones
    ait_exclusion_zone: tuple[float, ...] | None = FdsField(
        None, description="XB bounds for auto-ignition exclusion zone"
    )
    ait_exclusion_zone_temperature: float | None = FdsField(
        None, description="Temperature above which ignition is allowed [°C]"
    )
    ait_exclusion_zone_devc_id: str | None = FdsField(
        None, description="Device to control exclusion zone"
    )
    ait_exclusion_zone_ctrl_id: str | None = FdsField(
        None, description="Control logic for exclusion zone"
    )

    # Extinction Parameters
    extinction_model: str | None = FdsField(
        None, description="Extinction model: EXTINCTION_1, EXTINCTION_2"
    )
    critical_flame_temperature: float | None = FdsField(
        None, gt=0, description="Critical flame temperature for extinction (K)"
    )

    # Suppression Parameters
    suppression: bool = FdsField(False, exclude_if=False, description="Enable suppression model")
    k_suppression: float | None = FdsField(None, ge=0, description="Suppression rate constant")

    # Heat of Combustion Mode
    ideal: bool = FdsField(True, exclude_if=True, description="Use ideal heat of combustion")

    # Species Tracking
    spec_id_nu: list[str] = FdsField(
        default_factory=list, description="Species IDs for stoichiometry"
    )
    nu: list[float] = FdsField(default_factory=list, description="Stoichiometric coefficients")

    # Finite-rate kinetics parameters
    a: float | None = FdsField(None, gt=0, description="Pre-exponential factor [(mol/cm³)^(1-n)/s]")
    e: float | None = FdsField(None, ge=0, description="Activation energy [J/mol]")
    n_t: float = FdsField(0.0, exclude_if=0.0, description="Temperature exponent in rate equation")

    # Concentration exponents
    spec_id_n_s: list[str] | None = FdsField(
        None, description="Species IDs for concentration exponents"
    )
    n_s: list[float] | None = FdsField(None, description="Concentration exponents for each species")

    # Reaction specification
    equation: str | None = FdsField(None, description="Reaction equation in text form")
    priority: int | None = FdsField(
        None, ge=1, description="Reaction priority for multi-step schemes"
    )
    reverse: bool = FdsField(False, exclude_if=False, description="Enable reversible reaction")

    # Radiation
    fuel_radcal_id: str | None = FdsField(
        None, description="RadCal species for fuel radiation absorption"
    )

    # Mixing time bounds (COMB parameters, but can be set on REAC for convenience)
    tau_chem: float | None = FdsField(None, gt=0, description="Minimum mixing time bound [s]")
    tau_flame: float | None = FdsField(None, gt=0, description="Maximum mixing time bound [s]")

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

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "REAC"

    def _extra_fds_params(self) -> list[str]:
        """Handle special formatting cases for FDS parameters."""
        params = []
        if self.ait_exclusion_zone is not None:
            params.append(f"AIT_EXCLUSION_ZONE={list(self.ait_exclusion_zone)}")
        return params
