"""FDS REAC namelist for combustion reaction definition.

Defines the combustion reaction including fuel composition, heat of combustion,
and product yields.

Field Groups:
    identification: Fuel name and reaction ID
    composition: Fuel atom counts (C, H, O, N)
    energy: Heat of combustion and radiative fraction
    yields: Product yields (soot, CO, HCN)
    ignition: Auto-ignition parameters
    chemistry: Multi-step and finite-rate kinetics
    validation: Atom/mass balance checks
    extinction: Extinction and limiting oxygen parameters
    species: Species stoichiometry
    kinetics: Rate equation parameters (Arrhenius)
    falloff: Falloff reaction parameters (Lindemann, Troe)
    third_body: Third body reaction parameters
"""

from pydantic import model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Reaction"]


class Reaction(NamelistBase):
    """
    FDS REAC namelist - combustion reaction.

    Defines the combustion reaction including fuel composition, heat of combustion,
    and product yields. Multiple REAC namelists can be used for multi-step chemistry.

    Parameters
    ----------
    fuel : str, optional
        Fuel name for predefined fuels (e.g., 'PROPANE', 'METHANE')
    id : str, optional
        Reaction identifier
    c : float, optional
        Number of carbon atoms in fuel molecule (for simple chemistry)
    h : float, optional
        Number of hydrogen atoms in fuel molecule (for simple chemistry)
    o : float, optional
        Number of oxygen atoms in fuel molecule (for simple chemistry)
    n : float, optional
        Number of nitrogen atoms in fuel molecule (for simple chemistry)
    heat_of_combustion : float, optional
        Heat of combustion [kJ/kg]
    soot_yield : float, optional
        Soot yield [kg soot/kg fuel], default: 0.0
    co_yield : float, optional
        CO yield [kg CO/kg fuel], default: 0.0
    hcn_yield : float, optional
        HCN yield [kg/kg fuel], default: 0.0
    radiative_fraction : float, optional
        Fraction of energy radiated
    ramp_chi_r : str, optional
        Ramp ID for time-varying radiative fraction
    auto_ignition_temperature : float, optional
        Auto-ignition temperature [°C] (default: -273)
    epumo2 : float, optional
        Energy per unit mass of O2 consumed [kJ/kg] (default: 13100)
    hoc_complete : float, optional
        Complete heat of combustion [kJ/kg]
    ideal : bool, optional
        Use ideal heat of combustion (default: False)
    n_simple_chemistry_reactions : int, optional
        Number of simple chemistry reactions (1 or 2), default: 1
    fuel_c_to_co_fraction : float, optional
        Fraction of fuel carbon converted to CO (default: 2/3)
    fuel_h_to_h2_fraction : float, optional
        Fraction of fuel hydrogen converted to H2 (default: 0)
    fuel_n_to_hcn_fraction : float, optional
        Fraction of fuel nitrogen converted to HCN (default: 1/5)
    fuel_radcal_id : str, optional
        RadCal species for fuel radiation absorption
    check_atom_balance : bool, optional
        Check atom balance in reaction (default: True)
    reac_atom_error : float, optional
        Atom balance error tolerance (default: 1e-4)
    reac_mass_error : float, optional
        Mass balance error tolerance (default: 1e-4)
    lower_oxygen_limit : float, optional
        Lower oxygen index for extinction [mol/mol]
    critical_flame_temperature : float, optional
        Critical flame temperature for extinction [°C] (default: 1427)

    Examples
    --------
    >>> # Use predefined fuel
    >>> reac = Reaction(fuel='PROPANE')

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
    ...     n_s=[0.1, 1.65]
    ... )

    Notes
    -----
    - Yields must sum to less than 1.0

    See Also
    --------
    Species : Gas species definitions for reactants/products.
    Combustion : Global combustion model parameters.
    Surface : Burning surfaces that produce fuel.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "REAC"

    # =========================================================================
    # Identification
    # =========================================================================
    id: str | None = FdsField(
        None,
        fds_name="ID",
        description="Reaction identifier",
        group="identification",
    )

    fuel: str | None = FdsField(
        None,
        fds_name="FUEL",
        description="Fuel name (predefined or from SPEC)",
        group="identification",
    )

    # =========================================================================
    # Fuel Composition (for simple chemistry custom fuels)
    # These parameters are shared with SPEC namelist and can be used on REAC
    # to define fuel composition when not using a predefined fuel.
    # =========================================================================
    c: float | None = FdsField(
        None,
        fds_name="C",
        ge=0,
        description="Number of carbon atoms in fuel molecule",
        group="composition",
    )

    h: float | None = FdsField(
        None,
        fds_name="H",
        ge=0,
        description="Number of hydrogen atoms in fuel molecule",
        group="composition",
    )

    o: float | None = FdsField(
        None,
        fds_name="O",
        ge=0,
        description="Number of oxygen atoms in fuel molecule",
        group="composition",
    )

    n: float | None = FdsField(
        None,
        fds_name="N",
        ge=0,
        description="Number of nitrogen atoms in fuel molecule",
        group="composition",
    )

    # =========================================================================
    # Energy Parameters
    # =========================================================================
    heat_of_combustion: float | None = FdsField(
        None,
        fds_name="HEAT_OF_COMBUSTION",
        gt=0,
        description="Heat of combustion [kJ/kg]",
        group="energy",
    )

    hoc_complete: float | None = FdsField(
        None,
        fds_name="HOC_COMPLETE",
        gt=0,
        description="Complete heat of combustion [kJ/kg]",
        group="energy",
    )

    epumo2: float | None = FdsField(
        None,
        fds_name="EPUMO2",
        gt=0,
        description="Energy per unit mass of O2 consumed [kJ/kg] (default: 13100)",
        group="energy",
    )

    ideal: bool | None = FdsField(
        None,
        fds_name="IDEAL",
        description="Use ideal heat of combustion (default: False)",
        group="energy",
    )

    radiative_fraction: float | None = FdsField(
        None,
        fds_name="RADIATIVE_FRACTION",
        ge=0,
        le=1,
        description="Fraction of energy radiated",
        group="energy",
    )

    ramp_chi_r: str | None = FdsField(
        None,
        fds_name="RAMP_CHI_R",
        description="Ramp ID for time-varying radiative fraction",
        group="energy",
    )

    # =========================================================================
    # Product Yields
    # =========================================================================
    soot_yield: float | None = FdsField(
        None,
        fds_name="SOOT_YIELD",
        ge=0,
        le=1,
        description="Soot yield [kg soot/kg fuel] (default: 0)",
        group="yields",
    )

    co_yield: float | None = FdsField(
        None,
        fds_name="CO_YIELD",
        ge=0,
        le=1,
        description="CO yield [kg CO/kg fuel] (default: 0)",
        group="yields",
    )

    hcn_yield: float | None = FdsField(
        None,
        fds_name="HCN_YIELD",
        ge=0,
        le=1,
        description="HCN yield [kg/kg fuel] (default: 0)",
        group="yields",
    )

    # =========================================================================
    # Two-Step Chemistry / Incomplete Combustion
    # =========================================================================
    n_simple_chemistry_reactions: int | None = FdsField(
        None,
        fds_name="N_SIMPLE_CHEMISTRY_REACTIONS",
        ge=1,
        le=2,
        description="Number of simple chemistry reactions (1 or 2, default: 1)",
        group="chemistry",
    )

    fuel_c_to_co_fraction: float | None = FdsField(
        None,
        fds_name="FUEL_C_TO_CO_FRACTION",
        ge=0,
        le=1,
        description="Fraction of fuel carbon converted to CO (default: 2/3)",
        group="chemistry",
    )

    fuel_h_to_h2_fraction: float | None = FdsField(
        None,
        fds_name="FUEL_H_TO_H2_FRACTION",
        ge=0,
        le=1,
        description="Fraction of fuel hydrogen converted to H2 (default: 0)",
        group="chemistry",
    )

    fuel_n_to_hcn_fraction: float | None = FdsField(
        None,
        fds_name="FUEL_N_TO_HCN_FRACTION",
        ge=0,
        le=1,
        description="Fraction of fuel nitrogen converted to HCN (default: 1/5)",
        group="chemistry",
    )

    fuel_radcal_id: str | None = FdsField(
        None,
        fds_name="FUEL_RADCAL_ID",
        description="RadCal species for fuel radiation absorption",
        group="chemistry",
    )

    # =========================================================================
    # Auto-Ignition Parameters
    # =========================================================================
    auto_ignition_temperature: float | None = FdsField(
        None,
        fds_name="AUTO_IGNITION_TEMPERATURE",
        description="Auto-ignition temperature [°C] (default: -273)",
        group="ignition",
    )

    ait_exclusion_zone: list[tuple[float, float, float, float, float, float]] | None = FdsField(
        None,
        fds_name="AIT_EXCLUSION_ZONE",
        description="XB bounds for auto-ignition exclusion zones (list of 6-tuples)",
        group="ignition",
    )

    ait_exclusion_zone_ctrl_id: list[str] | None = FdsField(
        None,
        fds_name="AIT_EXCLUSION_ZONE_CTRL_ID",
        description="Control IDs for exclusion zones",
        group="ignition",
    )

    ait_exclusion_zone_devc_id: list[str] | None = FdsField(
        None,
        fds_name="AIT_EXCLUSION_ZONE_DEVC_ID",
        description="Device IDs for exclusion zones",
        group="ignition",
    )

    ait_exclusion_zone_temperature: list[float] | None = FdsField(
        None,
        fds_name="AIT_EXCLUSION_ZONE_TEMPERATURE",
        description="Temperatures for exclusion zones [°C]",
        group="ignition",
    )

    # =========================================================================
    # Extinction Parameters
    # =========================================================================
    critical_flame_temperature: float | None = FdsField(
        None,
        fds_name="CRITICAL_FLAME_TEMPERATURE",
        description="Critical flame temperature for extinction [°C] (default: 1427)",
        group="extinction",
    )

    lower_oxygen_limit: float | None = FdsField(
        None,
        fds_name="LOWER_OXYGEN_LIMIT",
        ge=0,
        le=1,
        description="Lower oxygen limit for extinction [mol/mol]",
        group="extinction",
    )

    # =========================================================================
    # Validation Parameters
    # =========================================================================
    check_atom_balance: bool | None = FdsField(
        None,
        fds_name="CHECK_ATOM_BALANCE",
        description="Check atom balance in reaction (default: True)",
        group="validation",
    )

    reac_atom_error: float | None = FdsField(
        None,
        fds_name="REAC_ATOM_ERROR",
        gt=0,
        description="Atom balance error tolerance [atoms] (default: 1e-4)",
        group="validation",
    )

    reac_mass_error: float | None = FdsField(
        None,
        fds_name="REAC_MASS_ERROR",
        gt=0,
        description="Mass balance error tolerance [kg/kg] (default: 1e-4)",
        group="validation",
    )

    # =========================================================================
    # Species Stoichiometry
    # =========================================================================
    spec_id_nu: list[str] | None = FdsField(
        None,
        fds_name="SPEC_ID_NU",
        description="Species IDs for stoichiometric coefficients",
        group="species",
    )

    nu: list[float] | None = FdsField(
        None,
        fds_name="NU",
        description="Stoichiometric coefficients (negative for reactants, positive for products)",
        group="species",
    )

    # =========================================================================
    # Finite-Rate Kinetics (Arrhenius)
    # =========================================================================
    a: float | None = FdsField(
        None,
        fds_name="A",
        gt=0,
        description="Pre-exponential factor",
        group="kinetics",
    )

    e: float | None = FdsField(
        None,
        fds_name="E",
        ge=0,
        description="Activation energy [J/mol]",
        group="kinetics",
    )

    n_t: float | None = FdsField(
        None,
        fds_name="N_T",
        description="Temperature exponent in rate equation",
        group="kinetics",
    )

    spec_id_n_s: list[str] | None = FdsField(
        None,
        fds_name="SPEC_ID_N_S",
        description="Species IDs for concentration exponents",
        group="kinetics",
    )

    n_s: list[float] | None = FdsField(
        None,
        fds_name="N_S",
        description="Concentration exponents for each species",
        group="kinetics",
    )

    equation: str | None = FdsField(
        None,
        fds_name="EQUATION",
        description="Reaction equation in text form (e.g., 'CH4+2*O2=CO2+2*H2O')",
        group="kinetics",
    )

    reactype: str | None = FdsField(
        None,
        fds_name="REACTYPE",
        description="Reaction type (default: 'ARRHENIUS-TYPE')",
        group="kinetics",
    )

    priority: int | None = FdsField(
        None,
        fds_name="PRIORITY",
        ge=1,
        description="Reaction priority for multi-step schemes (default: 1)",
        group="kinetics",
    )

    reverse: bool | None = FdsField(
        None,
        fds_name="REVERSE",
        description="Enable reversible reaction (default: False)",
        group="kinetics",
    )

    # =========================================================================
    # Falloff Reaction Parameters (Lindemann, Troe)
    # =========================================================================
    a_low_pr: float | None = FdsField(
        None,
        fds_name="A_LOW_PR",
        description="Low-pressure limit pre-exponential factor for falloff",
        group="falloff",
    )

    e_low_pr: float | None = FdsField(
        None,
        fds_name="E_LOW_PR",
        description="Low-pressure limit activation energy [J/mol] for falloff",
        group="falloff",
    )

    a_troe: float | None = FdsField(
        None,
        fds_name="A_TROE",
        description="Troe parameter A",
        group="falloff",
    )

    t1_troe: float | None = FdsField(
        None,
        fds_name="T1_TROE",
        description="Troe parameter T1 [K]",
        group="falloff",
    )

    t2_troe: float | None = FdsField(
        None,
        fds_name="T2_TROE",
        description="Troe parameter T2 [K]",
        group="falloff",
    )

    t3_troe: float | None = FdsField(
        None,
        fds_name="T3_TROE",
        description="Troe parameter T3 [K]",
        group="falloff",
    )

    # =========================================================================
    # Third Body Reaction Parameters
    # =========================================================================
    third_body: bool | None = FdsField(
        None,
        fds_name="THIRD_BODY",
        description="Enable third body reaction (default: False)",
        group="third_body",
    )

    third_eff: list[float] | None = FdsField(
        None,
        fds_name="THIRD_EFF",
        description="Third body efficiencies",
        group="third_body",
    )

    third_eff_id: list[str] | None = FdsField(
        None,
        fds_name="THIRD_EFF_ID",
        description="Species IDs for third body efficiencies",
        group="third_body",
    )

    # =========================================================================
    # Validators
    # =========================================================================
    @model_validator(mode="after")
    def validate_reaction(self) -> "Reaction":
        """Validate reaction parameters."""
        # Check yields sum
        total_yield = 0.0
        if self.soot_yield is not None:
            total_yield += self.soot_yield
        if self.co_yield is not None:
            total_yield += self.co_yield
        if self.hcn_yield is not None:
            total_yield += self.hcn_yield
        if total_yield > 1.0:
            raise ValueError(f"Sum of product yields ({total_yield:.2f}) exceeds 1.0")

        # Validate species stoichiometry arrays match
        if (
            self.spec_id_nu is not None
            and self.nu is not None
            and len(self.spec_id_nu) != len(self.nu)
        ):
            raise ValueError("SPEC_ID_NU and NU must have same length")

        # Validate concentration exponent arrays match
        if (
            self.spec_id_n_s is not None
            and self.n_s is not None
            and len(self.spec_id_n_s) != len(self.n_s)
        ):
            raise ValueError("SPEC_ID_N_S and N_S must have same length")

        # Validate third body arrays match
        if (
            self.third_eff is not None
            and self.third_eff_id is not None
            and len(self.third_eff) != len(self.third_eff_id)
        ):
            raise ValueError("THIRD_EFF and THIRD_EFF_ID must have same length")

        # Validate auto-ignition exclusion zone arrays
        if self.ait_exclusion_zone is not None:
            for zone in self.ait_exclusion_zone:
                if len(zone) != 6:
                    raise ValueError(
                        "Each AIT_EXCLUSION_ZONE must have exactly 6 values (XB bounds)"
                    )

        # Equation format validation
        if self.equation is not None and "=" not in self.equation:
            raise ValueError("EQUATION must contain '=' separating reactants and products")

        return self

    def _collect_fds_params(self) -> dict:
        """Collect FDS parameters including special formatting."""
        params = super()._collect_fds_params()

        # Format AIT_EXCLUSION_ZONE as 2D array
        if self.ait_exclusion_zone is not None:
            # FDS expects AIT_EXCLUSION_ZONE(6,:) format
            # Output as: AIT_EXCLUSION_ZONE(1:6,1)=x1,x2,y1,y2,z1,z2
            for i, zone in enumerate(self.ait_exclusion_zone, 1):
                key = f"AIT_EXCLUSION_ZONE(1:6,{i})"
                params[key] = self._format_value(list(zone))
            # Remove the default key if present
            params.pop("AIT_EXCLUSION_ZONE", None)

        return params
