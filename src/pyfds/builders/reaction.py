"""Builder for creating REAC namelists with predefined fuel database."""

from pyfds.builders.base import Builder
from pyfds.builders.libraries.fuels import (
    FUEL_DATABASE,
    FuelData,
    get_fuel_info,
    list_fuels,
)
from pyfds.core.namelists import Reaction


class ReactionBuilder(Builder[Reaction]):
    """
    Builder for creating REAC namelists.

    Provides convenient methods for defining combustion reactions using
    either predefined fuels from the database or custom fuel compositions.

    Examples
    --------
    >>> # Use predefined fuel
    >>> reac = ReactionBuilder() \\
    ...     .fuel('PROPANE') \\
    ...     .soot_yield(0.015) \\
    ...     .build()

    >>> # Custom fuel with heat of combustion
    >>> reac = ReactionBuilder() \\
    ...     .fuel('CUSTOM_FUEL') \\
    ...     .heat_of_combustion(44600) \\
    ...     .soot_yield(0.01) \\
    ...     .co_yield(0.02) \\
    ...     .radiative_fraction(0.33) \\
    ...     .build()

    >>> # Complex fuel with all parameters
    >>> reac = ReactionBuilder() \\
    ...     .fuel('POLYURETHANE') \\
    ...     .yields(soot=0.10, co=0.02) \\
    ...     .radiative_fraction(0.30) \\
    ...     .auto_ignition_temperature(350) \\
    ...     .build()
    """

    def __init__(self) -> None:
        """Initialize the ReactionBuilder."""
        super().__init__()
        self._fuel_name: str | None = None
        self._c: float | None = None
        self._h: float | None = None
        self._o: float | None = None
        self._n: float | None = None
        self._hoc: float | None = None
        self._soot_yield: float | None = None
        self._co_yield: float | None = None
        self._radiative_fraction: float | None = None
        self._ramp_chi_r: str | None = None
        self._auto_ignition_temp: float | None = None

        # Extinction parameters
        self._extinction_model: str | None = None
        self._critical_flame_temp: float | None = None
        self._lower_oxygen_limit: float | None = None
        self._ideal: bool | None = None

        # Species stoichiometry
        self._spec_id_nu: list[str] | None = None
        self._nu: list[float] | None = None

        # Advanced combustion parameters
        self._id: str | None = None
        self._hcn_yield: float | None = None
        self._epumo2: float | None = None
        self._hoc_complete: float | None = None
        self._n_simple_chemistry_reactions: int | None = None
        self._fuel_c_to_co_fraction: float | None = None
        self._fuel_n_to_hcn_fraction: float | None = None
        self._fuel_h_to_h2_fraction: float | None = None
        self._fuel_radcal_id: str | None = None
        self._check_atom_balance: bool | None = None
        self._reac_atom_error: float | None = None
        self._reac_mass_error: float | None = None

        # Auto-ignition exclusion zones
        self._ait_exclusion_zone: list[tuple[float, float, float, float, float, float]] | None = (
            None
        )
        self._ait_exclusion_zone_temperature: list[float] | None = None
        self._ait_exclusion_zone_devc_id: list[str] | None = None
        self._ait_exclusion_zone_ctrl_id: list[str] | None = None

        # Finite-rate kinetics
        self._a: float | None = None
        self._e: float | None = None
        self._n_t: float | None = None
        self._spec_id_n_s: list[str] | None = None
        self._n_s: list[float] | None = None
        self._equation: str | None = None
        self._reactype: str | None = None
        self._priority: int | None = None
        self._reverse: bool | None = None

        # Falloff parameters
        self._a_low_pr: float | None = None
        self._e_low_pr: float | None = None
        self._a_troe: float | None = None
        self._t1_troe: float | None = None
        self._t2_troe: float | None = None
        self._t3_troe: float | None = None

        # Third body
        self._third_body: bool | None = None
        self._third_eff: list[float] | None = None
        self._third_eff_id: list[str] | None = None

    def fuel(self, name: str) -> "ReactionBuilder":
        """
        Use a predefined fuel from the database.

        Parameters
        ----------
        name : str
            Name of the fuel (case-insensitive)

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Raises
        ------
        ValueError
            If fuel name is not in the database

        Examples
        --------
        >>> reac = ReactionBuilder().fuel('PROPANE').build()

        Notes
        -----
        Available fuels: METHANE, ETHANE, PROPANE, BUTANE, HYDROGEN,
        N-HEPTANE, N-HEXANE, GASOLINE, ACETONE, METHANOL, ETHANOL,
        POLYURETHANE, WOOD, PMMA, POLYSTYRENE, POLYETHYLENE, POLYPROPYLENE
        """
        fuel_key = name.upper()
        if fuel_key not in FUEL_DATABASE:
            available = ", ".join(sorted(FUEL_DATABASE.keys()))
            raise ValueError(f"Unknown fuel '{name}'. Available fuels:\n{available}")

        fuel_data = FUEL_DATABASE[fuel_key]
        self._fuel_name = fuel_key

        # Get fuel composition from database
        if "c" in fuel_data:
            self._c = float(fuel_data["c"])
        if "h" in fuel_data:
            self._h = float(fuel_data["h"])
        if "o" in fuel_data:
            self._o = float(fuel_data["o"])
        if "n" in fuel_data:
            self._n = float(fuel_data["n"])
        # Get heat of combustion from database
        if "hoc" in fuel_data:
            self._hoc = float(fuel_data["hoc"])
        # Use database soot yield as default, can be overridden
        if "soot_yield" in fuel_data:
            self._soot_yield = float(fuel_data["soot_yield"])
        # Use database co_yield if available
        if "co_yield" in fuel_data:
            self._co_yield = float(fuel_data["co_yield"])

        return self

    def predefined_fuel(self, name: str) -> "ReactionBuilder":
        """
        Use a predefined fuel from the database.

        This is an alias for fuel() method.

        Parameters
        ----------
        name : str
            Name of the fuel (case-insensitive)

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        return self.fuel(name)

    def custom_fuel(
        self,
        c: float | None = None,
        h: float | None = None,
        o: float | None = None,
        n: float | None = None,
        heat_of_combustion: float | None = None,
    ) -> "ReactionBuilder":
        """
        Define a custom fuel composition.

        For simple chemistry, you can specify the fuel composition
        using the atom counts C, H, O, N. If using a predefined fuel,
        you can just specify heat_of_combustion to override the default.

        Parameters
        ----------
        c : float, optional
            Number of carbon atoms in fuel molecule
        h : float, optional
            Number of hydrogen atoms in fuel molecule
        o : float, optional
            Number of oxygen atoms in fuel molecule
        n : float, optional
            Number of nitrogen atoms in fuel molecule
        heat_of_combustion : float, optional
            Heat of combustion in kJ/kg

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> # Define heptane-like fuel
        >>> reac = ReactionBuilder() \\
        ...     .custom_fuel(c=7, h=16, heat_of_combustion=44600) \\
        ...     .build()

        >>> # Override heat of combustion for predefined fuel
        >>> reac = ReactionBuilder() \\
        ...     .predefined_fuel('HEPTANE') \\
        ...     .custom_fuel(heat_of_combustion=44000) \\
        ...     .build()
        """
        if c is not None:
            self._c = c
        if h is not None:
            self._h = h
        if o is not None:
            self._o = o
        if n is not None:
            self._n = n
        if heat_of_combustion is not None:
            self._hoc = heat_of_combustion
        return self

    def heat_of_combustion(self, value: float) -> "ReactionBuilder":
        """
        Set heat of combustion.

        Parameters
        ----------
        value : float
            Heat of combustion in kJ/kg

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._hoc = value
        return self

    def soot_yield(self, value: float) -> "ReactionBuilder":
        """
        Set soot yield.

        Parameters
        ----------
        value : float
            Soot yield in kg soot per kg fuel, range [0, 1]

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._soot_yield = value
        return self

    def co_yield(self, value: float) -> "ReactionBuilder":
        """
        Set CO yield.

        Parameters
        ----------
        value : float
            CO yield in kg CO per kg fuel, range [0, 1]

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._co_yield = value
        return self

    def yields(self, soot: float = 0.0, co: float = 0.0) -> "ReactionBuilder":
        """
        Set both soot and CO yields.

        Parameters
        ----------
        soot : float, optional
            Soot yield, default: 0.0
        co : float, optional
            CO yield, default: 0.0

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('PROPANE') \\
        ...     .yields(soot=0.015, co=0.02) \\
        ...     .build()
        """
        self._soot_yield = soot
        self._co_yield = co
        return self

    def radiative_fraction(self, value: float) -> "ReactionBuilder":
        """
        Set radiative fraction.

        Parameters
        ----------
        value : float
            Fraction of energy radiated, range [0, 1]
            Typical values: 0.3-0.4 for most fuels

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._radiative_fraction = value
        return self

    def ramp_chi_r(self, ramp_id: str) -> "ReactionBuilder":
        """
        Set ramp for time-varying radiative fraction.

        Parameters
        ----------
        ramp_id : str
            Ramp ID for radiative fraction

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._ramp_chi_r = ramp_id
        return self

    def auto_ignition_temperature(self, temp: float) -> "ReactionBuilder":
        """
        Set auto-ignition temperature.

        Parameters
        ----------
        temp : float
            Auto-ignition temperature in °C

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._auto_ignition_temp = temp
        return self

    def with_extinction(
        self, model: str | None = None, critical_temp: float | None = None
    ) -> "ReactionBuilder":
        """
        Configure extinction parameters.

        Note: The extinction MODEL (EXTINCTION 1/2) is set on MISC namelist, not REAC.
        This method sets the CRITICAL_FLAME_TEMPERATURE on REAC.

        Parameters
        ----------
        model : str, optional
            Extinction model: 'EXTINCTION 1' or 'EXTINCTION 2'.
            Note: This is informational only - set on MISC.
        critical_temp : float, optional
            Critical flame temperature for extinction (°C)

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('PROPANE') \\
        ...     .with_extinction(critical_temp=1200.0) \\
        ...     .build()
        """
        # Note: extinction_model is a MISC parameter, not REAC
        # We store it for documentation but don't pass to Reaction
        if model is not None:
            self._extinction_model = model.upper()
        if critical_temp is not None:
            self._critical_flame_temp = critical_temp
        return self

    def with_species_stoichiometry(
        self, species: list[str], coefficients: list[float]
    ) -> "ReactionBuilder":
        """
        Set species stoichiometry.

        Parameters
        ----------
        species : list[str]
            List of species IDs
        coefficients : list[float]
            List of stoichiometric coefficients

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('METHANE') \\
        ...     .with_species_stoichiometry(['CO2', 'H2O'], [1.0, 2.0]) \\
        ...     .build()
        """
        self._spec_id_nu = species
        self._nu = coefficients
        return self

    def use_non_ideal_hoc(self) -> "ReactionBuilder":
        """
        Use non-ideal heat of combustion.

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('PROPANE') \\
        ...     .use_non_ideal_hoc() \\
        ...     .build()
        """
        self._ideal = False
        return self

    # Advanced combustion methods
    def reaction_id(self, id: str) -> "ReactionBuilder":
        """
        Set reaction identifier.

        Parameters
        ----------
        id : str
            Reaction identifier

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._id = id
        return self

    def hcn_yield(self, value: float) -> "ReactionBuilder":
        """
        Set HCN yield.

        Parameters
        ----------
        value : float
            HCN yield in kg HCN per kg fuel, range [0, 1]

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._hcn_yield = value
        return self

    def energy_per_o2(self, epumo2: float) -> "ReactionBuilder":
        """
        Set energy per unit mass of O2 consumed.

        Parameters
        ----------
        epumo2 : float
            Energy per unit mass of O2 [kJ/kg]

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._epumo2 = epumo2
        return self

    def complete_heat_of_combustion(self, hoc_complete: float) -> "ReactionBuilder":
        """
        Set complete heat of combustion.

        Parameters
        ----------
        hoc_complete : float
            Complete heat of combustion [kJ/kg]

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._hoc_complete = hoc_complete
        return self

    def with_two_step_chemistry(
        self, co_fraction: float = 0.1, hcn_fraction: float = 0.0, h2_fraction: float = 0.0
    ) -> "ReactionBuilder":
        """
        Enable two-step chemistry model.

        Parameters
        ----------
        co_fraction : float, optional
            Fraction of fuel carbon converted to CO, default: 0.1
        hcn_fraction : float, optional
            Fraction of fuel nitrogen converted to HCN, default: 0.0
        h2_fraction : float, optional
            Fraction of fuel hydrogen converted to H2, default: 0.0

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('WOOD') \\
        ...     .with_two_step_chemistry(co_fraction=0.15, hcn_fraction=0.001) \\
        ...     .build()
        """
        self._n_simple_chemistry_reactions = 2
        self._fuel_c_to_co_fraction = co_fraction
        self._fuel_n_to_hcn_fraction = hcn_fraction
        self._fuel_h_to_h2_fraction = h2_fraction
        return self

    def yields_all(self, soot: float = 0.0, co: float = 0.0, hcn: float = 0.0) -> "ReactionBuilder":
        """
        Set all product yields.

        Parameters
        ----------
        soot : float, optional
            Soot yield, default: 0.0
        co : float, optional
            CO yield, default: 0.0
        hcn : float, optional
            HCN yield, default: 0.0

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('POLYURETHANE') \\
        ...     .yields_all(soot=0.10, co=0.02, hcn=0.001) \\
        ...     .build()
        """
        self._soot_yield = soot
        self._co_yield = co
        self._hcn_yield = hcn
        return self

    def with_extinction_limit(self, lower_o2: float) -> "ReactionBuilder":
        """
        Set lower oxygen limit for extinction.

        Parameters
        ----------
        lower_o2 : float
            Lower oxygen index for extinction, range [0, 1]

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._lower_oxygen_limit = lower_o2
        return self

    def with_auto_ignition_exclusion(
        self,
        zone_bounds: tuple[float, float, float, float, float, float],
        temperature: float | None = None,
        device_id: str | None = None,
        control_id: str | None = None,
    ) -> "ReactionBuilder":
        """
        Add an auto-ignition exclusion zone.

        Parameters
        ----------
        zone_bounds : tuple[float, ...]
            XB bounds for exclusion zone (6 values)
        temperature : float, optional
            Temperature above which ignition is allowed [°C]
        device_id : str, optional
            Device to control exclusion zone
        control_id : str, optional
            Control logic for exclusion zone

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('PROPANE') \\
        ...     .with_auto_ignition_exclusion(
        ...         zone_bounds=(0, 1, 0, 1, 0, 1),
        ...         temperature=300.0
        ...     ) \\
        ...     .build()
        """
        if self._ait_exclusion_zone is None:
            self._ait_exclusion_zone = []
        if self._ait_exclusion_zone_temperature is None:
            self._ait_exclusion_zone_temperature = []
        if self._ait_exclusion_zone_devc_id is None:
            self._ait_exclusion_zone_devc_id = []
        if self._ait_exclusion_zone_ctrl_id is None:
            self._ait_exclusion_zone_ctrl_id = []

        self._ait_exclusion_zone.append(zone_bounds)
        if temperature is not None:
            self._ait_exclusion_zone_temperature.append(temperature)
        if device_id is not None:
            self._ait_exclusion_zone_devc_id.append(device_id)
        if control_id is not None:
            self._ait_exclusion_zone_ctrl_id.append(control_id)
        return self

    def disable_atom_balance_check(self) -> "ReactionBuilder":
        """
        Disable atom balance checking.

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._check_atom_balance = False
        return self

    def with_finite_rate_kinetics(
        self,
        a: float,
        e: float,
        n_t: float = 0.0,
        species: list[str] | None = None,
        exponents: list[float] | None = None,
    ) -> "ReactionBuilder":
        """
        Configure finite-rate Arrhenius kinetics.

        Parameters
        ----------
        a : float
            Pre-exponential factor
        e : float
            Activation energy [J/mol]
        n_t : float, optional
            Temperature exponent, default: 0.0
        species : list[str], optional
            Species IDs for concentration exponents
        exponents : list[float], optional
            Concentration exponents for each species

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._a = a
        self._e = e
        self._n_t = n_t
        if species is not None:
            self._spec_id_n_s = species
        if exponents is not None:
            self._n_s = exponents
        return self

    def with_third_body(
        self, efficiencies: list[float] | None = None, species: list[str] | None = None
    ) -> "ReactionBuilder":
        """
        Enable third body reaction.

        Parameters
        ----------
        efficiencies : list[float], optional
            Third body efficiencies
        species : list[str], optional
            Species IDs for third body efficiencies

        Returns
        -------
        ReactionBuilder
            Self for method chaining
        """
        self._third_body = True
        if efficiencies is not None:
            self._third_eff = efficiencies
        if species is not None:
            self._third_eff_id = species
        return self

    def _validate(self) -> list[str]:
        """Validate builder state before building."""
        errors = []
        # Either fuel name or custom composition (c/h) must be specified
        has_fuel_name = self._fuel_name is not None
        has_custom_composition = self._c is not None or self._h is not None
        if not has_fuel_name and not has_custom_composition:
            errors.append(
                "Must specify fuel composition using fuel() or custom_fuel() before building"
            )
        return errors

    def _create(self) -> Reaction:
        """Create the Reaction object."""
        params: dict = {}

        # Add fuel name
        if self._fuel_name is not None:
            params["fuel"] = self._fuel_name

        # Fuel composition
        # Only include c if >0 (FDS requires c>0 for simple chemistry,
        # so c=0 means fuel has no carbon and should not be output)
        if self._c is not None and self._c > 0:
            params["c"] = self._c
        if self._h is not None:
            params["h"] = self._h
        if self._o is not None and self._o > 0:
            params["o"] = self._o
        if self._n is not None and self._n > 0:
            params["n"] = self._n

        # Energy parameters
        if self._hoc is not None:
            params["heat_of_combustion"] = self._hoc
        if self._radiative_fraction is not None:
            params["radiative_fraction"] = self._radiative_fraction
        if self._ramp_chi_r is not None:
            params["ramp_chi_r"] = self._ramp_chi_r
        if self._ideal is not None:
            params["ideal"] = self._ideal
        if self._epumo2 is not None:
            params["epumo2"] = self._epumo2
        if self._hoc_complete is not None:
            params["hoc_complete"] = self._hoc_complete

        # Yields
        if self._soot_yield is not None:
            params["soot_yield"] = self._soot_yield
        if self._co_yield is not None:
            params["co_yield"] = self._co_yield
        if self._hcn_yield is not None:
            params["hcn_yield"] = self._hcn_yield

        # Ignition
        if self._auto_ignition_temp is not None:
            params["auto_ignition_temperature"] = self._auto_ignition_temp

        # Extinction parameters (note: EXTINCTION_MODEL is on MISC, not REAC)
        if self._critical_flame_temp is not None:
            params["critical_flame_temperature"] = self._critical_flame_temp
        if self._lower_oxygen_limit is not None:
            params["lower_oxygen_limit"] = self._lower_oxygen_limit

        # Species stoichiometry
        if self._spec_id_nu is not None:
            params["spec_id_nu"] = self._spec_id_nu
        if self._nu is not None:
            params["nu"] = self._nu

        # Advanced combustion parameters
        if self._id is not None:
            params["id"] = self._id
        if self._n_simple_chemistry_reactions is not None:
            params["n_simple_chemistry_reactions"] = self._n_simple_chemistry_reactions
        if self._fuel_c_to_co_fraction is not None:
            params["fuel_c_to_co_fraction"] = self._fuel_c_to_co_fraction
        if self._fuel_n_to_hcn_fraction is not None:
            params["fuel_n_to_hcn_fraction"] = self._fuel_n_to_hcn_fraction
        if self._fuel_h_to_h2_fraction is not None:
            params["fuel_h_to_h2_fraction"] = self._fuel_h_to_h2_fraction
        if self._fuel_radcal_id is not None:
            params["fuel_radcal_id"] = self._fuel_radcal_id
        if self._check_atom_balance is not None:
            params["check_atom_balance"] = self._check_atom_balance
        if self._reac_atom_error is not None:
            params["reac_atom_error"] = self._reac_atom_error
        if self._reac_mass_error is not None:
            params["reac_mass_error"] = self._reac_mass_error

        # Auto-ignition exclusion zones
        if self._ait_exclusion_zone:
            params["ait_exclusion_zone"] = self._ait_exclusion_zone
        if self._ait_exclusion_zone_temperature:
            params["ait_exclusion_zone_temperature"] = self._ait_exclusion_zone_temperature
        if self._ait_exclusion_zone_devc_id:
            params["ait_exclusion_zone_devc_id"] = self._ait_exclusion_zone_devc_id
        if self._ait_exclusion_zone_ctrl_id:
            params["ait_exclusion_zone_ctrl_id"] = self._ait_exclusion_zone_ctrl_id

        # Finite-rate kinetics
        if self._a is not None:
            params["a"] = self._a
        if self._e is not None:
            params["e"] = self._e
        if self._n_t is not None:
            params["n_t"] = self._n_t
        if self._spec_id_n_s is not None:
            params["spec_id_n_s"] = self._spec_id_n_s
        if self._n_s is not None:
            params["n_s"] = self._n_s
        if self._equation is not None:
            params["equation"] = self._equation
        if self._reactype is not None:
            params["reactype"] = self._reactype
        if self._priority is not None:
            params["priority"] = self._priority
        if self._reverse is not None:
            params["reverse"] = self._reverse

        # Falloff parameters
        if self._a_low_pr is not None:
            params["a_low_pr"] = self._a_low_pr
        if self._e_low_pr is not None:
            params["e_low_pr"] = self._e_low_pr
        if self._a_troe is not None:
            params["a_troe"] = self._a_troe
        if self._t1_troe is not None:
            params["t1_troe"] = self._t1_troe
        if self._t2_troe is not None:
            params["t2_troe"] = self._t2_troe
        if self._t3_troe is not None:
            params["t3_troe"] = self._t3_troe

        # Third body
        if self._third_body is not None:
            params["third_body"] = self._third_body
        if self._third_eff is not None:
            params["third_eff"] = self._third_eff
        if self._third_eff_id is not None:
            params["third_eff_id"] = self._third_eff_id

        return Reaction(**params)

    @classmethod
    def list_fuels(cls) -> list[str]:
        """
        List all available predefined fuels.

        Returns
        -------
        list[str]
            Sorted list of fuel names

        Examples
        --------
        >>> fuels = ReactionBuilder.list_fuels()
        >>> print(fuels)
        ['ACETONE', 'BUTANE', 'ETHANE', ...]
        """
        return list_fuels()

    @classmethod
    def get_fuel_info(cls, name: str) -> FuelData:
        """
        Get information about a predefined fuel.

        Parameters
        ----------
        name : str
            Name of the fuel (case-insensitive)

        Returns
        -------
        FuelData
            Dictionary with fuel composition and properties

        Raises
        ------
        ValueError
            If fuel name is not in the database

        Examples
        --------
        >>> info = ReactionBuilder.get_fuel_info('PROPANE')
        >>> print(info)
        {'c': 3, 'h': 8, 'o': 0, 'n': 0, 'hoc': 46000, 'soot_yield': 0.010, ...}
        """
        return get_fuel_info(name)
