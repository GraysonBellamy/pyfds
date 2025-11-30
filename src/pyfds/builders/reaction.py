"""Builder for creating REAC namelists with predefined fuel database."""

from ..core.namelists import Reaction
from .base import Builder

# Predefined fuel database with common fuels
FUEL_DATABASE = {
    # Gases
    "METHANE": {"c": 1, "h": 4, "o": 0, "n": 0, "hoc": 50000, "soot_yield": 0.001},
    "ETHANE": {"c": 2, "h": 6, "o": 0, "n": 0, "hoc": 47800, "soot_yield": 0.005},
    "PROPANE": {"c": 3, "h": 8, "o": 0, "n": 0, "hoc": 46000, "soot_yield": 0.010},
    "BUTANE": {"c": 4, "h": 10, "o": 0, "n": 0, "hoc": 45700, "soot_yield": 0.015},
    "HYDROGEN": {"c": 0, "h": 2, "o": 0, "n": 0, "hoc": 120000, "soot_yield": 0.0},
    # Liquids
    "N-HEPTANE": {"c": 7, "h": 16, "o": 0, "n": 0, "hoc": 44600, "soot_yield": 0.037},
    "N-HEXANE": {"c": 6, "h": 14, "o": 0, "n": 0, "hoc": 45000, "soot_yield": 0.030},
    "GASOLINE": {"c": 8, "h": 18, "o": 0, "n": 0, "hoc": 43700, "soot_yield": 0.059},
    "ACETONE": {"c": 3, "h": 6, "o": 1, "n": 0, "hoc": 25800, "soot_yield": 0.014},
    "METHANOL": {"c": 1, "h": 4, "o": 1, "n": 0, "hoc": 20000, "soot_yield": 0.001},
    "ETHANOL": {"c": 2, "h": 6, "o": 1, "n": 0, "hoc": 26900, "soot_yield": 0.008},
    # Solids/Polymers
    "POLYURETHANE": {"c": 3.52, "h": 5.48, "o": 0.88, "n": 0.32, "hoc": 23200, "soot_yield": 0.100},
    "WOOD": {"c": 3.4, "h": 6.2, "o": 2.5, "n": 0, "hoc": 17200, "soot_yield": 0.015},
    "PMMA": {"c": 5, "h": 8, "o": 2, "n": 0, "hoc": 25200, "soot_yield": 0.022},
    "POLYSTYRENE": {"c": 8, "h": 8, "o": 0, "n": 0, "hoc": 39900, "soot_yield": 0.060},
    "POLYETHYLENE": {"c": 2, "h": 4, "o": 0, "n": 0, "hoc": 43600, "soot_yield": 0.060},
    "POLYPROPYLENE": {"c": 3, "h": 6, "o": 0, "n": 0, "hoc": 43200, "soot_yield": 0.059},
}


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

    >>> # Custom fuel composition
    >>> reac = ReactionBuilder() \\
    ...     .custom_fuel(c=7, h=16, heat_of_combustion=44600) \\
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
        self._soot_yield: float = 0.01
        self._co_yield: float = 0.0
        self._radiative_fraction: float | None = None
        self._auto_ignition_temp: float | None = None

        # Extinction and suppression parameters
        self._extinction_model: str | None = None
        self._critical_flame_temp: float | None = None
        self._suppression: bool = False
        self._k_suppression: float | None = None
        self._ideal: bool = True
        self._spec_id_nu: list[str] = []
        self._nu: list[float] = []
        self._fixed_mix_time: float | None = None
        self._tau_chem: float | None = None
        self._tau_flame: float | None = None

        # Advanced combustion parameters
        self._id: str | None = None
        self._hcn_yield: float = 0.0
        self._epumo2: float | None = None
        self._hoc_complete: float | None = None
        self._n_simple_chemistry_reactions: int = 1
        self._fuel_c_to_co_fraction: float = 0.0
        self._fuel_n_to_hcn_fraction: float = 0.0
        self._fuel_h_to_h2_fraction: float = 0.0
        self._check_atom_balance: bool = True
        self._reac_atom_error: float = 1e-4
        self._reac_mass_error: float = 1e-4
        self._lower_oxygen_limit: float = 0.0
        self._ait_exclusion_zone: tuple[float, ...] | None = None
        self._ait_exclusion_zone_temperature: float | None = None
        self._ait_exclusion_zone_devc_id: str | None = None
        self._ait_exclusion_zone_ctrl_id: str | None = None

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
        self._c = fuel_data["c"]
        self._h = fuel_data["h"]
        self._o = fuel_data["o"]
        self._n = fuel_data["n"]
        self._hoc = fuel_data["hoc"]
        # Use database soot yield as default, can be overridden
        if "soot_yield" in fuel_data:
            self._soot_yield = fuel_data["soot_yield"]

        return self

    def custom_fuel(
        self,
        c: float,
        h: float,
        o: float = 0,
        n: float = 0,
        heat_of_combustion: float | None = None,
    ) -> "ReactionBuilder":
        """
        Define custom fuel composition.

        Parameters
        ----------
        c : float
            Number of carbon atoms
        h : float
            Number of hydrogen atoms
        o : float, optional
            Number of oxygen atoms, default: 0
        n : float, optional
            Number of nitrogen atoms, default: 0
        heat_of_combustion : float, optional
            Heat of combustion in kJ/kg

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder().custom_fuel(
        ...     c=7, h=16, heat_of_combustion=44600
        ... ).build()
        """
        self._c = c
        self._h = h
        self._o = o
        self._n = n
        self._hoc = heat_of_combustion
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

    def yields(self, soot: float = 0.01, co: float = 0.0) -> "ReactionBuilder":
        """
        Set both soot and CO yields.

        Parameters
        ----------
        soot : float, optional
            Soot yield, default: 0.01
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

    def with_extinction(self, model: str, critical_temp: float | None = None) -> "ReactionBuilder":
        """
        Configure extinction model.

        Parameters
        ----------
        model : str
            Extinction model: 'EXTINCTION_1' or 'EXTINCTION_2'
        critical_temp : float, optional
            Critical flame temperature for extinction (K)

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('PROPANE') \\
        ...     .with_extinction('EXTINCTION_1', critical_temp=1200.0) \\
        ...     .build()
        """
        self._extinction_model = model.upper()
        if critical_temp is not None:
            self._critical_flame_temp = critical_temp
        return self

    def with_suppression(self, k_suppression: float) -> "ReactionBuilder":
        """
        Enable suppression model.

        Parameters
        ----------
        k_suppression : float
            Suppression rate constant

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('WOOD') \\
        ...     .with_suppression(k_suppression=0.5) \\
        ...     .build()
        """
        self._suppression = True
        self._k_suppression = k_suppression
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

    def with_time_scales(
        self,
        fixed_mix_time: float | None = None,
        tau_chem: float | None = None,
        tau_flame: float | None = None,
    ) -> "ReactionBuilder":
        """
        Set reaction time scales.

        Parameters
        ----------
        fixed_mix_time : float, optional
            Fixed mixing time (s)
        tau_chem : float, optional
            Chemical time scale (s)
        tau_flame : float, optional
            Flame time scale (s)

        Returns
        -------
        ReactionBuilder
            Self for method chaining

        Examples
        --------
        >>> reac = ReactionBuilder() \\
        ...     .fuel('PROPANE') \\
        ...     .with_time_scales(tau_chem=0.1, tau_flame=0.5) \\
        ...     .build()
        """
        if fixed_mix_time is not None:
            self._fixed_mix_time = fixed_mix_time
        if tau_chem is not None:
            self._tau_chem = tau_chem
        if tau_flame is not None:
            self._tau_flame = tau_flame
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

    def yields_all(
        self, soot: float = 0.01, co: float = 0.0, hcn: float = 0.0
    ) -> "ReactionBuilder":
        """
        Set all product yields.

        Parameters
        ----------
        soot : float, optional
            Soot yield, default: 0.01
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
        zone_bounds: tuple[float, ...],
        temperature: float | None = None,
        device_id: str | None = None,
        control_id: str | None = None,
    ) -> "ReactionBuilder":
        """
        Configure auto-ignition exclusion zone.

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
        self._ait_exclusion_zone = zone_bounds
        if temperature is not None:
            self._ait_exclusion_zone_temperature = temperature
        if device_id is not None:
            self._ait_exclusion_zone_devc_id = device_id
        if control_id is not None:
            self._ait_exclusion_zone_ctrl_id = control_id
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

    def build(self) -> Reaction:
        """
        Build the Reaction object.

        Returns
        -------
        Reaction
            The constructed Reaction namelist object

        Raises
        ------
        ValueError
            If fuel composition is not defined
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        # Validate that we have fuel composition
        if self._c is None or self._h is None:
            raise ValueError(
                "ReactionBuilder: Must specify fuel composition using "
                "fuel() or custom_fuel() before building"
            )

        params: dict = {
            "soot_yield": self._soot_yield,
            "co_yield": self._co_yield,
        }

        # Add fuel name if specified
        if self._fuel_name is not None:
            params["fuel"] = self._fuel_name

        # Include c and h (can be 0 for fuels like hydrogen)
        if self._c is not None and self._c > 0:
            params["c"] = self._c
        if self._h is not None:
            params["h"] = self._h
        # Only include o and n if > 0
        if self._o is not None and self._o > 0:
            params["o"] = self._o
        if self._n is not None and self._n > 0:
            params["n"] = self._n
        if self._hoc:
            params["heat_of_combustion"] = self._hoc
        if self._radiative_fraction is not None:
            params["radiative_fraction"] = self._radiative_fraction
        if self._auto_ignition_temp is not None:
            params["auto_ignition_temperature"] = self._auto_ignition_temp

        # Extinction and suppression parameters
        if self._extinction_model:
            params["extinction_model"] = self._extinction_model
        if self._critical_flame_temp is not None:
            params["critical_flame_temperature"] = self._critical_flame_temp
        if self._suppression:
            params["suppression"] = self._suppression
        if self._k_suppression is not None:
            params["k_suppression"] = self._k_suppression
        if not self._ideal:
            params["ideal"] = self._ideal
        if self._spec_id_nu:
            params["spec_id_nu"] = self._spec_id_nu
        if self._nu:
            params["nu"] = self._nu
        if self._fixed_mix_time is not None:
            params["fixed_mix_time"] = self._fixed_mix_time
        if self._tau_chem is not None:
            params["tau_chem"] = self._tau_chem
        if self._tau_flame is not None:
            params["tau_flame"] = self._tau_flame

        # Advanced combustion parameters
        if self._id is not None:
            params["id"] = self._id
        if self._hcn_yield > 0:
            params["hcn_yield"] = self._hcn_yield
        if self._epumo2 is not None:
            params["epumo2"] = self._epumo2
        if self._hoc_complete is not None:
            params["hoc_complete"] = self._hoc_complete
        if self._n_simple_chemistry_reactions != 1:
            params["n_simple_chemistry_reactions"] = self._n_simple_chemistry_reactions
        if self._fuel_c_to_co_fraction > 0:
            params["fuel_c_to_co_fraction"] = self._fuel_c_to_co_fraction
        if self._fuel_n_to_hcn_fraction > 0:
            params["fuel_n_to_hcn_fraction"] = self._fuel_n_to_hcn_fraction
        if self._fuel_h_to_h2_fraction > 0:
            params["fuel_h_to_h2_fraction"] = self._fuel_h_to_h2_fraction
        if not self._check_atom_balance:
            params["check_atom_balance"] = self._check_atom_balance
        if self._reac_atom_error != 1e-4:
            params["reac_atom_error"] = self._reac_atom_error
        if self._reac_mass_error != 1e-4:
            params["reac_mass_error"] = self._reac_mass_error
        if self._lower_oxygen_limit > 0:
            params["lower_oxygen_limit"] = self._lower_oxygen_limit
        if self._ait_exclusion_zone is not None:
            params["ait_exclusion_zone"] = self._ait_exclusion_zone
        if self._ait_exclusion_zone_temperature is not None:
            params["ait_exclusion_zone_temperature"] = self._ait_exclusion_zone_temperature
        if self._ait_exclusion_zone_devc_id is not None:
            params["ait_exclusion_zone_devc_id"] = self._ait_exclusion_zone_devc_id
        if self._ait_exclusion_zone_ctrl_id is not None:
            params["ait_exclusion_zone_ctrl_id"] = self._ait_exclusion_zone_ctrl_id

        reaction = Reaction(**params)
        self._mark_built()
        return reaction

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
        return sorted(FUEL_DATABASE.keys())

    @classmethod
    def get_fuel_info(cls, name: str) -> dict:
        """
        Get information about a predefined fuel.

        Parameters
        ----------
        name : str
            Name of the fuel (case-insensitive)

        Returns
        -------
        dict
            Dictionary with fuel composition and properties

        Raises
        ------
        ValueError
            If fuel name is not in the database

        Examples
        --------
        >>> info = ReactionBuilder.get_fuel_info('PROPANE')
        >>> print(info)
        {'c': 3, 'h': 8, 'o': 0, 'n': 0, 'hoc': 46000, 'soot_yield': 0.010}
        """
        fuel_key = name.upper()
        if fuel_key not in FUEL_DATABASE:
            available = ", ".join(sorted(FUEL_DATABASE.keys()))
            raise ValueError(f"Unknown fuel '{name}'. Available fuels:\n{available}")

        return FUEL_DATABASE[fuel_key].copy()
