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
