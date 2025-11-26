"""Predefined fuel database for common combustion reactions."""

# Comprehensive fuel database
FUEL_DATABASE = {
    # Gases
    "METHANE": {
        "c": 1,
        "h": 4,
        "o": 0,
        "n": 0,
        "hoc": 50000,
        "soot_yield": 0.001,
        "co_yield": 0.0,
        "description": "Natural gas primary component",
    },
    "ETHANE": {
        "c": 2,
        "h": 6,
        "o": 0,
        "n": 0,
        "hoc": 47800,
        "soot_yield": 0.005,
        "co_yield": 0.0,
        "description": "Hydrocarbon gas",
    },
    "PROPANE": {
        "c": 3,
        "h": 8,
        "o": 0,
        "n": 0,
        "hoc": 46000,
        "soot_yield": 0.010,
        "co_yield": 0.0,
        "description": "LPG, common fuel gas",
    },
    "BUTANE": {
        "c": 4,
        "h": 10,
        "o": 0,
        "n": 0,
        "hoc": 45700,
        "soot_yield": 0.015,
        "co_yield": 0.0,
        "description": "Lighter fuel",
    },
    "HYDROGEN": {
        "c": 0,
        "h": 2,
        "o": 0,
        "n": 0,
        "hoc": 120000,
        "soot_yield": 0.0,
        "co_yield": 0.0,
        "description": "Clean burning fuel",
    },
    # Liquids
    "N-HEPTANE": {
        "c": 7,
        "h": 16,
        "o": 0,
        "n": 0,
        "hoc": 44600,
        "soot_yield": 0.037,
        "co_yield": 0.01,
        "description": "Hydrocarbon liquid, gasoline surrogate",
    },
    "N-HEXANE": {
        "c": 6,
        "h": 14,
        "o": 0,
        "n": 0,
        "hoc": 45000,
        "soot_yield": 0.030,
        "co_yield": 0.008,
        "description": "Solvent",
    },
    "GASOLINE": {
        "c": 8,
        "h": 18,
        "o": 0,
        "n": 0,
        "hoc": 43700,
        "soot_yield": 0.059,
        "co_yield": 0.01,
        "description": "Motor vehicle fuel",
    },
    "ACETONE": {
        "c": 3,
        "h": 6,
        "o": 1,
        "n": 0,
        "hoc": 25800,
        "soot_yield": 0.014,
        "co_yield": 0.005,
        "description": "Solvent",
    },
    "METHANOL": {
        "c": 1,
        "h": 4,
        "o": 1,
        "n": 0,
        "hoc": 20000,
        "soot_yield": 0.001,
        "co_yield": 0.0,
        "description": "Alcohol fuel",
    },
    "ETHANOL": {
        "c": 2,
        "h": 6,
        "o": 1,
        "n": 0,
        "hoc": 26900,
        "soot_yield": 0.008,
        "co_yield": 0.005,
        "description": "Biofuel, drinking alcohol",
    },
    # Solids/Polymers
    "POLYURETHANE": {
        "c": 3.52,
        "h": 5.48,
        "o": 0.88,
        "n": 0.32,
        "hoc": 23200,
        "soot_yield": 0.100,
        "co_yield": 0.03,
        "description": "Foam insulation, furniture",
    },
    "WOOD": {
        "c": 3.4,
        "h": 6.2,
        "o": 2.5,
        "n": 0,
        "hoc": 17200,
        "soot_yield": 0.015,
        "co_yield": 0.01,
        "description": "Cellulosic material",
    },
    "PMMA": {
        "c": 5,
        "h": 8,
        "o": 2,
        "n": 0,
        "hoc": 25200,
        "soot_yield": 0.022,
        "co_yield": 0.005,
        "description": "Acrylic, plexiglass",
    },
    "POLYSTYRENE": {
        "c": 8,
        "h": 8,
        "o": 0,
        "n": 0,
        "hoc": 39900,
        "soot_yield": 0.060,
        "co_yield": 0.015,
        "description": "Styrofoam, packaging",
    },
    "POLYETHYLENE": {
        "c": 2,
        "h": 4,
        "o": 0,
        "n": 0,
        "hoc": 43600,
        "soot_yield": 0.060,
        "co_yield": 0.01,
        "description": "Plastic bags, containers",
    },
    "POLYPROPYLENE": {
        "c": 3,
        "h": 6,
        "o": 0,
        "n": 0,
        "hoc": 43200,
        "soot_yield": 0.059,
        "co_yield": 0.01,
        "description": "Plastic containers, automotive parts",
    },
}


def list_fuels() -> list[str]:
    """
    Get list of all available predefined fuels.

    Returns
    -------
    list[str]
        Sorted list of fuel names

    Examples
    --------
    >>> fuels = list_fuels()
    >>> print(fuels[:5])
    ['ACETONE', 'BUTANE', 'ETHANE', 'ETHANOL', 'GASOLINE']
    """
    return sorted(FUEL_DATABASE.keys())


def get_fuel_info(name: str) -> dict:
    """
    Get detailed information about a fuel.

    Parameters
    ----------
    name : str
        Fuel name (case-insensitive)

    Returns
    -------
    dict
        Dictionary with fuel composition and properties

    Raises
    ------
    ValueError
        If fuel name is not in database

    Examples
    --------
    >>> info = get_fuel_info('PROPANE')
    >>> print(f"Heat of combustion: {info['hoc']} kJ/kg")
    Heat of combustion: 46000 kJ/kg
    """
    fuel_key = name.upper()
    if fuel_key not in FUEL_DATABASE:
        available = ", ".join(sorted(FUEL_DATABASE.keys()))
        raise ValueError(f"Unknown fuel '{name}'. Available fuels:\n{available}")

    return FUEL_DATABASE[fuel_key].copy()
