"""Predefined species database for common gas species in FDS simulations."""

from typing import Any

# Comprehensive species database with accurate molecular weights
# Data sourced from NIST and FDS documentation
PREDEFINED_SPECIES = {
    # Atmospheric gases (major air components)
    "NITROGEN": {
        "formula": "N2",
        "mw": 28.0134,
        "description": "Nitrogen (major air component)",
    },
    "OXYGEN": {
        "formula": "O2",
        "mw": 31.9988,
        "description": "Oxygen (air component)",
    },
    "ARGON": {
        "formula": "Ar",
        "mw": 39.948,
        "description": "Argon (air component)",
    },
    # Combustion products
    "CARBON_DIOXIDE": {
        "formula": "CO2",
        "mw": 44.0095,
        "description": "Carbon dioxide",
    },
    "WATER_VAPOR": {
        "formula": "H2O",
        "mw": 18.0153,
        "description": "Water vapor",
    },
    "CARBON_MONOXIDE": {
        "formula": "CO",
        "mw": 28.0101,
        "description": "Carbon monoxide",
    },
    # Soot and particulates
    "SOOT": {
        "formula": "C",
        "mw": 12.011,
        "aerosol": True,
        "description": "Carbon soot particles",
    },
    # Fuel gases
    "METHANE": {
        "formula": "CH4",
        "mw": 16.0425,
        "description": "Methane (natural gas)",
    },
    "ETHANE": {
        "formula": "C2H6",
        "mw": 30.0690,
        "description": "Ethane",
    },
    "PROPANE": {
        "formula": "C3H8",
        "mw": 44.0956,
        "description": "Propane (LPG)",
    },
    "BUTANE": {
        "formula": "C4H10",
        "mw": 58.1222,
        "description": "Butane",
    },
    "HYDROGEN": {
        "formula": "H2",
        "mw": 2.0159,
        "description": "Hydrogen gas",
    },
    # Unsaturated hydrocarbons
    "ETHYLENE": {
        "formula": "C2H4",
        "mw": 28.0532,
        "description": "Ethylene",
    },
    "ACETYLENE": {
        "formula": "C2H2",
        "mw": 26.0373,
        "description": "Acetylene",
    },
    # Alcohols
    "METHANOL": {
        "formula": "CH4O",
        "mw": 32.0419,
        "description": "Methanol",
    },
    "ETHANOL": {
        "formula": "C2H6O",
        "mw": 46.0684,
        "description": "Ethanol",
    },
    # Toxic gases (common in fire effluents)
    "HYDROGEN_CYANIDE": {
        "formula": "HCN",
        "mw": 27.0253,
        "description": "Hydrogen cyanide",
    },
    "HYDROGEN_CHLORIDE": {
        "formula": "HCl",
        "mw": 36.4609,
        "description": "Hydrogen chloride",
    },
    "HYDROGEN_FLUORIDE": {
        "formula": "HF",
        "mw": 20.0063,
        "description": "Hydrogen fluoride",
    },
    "HYDROGEN_BROMIDE": {
        "formula": "HBr",
        "mw": 80.9119,
        "description": "Hydrogen bromide",
    },
    # Nitrogen compounds
    "NITRIC_OXIDE": {
        "formula": "NO",
        "mw": 30.0061,
        "description": "Nitric oxide",
    },
    "NITROGEN_DIOXIDE": {
        "formula": "NO2",
        "mw": 46.0055,
        "description": "Nitrogen dioxide",
    },
    "AMMONIA": {
        "formula": "NH3",
        "mw": 17.0305,
        "description": "Ammonia",
    },
    # Sulfur compounds
    "SULFUR_DIOXIDE": {
        "formula": "SO2",
        "mw": 64.0638,
        "description": "Sulfur dioxide",
    },
    "HYDROGEN_SULFIDE": {
        "formula": "H2S",
        "mw": 34.0809,
        "description": "Hydrogen sulfide",
    },
    # Other organics
    "ACETONE": {
        "formula": "C3H6O",
        "mw": 58.0791,
        "description": "Acetone",
    },
    "BENZENE": {
        "formula": "C6H6",
        "mw": 78.1118,
        "description": "Benzene",
    },
    "TOLUENE": {
        "formula": "C7H8",
        "mw": 92.1384,
        "description": "Toluene",
    },
    # Alkanes (liquid fuels)
    "N_HEXANE": {
        "formula": "C6H14",
        "mw": 86.1754,
        "description": "n-Hexane",
    },
    "N_HEPTANE": {
        "formula": "C7H16",
        "mw": 100.2019,
        "description": "n-Heptane",
    },
    "N_OCTANE": {
        "formula": "C8H18",
        "mw": 114.2285,
        "description": "n-Octane",
    },
    "N_DECANE": {
        "formula": "C10H22",
        "mw": 142.2817,
        "description": "n-Decane",
    },
    # Other common species
    "HELIUM": {
        "formula": "He",
        "mw": 4.0026,
        "description": "Helium",
    },
    "NEON": {
        "formula": "Ne",
        "mw": 20.1797,
        "description": "Neon",
    },
    "KRYPTON": {
        "formula": "Kr",
        "mw": 83.798,
        "description": "Krypton",
    },
    "XENON": {
        "formula": "Xe",
        "mw": 131.293,
        "description": "Xenon",
    },
}


def list_predefined_species() -> list[str]:
    """
    Get list of all available predefined species.

    Returns
    -------
    list[str]
        Sorted list of species names

    Examples
    --------
    >>> species = list_predefined_species()
    >>> print(f"Available species: {len(species)}")
    Available species: 35
    >>> print(species[:5])
    ['ACETONE', 'ACETYLENE', 'AMMONIA', 'ARGON', 'BENZENE']
    """
    return sorted(PREDEFINED_SPECIES.keys())


def get_species_info(name: str) -> dict[str, Any]:
    """
    Get detailed information about a predefined species.

    Parameters
    ----------
    name : str
        Species name (case-insensitive)

    Returns
    -------
    dict
        Dictionary with species properties including formula, molecular weight,
        and description

    Raises
    ------
    ValueError
        If species name is not in database

    Examples
    --------
    >>> info = get_species_info('PROPANE')
    >>> print(f"Formula: {info['formula']}, MW: {info['mw']} g/mol")
    Formula: C3H8, MW: 44.0956 g/mol

    >>> info = get_species_info('oxygen')
    >>> print(f"Description: {info['description']}")
    Description: Oxygen (air component)
    """
    species_key = name.upper()
    if species_key not in PREDEFINED_SPECIES:
        available = ", ".join(sorted(PREDEFINED_SPECIES.keys()))
        raise ValueError(f"Unknown species '{name}'. Available species:\n{available}")

    return PREDEFINED_SPECIES[species_key].copy()


def is_predefined(name: str) -> bool:
    """
    Check if a species name is in the predefined database.

    Parameters
    ----------
    name : str
        Species name to check (case-insensitive)

    Returns
    -------
    bool
        True if species is predefined, False otherwise

    Examples
    --------
    >>> is_predefined('METHANE')
    True
    >>> is_predefined('CUSTOM_FUEL')
    False
    >>> is_predefined('methane')  # Case insensitive
    True
    """
    return name.upper() in PREDEFINED_SPECIES


def create_standard_air(humidity: float = 40.0) -> dict:
    """
    Create standard air composition dictionary.

    This function creates a dictionary suitable for creating a lumped Species
    object representing standard atmospheric air composition. The composition
    is adjusted for humidity.

    Parameters
    ----------
    humidity : float, optional
        Relative humidity percentage (default: 40.0)
        Valid range: 0-100

    Returns
    -------
    dict
        Dictionary with species definition parameters suitable for
        creating a lumped Species with background=True

    Examples
    --------
    >>> air_dict = create_standard_air(humidity=50.0)
    >>> print(air_dict['id'])
    AIR
    >>> print(f"Components: {len(air_dict['spec_id'])}")
    Components: 4

    Notes
    -----
    Standard dry air composition (by volume):
    - Nitrogen: 78.084%
    - Oxygen: 20.946%
    - Argon: 0.934%
    - Carbon dioxide: 0.036%

    Water vapor is added based on humidity, displacing nitrogen and oxygen.
    """
    if not (0 <= humidity <= 100):
        raise ValueError("Humidity must be between 0 and 100 percent")

    # Standard dry air composition (volume fractions)
    dry_air = {
        "NITROGEN": 0.78084,
        "OXYGEN": 0.20946,
        "ARGON": 0.00934,
        "CARBON_DIOXIDE": 0.00036,
    }

    # Adjust for humidity (approximate - assumes 25°C, atmospheric pressure)
    # Water vapor volume fraction ≈ (humidity/100) * (saturation_vapor_pressure / atmospheric_pressure)
    # At 25°C, saturation vapor pressure of water is ~23.8 mmHg = 0.0317 atm
    # Atmospheric pressure = 1 atm
    # So maximum water vapor volume fraction ≈ 0.0317
    water_vapor_fraction = (humidity / 100.0) * 0.0317

    # Adjust dry air components (water vapor displaces N2 and O2 proportionally)
    total_dry = sum(dry_air.values())
    adjustment_factor = (total_dry - water_vapor_fraction) / total_dry

    adjusted_fractions = {}
    for species, fraction in dry_air.items():
        adjusted_fractions[species] = fraction * adjustment_factor

    # Add water vapor
    adjusted_fractions["WATER_VAPOR"] = water_vapor_fraction

    # Convert to mass fractions (need molecular weights)
    species_list = list(adjusted_fractions.keys())

    # Calculate mass fractions properly
    total_mass = sum(
        adjusted_fractions[species] * get_species_info(species)["mw"]
        for species in adjusted_fractions
    )

    mass_fractions = [
        (adjusted_fractions[species] * get_species_info(species)["mw"]) / total_mass
        for species in species_list
    ]

    return {
        "id": "AIR",
        "background": True,
        "spec_id": species_list,
        "mass_fraction": mass_fractions,
        "description": f"Standard air (humidity: {humidity}%)",
    }


def get_species_molecular_weight(name: str) -> float:
    """
    Get the molecular weight of a predefined species.

    This is a convenience function for quickly accessing molecular weights.

    Parameters
    ----------
    name : str
        Species name (case-insensitive)

    Returns
    -------
    float
        Molecular weight in g/mol

    Examples
    --------
    >>> mw = get_species_molecular_weight('WATER_VAPOR')
    >>> print(f"Water molecular weight: {mw} g/mol")
    Water molecular weight: 18.0153 g/mol
    """
    info = get_species_info(name)
    return float(info["mw"])


def get_species_formula(name: str) -> str:
    """
    Get the chemical formula of a predefined species.

    Parameters
    ----------
    name : str
        Species name (case-insensitive)

    Returns
    -------
    str
        Chemical formula

    Examples
    --------
    >>> formula = get_species_formula('PROPANE')
    >>> print(f"Propane formula: {formula}")
    Propane formula: C3H8
    """
    info = get_species_info(name)
    return str(info["formula"])
