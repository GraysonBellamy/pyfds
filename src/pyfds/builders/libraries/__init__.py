"""Predefined libraries for common materials, fuels, species, and device properties."""

from pyfds.builders.libraries.fuels import FUEL_DATABASE, get_fuel_info, list_fuels
from pyfds.builders.libraries.holes import CommonHoles
from pyfds.builders.libraries.materials import CommonMaterials
from pyfds.builders.libraries.props import CommonProps
from pyfds.builders.libraries.ramps import CommonRamps
from pyfds.builders.libraries.species import (
    PREDEFINED_SPECIES,
    create_standard_air,
    get_species_formula,
    get_species_info,
    get_species_molecular_weight,
    is_predefined,
    list_predefined_species,
)

__all__ = [
    "FUEL_DATABASE",
    "PREDEFINED_SPECIES",
    "CommonHoles",
    "CommonMaterials",
    "CommonProps",
    "CommonRamps",
    "create_standard_air",
    "get_fuel_info",
    "get_species_formula",
    "get_species_info",
    "get_species_molecular_weight",
    "is_predefined",
    "list_fuels",
    "list_predefined_species",
]
