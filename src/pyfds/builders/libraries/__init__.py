"""Predefined libraries for common materials, fuels, and ramp patterns."""

from .fuels import FUEL_DATABASE, get_fuel_info, list_fuels
from .materials import CommonMaterials
from .ramps import CommonRamps

__all__ = [
    "FUEL_DATABASE",
    "CommonMaterials",
    "CommonRamps",
    "get_fuel_info",
    "list_fuels",
]
