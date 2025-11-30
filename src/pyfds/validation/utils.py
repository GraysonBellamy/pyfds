"""Shared utilities for validation."""

from typing import Any


def flatten_to_list(nested: str | list | None) -> list[str]:
    """Flatten nested lists to a single list of strings.

    Handles MATL_ID, SPEC_ID, and similar FDS array parameters
    that can be strings, lists, or nested lists.

    Parameters
    ----------
    nested : str | list | None
        Value that may be a string, list, or nested list

    Returns
    -------
    list[str]
        Flattened list of string values

    Examples
    --------
    >>> flatten_to_list("WOOD")
    ['WOOD']
    >>> flatten_to_list(["WOOD", "CHAR"])
    ['WOOD', 'CHAR']
    >>> flatten_to_list([["WOOD", "CHAR"], ["ASH"]])
    ['WOOD', 'CHAR', 'ASH']
    """
    if nested is None:
        return []
    if isinstance(nested, str):
        return [nested]

    flat: list[str] = []
    for item in nested:
        if isinstance(item, list):
            flat.extend(flatten_to_list(item))
        elif item is not None:
            flat.append(item)
    return flat


def get_surface_ids_from_obstruction(obst: Any) -> list[str]:
    """Extract all SURF_ID references from an obstruction.

    Parameters
    ----------
    obst : Obstruction
        Obstruction namelist object

    Returns
    -------
    list[str]
        List of all referenced surface IDs
    """
    surf_ids = []
    for attr in [
        "surf_id",
        "surf_id_top",
        "surf_id_bottom",
        "surf_id_sides",
        "surf_id_interior",
    ]:
        value = getattr(obst, attr, None)
        if value:
            surf_ids.append(value)
    return surf_ids


__all__ = ["flatten_to_list", "get_surface_ids_from_obstruction"]
