"""
Read-only views into Registry collections.

This module provides RegistryView, a typed read-only interface for accessing
registry contents without exposing mutation operations.
"""

from collections.abc import Iterator
from typing import TYPE_CHECKING, Generic, TypeVar, overload

if TYPE_CHECKING:
    from pyfds.core.registry import Registry

T = TypeVar("T")


class RegistryView(Generic[T]):
    """
    Read-only typed view into a Registry.

    Provides dictionary-like read access to registry contents without
    exposing mutation operations. This ensures that namelists can only
    be added through the Simulation.add() method.

    Parameters
    ----------
    registry : Registry[T]
        The underlying registry to wrap

    Examples
    --------
    >>> sim = Simulation(chid="test")
    >>> sim.add(Mesh(id="mesh1", ...))
    >>>
    >>> # Access via view
    >>> mesh = sim.meshes["mesh1"]  # Get by ID
    >>> len(sim.meshes)             # Count items
    >>> "mesh1" in sim.meshes       # Check existence
    >>> for m in sim.meshes:        # Iterate
    ...     print(m.id)
    """

    def __init__(self, registry: "Registry[T]") -> None:
        self._registry = registry

    @overload
    def __getitem__(self, key: str) -> T: ...

    @overload
    def __getitem__(self, key: int) -> T: ...

    def __getitem__(self, key: str | int) -> T:
        """
        Get an item by its ID or index.

        Parameters
        ----------
        key : str or int
            The unique identifier of the item, or an integer index

        Returns
        -------
        T
            The item with the given ID or at the given index

        Raises
        ------
        UnknownIdError
            If a string ID does not exist in the registry
        IndexError
            If an integer index is out of range
        """
        if isinstance(key, int):
            items = self._registry.list_items()
            return items[key]
        return self._registry.get(key)

    def __contains__(self, item: str | T) -> bool:
        """
        Check if an ID or item exists in the registry.

        Parameters
        ----------
        item : str or T
            The ID to check, or an item to check for membership

        Returns
        -------
        bool
            True if the ID/item exists
        """
        if isinstance(item, str):
            return self._registry.contains(item)
        # Check if the item itself exists
        return item in self._registry.list_items()

    def __iter__(self) -> Iterator[T]:
        """
        Iterate over all items in the registry.

        Yields
        ------
        T
            Each item in the registry
        """
        return iter(self._registry)

    def __len__(self) -> int:
        """
        Return the number of items in the registry.

        Returns
        -------
        int
            Count of registered items
        """
        return len(self._registry)

    def __bool__(self) -> bool:
        """
        Return True if registry contains items.

        Returns
        -------
        bool
            True if not empty
        """
        return len(self._registry) > 0

    def ids(self) -> list[str]:
        """
        Get all registered IDs.

        Returns
        -------
        list[str]
            List of all IDs in the registry
        """
        return self._registry.list_ids()

    def all(self) -> list[T]:
        """
        Get all registered items.

        Returns
        -------
        list[T]
            List of all items in the registry
        """
        return self._registry.list_items()

    def get(self, item_id: str, default: T | None = None) -> T | None:
        """
        Get an item by ID with optional default.

        Parameters
        ----------
        item_id : str
            The ID to look up
        default : T, optional
            Value to return if ID not found

        Returns
        -------
        T or None
            The item if found, otherwise default
        """
        if item_id in self._registry:
            return self._registry.get(item_id)
        return default


__all__ = ["RegistryView"]
