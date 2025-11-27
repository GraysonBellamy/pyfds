"""
Base manager class with common patterns.
"""

from typing import TypeVar

T = TypeVar("T")


class BaseManager:
    """
    Base class for simulation component managers.

    Provides common functionality for managing collections of namelist objects.
    """

    def __init__(self) -> None:
        """Initialize the base manager."""
        pass

    def validate(self) -> list[str]:
        """
        Validate the managed components.

        Returns
        -------
        List[str]
            List of validation warnings (empty if no issues)
        """
        return []
