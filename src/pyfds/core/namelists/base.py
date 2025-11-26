"""
Base namelist classes for FDS input file generation.

This module provides the foundation for creating FDS namelist groups
with proper validation and formatting.
"""

from abc import ABC, abstractmethod
from typing import Any

from pydantic import BaseModel, ConfigDict


class NamelistBase(BaseModel, ABC):
    """
    Abstract base class for all FDS namelist groups.

    All FDS namelist groups inherit from this class and implement
    the to_fds() method to generate their FDS format representation.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True, validate_assignment=True)

    @abstractmethod
    def to_fds(self) -> str:
        """
        Convert the namelist to FDS format string.

        Returns
        -------
        str
            FDS namelist format string
        """
        pass

    def _format_value(self, value: Any) -> str:
        """
        Format a Python value for FDS namelist format.

        Parameters
        ----------
        value : Any
            Python value to format

        Returns
        -------
        str
            FDS-formatted string representation
        """
        if isinstance(value, str):
            return f"'{value}'"
        if isinstance(value, bool):
            return ".TRUE." if value else ".FALSE."
        if isinstance(value, (list, tuple)):
            return ",".join(self._format_value(v) for v in value)
        if isinstance(value, (int, float)):
            return str(value)
        if value is None:
            return ""
        return str(value)

    def _build_namelist(self, group_name: str, params: dict[str, Any]) -> str:
        """
        Build an FDS namelist string from parameters.

        Parameters
        ----------
        group_name : str
            Name of the FDS namelist group (e.g., 'MESH', 'SURF')
        params : Dict[str, Any]
            Dictionary of parameter name-value pairs

        Returns
        -------
        str
            Formatted FDS namelist string
        """
        # Filter out None values and empty strings
        filtered_params = {k: v for k, v in params.items() if v is not None and v != ""}

        if not filtered_params:
            return ""

        # Build parameter string
        param_strings = []
        for key, value in filtered_params.items():
            fds_key = key.upper()
            fds_value = self._format_value(value)
            param_strings.append(f"{fds_key}={fds_value}")

        params_line = ", ".join(param_strings)
        return f"&{group_name} {params_line} /\n"
