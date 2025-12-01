"""
Base namelist classes for FDS input file generation.

This module provides the foundation for creating FDS namelist groups
with proper validation and formatting.
"""

from abc import ABC, abstractmethod
from typing import Any

from pydantic import BaseModel, ConfigDict, Field

__all__ = ["FdsField", "NamelistBase"]


def FdsField(
    default: Any = None,
    *,
    fds_name: str | None = None,
    fds_format: str | None = None,
    exclude_if: Any = None,
    group: str | None = None,
    **kwargs: Any,
) -> Any:
    """
    Create a Pydantic FdsField with FDS-specific metadata.

    Args:
        default: Default value
        fds_name: Name in FDS output (defaults to FdsField name)
        fds_format: Format string for value (e.g., ".2f" for floats)
        exclude_if: Value to exclude from output (usually None or default)
        group: Logical group for documentation (e.g., "thermal", "burning")
        **kwargs: Additional Pydantic FdsField arguments
    """
    json_schema_extra = kwargs.pop("json_schema_extra", {})
    json_schema_extra.update(
        {
            "fds_name": fds_name,
            "fds_format": fds_format,
            "exclude_if": exclude_if,
            "group": group,
        }
    )

    # Handle default vs default_factory conflict
    if "default_factory" in kwargs:
        return Field(json_schema_extra=json_schema_extra, **kwargs)
    return Field(default, json_schema_extra=json_schema_extra, **kwargs)


class NamelistBase(BaseModel, ABC):
    """
    Abstract base class for all FDS namelist groups.

    All FDS namelist groups inherit from this class and implement
    the to_fds() method to generate their FDS format representation.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True, validate_assignment=True)

    def to_fds(self) -> str:
        """
        Convert the namelist to FDS format string.

        Returns
        -------
        str
            FDS namelist format string
        """
        params = self._collect_fds_params()
        return self._format_namelist(params)

    def _collect_fds_params(self) -> dict[str, Any]:
        """
        Collect all FDS parameters from FdsField values and metadata.

        Returns
        -------
        dict[str, Any]
            Dictionary of parameter name-value pairs
        """
        params = {}

        for field_name, field_info in self.__class__.model_fields.items():
            value = getattr(self, field_name)

            # Get FDS metadata
            extra = field_info.json_schema_extra
            if not isinstance(extra, dict):
                extra = {}
            fds_name = extra.get("fds_name") or field_name
            fds_format = extra.get("fds_format")
            if not isinstance(fds_format, (str, type(None))):
                fds_format = None
            exclude_if = extra.get("exclude_if")

            # Skip if value matches exclude condition
            if value is None or value == exclude_if:
                continue

            # Skip if value equals the field's default value (but not for required fields)
            if field_info.default != ... and value == field_info.default:
                continue

            # Format and add parameter
            formatted_value = self._format_value(value, fds_format)
            params[str(fds_name).upper()] = formatted_value

        # Ensure ID comes first if it exists
        if "ID" in params:
            id_value = params.pop("ID")
            ordered_params = {"ID": id_value}
            ordered_params.update(params)
            return ordered_params

        return params

    def _format_namelist(self, params: dict[str, Any]) -> str:
        """
        Format parameters into FDS namelist string.

        Parameters
        ----------
        params : dict[str, Any]
            Dictionary of parameter name-value pairs

        Returns
        -------
        str
            Formatted FDS namelist string
        """
        # Always output the namelist, even with no parameters
        # Build parameter strings from dict
        param_strings = [f"{key}={value}" for key, value in params.items()]

        if param_strings:
            params_line = ", ".join(param_strings)
            return f"&{self._get_namelist_name()} {params_line} /\n"
        # No parameters, put / on separate line
        return f"&{self._get_namelist_name()}\n/\n"

    def _build_namelist(self, name: str, params: dict[str, Any]) -> str:
        """
        Build namelist string with explicit name.

        Parameters
        ----------
        name : str
            Namelist name (e.g., 'MESH', 'SURF')
        params : dict[str, Any]
            Dictionary of parameter name-value pairs

        Returns
        -------
        str
            Formatted FDS namelist string
        """
        if not params:
            return ""

        # Build parameter strings from dict
        param_strings = [f"{key}={value}" for key, value in params.items()]

        params_line = ", ".join(param_strings)
        return f"&{name} {params_line} /\n"

    @abstractmethod
    def _get_namelist_name(self) -> str:
        """
        Get the FDS namelist group name (e.g., 'MESH', 'SURF').

        Returns
        -------
        str
            FDS namelist group name
        """
        pass

    def _format_value(self, value: Any, fds_format: str | None = None) -> str:
        """
        Format a Python value for FDS namelist format.

        Parameters
        ----------
        value : Any
            Python value to format
        fds_format : str, optional
            Format string for numeric values

        Returns
        -------
        str
            FDS-formatted string representation
        """
        # Handle enums first (including str enums)
        if hasattr(value, "value"):
            return self._format_value(value.value, fds_format)

        if isinstance(value, str):
            # Don't add quotes if already quoted
            if value.startswith("'") and value.endswith("'"):
                return value
            return f"'{value}'"
        if isinstance(value, bool):
            return ".TRUE." if value else ".FALSE."
        if isinstance(value, (list, tuple)):
            return ",".join(self._format_value(v) for v in value)
        if isinstance(value, (int, float)) and fds_format:
            return f"{value:{fds_format}}"
        if isinstance(value, (int, float)):
            return str(value)
        if value is None:
            return ""

        # Handle geometry objects
        if hasattr(value, "as_tuple") and hasattr(value, "__class__"):
            class_name = value.__class__.__name__
            if class_name in ("Bounds3D", "Grid3D", "Point3D"):
                return ",".join(str(x) for x in value.as_tuple())

        return str(value)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "NamelistBase":
        """
        Create a namelist instance from a dictionary.

        Parameters
        ----------
        data : dict[str, Any]
            Dictionary containing namelist parameters

        Returns
        -------
        NamelistBase
            Instance of the namelist class
        """
        # Convert keys to lowercase for case-insensitive matching
        normalized_data = {k.lower(): v for k, v in data.items()}

        # Convert geometry FdsFields to proper value objects
        if "ijk" in normalized_data and isinstance(normalized_data["ijk"], (list, tuple)):
            from pyfds.core.geometry import Grid3D

            if isinstance(normalized_data["ijk"], list):
                normalized_data["ijk"] = tuple(normalized_data["ijk"])
            normalized_data["ijk"] = Grid3D.of(*normalized_data["ijk"])

        if "xb" in normalized_data and isinstance(normalized_data["xb"], (list, tuple)):
            from pyfds.core.geometry import Bounds3D

            if isinstance(normalized_data["xb"], list):
                normalized_data["xb"] = tuple(normalized_data["xb"])
            normalized_data["xb"] = Bounds3D.of(*normalized_data["xb"])

        return cls(**normalized_data)

    def __repr__(self) -> str:
        """Return a concise string representation for debugging.

        Shows the namelist type and key identifying fields (id, chid, or first required field).

        Returns
        -------
        str
            Concise representation like "<Head(HEAD) id='room_fire'>"
        """
        class_name = self.__class__.__name__
        namelist_name = self._get_namelist_name()

        # Find the identifying field
        id_value = None
        for field_name in ["id", "chid", "ID", "CHID"]:
            if hasattr(self, field_name):
                id_value = getattr(self, field_name)
                if id_value is not None:
                    break

        if id_value:
            return f"<{class_name}({namelist_name}) id={id_value!r}>"

        # For namelists without id (like Time, Misc), show key fields
        key_fields = []
        for field_name, field_info in self.__class__.model_fields.items():
            if field_info.is_required():
                value = getattr(self, field_name)
                if value is not None:
                    # Truncate long values
                    value_repr = repr(value)
                    if len(value_repr) > 30:
                        value_repr = value_repr[:27] + "..."
                    key_fields.append(f"{field_name}={value_repr}")
                    if len(key_fields) >= 2:  # Limit to 2 fields
                        break

        if key_fields:
            fields_str = ", ".join(key_fields)
            return f"<{class_name}({namelist_name}) {fields_str}>"

        return f"<{class_name}({namelist_name})>"

    def __str__(self) -> str:
        """Return the FDS namelist output.

        This is equivalent to calling to_fds() but truncated for display.

        Returns
        -------
        str
            FDS output, truncated to 100 characters if needed
        """
        fds_output = self.to_fds()
        if len(fds_output) > 100:
            return fds_output[:97] + "..."
        return fds_output.strip()
