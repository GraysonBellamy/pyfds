"""Namelist Factory for creating namelists from dictionaries and parsing FDS text.

This module provides centralized factory methods for creating namelist instances
from various input formats including dictionaries and FDS text.
"""

import re
from pathlib import Path
from typing import Any, ClassVar

from pyfds.core.namelists.base import NamelistBase
from pyfds.core.namelists.comb import Combustion
from pyfds.core.namelists.ctrl import Control
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.geom import Geometry
from pyfds.core.namelists.head import Head
from pyfds.core.namelists.hole import Hole
from pyfds.core.namelists.init import Initialization
from pyfds.core.namelists.matl import Material
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.misc import Misc
from pyfds.core.namelists.move import Move
from pyfds.core.namelists.mult import Multiplier
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.part import Particle
from pyfds.core.namelists.prop import Property
from pyfds.core.namelists.ramp import Ramp
from pyfds.core.namelists.reac import Reaction
from pyfds.core.namelists.spec import Species
from pyfds.core.namelists.surf import Surface
from pyfds.core.namelists.time import Time
from pyfds.core.namelists.vent import Vent

__all__ = ["NamelistFactory"]


class NamelistFactory:
    """
    Factory for creating namelist instances from dictionaries and FDS text.

    Provides centralized creation methods with automatic type validation
    and case-insensitive parameter handling.
    """

    # Registry mapping FDS namelist names to their corresponding classes
    _registry: ClassVar[dict[str, type[NamelistBase]]] = {
        # Simulation control
        "HEAD": Head,
        "TIME": Time,
        "MISC": Misc,
        # Domain geometry
        "MESH": Mesh,
        "MULT": Multiplier,
        "OBST": Obstruction,
        "HOLE": Hole,
        "VENT": Vent,
        "GEOM": Geometry,
        "MOVE": Move,
        # Materials and surfaces
        "SURF": Surface,
        "MATL": Material,
        # Combustion and species
        "REAC": Reaction,
        "SPEC": Species,
        "COMB": Combustion,
        "PART": Particle,
        # Devices and control
        "DEVC": Device,
        "CTRL": Control,
        "PROP": Property,
        "RAMP": Ramp,
        # Initial conditions
        "INIT": Initialization,
    }

    @classmethod
    def get_class(cls, namelist_type: str) -> type[NamelistBase] | None:
        """Get namelist class for a type, or None if unsupported.

        Parameters
        ----------
        namelist_type : str
            FDS namelist name (case-insensitive)

        Returns
        -------
        type[NamelistBase] | None
            The namelist class, or None if not found
        """
        return cls._registry.get(namelist_type.upper())

    @classmethod
    def supported_types(cls) -> set[str]:
        """Get set of supported namelist type names.

        Returns
        -------
        set[str]
            Set of all supported namelist type names
        """
        return set(cls._registry.keys())

    @classmethod
    def create(cls, name: str, **kwargs: Any) -> NamelistBase:
        """
        Create a namelist instance by name with keyword arguments.

        Parameters
        ----------
        name : str
            FDS namelist name (case-insensitive)
        **kwargs
            Namelist parameters as keyword arguments

        Returns
        -------
        NamelistBase
            Instance of the appropriate namelist class

        Raises
        ------
        ValueError
            If the namelist name is not recognized
        """
        namelist_name = name.upper()
        if namelist_name not in cls._registry:
            available = ", ".join(sorted(cls._registry.keys()))
            raise ValueError(f"Unknown namelist type '{name}'. Available types: {available}")

        namelist_class = cls._registry[namelist_name]
        return namelist_class(**kwargs)

    @classmethod
    def from_dict(cls, name: str, data: dict[str, Any]) -> NamelistBase:
        """
        Create a namelist instance from a dictionary.

        Parameters
        ----------
        name : str
            FDS namelist name (case-insensitive)
        data : dict[str, Any]
            Dictionary containing namelist parameters

        Returns
        -------
        NamelistBase
            Instance of the appropriate namelist class

        Raises
        ------
        ValueError
            If the namelist name is not recognized
        """
        namelist_name = name.upper()
        if namelist_name not in cls._registry:
            available = ", ".join(sorted(cls._registry.keys()))
            raise ValueError(f"Unknown namelist type '{name}'. Available types: {available}")

        namelist_class = cls._registry[namelist_name]
        return namelist_class.from_dict(data)

    @classmethod
    def parse_fds_namelist(cls, fds_text: str) -> NamelistBase:
        """
        Parse a single FDS namelist string into a namelist instance.

        Parameters
        ----------
        fds_text : str
            FDS namelist text (e.g., "&HEAD CHID='test' /")

        Returns
        -------
        NamelistBase
            Instance of the appropriate namelist class

        Raises
        ------
        ValueError
            If parsing fails or namelist type is unknown
        """
        # Remove comments and normalize whitespace
        fds_text = re.sub(r"!.*$", "", fds_text, flags=re.MULTILINE)
        fds_text = re.sub(r"\s+", " ", fds_text.strip())

        # Match namelist pattern: &NAME param=value, param=value /
        pattern = r"&(\w+)\s*(.*?)\s*/"
        match = re.match(pattern, fds_text, re.IGNORECASE)

        if not match:
            raise ValueError(f"Invalid FDS namelist format: {fds_text}")

        namelist_name = match.group(1).upper()
        params_text = match.group(2).strip()

        # Parse parameters
        params = {}
        if params_text:
            # Split parameters on commas, but respect quotes
            param_pairs = cls._split_fds_parameters(params_text)
            for pair in param_pairs:
                if "=" in pair:
                    key, value = pair.split("=", 1)
                    key = key.strip().lower()
                    value = cls._parse_fds_value(value.strip())
                    params[key] = value

        return cls.from_dict(namelist_name, params)

    @classmethod
    def parse_fds_file(cls, filepath: str) -> dict[str, list[NamelistBase]]:
        """
        Parse an entire FDS file into a dictionary of namelist instances.

        Parameters
        ----------
        filepath : str
            Path to FDS input file

        Returns
        -------
        dict[str, list[NamelistBase]]
            Dictionary mapping namelist names to lists of instances
        """
        with Path(filepath).open() as f:
            content = f.read()

        # Parse namelists, handling multi-line entries
        namelists: dict[str, list[NamelistBase]] = {}
        lines = content.split("\n")
        i = 0

        while i < len(lines):
            line = lines[i].strip()
            i += 1

            # Skip empty lines and comments
            if not line or line.startswith("!"):
                continue

            # Check if this line starts a namelist
            if line.startswith("&"):
                # Collect all lines until we find the closing '/'
                namelist_lines = [line]
                while i < len(lines) and not line.endswith("/"):
                    line = lines[i].strip()
                    i += 1
                    if line and not line.startswith("!"):
                        namelist_lines.append(line)

                # Join the namelist lines
                namelist_text = " ".join(namelist_lines)

                # Parse the namelist
                try:
                    namelist_obj = cls.parse_fds_namelist(namelist_text)
                    name = type(namelist_obj).__name__.upper()
                    if name not in namelists:
                        namelists[name] = []
                    namelists[name].append(namelist_obj)
                except ValueError:
                    # Skip invalid namelists but continue parsing
                    pass

        return namelists

    @staticmethod
    def _split_fds_parameters(params_text: str) -> list[str]:
        """Split FDS parameter text on parameter boundaries."""
        # Use regex to split on commas that are followed by a parameter name (word=)
        # This handles the case where commas appear inside parameter values
        import re

        # Split on comma followed by word= (parameter pattern)
        parts = re.split(r",\s*(?=\w+\s*=\s*)", params_text.strip())
        return [part.strip() for part in parts if part.strip()]

    @staticmethod
    def _parse_fds_value(value_str: str) -> Any:
        """Parse an FDS parameter value into Python types."""
        value_str = value_str.strip()

        # Handle quoted strings
        if (value_str.startswith("'") and value_str.endswith("'")) or (
            value_str.startswith('"') and value_str.endswith('"')
        ):
            return value_str[1:-1]

        # Handle booleans
        if value_str.upper() == ".TRUE.":
            return True
        if value_str.upper() == ".FALSE.":
            return False

        # Handle arrays (comma-separated values, possibly in parentheses)
        if value_str.startswith("(") and value_str.endswith(")"):
            inner = value_str[1:-1]
            if inner.strip():
                return [NamelistFactory._parse_fds_value(v.strip()) for v in inner.split(",")]
            return []
        if "," in value_str:
            # Handle comma-separated arrays without parentheses (like XB=1,2,3,4,5,6)
            return [NamelistFactory._parse_fds_value(v.strip()) for v in value_str.split(",")]

        # Handle numbers
        try:
            # Try int first
            if "." not in value_str and "E" not in value_str.upper():
                return int(value_str)
            return float(value_str)
        except ValueError:
            pass

        # Default to string
        return value_str
