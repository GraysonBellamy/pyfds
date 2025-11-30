"""
FDS input file parser.

Parse existing FDS files into PyFDS Simulation objects.
"""

import re
from pathlib import Path
from typing import Any

from pyfds.core.namelists import NamelistBase
from pyfds.core.simulation import Simulation
from pyfds.logging import get_logger

logger = get_logger(__name__)


class FDSParser:
    """
    Parse FDS input files into PyFDS objects.

    Handles FDS syntax including:
    - Multi-line namelists
    - Comments (preserved in metadata)
    - Array indexing (A(1)=, A(1,2)=)
    - Case-insensitive parameters

    Examples
    --------
    >>> parser = FDSParser()
    >>> sim = parser.parse("model.fds")
    >>> sim.add(Device(id="NEW_TEMP", quantity="TEMPERATURE", xyz=Point3D.of(1, 1, 1)))
    >>> sim.write("modified.fds")
    """

    def __init__(self) -> None:
        self._comment_pattern = re.compile(r"^\s*!.*$", re.MULTILINE)
        self._namelist_pattern = re.compile(r"&(\w+)\s*(.*?)\s*/", re.IGNORECASE | re.DOTALL)
        self._param_pattern = re.compile(
            r"(\w+(?:\([^)]*\))?)\s*=\s*([^/]+?)(?=\s*(?:,\s*)?\w+\s*=|\s*/|$)", re.IGNORECASE
        )

    def parse(self, filepath: str | Path) -> Simulation:
        """
        Parse an FDS file into a Simulation.

        Parameters
        ----------
        filepath : str or Path
            Path to FDS input file

        Returns
        -------
        Simulation
            Parsed simulation object

        Raises
        ------
        FileNotFoundError
            If file doesn't exist
        ValueError
            If file format is invalid
        """
        path = Path(filepath)
        if not path.exists():
            raise FileNotFoundError(f"FDS file not found: {filepath}")

        content = path.read_text()
        return self.parse_string(content)

    def parse_string(self, content: str) -> Simulation:
        """
        Parse FDS content string into a Simulation.

        Parameters
        ----------
        content : str
            FDS input file content

        Returns
        -------
        Simulation
            Parsed simulation object
        """
        # Extract comments for metadata
        comments = self._extract_comments(content)

        # Tokenize into namelist dictionaries
        namelists = self._tokenize(content)

        # Build simulation from namelists
        return self._build_simulation(namelists, comments)

    def _extract_comments(self, content: str) -> list[str]:
        """Extract all comment lines from FDS content."""
        return self._comment_pattern.findall(content)

    def _tokenize(self, content: str) -> list[dict[str, Any]]:
        """
        Tokenize FDS content into namelist dictionaries.

        Handles:
        - Multi-line namelists
        - Parameter parsing with array indexing
        - Case-insensitive parameter names
        """
        namelists = []

        # Remove comments for parsing
        content_no_comments = self._comment_pattern.sub("", content)

        for match in self._namelist_pattern.finditer(content_no_comments):
            group_name = match.group(1).upper()
            params_str = match.group(2)

            # Parse parameters
            params = self._parse_parameters(params_str)

            # Add group name
            params["_fds_group"] = group_name

            namelists.append(params)

        return namelists

    def _parse_parameters(self, params_str: str) -> dict[str, Any]:
        """Parse parameter string into dictionary."""
        params: dict[str, Any] = {}

        for match in self._param_pattern.finditer(params_str):
            param_name = match.group(1).strip()
            param_value = match.group(2).strip()

            # Handle array indexing like A(1) or A(1,2)
            if "(" in param_name:
                base_name, indices = self._parse_array_index(param_name)
                if base_name not in params:
                    params[base_name] = {}
                params[base_name][indices] = self._parse_value(param_value)
            else:
                params[param_name.upper()] = self._parse_value(param_value)

        return params

    def _parse_array_index(self, param_name: str) -> tuple[str, tuple[int, ...]]:
        """Parse array index like A(1) or A(1,2) into base name and indices."""
        match = re.match(r"(\w+)\(([^)]+)\)", param_name)
        if not match:
            raise ValueError(f"Invalid array index: {param_name}")

        base_name = match.group(1).upper()
        indices_str = match.group(2)
        indices = tuple(int(x.strip()) for x in indices_str.split(","))

        return base_name, indices

    def _parse_value(self, value: str) -> Any:
        """Parse FDS parameter value into Python type."""
        value = value.strip()

        # Handle quoted strings
        if value.startswith("'") and value.endswith("'"):
            return value[1:-1]
        if value.startswith('"') and value.endswith('"'):
            return value[1:-1]

        # Handle booleans
        if value.upper() == ".TRUE.":
            return True
        if value.upper() == ".FALSE.":
            return False

        # Handle comma-separated numbers (arrays)
        if "," in value:
            # Split by comma and parse each element
            elements = []
            for part in value.split(","):
                part = part.strip()
                if part:
                    elements.append(self._parse_single_value(part))
            return tuple(elements)

        # Handle single values
        return self._parse_single_value(value)

    def _parse_single_value(self, value: str) -> Any:
        """Parse a single FDS value (not comma-separated)."""
        # Handle numbers
        try:
            # Try int first
            if "." not in value and "E" not in value.upper():
                return int(value)
            # Then float
            return float(value)
        except ValueError:
            pass

        # Handle arrays like (1,2,3) - though these shouldn't appear after our regex fix
        if value.startswith("(") and value.endswith(")"):
            array_str = value[1:-1]
            elements = [self._parse_single_value(x.strip()) for x in array_str.split(",")]
            return tuple(elements)

        # Default to string
        return value

    def _build_simulation(
        self, namelists: list[dict[str, Any]], _comments: list[str]
    ) -> Simulation:
        """Build Simulation object from parsed namelists."""
        # Find HEAD namelist for basic info
        head_params = None
        other_namelists = []

        for namelist in namelists:
            if namelist.get("_fds_group") == "HEAD":
                head_params = namelist
            else:
                other_namelists.append(namelist)

        if not head_params:
            raise ValueError("FDS file must contain a HEAD namelist")

        # Extract CHID and TITLE
        chid = head_params.get("CHID")
        title = head_params.get("TITLE")

        if not chid:
            raise ValueError("HEAD namelist must contain CHID parameter")

        # Create simulation
        sim = Simulation(chid=chid, title=title)

        # Add metadata
        # sim._metadata = {"comments": comments}  # TODO: Add metadata support to Simulation

        # Convert and add other namelists
        for namelist_dict in other_namelists:
            namelist_obj = self._dict_to_namelist(namelist_dict)
            if namelist_obj:
                sim.add(namelist_obj)

        return sim

    def _dict_to_namelist(self, namelist_dict: dict[str, Any]) -> NamelistBase | None:
        """Convert parsed namelist dict to NamelistBase object."""
        group_name = namelist_dict.pop("_fds_group")

        # Import the appropriate namelist class
        try:
            from pyfds.core.namelists import (
                Combustion,
                Ctrl,
                Device,
                Head,
                Material,
                Mesh,
                Mult,
                Obstruction,
                Prop,
                Ramp,
                Reaction,
                Species,
                Surface,
                Time,
                Vent,
            )

            namelist_classes = {
                "TIME": Time,
                "HEAD": Head,
                "RAMP": Ramp,
                "COMB": Combustion,
                "MESH": Mesh,
                "SURF": Surface,
                "MATL": Material,
                "DEVC": Device,
                "OBST": Obstruction,
                "VENT": Vent,
                "REAC": Reaction,
                "SPEC": Species,
                "PROP": Prop,
                "CTRL": Ctrl,
                "MULT": Mult,
            }

            cls = namelist_classes.get(group_name)
            if cls:
                logger.debug(f"Creating {group_name} object with data: {namelist_dict}")
                obj = cls.from_dict(namelist_dict)  # type: ignore
                logger.debug(f"Created object: {obj}, type: {type(obj)}")
                return obj  # type: ignore

        except ImportError as e:
            logger.warning(f"Import error: {e}")
        except Exception as e:
            # Log the full exception with traceback using structured logging
            logger.exception(f"Error creating {group_name}: {e}")

        # Unknown namelist type - skip for now
        return None
