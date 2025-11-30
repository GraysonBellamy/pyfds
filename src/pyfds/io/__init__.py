"""
I/O module for reading and writing FDS files.
"""

from pathlib import Path
from typing import TYPE_CHECKING

from .parsers import CSVParser, FDSParser

if TYPE_CHECKING:
    from ..core.simulation import Simulation


def parse_fds(filepath: str | Path) -> "Simulation":
    """
    Parse an FDS input file into a Simulation object.

    This is a convenience function that creates an FDSParser and parses the file.

    Parameters
    ----------
    filepath : str or Path
        Path to FDS input file

    Returns
    -------
    Simulation
        Parsed simulation object

    Examples
    --------
    >>> sim = parse_fds("model.fds")
    >>> sim.add(Device(id="TEMP", quantity="TEMPERATURE", xyz=Point3D.of(1, 1, 1)))
    >>> sim.write("modified.fds")
    """
    parser = FDSParser()
    return parser.parse(filepath)


__all__ = ["CSVParser", "FDSParser", "parse_fds"]
