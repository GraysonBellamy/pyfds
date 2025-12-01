"""Common utilities for PyFDS examples.

This module provides shared utilities, helper functions, and constants
used across all example categories.
"""

from pathlib import Path

# Base directory for all examples
EXAMPLES_DIR = Path(__file__).parent.parent

# Output directory for generated FDS files
OUTPUT_DIR = EXAMPLES_DIR / "fds"


def get_output_dir(category: str) -> Path:
    """Get the output directory for a specific example category.

    Parameters
    ----------
    category : str
        The category name (e.g., 'getting_started', 'fires')

    Returns
    -------
    Path
        The output directory path, created if it doesn't exist
    """
    output_path = OUTPUT_DIR / category
    output_path.mkdir(parents=True, exist_ok=True)
    return output_path


def write_example(sim, category: str, filename: str | None = None) -> Path:
    """Write a simulation to the appropriate output directory.

    Parameters
    ----------
    sim : Simulation
        The simulation object to write
    category : str
        The category name for organizing output
    filename : str, optional
        Override filename (default: {chid}.fds)

    Returns
    -------
    Path
        Path to the written file
    """
    output_dir = get_output_dir(category)
    if filename is None:
        filename = f"{sim.chid}.fds"
    output_path = output_dir / filename
    return sim.write(output_path)


__all__ = [
    "EXAMPLES_DIR",
    "OUTPUT_DIR",
    "get_output_dir",
    "write_example",
]
