"""Validation module for PyFDS.

This module provides comprehensive validation for FDS simulations,
including input sanitization, simulation-level validation, cross-reference
checking, and execution configuration validation.

Examples
--------
>>> from pyfds.validation import validate_simulation
>>> result = validate_simulation(sim)
>>> if not result.is_valid:
...     for error in result.errors:
...         print(error)
"""

from typing import TYPE_CHECKING

from pyfds.core.enums import Severity
from pyfds.validation.base import Issue, ValidationResult
from pyfds.validation.cross_references import CrossReferenceValidator
from pyfds.validation.execution import ExecutionValidator, ParallelValidator
from pyfds.validation.input import (
    CHID_MAX_LENGTH,
    CHID_PATTERN,
    MAX_FILE_SIZE,
    safe_read_text,
    validate_chid,
    validate_file_size,
    validate_non_negative_number,
    validate_path,
    validate_positive_number,
)
from pyfds.validation.simulation import SimulationValidator, Validator, validate_fds_file

if TYPE_CHECKING:
    from pyfds.core.simulation import Simulation


def validate_simulation(simulation: "Simulation") -> ValidationResult:
    """Validate a complete simulation configuration.

    This is the main entry point for simulation validation.
    Runs all validation checks including required components,
    cross-references, geometry quality, and physical bounds.

    Parameters
    ----------
    simulation : Simulation
        Simulation to validate

    Returns
    -------
    ValidationResult
        Validation result with all issues

    Examples
    --------
    >>> sim = Simulation(chid="test")
    >>> sim.add(Mesh(...), Time(...))
    >>> result = validate_simulation(sim)
    >>> if result.is_valid:
    ...     print("Simulation is valid!")
    """
    validator = SimulationValidator(simulation)
    return validator.validate()


__all__ = [
    "CHID_MAX_LENGTH",
    "CHID_PATTERN",
    "MAX_FILE_SIZE",
    "CrossReferenceValidator",
    "ExecutionValidator",
    "Issue",
    "ParallelValidator",
    "Severity",
    "SimulationValidator",
    "ValidationResult",
    "Validator",
    "safe_read_text",
    "validate_chid",
    "validate_fds_file",
    "validate_file_size",
    "validate_non_negative_number",
    "validate_path",
    "validate_positive_number",
    "validate_simulation",
]
