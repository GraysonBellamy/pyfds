"""Enumerations for FDS parameters.

This module contains all enumeration types used across PyFDS,
organized by their FDS namelist or functional area.
"""

from enum import Enum

# =============================================================================
# Validation Enums
# =============================================================================


class Severity(str, Enum):
    """Validation issue severity levels."""

    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


# =============================================================================
# SURF (Surface) Enums
# =============================================================================


class SolidGeometry(str, Enum):
    """Solid phase geometry types for SURF."""

    CARTESIAN = "CARTESIAN"
    CYLINDRICAL = "CYLINDRICAL"
    SPHERICAL = "SPHERICAL"
    INNER_CYLINDRICAL = "INNER CYLINDRICAL"


class BackingCondition(str, Enum):
    """Backing conditions for SURF."""

    VOID = "VOID"
    INSULATED = "INSULATED"
    EXPOSED = "EXPOSED"


class HeatTransferModel(str, Enum):
    """Heat transfer models for SURF."""

    LOGLAW = "LOGLAW"
    IMPINGING_JET = "IMPINGING JET"


class SprayPattern(str, Enum):
    """Spray patterns for particle generation."""

    UNIFORM = "UNIFORM"
    GAUSSIAN = "GAUSSIAN"


# =============================================================================
# MISC (Miscellaneous) Enums
# =============================================================================


class TurbulenceModel(str, Enum):
    """LES turbulence models for MISC namelist."""

    DEARDORFF = "DEARDORFF"
    DYNAMIC_SMAGORINSKY = "DYNAMIC SMAGORINSKY"
    VREMAN = "VREMAN"
    WALE = "WALE"


# =============================================================================
# VENT Enums
# =============================================================================


class VentType(str, Enum):
    """Types of vents in FDS."""

    OPEN = "OPEN"
    HVAC = "HVAC"
    SURFACE = "SURFACE"
    MIRROR = "MIRROR"
    PERIODIC = "PERIODIC"


class VentShape(str, Enum):
    """Vent geometry types."""

    RECTANGULAR = "RECTANGULAR"
    CIRCULAR = "CIRCULAR"
    ANNULAR = "ANNULAR"


# =============================================================================
# CTRL (Control) Enums
# =============================================================================


class ControlFunction(str, Enum):
    """Control function types for CTRL namelist."""

    ANY = "ANY"
    ALL = "ALL"
    ONLY = "ONLY"
    TIME_DELAY = "TIME_DELAY"
    CUSTOM = "CUSTOM"
    KILL = "KILL"
    RESTART = "RESTART"


__all__ = [
    "BackingCondition",
    "ControlFunction",
    "HeatTransferModel",
    "Severity",
    "SolidGeometry",
    "SprayPattern",
    "TurbulenceModel",
    "VentShape",
    "VentType",
]
