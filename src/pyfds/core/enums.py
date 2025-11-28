"""Enumerations for FDS parameters."""

from enum import Enum


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
