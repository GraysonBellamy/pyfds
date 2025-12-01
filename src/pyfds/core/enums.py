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


class SimulationMode(str, Enum):
    """Simulation mode for MISC namelist."""

    VLES = "VLES"
    LES = "LES"
    DNS = "DNS"
    SVLES = "SVLES"


class LESFilterType(str, Enum):
    """LES filter type for MISC namelist."""

    MEAN = "MEAN"
    MAX = "MAX"


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


# =============================================================================
# OBST (Obstruction) Enums
# =============================================================================


class ObstShape(str, Enum):
    """Geometric shape types for OBST."""

    SPHERE = "SPHERE"
    CYLINDER = "CYLINDER"
    CONE = "CONE"
    BOX = "BOX"


# =============================================================================
# GEOM Enums
# =============================================================================


class TextureMapping(str, Enum):
    """Texture mapping types for GEOM."""

    RECTANGULAR = "RECTANGULAR"
    SPHERICAL = "SPHERICAL"


class CoordinateSystem(str, Enum):
    """Coordinate system types."""

    RECTANGULAR = "RECTANGULAR"
    SPHERICAL = "SPHERICAL"
    CYLINDRICAL = "CYLINDRICAL"


# =============================================================================
# REAC (Reaction) Enums
# =============================================================================


class ExtinctionModel(str, Enum):
    """Combustion extinction models."""

    EXTINCTION_1 = "EXTINCTION 1"
    EXTINCTION_2 = "EXTINCTION 2"


# =============================================================================
# DEVC (Device) Enums
# =============================================================================


class StatisticsType(str, Enum):
    """Device statistics types."""

    MIN = "MIN"
    MAX = "MAX"
    MEAN = "MEAN"
    RMS = "RMS"
    VARIANCE = "VARIANCE"
    RANGE = "RANGE"
    TIME_MIN = "TIME MIN"
    TIME_MAX = "TIME MAX"
    COV = "COV"
    CORRCOEF = "CORRCOEF"


# =============================================================================
# RAMP Enums
# =============================================================================


class RampInterpolation(str, Enum):
    """RAMP interpolation types."""

    LINEAR = "LINEAR"
    STEP = "STEP"


# =============================================================================
# PART (Particle) Enums
# =============================================================================


class DragLaw(str, Enum):
    """Drag law for particles."""

    SPHERE = "SPHERE"
    CYLINDER = "CYLINDER"
    SCREEN = "SCREEN"


# =============================================================================
# Mesh Boundary Enums
# =============================================================================


class MeshBoundary(str, Enum):
    """Mesh boundary locations."""

    XMIN = "XMIN"
    XMAX = "XMAX"
    YMIN = "YMIN"
    YMAX = "YMAX"
    ZMIN = "ZMIN"
    ZMAX = "ZMAX"


# =============================================================================
# Built-in FDS Objects (surfaces and species that don't require definition)
# =============================================================================


class BuiltinSurface(str, Enum):
    """Built-in FDS surfaces that don't require explicit definition.

    These surface IDs are predefined by FDS and can be referenced
    without creating a corresponding SURF namelist.
    """

    INERT = "INERT"
    OPEN = "OPEN"
    MIRROR = "MIRROR"
    PERIODIC = "PERIODIC"
    HVAC = "HVAC"
    MASSLESS_TRACER = "MASSLESS TRACER"
    DROPLET = "DROPLET"
    VEGETATION = "VEGETATION"
    EVACUATION = "EVACUATION"

    @classmethod
    def values(cls) -> frozenset[str]:
        """Return all builtin surface values as a frozenset."""
        return frozenset(member.value for member in cls)


class BuiltinSpecies(str, Enum):
    """Built-in FDS species that don't require explicit definition.

    These species IDs are predefined by FDS and can be referenced
    without creating a corresponding SPEC namelist.
    """

    AIR = "AIR"
    PRODUCTS = "PRODUCTS"
    SOOT = "SOOT"
    WATER_VAPOR = "WATER VAPOR"
    CARBON_DIOXIDE = "CARBON DIOXIDE"
    CARBON_MONOXIDE = "CARBON MONOXIDE"
    NITROGEN = "NITROGEN"
    OXYGEN = "OXYGEN"

    @classmethod
    def values(cls) -> frozenset[str]:
        """Return all builtin species values as a frozenset."""
        return frozenset(member.value for member in cls)


__all__ = [
    "BackingCondition",
    "BuiltinSpecies",
    "BuiltinSurface",
    "ControlFunction",
    "CoordinateSystem",
    "DragLaw",
    "ExtinctionModel",
    "HeatTransferModel",
    "LESFilterType",
    "MeshBoundary",
    "ObstShape",
    "RampInterpolation",
    "Severity",
    "SimulationMode",
    "SolidGeometry",
    "SprayPattern",
    "StatisticsType",
    "TextureMapping",
    "TurbulenceModel",
    "VentShape",
    "VentType",
]
