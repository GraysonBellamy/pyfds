"""
Geometry utilities and value objects for PyFDS.

This module provides value objects for geometric concepts like points and coordinates,
replacing primitive tuples to avoid primitive obsession.
"""

from dataclasses import dataclass
from typing import Self


@dataclass(frozen=True)
class Point3D:
    """A 3D point in space with x, y, z coordinates."""

    x: float
    y: float
    z: float

    def as_tuple(self) -> tuple[float, float, float]:
        """Return the point as a tuple (x, y, z)."""
        return (self.x, self.y, self.z)

    def distance_to(self, other: Self) -> float:
        """Calculate Euclidean distance to another point."""
        dx = self.x - other.x
        dy = self.y - other.y
        dz = self.z - other.z
        return float((dx**2 + dy**2 + dz**2) ** 0.5)

    def __add__(self, other: Self) -> "Point3D":
        """Add two points component-wise."""
        return Point3D(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: Self) -> "Point3D":
        """Subtract two points component-wise."""
        return Point3D(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, scalar: float) -> "Point3D":
        """Multiply point by scalar."""
        return Point3D(self.x * scalar, self.y * scalar, self.z * scalar)

    def __truediv__(self, scalar: float) -> "Point3D":
        """Divide point by scalar."""
        return Point3D(self.x / scalar, self.y / scalar, self.z / scalar)

    @classmethod
    def from_tuple(cls, t: tuple[float, float, float]) -> "Point3D":
        """Create a Point3D from a tuple (x, y, z)."""
        if len(t) != 3:
            raise ValueError(f"Tuple must have exactly 3 elements, got {len(t)}")
        return cls(t[0], t[1], t[2])


@dataclass(frozen=True)
class Bounds3D:
    """A 3D bounding box with min/max coordinates in each dimension."""

    xmin: float
    xmax: float
    ymin: float
    ymax: float
    zmin: float
    zmax: float

    def __post_init__(self) -> None:
        """Validate bounds after initialization."""
        if self.xmin > self.xmax:
            raise ValueError(f"xmin ({self.xmin}) must be less than or equal to xmax ({self.xmax})")
        if self.ymin > self.ymax:
            raise ValueError(f"ymin ({self.ymin}) must be less than or equal to ymax ({self.ymax})")
        if self.zmin > self.zmax:
            raise ValueError(f"zmin ({self.zmin}) must be less than or equal to zmax ({self.zmax})")

    def as_tuple(self) -> tuple[float, float, float, float, float, float]:
        """Return the bounds as a tuple (xmin, xmax, ymin, ymax, zmin, zmax)."""
        return (self.xmin, self.xmax, self.ymin, self.ymax, self.zmin, self.zmax)

    @property
    def volume(self) -> float:
        """Calculate the volume of the bounding box."""
        return (self.xmax - self.xmin) * (self.ymax - self.ymin) * (self.zmax - self.zmin)

    @property
    def center(self) -> Point3D:
        """Calculate the center point of the bounding box."""
        return Point3D(
            (self.xmin + self.xmax) / 2,
            (self.ymin + self.ymax) / 2,
            (self.zmin + self.zmax) / 2,
        )

    @classmethod
    def from_tuple(cls, t: tuple[float, float, float, float, float, float]) -> "Bounds3D":
        """Create a Bounds3D from a tuple (xmin, xmax, ymin, ymax, zmin, zmax)."""
        if len(t) != 6:
            raise ValueError(f"Tuple must have exactly 6 elements, got {len(t)}")
        return cls(t[0], t[1], t[2], t[3], t[4], t[5])


@dataclass(frozen=True)
class Grid3D:
    """A 3D grid resolution with cell counts in each dimension."""

    nx: int
    ny: int
    nz: int

    def __post_init__(self) -> None:
        """Validate grid dimensions after initialization."""
        if self.nx <= 0:
            raise ValueError(f"nx ({self.nx}) must be positive")
        if self.ny <= 0:
            raise ValueError(f"ny ({self.ny}) must be positive")
        if self.nz <= 0:
            raise ValueError(f"nz ({self.nz}) must be positive")

    def as_tuple(self) -> tuple[int, int, int]:
        """Return the grid as a tuple (nx, ny, nz)."""
        return (self.nx, self.ny, self.nz)

    @property
    def total_cells(self) -> int:
        """Calculate the total number of cells in the grid."""
        return self.nx * self.ny * self.nz

    def get_cell_sizes(self, bounds: Bounds3D) -> tuple[float, float, float]:
        """
        Calculate the cell size in each direction given the bounds.

        Parameters
        ----------
        bounds : Bounds3D
            The spatial bounds of the domain

        Returns
        -------
        Tuple[float, float, float]
            Cell sizes (dx, dy, dz) in meters
        """
        dx = (bounds.xmax - bounds.xmin) / self.nx
        dy = (bounds.ymax - bounds.ymin) / self.ny
        dz = (bounds.zmax - bounds.zmin) / self.nz
        return (dx, dy, dz)

    @classmethod
    def from_tuple(cls, t: tuple[int, int, int]) -> "Grid3D":
        """Create a Grid3D from a tuple (nx, ny, nz)."""
        if len(t) != 3:
            raise ValueError(f"Tuple must have exactly 3 elements, got {len(t)}")
        return cls(t[0], t[1], t[2])
