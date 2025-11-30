"""
Geometry utilities and value objects for PyFDS.

This module provides value objects for geometric concepts like points and coordinates,
replacing primitive tuples to avoid primitive obsession.
"""

from dataclasses import dataclass
from typing import Self


@dataclass(frozen=True, slots=True)
class Point3D:
    """Immutable 3D point."""

    x: float
    y: float
    z: float

    @classmethod
    def of(cls, x: float, y: float, z: float) -> Self:
        """Factory method for clean construction."""
        return cls(x, y, z)

    def as_tuple(self) -> tuple[float, float, float]:
        return (self.x, self.y, self.z)

    def distance_to(self, other: Self) -> float:
        """Calculate Euclidean distance to another point."""
        dx = self.x - other.x
        dy = self.y - other.y
        dz = self.z - other.z
        return float((dx**2 + dy**2 + dz**2) ** 0.5)

    def __add__(self, other: Self) -> "Point3D":
        """Add two points component-wise."""
        return Point3D.of(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: Self) -> "Point3D":
        """Subtract two points component-wise."""
        return Point3D.of(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, scalar: float) -> "Point3D":
        """Multiply point by scalar."""
        return Point3D.of(self.x * scalar, self.y * scalar, self.z * scalar)

    def __truediv__(self, scalar: float) -> "Point3D":
        """Divide point by scalar."""
        return Point3D.of(self.x / scalar, self.y / scalar, self.z / scalar)

    @classmethod
    def from_tuple(cls, t: tuple[float, float, float]) -> "Point3D":
        """Create a Point3D from a tuple (x, y, z)."""
        if len(t) != 3:
            raise ValueError(f"Tuple must have exactly 3 elements, got {len(t)}")
        return cls(t[0], t[1], t[2])


@dataclass(frozen=True, slots=True)
class Bounds3D:
    """Immutable 3D bounding box."""

    xmin: float
    xmax: float
    ymin: float
    ymax: float
    zmin: float
    zmax: float

    def __post_init__(self) -> None:
        if self.xmin > self.xmax:
            raise ValueError(f"xmin ({self.xmin}) > xmax ({self.xmax})")
        if self.ymin > self.ymax:
            raise ValueError(f"ymin ({self.ymin}) > ymax ({self.ymax})")
        if self.zmin > self.zmax:
            raise ValueError(f"zmin ({self.zmin}) > zmax ({self.zmax})")

    @classmethod
    def of(
        cls,
        xmin: float,
        xmax: float,
        ymin: float,
        ymax: float,
        zmin: float,
        zmax: float,
    ) -> Self:
        """Factory method for clean construction."""
        return cls(xmin, xmax, ymin, ymax, zmin, zmax)

    @classmethod
    def cube(cls, origin: Point3D, size: float) -> Self:
        """Create a cube from origin point and size."""
        return cls(
            origin.x,
            origin.x + size,
            origin.y,
            origin.y + size,
            origin.z,
            origin.z + size,
        )

    @property
    def volume(self) -> float:
        return (self.xmax - self.xmin) * (self.ymax - self.ymin) * (self.zmax - self.zmin)

    @property
    def center(self) -> Point3D:
        return Point3D(
            (self.xmin + self.xmax) / 2,
            (self.ymin + self.ymax) / 2,
            (self.zmin + self.zmax) / 2,
        )

    def as_tuple(self) -> tuple[float, float, float, float, float, float]:
        return (self.xmin, self.xmax, self.ymin, self.ymax, self.zmin, self.zmax)


@dataclass(frozen=True, slots=True)
class Grid3D:
    """Immutable 3D grid resolution."""

    nx: int
    ny: int
    nz: int

    def __post_init__(self) -> None:
        if self.nx <= 0 or self.ny <= 0 or self.nz <= 0:
            raise ValueError("Grid dimensions must be positive")

    @classmethod
    def of(cls, nx: int, ny: int, nz: int) -> Self:
        """Factory method for clean construction."""
        return cls(nx, ny, nz)

    @classmethod
    def uniform(cls, n: int) -> Self:
        """Create uniform grid (same resolution in all directions)."""
        return cls(n, n, n)

    @property
    def total_cells(self) -> int:
        return self.nx * self.ny * self.nz

    def cell_sizes(self, bounds: Bounds3D) -> tuple[float, float, float]:
        """Calculate cell sizes for given bounds."""
        return (
            (bounds.xmax - bounds.xmin) / self.nx,
            (bounds.ymax - bounds.ymin) / self.ny,
            (bounds.zmax - bounds.zmin) / self.nz,
        )

    def as_tuple(self) -> tuple[int, int, int]:
        return (self.nx, self.ny, self.nz)


__all__ = ["Bounds3D", "Grid3D", "Point3D"]
