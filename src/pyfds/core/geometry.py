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
