"""
Builders for MOVE namelist.

Provides fluent API for creating geometry transformations.
"""

from typing import Any, Self

from pyfds.builders.base import Builder
from pyfds.core.namelists.move import Move


class MoveBuilder(Builder[Move]):
    """
    Builder for FDS MOVE namelist.

    Provides a fluent API for creating geometry transformations including
    translation, rotation, and scaling operations.

    Examples
    --------
    >>> # Simple translation
    >>> move = (MoveBuilder("SHIFT")
    ...         .translate(dx=5.0, dy=0.0, dz=2.0)
    ...         .build())

    >>> # Rotation around Z-axis
    >>> rotate = (MoveBuilder("SPIN")
    ...          .rotate(axis=(0,0,1), angle=90.0)
    ...          .build())

    >>> # Combined transformation
    >>> transform = (MoveBuilder("COMPLEX")
    ...              .translate(dx=1.0, dy=1.0, dz=0.0)
    ...              .rotate(axis=(0,0,1), angle=45.0)
    ...              .scale(sx=2.0, sy=2.0, sz=1.0)
    ...              .build())
    """

    def __init__(self, id: str):
        """Initialize MoveBuilder."""
        super().__init__()
        self._params: dict[str, Any] = {}
        self._params["id"] = id

    def translate(self, dx: float = 0.0, dy: float = 0.0, dz: float = 0.0) -> Self:
        """
        Set translation offsets.

        Parameters
        ----------
        dx, dy, dz : float, optional
            Translation offsets in x, y, z directions [m]

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["dx"] = dx
        self._params["dy"] = dy
        self._params["dz"] = dz
        return self

    def rotate(self, axis: tuple[float, float, float], angle: float) -> Self:
        """
        Set rotation transformation.

        Parameters
        ----------
        axis : tuple[float, float, float]
            Rotation axis vector (will be normalized)
        angle : float
            Rotation angle [degrees]

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["axis"] = axis
        self._params["rotation_angle"] = angle
        return self

    def scale(self, sx: float = 1.0, sy: float = 1.0, sz: float = 1.0) -> Self:
        """
        Set scaling factors.

        Parameters
        ----------
        sx, sy, sz : float, optional
            Scaling factors in x, y, z directions

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        if sx != 1.0 or sy != 1.0 or sz != 1.0:
            self._params["scale"] = (sx, sy, sz)
        return self

    def build(self) -> Move:
        """
        Build and return the Move object.

        Returns
        -------
        Move
            The constructed transformation object

        Raises
        ------
        RuntimeError
            If builder has already been used
        """
        if self._built:
            raise RuntimeError("Builder has already been used")
        self._built = True
        return Move(**self._params)
