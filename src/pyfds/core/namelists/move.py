"""
FDS MOVE namelist.

Transformations for geometry objects.
"""

from pydantic import field_validator

from pyfds.core.namelists.base import FdsField, NamelistBase


class Move(NamelistBase):
    """
    FDS MOVE namelist - geometry transformations.

    Defines translation, rotation, and scaling operations that can be
    applied to GEOM objects.

    Parameters
    ----------
    id : str
        Transformation identifier (referenced by MOVE_ID)
    dx, dy, dz : float, optional
        Translation offsets [m]
    axis : tuple[float, float, float], optional
        Rotation axis vector
    rotation_angle : float, optional
        Rotation angle [degrees]
    scale : tuple[float, float, float], optional
        Scaling factors (sx, sy, sz)

    Examples
    --------
    >>> # Translation
    >>> move = Move(id='SHIFT', dx=5.0, dy=0.0, dz=2.0)

    >>> # Rotation
    >>> move = Move(
    ...     id='ROTATE_90',
    ...     axis=(0, 0, 1),
    ...     rotation_angle=90.0
    ... )

    >>> # Combined
    >>> move = Move(
    ...     id='TRANSFORM',
    ...     dx=1.0, dy=1.0, dz=0.0,
    ...     axis=(0, 0, 1),
    ...     rotation_angle=45.0,
    ...     scale=(2.0, 2.0, 1.0)
    ... )
    """

    id: str = FdsField(..., description="Transformation identifier")

    # Translation
    dx: float = FdsField(0.0, exclude_if=0.0, description="X translation [m]")
    dy: float = FdsField(0.0, exclude_if=0.0, description="Y translation [m]")
    dz: float = FdsField(0.0, exclude_if=0.0, description="Z translation [m]")

    # Rotation
    axis: tuple[float, float, float] | None = FdsField(None, description="Rotation axis vector")
    rotation_angle: float | None = FdsField(None, description="Rotation angle [degrees]")

    # Scaling
    scale: tuple[float, float, float] | None = FdsField(
        None, description="Scale factors (sx, sy, sz)"
    )

    @field_validator("axis")
    @classmethod
    def validate_axis(
        cls, v: tuple[float, float, float] | None
    ) -> tuple[float, float, float] | None:
        """Validate rotation axis is a unit vector."""
        if v is not None:
            # Check it's a 3D vector
            if len(v) != 3:
                raise ValueError("axis must be a 3D vector (x,y,z)")
            # Check it's not a zero vector
            if all(abs(component) < 1e-10 for component in v):
                raise ValueError("axis cannot be a zero vector")
        return v

    @field_validator("scale")
    @classmethod
    def validate_scale(
        cls, v: tuple[float, float, float] | None
    ) -> tuple[float, float, float] | None:
        """Validate scale factors are positive."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("scale must be a 3-tuple (sx, sy, sz)")
            if any(s <= 0 for s in v):
                raise ValueError("scale factors must be positive")
        return v

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MOVE"
