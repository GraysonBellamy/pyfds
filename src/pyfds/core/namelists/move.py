"""FDS MOVE namelist for geometry transformations.

Defines translation, rotation, and scaling operations that can be
applied to GEOM objects.

Field Groups:
    identification: Transformation ID
    translation: X, Y, Z offsets
    rotation: Axis and angle
    scaling: Scale factors
"""

from pydantic import field_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Move"]


class Move(NamelistBase):
    """FDS MOVE namelist - geometry transformations.

    Defines translation, rotation, and scaling operations that can be
    applied to GEOM objects.

    Parameters
    ----------
    id : str
        Transformation identifier (referenced by MOVE_ID).
    dx, dy, dz : float, optional
        Translation offsets [m].
    axis : tuple[float, float, float], optional
        Rotation axis vector.
    rotation_angle : float, optional
        Rotation angle [degrees].
    scale : tuple[float, float, float], optional
        Scaling factors (sx, sy, sz).

    Examples
    --------
    >>> move = Move(id='SHIFT', dx=5.0, dy=0.0, dz=2.0)

    See Also
    --------
    Geometry : Geometry objects that use MOVE transformations.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MOVE"

    # --- Identification ---
    id: str = FdsField(..., description="Transformation identifier", group="identification")

    # --- Translation ---
    dx: float = FdsField(0.0, exclude_if=0.0, description="X translation [m]", group="translation")
    dy: float = FdsField(0.0, exclude_if=0.0, description="Y translation [m]", group="translation")
    dz: float = FdsField(0.0, exclude_if=0.0, description="Z translation [m]", group="translation")

    # --- Rotation ---
    axis: tuple[float, float, float] | None = FdsField(
        None, description="Rotation axis vector", group="rotation"
    )
    rotation_angle: float | None = FdsField(
        None, description="Rotation angle [degrees]", group="rotation"
    )

    # --- Scaling ---
    scale: tuple[float, float, float] | None = FdsField(
        None, description="Scale factors (sx, sy, sz)", group="scaling"
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
