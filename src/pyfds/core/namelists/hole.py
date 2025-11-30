"""
FDS HOLE namelist.

Holes in obstructions (doors, windows, vents).
"""

from pydantic import field_validator

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import FdsField, NamelistBase


class Hole(NamelistBase):
    """
    FDS HOLE namelist - holes in obstructions.

    Holes are used to create openings in obstructions such as doors,
    windows, and vents. They can be controlled to open/close during
    the simulation.

    Parameters
    ----------
    xb : Bounds3D
        Hole bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    id : str, optional
        Hole identifier
    ctrl_id : str, optional
        Control ID for activation/deactivation
    devc_id : str, optional
        Device ID for activation/deactivation
    color : str, optional
        Color when hole is closed
    rgb : tuple[int, int, int], optional
        RGB color when closed (0-255)
    transparency : float, optional
        Transparency when closed (0-1, default: 1.0)
    mult_id : str, optional
        Multiplier ID for array replication

    Examples
    --------
    >>> from pyfds.core.geometry import Bounds3D
    >>> door = Hole(xb=Bounds3D.of(5, 5.1, 2, 4, 0, 2.1).1), id='DOOR')
    >>> print(door.to_fds())
    &HOLE XB=5,5.1,2,4,0,2.1, ID='DOOR' /

    >>> # Controlled hole
    >>> window = Hole(
    ...     xb=Bounds3D.of(5, 5.1, 2, 4, 0, 2.1),
    ...     id='WINDOW',
    ...     ctrl_id='WINDOW_CTRL',
    ...     color='GRAY'
    ... )
    >>> print(window.to_fds())
    &HOLE XB=5,5.1,2,4,0,2.1, ID='WINDOW', CTRL_ID='WINDOW_CTRL', COLOR='GRAY' /
    """

    xb: Bounds3D = FdsField(..., description="Hole bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    id: str | None = FdsField(None, description="Hole identifier")
    ctrl_id: str | None = FdsField(None, description="Control ID for activation/deactivation")
    devc_id: str | None = FdsField(None, description="Device ID for activation/deactivation")
    color: str | None = FdsField(None, description="Color when hole is closed")
    rgb: tuple[int, int, int] | None = FdsField(None, description="RGB color when closed (0-255)")
    transparency: float | None = FdsField(
        1.0, ge=0, le=1, exclude_if=1.0, description="Transparency when closed (0-1)"
    )
    mult_id: str | None = FdsField(None, description="Multiplier ID for array replication")

    @field_validator("rgb", mode="before")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB color values."""
        if v is None:
            return None
        if len(v) != 3:
            raise ValueError("RGB must be a tuple of 3 integers")
        for component in v:
            if not (0 <= component <= 255):
                raise ValueError("RGB components must be between 0 and 255")
        return v

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "HOLE"

    def _extra_fds_params(self) -> list[str]:
        """Handle special formatting cases for FDS parameters."""
        return [f"XB={self.xb.as_tuple()}"]
