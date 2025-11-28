"""
FDS HOLE namelist.

Holes in obstructions (doors, windows, vents).
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import NamelistBase


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
    >>> door = Hole(xb=Bounds3D(5, 5.1, 2, 4, 0, 2.1), id='DOOR')
    >>> print(door.to_fds())
    &HOLE XB=5,5.1,2,4,0,2.1, ID='DOOR' /

    >>> # Controlled hole
    >>> window = Hole(
    ...     xb=(5, 5.1, 2, 4, 0, 2.1),
    ...     id='WINDOW',
    ...     ctrl_id='WINDOW_CTRL',
    ...     color='GRAY'
    ... )
    >>> print(window.to_fds())
    &HOLE XB=5,5.1,2,4,0,2.1, ID='WINDOW', CTRL_ID='WINDOW_CTRL', COLOR='GRAY' /
    """

    xb: Bounds3D = Field(..., description="Hole bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    id: str | None = Field(None, description="Hole identifier")
    ctrl_id: str | None = Field(None, description="Control ID for activation/deactivation")
    devc_id: str | None = Field(None, description="Device ID for activation/deactivation")
    color: str | None = Field(None, description="Color when hole is closed")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color when closed (0-255)")
    transparency: float | None = Field(
        1.0, ge=0, le=1, description="Transparency when closed (0-1)"
    )
    mult_id: str | None = Field(None, description="Multiplier ID for array replication")

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Bounds3D:
        """Validate and convert hole bounds."""
        if isinstance(v, Bounds3D):
            return v
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        raise ValueError("XB must be a Bounds3D or tuple of 6 floats")

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

    def to_fds(self) -> str:
        """Generate FDS HOLE namelist."""
        params: dict[str, Any] = {"xb": self.xb.as_tuple()}

        if self.id:
            params["id"] = self.id
        if self.ctrl_id:
            params["ctrl_id"] = self.ctrl_id
        if self.devc_id:
            params["devc_id"] = self.devc_id
        if self.color:
            params["color"] = self.color
        if self.rgb:
            params["rgb"] = self.rgb
        if self.transparency is not None and self.transparency != 1.0:
            params["transparency"] = self.transparency
        if self.mult_id:
            params["mult_id"] = self.mult_id

        return self._build_namelist("HOLE", params)
