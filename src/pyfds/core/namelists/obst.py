"""
FDS OBST namelist.

Obstructions and solid objects.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import NamelistBase


class Obstruction(NamelistBase):
    """
    FDS OBST namelist - obstructions and solid objects.

    Parameters
    ----------
    xb : Bounds3D
        Obstruction bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    surf_id : str, optional
        Surface ID for all faces
    surf_ids : Dict[str, str], optional
        Surface IDs for individual faces (e.g., {'top': 'FIRE', 'sides': 'WALL'})
    color : str, optional
        Named color

    Examples
    --------
    >>> from pyfds.core.geometry import Bounds3D
    >>> burner = Obstruction(xb=Bounds3D(4, 6, 4, 6, 0, 0.5), surf_id='FIRE')
    >>> print(burner.to_fds())
    &OBST XB=4,6,4,6,0,0.5, SURF_ID='FIRE' /
    """

    xb: Bounds3D = Field(..., description="Obstruction bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    surf_id: str | None = Field(None, description="Surface ID for all faces")
    surf_id_top: str | None = Field(None, description="Top surface ID")
    surf_id_bottom: str | None = Field(None, description="Bottom surface ID")
    surf_id_sides: str | None = Field(None, description="Side surfaces ID")
    color: str | None = Field(None, description="Named color")

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Bounds3D:
        """Validate and convert obstruction bounds."""
        if isinstance(v, Bounds3D):
            return v
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        raise ValueError("XB must be a Bounds3D or tuple of 6 floats")

    def to_fds(self) -> str:
        """Generate FDS OBST namelist."""
        params: dict[str, Any] = {"xb": self.xb.as_tuple()}
        if self.surf_id:
            params["surf_id"] = self.surf_id
        if self.surf_id_top:
            params["surf_id_top"] = self.surf_id_top
        if self.surf_id_bottom:
            params["surf_id_bottom"] = self.surf_id_bottom
        if self.surf_id_sides:
            params["surf_id_sides"] = self.surf_id_sides
        if self.color:
            params["color"] = self.color
        return self._build_namelist("OBST", params)
