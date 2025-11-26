"""
FDS SURF namelist.

Surface properties for boundaries and materials.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.namelists.base import NamelistBase


class Surface(NamelistBase):
    """
    FDS SURF namelist - surface properties.

    Parameters
    ----------
    id : str
        Unique surface identifier
    rgb : Tuple[int, int, int], optional
        RGB color values (0-255)
    color : str, optional
        Named color (e.g., 'RED', 'BLUE')
    hrrpua : float, optional
        Heat release rate per unit area (kW/m²)
    tmp_front : float, optional
        Front surface temperature (°C)
    matl_id : str, optional
        Material identifier
    thickness : float, optional
        Material thickness (m)

    Examples
    --------
    >>> fire_surf = Surface(id='FIRE', hrrpua=1000.0, color='RED')
    >>> print(fire_surf.to_fds())
    &SURF ID='FIRE', HRRPUA=1000.0, COLOR='RED' /
    """

    id: str = Field(..., description="Surface identifier")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    color: str | None = Field(None, description="Named color")
    hrrpua: float | None = Field(None, ge=0, description="Heat release rate per unit area (kW/m²)")
    tmp_front: float | None = Field(None, description="Front surface temperature (°C)")
    matl_id: str | None = Field(None, description="Material identifier")
    thickness: float | None = Field(None, gt=0, description="Material thickness (m)")

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB values are in range 0-255."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in v):
                raise ValueError("RGB values must be in range 0-255")
        return v

    def to_fds(self) -> str:
        """Generate FDS SURF namelist."""
        params: dict[str, Any] = {"id": self.id}
        if self.rgb:
            params["rgb"] = self.rgb
        if self.color:
            params["color"] = self.color
        if self.hrrpua is not None:
            params["hrrpua"] = self.hrrpua
        if self.tmp_front is not None:
            params["tmp_front"] = self.tmp_front
        if self.matl_id:
            params["matl_id"] = self.matl_id
        if self.thickness is not None:
            params["thickness"] = self.thickness
        return self._build_namelist("SURF", params)
