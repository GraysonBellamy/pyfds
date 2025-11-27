"""
FDS DEVC namelist.

Measurement devices and sensors.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import NamelistBase


class Device(NamelistBase):
    """
    FDS DEVC namelist - measurement devices.

    Parameters
    ----------
    id : str
        Unique device identifier
    quantity : str
        FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY')
    xyz : Point3D, optional
        Device location (x, y, z) in meters
    xb : Tuple[float, float, float, float, float, float], optional
        Device bounds for spatial averaging

    Examples
    --------
    >>> from pyfds.core.geometry import Point3D
    >>> temp_sensor = Device(id='TEMP1', quantity='TEMPERATURE', xyz=Point3D(2.5, 2.5, 2.0))
    >>> print(temp_sensor.to_fds())
    &DEVC ID='TEMP1', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.0 /
    """

    id: str = Field(..., description="Device identifier")
    quantity: str = Field(..., description="Quantity to measure")
    xyz: Point3D | None = Field(None, description="Device location")
    xb: Bounds3D | None = Field(None, description="Device bounds (xmin,xmax,ymin,ymax,zmin,zmax)")

    @field_validator("xyz", mode="before")
    @classmethod
    def validate_xyz(cls, v: Any) -> Any:
        if isinstance(v, tuple):
            return Point3D.from_tuple(v)
        return v

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Any:
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        return v

    def to_fds(self) -> str:
        """Generate FDS DEVC namelist."""
        params: dict[str, Any] = {"id": self.id, "quantity": self.quantity}
        if self.xyz:
            params["xyz"] = self.xyz.as_tuple()
        if self.xb:
            params["xb"] = self.xb.as_tuple()
        return self._build_namelist("DEVC", params)
