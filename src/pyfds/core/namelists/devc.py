"""
FDS DEVC namelist.

Measurement devices and sensors.
"""

from typing import Any

from pydantic import Field

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
    xyz : Tuple[float, float, float], optional
        Device location (x, y, z) in meters
    xb : Tuple[float, float, float, float, float, float], optional
        Device bounds for spatial averaging

    Examples
    --------
    >>> temp_sensor = Device(id='TEMP1', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.0))
    >>> print(temp_sensor.to_fds())
    &DEVC ID='TEMP1', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.0 /
    """

    id: str = Field(..., description="Device identifier")
    quantity: str = Field(..., description="Quantity to measure")
    xyz: tuple[float, float, float] | None = Field(None, description="Device location")
    xb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Device bounds"
    )

    def to_fds(self) -> str:
        """Generate FDS DEVC namelist."""
        params: dict[str, Any] = {"id": self.id, "quantity": self.quantity}
        if self.xyz:
            params["xyz"] = self.xyz
        if self.xb:
            params["xb"] = self.xb
        return self._build_namelist("DEVC", params)
