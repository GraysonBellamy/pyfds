"""
FDS PROP namelist.

Device properties for sprinklers, detectors, etc.
"""

from typing import Any

from pydantic import Field

from pyfds.core.namelists.base import NamelistBase


class Prop(NamelistBase):
    """
    FDS PROP namelist - device properties.

    Defines properties for sprinklers, smoke detectors, heat detectors, and
    other devices.

    Parameters
    ----------
    id : str
        Unique property identifier
    quantity : str, optional
        Measured quantity (e.g., 'TEMPERATURE', 'CHAMBER_OBSCURATION')
    activation_temperature : float, optional
        Activation temperature for heat detectors/sprinklers [°C]
    activation_obscuration : float, optional
        Activation obscuration for smoke detectors [%/m]
    rti : float, optional
        Response Time Index [m^0.5·s^0.5]
    flow_rate : float, optional
        Sprinkler flow rate [L/min]
    k_factor : float, optional
        Sprinkler K-factor [(L/min)/bar^0.5]
    spray_angle : tuple[float, float], optional
        Spray angle limits [degrees]

    Examples
    --------
    >>> # Sprinkler property
    >>> sprinkler = Prop(
    ...     id='SPRINKLER',
    ...     activation_temperature=68,
    ...     rti=50,
    ...     flow_rate=60
    ... )

    >>> # Smoke detector property
    >>> detector = Prop(
    ...     id='SMOKE_DETECTOR',
    ...     quantity='CHAMBER_OBSCURATION',
    ...     activation_obscuration=3.28
    ... )
    """

    id: str = Field(..., description="Property identifier")
    quantity: str | None = Field(None, description="Measured quantity")
    activation_temperature: float | None = Field(None, description="Activation temperature [°C]")
    activation_obscuration: float | None = Field(
        None, ge=0, description="Activation obscuration [%/m]"
    )
    rti: float | None = Field(None, gt=0, description="Response Time Index")
    flow_rate: float | None = Field(None, gt=0, description="Flow rate [L/min]")
    k_factor: float | None = Field(None, gt=0, description="K-factor")
    spray_angle: tuple[float, float] | None = Field(None, description="Spray angle [degrees]")

    def to_fds(self) -> str:
        """Generate FDS PROP namelist."""
        params: dict[str, Any] = {"id": self.id}

        if self.quantity:
            params["quantity"] = self.quantity
        if self.activation_temperature is not None:
            params["activation_temperature"] = self.activation_temperature
        if self.activation_obscuration is not None:
            params["activation_obscuration"] = self.activation_obscuration
        if self.rti is not None:
            params["rti"] = self.rti
        if self.flow_rate is not None:
            params["flow_rate"] = self.flow_rate
        if self.k_factor is not None:
            params["k_factor"] = self.k_factor
        if self.spray_angle:
            params["spray_angle"] = self.spray_angle

        return self._build_namelist("PROP", params)
