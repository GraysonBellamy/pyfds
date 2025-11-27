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

    # Identification
    id: str = Field(..., description="Property identifier")
    quantity: str | None = Field(None, description="Measured quantity")

    # Sprinkler Properties (Stage 2.3)
    activation_temperature: float | None = Field(None, description="Activation temperature [°C]")
    activation_obscuration: float | None = Field(
        None, ge=0, description="Activation obscuration [%/m]"
    )
    rti: float | None = Field(None, gt=0, description="Response Time Index (m½s½)")
    c_factor: float | None = Field(None, gt=0, description="Sprinkler C-factor")
    spray_angle: tuple[float, float] | None = Field(None, description="Spray cone angles [degrees]")

    # Nozzle/Spray Properties (Stage 2.3)
    flow_rate: float | None = Field(None, gt=0, description="Flow rate (L/min or kg/s)")
    pressure: float | None = Field(None, gt=0, description="Operating pressure (Pa)")
    orifice_diameter: float | None = Field(None, gt=0, description="Orifice diameter (m)")
    k_factor: float | None = Field(None, gt=0, description="K-factor [(L/min)/bar^0.5]")

    # Smoke Detector Properties (Stage 2.3)
    smokeview_id: str | None = Field(None, description="Smokeview object ID")
    alpha_e: float | None = Field(None, gt=0, description="Extinction coefficient (1/m)")
    beta_e: float | None = Field(None, gt=0, description="Scattering coefficient (1/m)")

    # Heat Detector Properties (Stage 2.3)
    bead_diameter: float | None = Field(None, gt=0, description="Detector bead diameter (m)")
    bead_density: float | None = Field(None, gt=0, description="Detector bead density (kg/m³)")
    bead_specific_heat: float | None = Field(None, gt=0, description="Bead specific heat (kJ/kg/K)")

    # Visualization (Stage 2.3)
    offset: float | None = Field(None, description="Display offset")

    def to_fds(self) -> str:
        """Generate FDS PROP namelist."""
        params: dict[str, Any] = {"id": self.id}

        # Quantity
        if self.quantity:
            params["quantity"] = self.quantity

        # Sprinkler Properties
        if self.activation_temperature is not None:
            params["activation_temperature"] = self.activation_temperature
        if self.activation_obscuration is not None:
            params["activation_obscuration"] = self.activation_obscuration
        if self.rti is not None:
            params["rti"] = self.rti
        if self.c_factor is not None:
            params["c_factor"] = self.c_factor
        if self.spray_angle:
            params["spray_angle"] = self.spray_angle

        # Nozzle/Spray Properties
        if self.flow_rate is not None:
            params["flow_rate"] = self.flow_rate
        if self.pressure is not None:
            params["pressure"] = self.pressure
        if self.orifice_diameter is not None:
            params["orifice_diameter"] = self.orifice_diameter
        if self.k_factor is not None:
            params["k_factor"] = self.k_factor

        # Smoke Detector Properties
        if self.smokeview_id:
            params["smokeview_id"] = self.smokeview_id
        if self.alpha_e is not None:
            params["alpha_e"] = self.alpha_e
        if self.beta_e is not None:
            params["beta_e"] = self.beta_e

        # Heat Detector Properties
        if self.bead_diameter is not None:
            params["bead_diameter"] = self.bead_diameter
        if self.bead_density is not None:
            params["bead_density"] = self.bead_density
        if self.bead_specific_heat is not None:
            params["bead_specific_heat"] = self.bead_specific_heat

        # Visualization
        if self.offset is not None:
            params["offset"] = self.offset

        return self._build_namelist("PROP", params)
