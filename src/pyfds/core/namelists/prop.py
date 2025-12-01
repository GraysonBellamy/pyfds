"""FDS PROP namelist for device properties.

Defines properties for sprinklers, smoke detectors, heat detectors, and other devices.

Field Groups:
    identification: Property ID and quantity
    sprinkler: Activation temperature, RTI, C-factor
    nozzle: Flow rate, pressure, K-factor
    smoke_detector: Obscuration and extinction coefficients
    heat_detector: Bead properties for thermal response
    visualization: Display properties
"""

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Property"]


class Property(NamelistBase):
    """FDS PROP namelist - device properties.

    Defines properties for sprinklers, smoke detectors, heat detectors, and
    other devices.

    Parameters
    ----------
    id : str
        Unique property identifier.
    quantity : str, optional
        Measured quantity (e.g., 'TEMPERATURE', 'CHAMBER_OBSCURATION').
    activation_temperature : float, optional
        Activation temperature for heat detectors/sprinklers [°C].
    activation_obscuration : float, optional
        Activation obscuration for smoke detectors [%/m].
    rti : float, optional
        Response Time Index [m^0.5·s^0.5].
    flow_rate : float, optional
        Sprinkler flow rate [L/min].
    k_factor : float, optional
        Sprinkler K-factor [(L/min)/bar^0.5].
    spray_angle : tuple[float, float], optional
        Spray angle limits [degrees].

    Examples
    --------
    >>> sprinkler = Property(
    ...     id='SPRINKLER',
    ...     activation_temperature=68,
    ...     rti=50,
    ...     flow_rate=60
    ... )

    See Also
    --------
    Device : Devices that use PROP for physical properties.
    Particle : Particle class for sprinkler droplets.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "PROP"

    # --- Identification ---
    id: str = FdsField(..., description="Property identifier", group="identification")
    quantity: str | None = FdsField(None, description="Measured quantity", group="identification")

    # --- Sprinkler Properties ---
    activation_temperature: float | None = FdsField(
        None, description="Activation temperature [°C]", group="sprinkler"
    )
    activation_obscuration: float | None = FdsField(
        None, ge=0, description="Activation obscuration [%/m]", group="sprinkler"
    )
    rti: float | None = FdsField(
        None, gt=0, description="Response Time Index (m½s½)", group="sprinkler"
    )
    c_factor: float | None = FdsField(
        None, gt=0, description="Sprinkler C-factor", group="sprinkler"
    )
    spray_angle: tuple[float, float] | None = FdsField(
        None, description="Spray cone angles [degrees]", group="sprinkler"
    )

    # --- Nozzle/Spray Properties ---
    flow_rate: float | None = FdsField(
        None, gt=0, description="Flow rate (L/min or kg/s)", group="nozzle"
    )
    flow_ramp: str | None = FdsField(
        None, description="Ramp ID for time-varying flow rate", group="nozzle"
    )
    pressure: float | None = FdsField(
        None, gt=0, description="Operating pressure (Pa)", group="nozzle"
    )
    orifice_diameter: float | None = FdsField(
        None, gt=0, description="Orifice diameter (m)", group="nozzle"
    )
    k_factor: float | None = FdsField(
        None, gt=0, description="K-factor [(L/min)/bar^0.5]", group="nozzle"
    )
    part_id: str | None = FdsField(
        None, description="Particle class ID for spray droplets", group="nozzle"
    )
    particle_velocity: float | None = FdsField(
        None, gt=0, description="Initial particle velocity [m/s]", group="nozzle"
    )
    spray_pattern_table: str | None = FdsField(
        None, description="Table ID defining spray pattern", group="nozzle"
    )

    # --- Smoke Detector Properties ---
    smokeview_id: str | None = FdsField(
        None, description="Smokeview object ID", group="smoke_detector"
    )
    alpha_e: float | None = FdsField(
        None, gt=0, description="Extinction coefficient (1/m)", group="smoke_detector"
    )
    beta_e: float | None = FdsField(
        None, gt=0, description="Scattering coefficient (1/m)", group="smoke_detector"
    )

    # --- Heat Detector Properties ---
    bead_diameter: float | None = FdsField(
        None, gt=0, description="Detector bead diameter (m)", group="heat_detector"
    )
    bead_density: float | None = FdsField(
        None, gt=0, description="Detector bead density (kg/m³)", group="heat_detector"
    )
    bead_specific_heat: float | None = FdsField(
        None, gt=0, description="Bead specific heat (kJ/kg/K)", group="heat_detector"
    )

    # --- Visualization ---
    offset: float | None = FdsField(None, description="Display offset", group="visualization")
