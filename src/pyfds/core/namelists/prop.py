"""
FDS PROP namelist.

Device properties for sprinklers, detectors, etc.
"""

from typing import TYPE_CHECKING

from pyfds.core.namelists.base import FdsField, NamelistBase

if TYPE_CHECKING:
    from pyfds.builders import PropBuilder


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

    @classmethod
    def builder(cls) -> "PropBuilder":
        """Return a fluent builder for Prop.

        Returns
        -------
        PropBuilder
            A builder instance for fluent construction

        Examples
        --------
        >>> prop = Prop.builder() \\
        ...     .id("SPRINKLER") \\
        ...     .sprinkler(rti=50, activation_temperature=68) \\
        ...     .build()
        """
        from pyfds.builders import PropBuilder

        return PropBuilder()

    # Identification
    id: str = FdsField(..., description="Property identifier")
    quantity: str | None = FdsField(None, description="Measured quantity")

    # Sprinkler Properties
    activation_temperature: float | None = FdsField(None, description="Activation temperature [°C]")
    activation_obscuration: float | None = FdsField(
        None, ge=0, description="Activation obscuration [%/m]"
    )
    rti: float | None = FdsField(None, gt=0, description="Response Time Index (m½s½)")
    c_factor: float | None = FdsField(None, gt=0, description="Sprinkler C-factor")
    spray_angle: tuple[float, float] | None = FdsField(
        None, description="Spray cone angles [degrees]"
    )

    # Nozzle/Spray Properties
    flow_rate: float | None = FdsField(None, gt=0, description="Flow rate (L/min or kg/s)")
    pressure: float | None = FdsField(None, gt=0, description="Operating pressure (Pa)")
    orifice_diameter: float | None = FdsField(None, gt=0, description="Orifice diameter (m)")
    k_factor: float | None = FdsField(None, gt=0, description="K-factor [(L/min)/bar^0.5]")

    # Smoke Detector Properties
    smokeview_id: str | None = FdsField(None, description="Smokeview object ID")
    alpha_e: float | None = FdsField(None, gt=0, description="Extinction coefficient (1/m)")
    beta_e: float | None = FdsField(None, gt=0, description="Scattering coefficient (1/m)")

    # Heat Detector Properties
    bead_diameter: float | None = FdsField(None, gt=0, description="Detector bead diameter (m)")
    bead_density: float | None = FdsField(None, gt=0, description="Detector bead density (kg/m³)")
    bead_specific_heat: float | None = FdsField(
        None, gt=0, description="Bead specific heat (kJ/kg/K)"
    )

    # Visualization
    offset: float | None = FdsField(None, description="Display offset")

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "PROP"
