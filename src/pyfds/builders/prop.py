"""Builder for creating PROP namelists for device properties."""

from ..core.namelists import Prop
from .base import Builder


class PropBuilder(Builder[Prop]):
    """
    Builder for creating PROP namelists.

    Provides factory methods for creating common device properties
    such as sprinklers, smoke detectors, and heat detectors.

    Examples
    --------
    >>> # Sprinkler property
    >>> sprinkler = PropBuilder.sprinkler(
    ...     id='QUICK_RESPONSE',
    ...     activation_temp=68,
    ...     rti=50,
    ...     flow_rate=60
    ... )

    >>> # Smoke detector property
    >>> detector = PropBuilder.smoke_detector(
    ...     id='PHOTOELECTRIC',
    ...     activation_obscuration=3.28
    ... )

    >>> # Heat detector property
    >>> heat_det = PropBuilder.heat_detector(
    ...     id='HEAT_DET',
    ...     activation_temp=74
    ... )
    """

    @classmethod
    def sprinkler(
        cls,
        id: str,
        activation_temp: float,
        rti: float,
        flow_rate: float | None = None,
        k_factor: float | None = None,
        spray_angle: tuple[float, float] | None = None,
        c_factor: float | None = None,
        pressure: float | None = None,
        orifice_diameter: float | None = None,
    ) -> Prop:
        """
        Create a sprinkler property.

        Parameters
        ----------
        id : str
            Unique property identifier
        activation_temp : float
            Activation temperature in °C
        rti : float
            Response Time Index in (m·s)^0.5
        flow_rate : float, optional
            Flow rate in L/min
        k_factor : float, optional
            K-factor in (L/min)/bar^0.5
        spray_angle : tuple[float, float], optional
            Spray angle limits in degrees
        c_factor : float, optional
            Sprinkler C-factor
        pressure : float, optional
            Operating pressure in Pa
        orifice_diameter : float, optional
            Orifice diameter in m

        Returns
        -------
        Prop
            Sprinkler property object

        Examples
        --------
        >>> # Quick response sprinkler
        >>> sprinkler = PropBuilder.sprinkler(
        ...     id='QUICK_RESPONSE',
        ...     activation_temp=68,
        ...     rti=50,
        ...     flow_rate=60
        ... )

        >>> # Standard response sprinkler with pressure
        >>> sprinkler = PropBuilder.sprinkler(
        ...     id='STANDARD',
        ...     activation_temp=74,
        ...     rti=100,
        ...     k_factor=80,
        ...     pressure=200000
        ... )
        """
        return Prop(
            id=id,
            activation_temperature=activation_temp,
            rti=rti,
            flow_rate=flow_rate,
            k_factor=k_factor,
            spray_angle=spray_angle,
            c_factor=c_factor,
            pressure=pressure,
            orifice_diameter=orifice_diameter,
        )

    @classmethod
    def smoke_detector(
        cls,
        id: str,
        activation_obscuration: float = 3.28,
        alpha_e: float | None = None,
        beta_e: float | None = None,
        smokeview_id: str | None = None,
    ) -> Prop:
        """
        Create a smoke detector property.

        Parameters
        ----------
        id : str
            Unique property identifier
        activation_obscuration : float, optional
            Activation obscuration in %/m (default: 3.28, UL standard)
        alpha_e : float, optional
            Extinction coefficient in 1/m
        beta_e : float, optional
            Scattering coefficient in 1/m
        smokeview_id : str, optional
            Smokeview object ID for visualization

        Returns
        -------
        Prop
            Smoke detector property object

        Examples
        --------
        >>> # Basic smoke detector
        >>> detector = PropBuilder.smoke_detector(
        ...     id='PHOTOELECTRIC',
        ...     activation_obscuration=3.28
        ... )

        >>> # Smoke detector with optical properties
        >>> detector = PropBuilder.smoke_detector(
        ...     id='OPTICAL',
        ...     activation_obscuration=3.28,
        ...     alpha_e=0.5,
        ...     beta_e=0.3
        ... )

        Notes
        -----
        Default value of 3.28 %/m is the UL listed smoke detector sensitivity.
        """
        return Prop(
            id=id,
            quantity="CHAMBER_OBSCURATION",
            activation_obscuration=activation_obscuration,
            alpha_e=alpha_e,
            beta_e=beta_e,
            smokeview_id=smokeview_id,
        )

    @classmethod
    def heat_detector(
        cls,
        id: str,
        activation_temp: float,
        rti: float = 5.0,
        bead_diameter: float | None = None,
        bead_density: float | None = None,
        bead_specific_heat: float | None = None,
    ) -> Prop:
        """
        Create a heat detector property.

        Parameters
        ----------
        id : str
            Unique property identifier
        activation_temp : float
            Activation temperature in °C
        rti : float, optional
            Response Time Index in (m·s)^0.5, default: 5.0
        bead_diameter : float, optional
            Detector bead diameter in m
        bead_density : float, optional
            Detector bead density in kg/m³
        bead_specific_heat : float, optional
            Bead specific heat in kJ/kg/K

        Returns
        -------
        Prop
            Heat detector property object

        Examples
        --------
        >>> # Basic heat detector
        >>> heat_det = PropBuilder.heat_detector(
        ...     id='HEAT_DET',
        ...     activation_temp=74
        ... )

        >>> # Heat detector with bead properties
        >>> heat_det = PropBuilder.heat_detector(
        ...     id='HEAT_DET_CUSTOM',
        ...     activation_temp=74,
        ...     rti=10.0,
        ...     bead_diameter=0.001,
        ...     bead_density=8000
        ... )

        Notes
        -----
        Default RTI of 5.0 represents a fast-response heat detector.
        """
        return Prop(
            id=id,
            activation_temperature=activation_temp,
            rti=rti,
            bead_diameter=bead_diameter,
            bead_density=bead_density,
            bead_specific_heat=bead_specific_heat,
        )

    @classmethod
    def nozzle(
        cls,
        id: str,
        flow_rate: float | None = None,
        pressure: float | None = None,
        orifice_diameter: float | None = None,
        k_factor: float | None = None,
    ) -> Prop:
        """
        Create a nozzle/spray property.

        Parameters
        ----------
        id : str
            Unique property identifier
        flow_rate : float, optional
            Flow rate in L/min or kg/s
        pressure : float, optional
            Operating pressure in Pa
        orifice_diameter : float, optional
            Orifice diameter in m
        k_factor : float, optional
            K-factor in (L/min)/bar^0.5

        Returns
        -------
        Prop
            Nozzle property object

        Examples
        --------
        >>> # Nozzle with flow rate
        >>> nozzle = PropBuilder.nozzle(
        ...     id='SPRAY_NOZZLE',
        ...     flow_rate=50,
        ...     pressure=300000
        ... )

        >>> # Nozzle with k-factor
        >>> nozzle = PropBuilder.nozzle(
        ...     id='K_NOZZLE',
        ...     k_factor=80,
        ...     orifice_diameter=0.01
        ... )
        """
        return Prop(
            id=id,
            flow_rate=flow_rate,
            pressure=pressure,
            orifice_diameter=orifice_diameter,
            k_factor=k_factor,
        )

    @classmethod
    def quick_response_sprinkler(cls, id: str = "SPRINKLER_QR") -> Prop:
        """
        Create a standard quick-response sprinkler (68°C, RTI=50).

        Parameters
        ----------
        id : str, optional
            Unique property identifier, default: 'SPRINKLER_QR'

        Returns
        -------
        Prop
            Quick-response sprinkler property

        Examples
        --------
        >>> sprinkler = PropBuilder.quick_response_sprinkler()
        """
        return cls.sprinkler(id=id, activation_temp=68, rti=50, flow_rate=60)

    @classmethod
    def standard_response_sprinkler(cls, id: str = "SPRINKLER_SR") -> Prop:
        """
        Create a standard-response sprinkler (74°C, RTI=100).

        Parameters
        ----------
        id : str, optional
            Unique property identifier, default: 'SPRINKLER_SR'

        Returns
        -------
        Prop
            Standard-response sprinkler property

        Examples
        --------
        >>> sprinkler = PropBuilder.standard_response_sprinkler()
        """
        return cls.sprinkler(id=id, activation_temp=74, rti=100, k_factor=80)

    def build(self) -> Prop:
        """
        Build method for consistency with Builder pattern.

        Note: PropBuilder uses class methods, so this is not typically used.

        Raises
        ------
        NotImplementedError
            This builder uses factory methods instead
        """
        raise NotImplementedError(
            "PropBuilder uses factory methods (sprinkler, smoke_detector, etc.) "
            "instead of the standard build() pattern"
        )
