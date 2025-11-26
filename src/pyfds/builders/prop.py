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

        >>> # Standard response sprinkler
        >>> sprinkler = PropBuilder.sprinkler(
        ...     id='STANDARD',
        ...     activation_temp=74,
        ...     rti=100,
        ...     k_factor=80
        ... )
        """
        return Prop(
            id=id,
            activation_temperature=activation_temp,
            rti=rti,
            flow_rate=flow_rate,
            k_factor=k_factor,
            spray_angle=spray_angle,
        )

    @classmethod
    def smoke_detector(cls, id: str, activation_obscuration: float = 3.28) -> Prop:
        """
        Create a smoke detector property.

        Parameters
        ----------
        id : str
            Unique property identifier
        activation_obscuration : float, optional
            Activation obscuration in %/m (default: 3.28, UL standard)

        Returns
        -------
        Prop
            Smoke detector property object

        Examples
        --------
        >>> detector = PropBuilder.smoke_detector(
        ...     id='PHOTOELECTRIC',
        ...     activation_obscuration=3.28
        ... )

        Notes
        -----
        Default value of 3.28 %/m is the UL listed smoke detector sensitivity.
        """
        return Prop(
            id=id,
            quantity="CHAMBER_OBSCURATION",
            activation_obscuration=activation_obscuration,
        )

    @classmethod
    def heat_detector(cls, id: str, activation_temp: float, rti: float = 5.0) -> Prop:
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

        Returns
        -------
        Prop
            Heat detector property object

        Examples
        --------
        >>> heat_det = PropBuilder.heat_detector(
        ...     id='HEAT_DET',
        ...     activation_temp=74
        ... )

        Notes
        -----
        Default RTI of 5.0 represents a fast-response heat detector.
        """
        return Prop(id=id, activation_temperature=activation_temp, rti=rti)

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
