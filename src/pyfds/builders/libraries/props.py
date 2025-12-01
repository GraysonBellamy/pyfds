"""Common device property presets for sprinklers, detectors, and nozzles."""

from pyfds.core.namelists import Property


class CommonProps:
    """
    Library of predefined device properties.

    Provides factory methods for common sprinklers, smoke detectors,
    heat detectors, and nozzles with typical configurations.

    Examples
    --------
    >>> sprinkler = CommonProps.quick_response_sprinkler()
    >>> detector = CommonProps.smoke_detector(id="SMOKE_DET")
    >>> heat_det = CommonProps.heat_detector(id="HEAT_DET", activation_temp=74)
    """

    @staticmethod
    def sprinkler(
        id: str,
        activation_temp: float,
        rti: float,
        flow_rate: float | None = None,
        k_factor: float | None = None,
        spray_angle: tuple[float, float] | None = None,
        c_factor: float | None = None,
        pressure: float | None = None,
        orifice_diameter: float | None = None,
    ) -> Property:
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
        Property
            Sprinkler property object

        Examples
        --------
        >>> # Quick response sprinkler
        >>> sprinkler = CommonProps.sprinkler(
        ...     id='QUICK_RESPONSE',
        ...     activation_temp=68,
        ...     rti=50,
        ...     flow_rate=60
        ... )

        >>> # Standard response sprinkler with pressure
        >>> sprinkler = CommonProps.sprinkler(
        ...     id='STANDARD',
        ...     activation_temp=74,
        ...     rti=100,
        ...     k_factor=80,
        ...     pressure=200000
        ... )
        """
        return Property(
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

    @staticmethod
    def smoke_detector(
        id: str,
        activation_obscuration: float = 3.28,
        alpha_e: float | None = None,
        beta_e: float | None = None,
        smokeview_id: str | None = None,
    ) -> Property:
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
        Property
            Smoke detector property object

        Examples
        --------
        >>> # Basic smoke detector
        >>> detector = CommonProps.smoke_detector(
        ...     id='PHOTOELECTRIC',
        ...     activation_obscuration=3.28
        ... )

        >>> # Smoke detector with optical properties
        >>> detector = CommonProps.smoke_detector(
        ...     id='OPTICAL',
        ...     activation_obscuration=3.28,
        ...     alpha_e=0.5,
        ...     beta_e=0.3
        ... )

        Notes
        -----
        Default value of 3.28 %/m is the UL listed smoke detector sensitivity.
        """
        return Property(
            id=id,
            quantity="CHAMBER_OBSCURATION",
            activation_obscuration=activation_obscuration,
            alpha_e=alpha_e,
            beta_e=beta_e,
            smokeview_id=smokeview_id,
        )

    @staticmethod
    def heat_detector(
        id: str,
        activation_temp: float,
        rti: float = 5.0,
        bead_diameter: float | None = None,
        bead_density: float | None = None,
        bead_specific_heat: float | None = None,
    ) -> Property:
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
        Property
            Heat detector property object

        Examples
        --------
        >>> # Basic heat detector
        >>> heat_det = CommonProps.heat_detector(
        ...     id='HEAT_DET',
        ...     activation_temp=74
        ... )

        >>> # Heat detector with bead properties
        >>> heat_det = CommonProps.heat_detector(
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
        return Property(
            id=id,
            activation_temperature=activation_temp,
            rti=rti,
            bead_diameter=bead_diameter,
            bead_density=bead_density,
            bead_specific_heat=bead_specific_heat,
        )

    @staticmethod
    def nozzle(
        id: str,
        flow_rate: float | None = None,
        pressure: float | None = None,
        orifice_diameter: float | None = None,
        k_factor: float | None = None,
    ) -> Property:
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
        Property
            Nozzle property object

        Examples
        --------
        >>> # Nozzle with flow rate
        >>> nozzle = CommonProps.nozzle(
        ...     id='SPRAY_NOZZLE',
        ...     flow_rate=50,
        ...     pressure=300000
        ... )

        >>> # Nozzle with k-factor
        >>> nozzle = CommonProps.nozzle(
        ...     id='K_NOZZLE',
        ...     k_factor=80,
        ...     orifice_diameter=0.01
        ... )
        """
        return Property(
            id=id,
            flow_rate=flow_rate,
            pressure=pressure,
            orifice_diameter=orifice_diameter,
            k_factor=k_factor,
        )

    @staticmethod
    def quick_response_sprinkler(id: str = "SPRINKLER_QR") -> Property:
        """
        Create a standard quick-response sprinkler (68°C, RTI=50).

        Parameters
        ----------
        id : str, optional
            Unique property identifier, default: 'SPRINKLER_QR'

        Returns
        -------
        Property
            Quick-response sprinkler property

        Examples
        --------
        >>> sprinkler = CommonProps.quick_response_sprinkler()
        """
        return CommonProps.sprinkler(id=id, activation_temp=68, rti=50, flow_rate=60)

    @staticmethod
    def standard_response_sprinkler(id: str = "SPRINKLER_SR") -> Property:
        """
        Create a standard-response sprinkler (74°C, RTI=100).

        Parameters
        ----------
        id : str, optional
            Unique property identifier, default: 'SPRINKLER_SR'

        Returns
        -------
        Property
            Standard-response sprinkler property

        Examples
        --------
        >>> sprinkler = CommonProps.standard_response_sprinkler()
        """
        return CommonProps.sprinkler(id=id, activation_temp=74, rti=100, k_factor=80)
