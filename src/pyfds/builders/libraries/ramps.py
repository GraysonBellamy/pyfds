"""Common ramp function patterns library."""

from pyfds.builders.ramp import RampBuilder
from pyfds.core.namelists import Ramp


class CommonRamps:
    """
    Library of common ramp function patterns.

    Provides predefined ramps for common fire growth scenarios,
    HVAC schedules, and other time-varying functions.

    Examples
    --------
    >>> fire_ramp = CommonRamps.t_squared_fast(peak_hrr=2500)
    >>> step_ramp = CommonRamps.step_at(t=60, value=1.0)
    """

    @staticmethod
    def t_squared_slow(peak_hrr: float, t_peak: float = 600, id: str = "T2_SLOW") -> Ramp:
        """
        Slow t-squared fire growth (600s to 1055 kW).

        Parameters
        ----------
        peak_hrr : float
            Peak heat release rate in kW
        t_peak : float, optional
            Time to reach peak in seconds, default: 600
        id : str, optional
            Ramp identifier, default: 'T2_SLOW'

        Returns
        -------
        Ramp
            Slow growth fire ramp

        Examples
        --------
        >>> ramp = CommonRamps.t_squared_slow(peak_hrr=1500)
        """
        return RampBuilder(id).t_squared("SLOW", peak_hrr, t_peak).build()

    @staticmethod
    def t_squared_medium(peak_hrr: float, t_peak: float = 300, id: str = "T2_MEDIUM") -> Ramp:
        """
        Medium t-squared fire growth (300s to 1055 kW).

        Parameters
        ----------
        peak_hrr : float
            Peak heat release rate in kW
        t_peak : float, optional
            Time to reach peak in seconds, default: 300
        id : str, optional
            Ramp identifier, default: 'T2_MEDIUM'

        Returns
        -------
        Ramp
            Medium growth fire ramp

        Examples
        --------
        >>> ramp = CommonRamps.t_squared_medium(peak_hrr=2500)
        """
        return RampBuilder(id).t_squared("MEDIUM", peak_hrr, t_peak).build()

    @staticmethod
    def t_squared_fast(peak_hrr: float, t_peak: float = 150, id: str = "T2_FAST") -> Ramp:
        """
        Fast t-squared fire growth (150s to 1055 kW).

        Parameters
        ----------
        peak_hrr : float
            Peak heat release rate in kW
        t_peak : float, optional
            Time to reach peak in seconds, default: 150
        id : str, optional
            Ramp identifier, default: 'T2_FAST'

        Returns
        -------
        Ramp
            Fast growth fire ramp

        Examples
        --------
        >>> ramp = CommonRamps.t_squared_fast(peak_hrr=3000)
        """
        return RampBuilder(id).t_squared("FAST", peak_hrr, t_peak).build()

    @staticmethod
    def t_squared_ultrafast(peak_hrr: float, t_peak: float = 75, id: str = "T2_ULTRAFAST") -> Ramp:
        """
        Ultrafast t-squared fire growth (75s to 1055 kW).

        Parameters
        ----------
        peak_hrr : float
            Peak heat release rate in kW
        t_peak : float, optional
            Time to reach peak in seconds, default: 75
        id : str, optional
            Ramp identifier, default: 'T2_ULTRAFAST'

        Returns
        -------
        Ramp
            Ultrafast growth fire ramp

        Examples
        --------
        >>> ramp = CommonRamps.t_squared_ultrafast(peak_hrr=5000)
        """
        return RampBuilder(id).t_squared("ULTRAFAST", peak_hrr, t_peak).build()

    @staticmethod
    def step_at(t: float, value: float = 1.0, id: str = "STEP") -> Ramp:
        """
        Step function at specified time.

        Parameters
        ----------
        t : float
            Time at which step occurs in seconds
        value : float, optional
            Value after step, default: 1.0
        id : str, optional
            Ramp identifier, default: 'STEP'

        Returns
        -------
        Ramp
            Step function ramp

        Examples
        --------
        >>> ramp = CommonRamps.step_at(t=60, value=1.0)
        """
        return RampBuilder(id).step(t, 0, value).build()

    @staticmethod
    def linear_growth(t_end: float, f_end: float = 1.0, id: str = "LINEAR") -> Ramp:
        """
        Linear growth from 0 to specified value.

        Parameters
        ----------
        t_end : float
            End time in seconds
        f_end : float, optional
            Final value, default: 1.0
        id : str, optional
            Ramp identifier, default: 'LINEAR'

        Returns
        -------
        Ramp
            Linear growth ramp

        Examples
        --------
        >>> ramp = CommonRamps.linear_growth(t_end=300, f_end=1000)
        """
        return RampBuilder(id).linear(0, t_end, 0, f_end).build()

    @staticmethod
    def exponential_growth(t_end: float, f_end: float = 1.0, id: str = "EXPONENTIAL") -> Ramp:
        """
        Exponential growth from 0 to specified value.

        Parameters
        ----------
        t_end : float
            End time in seconds
        f_end : float, optional
            Final value, default: 1.0
        id : str, optional
            Ramp identifier, default: 'EXPONENTIAL'

        Returns
        -------
        Ramp
            Exponential growth ramp

        Examples
        --------
        >>> ramp = CommonRamps.exponential_growth(t_end=300, f_end=2000)
        """
        return RampBuilder(id).exponential(0, t_end, 0, f_end).build()

    @staticmethod
    def hvac_schedule_24h(
        on_time: float = 8.0,
        off_time: float = 18.0,
        id: str = "HVAC_SCHEDULE",
    ) -> Ramp:
        """
        24-hour HVAC on/off schedule.

        Parameters
        ----------
        on_time : float, optional
            Hour when HVAC turns on (0-24), default: 8.0 (8 AM)
        off_time : float, optional
            Hour when HVAC turns off (0-24), default: 18.0 (6 PM)
        id : str, optional
            Ramp identifier, default: 'HVAC_SCHEDULE'

        Returns
        -------
        Ramp
            HVAC schedule ramp (0=off, 1=on)

        Examples
        --------
        >>> # On from 6 AM to 10 PM
        >>> ramp = CommonRamps.hvac_schedule_24h(on_time=6, off_time=22)
        """
        # Convert hours to seconds
        on_seconds = on_time * 3600
        off_seconds = off_time * 3600

        return (
            RampBuilder(id)
            .add_point(0, 0)
            .add_point(on_seconds - 1e-6, 0)
            .add_point(on_seconds, 1)
            .add_point(off_seconds - 1e-6, 1)
            .add_point(off_seconds, 0)
            .add_point(86400, 0)  # 24 hours
            .build()
        )
