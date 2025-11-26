"""Builder for creating RAMP namelists with common patterns."""

import numpy as np

from ..core.namelists import Ramp
from .base import Builder


class RampBuilder(Builder[Ramp]):
    """
    Builder for creating RAMP namelists.

    Provides convenient methods for creating common ramp patterns including
    linear, step, exponential, t-squared fire growth, and custom functions.

    Parameters
    ----------
    id : str
        Unique identifier for the ramp

    Examples
    --------
    >>> # Linear ramp
    >>> ramp = RampBuilder('HRR_GROWTH') \\
    ...     .linear(t_start=0, t_end=300, f_start=0, f_end=1000) \\
    ...     .build()

    >>> # T-squared fire growth
    >>> ramp = RampBuilder('FIRE') \\
    ...     .t_squared(growth_rate='MEDIUM', peak_hrr=2500, t_peak=300) \\
    ...     .build()

    >>> # Temperature-dependent conductivity
    >>> ramp = RampBuilder('STEEL_K') \\
    ...     .temperature_table({
    ...         20: 45.8,
    ...         100: 43.3,
    ...         200: 40.7,
    ...         400: 36.4
    ...     }) \\
    ...     .build()

    >>> # Custom points
    >>> ramp = RampBuilder('CUSTOM') \\
    ...     .add_point(0, 0) \\
    ...     .add_point(60, 100) \\
    ...     .add_point(120, 500) \\
    ...     .add_point(300, 2000) \\
    ...     .build()
    """

    def __init__(self, id: str):
        """
        Initialize the RampBuilder.

        Parameters
        ----------
        id : str
            Unique identifier for the ramp
        """
        super().__init__()
        self._id = id
        self._points: list[tuple[float, float]] = []

    def add_point(self, t: float, f: float) -> "RampBuilder":
        """
        Add a single (T, F) point to the ramp.

        Parameters
        ----------
        t : float
            Time or temperature value
        f : float
            Function value at this point

        Returns
        -------
        RampBuilder
            Self for method chaining
        """
        self._points.append((t, f))
        return self

    def add_points(self, points: list[tuple[float, float]]) -> "RampBuilder":
        """
        Add multiple points at once.

        Parameters
        ----------
        points : list[tuple[float, float]]
            List of (T, F) point tuples

        Returns
        -------
        RampBuilder
            Self for method chaining
        """
        self._points.extend(points)
        return self

    def linear(
        self, t_start: float, t_end: float, f_start: float = 0.0, f_end: float = 1.0
    ) -> "RampBuilder":
        """
        Create a linear ramp between two points.

        Parameters
        ----------
        t_start : float
            Start time/temperature
        t_end : float
            End time/temperature
        f_start : float, optional
            Function value at start, default: 0.0
        f_end : float, optional
            Function value at end, default: 1.0

        Returns
        -------
        RampBuilder
            Self for method chaining

        Examples
        --------
        >>> ramp = RampBuilder('LINEAR').linear(0, 300, 0, 1000).build()
        """
        self._points = [(t_start, f_start), (t_end, f_end)]
        return self

    def step(self, t_step: float, f_before: float = 0.0, f_after: float = 1.0) -> "RampBuilder":
        """
        Create a step function at time t_step.

        Parameters
        ----------
        t_step : float
            Time/temperature where step occurs
        f_before : float, optional
            Function value before step, default: 0.0
        f_after : float, optional
            Function value after step, default: 1.0

        Returns
        -------
        RampBuilder
            Self for method chaining

        Examples
        --------
        >>> ramp = RampBuilder('STEP').step(60, 0, 1).build()
        """
        epsilon = 1e-6
        self._points = [
            (0, f_before),
            (t_step - epsilon, f_before),
            (t_step, f_after),
        ]
        return self

    def t_squared(
        self,
        growth_rate: str | float,
        peak_hrr: float,
        t_peak: float,
        t_start: float = 0.0,
    ) -> "RampBuilder":
        """
        Create a t-squared fire growth curve.

        The t-squared growth model is commonly used to represent fire development:
        HRR(t) = alpha * (t - t_start)²

        Parameters
        ----------
        growth_rate : str or float
            Named growth rate: 'SLOW' (600s), 'MEDIUM' (300s), 'FAST' (150s),
            'ULTRAFAST' (75s), or custom alpha value (kW/s²)
        peak_hrr : float
            Peak heat release rate (kW)
        t_peak : float
            Time to reach peak (s)
        t_start : float, optional
            Start time (s), default: 0.0

        Returns
        -------
        RampBuilder
            Self for method chaining

        Examples
        --------
        >>> # Medium growth rate fire
        >>> ramp = RampBuilder('FIRE').t_squared('MEDIUM', 2500, 300).build()

        >>> # Custom growth rate
        >>> ramp = RampBuilder('FIRE').t_squared(0.01876, 2500, 300).build()

        Notes
        -----
        Growth rate times are based on time to reach 1055 kW (NFPA standard).
        """
        # Growth rate constants (time to 1055 kW)
        alpha_map = {"SLOW": 600, "MEDIUM": 300, "FAST": 150, "ULTRAFAST": 75}

        if isinstance(growth_rate, str):
            growth_key = growth_rate.upper()
            if growth_key not in alpha_map:
                available = ", ".join(alpha_map.keys())
                raise ValueError(f"Unknown growth rate '{growth_rate}'. Available: {available}")
            t_g = alpha_map[growth_key]
            alpha = 1055 / (t_g**2)
        else:
            alpha = growth_rate

        # Generate points along the curve (stop before t_peak to avoid duplicate)
        t_vals = np.linspace(t_start, t_peak, 21)[:-1]  # 20 points excluding t_peak
        self._points = [(float(t), float(alpha * (t - t_start) ** 2)) for t in t_vals]

        # Add the exact peak value
        self._points.append((float(t_peak), float(peak_hrr)))

        return self

    def temperature_table(self, table: dict[float, float]) -> "RampBuilder":
        """
        Create temperature-dependent property ramp from table.

        Useful for defining thermal properties that vary with temperature.

        Parameters
        ----------
        table : dict[float, float]
            Dictionary of {temperature: value} pairs

        Returns
        -------
        RampBuilder
            Self for method chaining

        Examples
        --------
        >>> # Steel thermal conductivity vs temperature
        >>> ramp = RampBuilder('STEEL_K').temperature_table({
        ...     20: 45.8,
        ...     100: 43.3,
        ...     200: 40.7,
        ...     400: 36.4,
        ...     600: 31.0
        ... }).build()
        """
        self._points = sorted(table.items())
        return self

    def exponential(
        self,
        t_start: float,
        t_end: float,
        f_start: float,
        f_end: float,
        n_points: int = 20,
    ) -> "RampBuilder":
        """
        Create exponential growth/decay curve.

        Parameters
        ----------
        t_start : float
            Start time/temperature
        t_end : float
            End time/temperature
        f_start : float
            Function value at start
        f_end : float
            Function value at end
        n_points : int, optional
            Number of points to generate, default: 20

        Returns
        -------
        RampBuilder
            Self for method chaining

        Examples
        --------
        >>> ramp = RampBuilder('EXP').exponential(0, 300, 1, 1000, n_points=30).build()
        """
        # Generate exponential curve
        t_vals = np.linspace(t_start, t_end, n_points)
        # Normalize to 0-1 range
        t_norm = (t_vals - t_start) / (t_end - t_start)
        # Exponential scaling (using e^3 for moderate curvature)
        f_norm = (np.exp(3 * t_norm) - 1) / (np.exp(3) - 1)
        # Scale to actual range
        f_vals = f_start + (f_end - f_start) * f_norm

        self._points = list(zip(t_vals.tolist(), f_vals.tolist(), strict=True))
        return self

    def sine_wave(
        self,
        t_start: float,
        t_end: float,
        amplitude: float,
        period: float,
        offset: float = 0.0,
        n_points: int = 50,
    ) -> "RampBuilder":
        """
        Create sinusoidal variation.

        Parameters
        ----------
        t_start : float
            Start time/temperature
        t_end : float
            End time/temperature
        amplitude : float
            Wave amplitude
        period : float
            Wave period
        offset : float, optional
            Vertical offset, default: 0.0
        n_points : int, optional
            Number of points to generate, default: 50

        Returns
        -------
        RampBuilder
            Self for method chaining

        Examples
        --------
        >>> # Oscillating heat flux
        >>> ramp = RampBuilder('SINE').sine_wave(
        ...     t_start=0, t_end=600,
        ...     amplitude=50, period=60, offset=100
        ... ).build()
        """
        t_vals = np.linspace(t_start, t_end, n_points)
        f_vals = offset + amplitude * np.sin(2 * np.pi * t_vals / period)

        self._points = list(zip(t_vals.tolist(), f_vals.tolist(), strict=True))
        return self

    def build(self) -> Ramp:
        """
        Build the Ramp object.

        Returns
        -------
        Ramp
            The constructed Ramp namelist object

        Raises
        ------
        ValueError
            If no points have been defined
        RuntimeError
            If the builder has already been used

        Examples
        --------
        >>> ramp = RampBuilder('MY_RAMP').linear(0, 100, 0, 1).build()
        """
        self._check_built()

        if not self._points:
            raise ValueError(f"RampBuilder '{self._id}': No points defined")

        ramp = Ramp(id=self._id, points=self._points)
        self._mark_built()
        return ramp
