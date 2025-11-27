"""Builder for creating DEVC namelists with measurement and control properties."""

from ..core.geometry import Bounds3D, Point3D
from ..core.namelists import Device
from .base import Builder


class DevcBuilder(Builder[Device]):
    """
    Builder for creating DEVC namelists.

    Provides a fluent API for constructing measurement devices
    with control logic, statistics, and device relationships.

    Parameters
    ----------
    id : str
        Unique identifier for the device

    Examples
    --------
    >>> # Simple temperature sensor
    >>> temp = DevcBuilder('TEMP1') \\
    ...     .with_quantity('TEMPERATURE') \\
    ...     .at_point(Point3D(2.5, 2.5, 2.0)) \\
    ...     .build()

    >>> # Sprinkler with control logic
    >>> sprinkler = DevcBuilder('SPRINK') \\
    ...     .with_quantity('SPRINKLER_LINK_TEMPERATURE') \\
    ...     .with_control(setpoint=74.0, trip_direction=1, latch=True) \\
    ...     .at_point(Point3D(5.0, 5.0, 3.0)) \\
    ...     .build()

    >>> # Statistical device for volume averaging
    >>> avg_temp = DevcBuilder('AVG_TEMP') \\
    ...     .with_quantity('TEMPERATURE') \\
    ...     .with_statistics('MEAN', start_time=10.0) \\
    ...     .in_bounds(Bounds3D(0, 10, 0, 10, 0, 3)) \\
    ...     .build()
    """

    def __init__(self, id: str):
        """
        Initialize the DevcBuilder.

        Parameters
        ----------
        id : str
            Unique identifier for the device
        """
        super().__init__()
        self._id = id
        self._params: dict = {}

    def with_quantity(self, quantity: str) -> "DevcBuilder":
        """
        Set the quantity to measure.

        Parameters
        ----------
        quantity : str
            FDS quantity name (e.g., 'TEMPERATURE', 'VELOCITY', 'PRESSURE')

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> devc = DevcBuilder('TEMP').with_quantity('TEMPERATURE').build()
        """
        self._params["quantity"] = quantity
        return self

    def at_point(self, point: Point3D | tuple[float, float, float]) -> "DevcBuilder":
        """
        Set device location at a point.

        Parameters
        ----------
        point : Point3D or tuple[float, float, float]
            Device location (x, y, z) in meters

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> devc = DevcBuilder('TEMP').at_point(Point3D(5.0, 5.0, 2.0)).build()
        >>> devc = DevcBuilder('TEMP').at_point((5.0, 5.0, 2.0)).build()
        """
        if isinstance(point, tuple):
            point = Point3D.from_tuple(point)
        self._params["xyz"] = point
        return self

    def in_bounds(
        self, bounds: Bounds3D | tuple[float, float, float, float, float, float]
    ) -> "DevcBuilder":
        """
        Set device bounds for spatial averaging.

        Parameters
        ----------
        bounds : Bounds3D or tuple
            Device bounds (xmin, xmax, ymin, ymax, zmin, zmax)

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> devc = DevcBuilder('AVG').in_bounds(Bounds3D(0, 10, 0, 10, 0, 3)).build()
        """
        if isinstance(bounds, tuple):
            bounds = Bounds3D.from_tuple(bounds)
        self._params["xb"] = bounds
        return self

    def with_prop(self, prop_id: str) -> "DevcBuilder":
        """
        Set device property ID.

        Parameters
        ----------
        prop_id : str
            Property ID (for sprinklers, detectors, etc.)

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> devc = DevcBuilder('SPRINK').with_prop('STANDARD_SPRINKLER').build()
        """
        self._params["prop_id"] = prop_id
        return self

    def with_control(
        self,
        setpoint: float,
        trip_direction: int = 1,
        latch: bool = True,
        delay: float = 0.0,
    ) -> "DevcBuilder":
        """
        Configure device as a controller.

        Parameters
        ----------
        setpoint : float
            Activation setpoint value
        trip_direction : int, optional
            Direction to trip: 1=above (default), -1=below, 0=both
        latch : bool, optional
            Latch output state after activation, default: True
        delay : float, optional
            Activation delay time in seconds, default: 0.0

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> # Sprinkler activates when temperature exceeds 74°C
        >>> sprinkler = DevcBuilder('SPRINK') \\
        ...     .with_quantity('SPRINKLER_LINK_TEMPERATURE') \\
        ...     .with_control(setpoint=74.0, trip_direction=1, latch=True) \\
        ...     .build()
        """
        self._params["setpoint"] = setpoint
        self._params["trip_direction"] = trip_direction
        self._params["latch"] = latch
        if delay > 0:
            self._params["delay"] = delay
        return self

    def with_statistics(self, stat_type: str, start_time: float = 0.0) -> "DevcBuilder":
        """
        Add statistical analysis to device.

        Parameters
        ----------
        stat_type : str
            Statistical operation: MIN, MAX, MEAN, RMS, VARIANCE, RANGE
        start_time : float, optional
            Start time for statistics in seconds, default: 0.0

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> # Mean temperature over volume, starting at t=10s
        >>> avg = DevcBuilder('AVG_TEMP') \\
        ...     .with_quantity('TEMPERATURE') \\
        ...     .with_statistics('MEAN', start_time=10.0) \\
        ...     .in_bounds(Bounds3D(0, 10, 0, 10, 0, 3)) \\
        ...     .build()
        """
        self._params["statistics"] = stat_type.upper()
        if start_time > 0:
            self._params["statistics_start"] = start_time
        return self

    def with_temporal_statistic(self, stat_type: str) -> "DevcBuilder":
        """
        Set temporal statistic type.

        Parameters
        ----------
        stat_type : str
            Temporal statistic type

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["temporal_statistic"] = stat_type
        return self

    def with_spatial_statistic(self, stat_type: str) -> "DevcBuilder":
        """
        Set spatial statistic type.

        Parameters
        ----------
        stat_type : str
            Spatial statistic type

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["spatial_statistic"] = stat_type
        return self

    def with_orientation(
        self, axis: tuple[float, float, float], rotation: float = 0.0
    ) -> "DevcBuilder":
        """
        Set device orientation.

        Parameters
        ----------
        axis : tuple[float, float, float]
            Orientation axis vector
        rotation : float, optional
            Rotation angle in degrees, default: 0.0

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> devc = DevcBuilder('ORIENTED').with_orientation((0, 0, 1), rotation=45.0).build()
        """
        self._params["orientation"] = axis
        if rotation != 0:
            self._params["rotation"] = rotation
        return self

    def with_ctrl(self, ctrl_id: str) -> "DevcBuilder":
        """
        Set control function ID.

        Parameters
        ----------
        ctrl_id : str
            Control function ID

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["ctrl_id"] = ctrl_id
        return self

    def with_devc_id(self, devc_id: str) -> "DevcBuilder":
        """
        Set device ID for control logic.

        Parameters
        ----------
        devc_id : str
            Device ID for control logic

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["devc_id"] = devc_id
        return self

    def with_input_ids(self, *input_ids: str) -> "DevcBuilder":
        """
        Set input device IDs.

        Parameters
        ----------
        *input_ids : str
            Variable number of input device IDs

        Returns
        -------
        DevcBuilder
            Self for method chaining

        Examples
        --------
        >>> devc = DevcBuilder('CONTROL').with_input_ids('TEMP1', 'TEMP2', 'TEMP3').build()
        """
        self._params["input_id"] = list(input_ids)
        return self

    def with_initial_state(self, state: bool) -> "DevcBuilder":
        """
        Set initial state of controlled device.

        Parameters
        ----------
        state : bool
            Initial state (True=on, False=off)

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["initial_state"] = state
        return self

    def with_time_history(self, enabled: bool = True) -> "DevcBuilder":
        """
        Enable time history output.

        Parameters
        ----------
        enabled : bool, optional
            Enable time history, default: True

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["time_history"] = enabled
        return self

    def hide_coordinates(self, hide: bool = True) -> "DevcBuilder":
        """
        Hide coordinates in output.

        Parameters
        ----------
        hide : bool, optional
            Hide coordinates, default: True

        Returns
        -------
        DevcBuilder
            Self for method chaining
        """
        self._params["hide_coordinates"] = hide
        return self

    def build(self) -> Device:
        """
        Build the Device object.

        Returns
        -------
        Device
            The constructed Device namelist object

        Raises
        ------
        ValueError
            If required parameters are missing
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        # Validate required parameters
        if "quantity" not in self._params:
            raise ValueError(f"DevcBuilder '{self._id}': quantity is required")

        params = {"id": self._id, **self._params}
        devc = Device(**params)
        self._mark_built()
        return devc
