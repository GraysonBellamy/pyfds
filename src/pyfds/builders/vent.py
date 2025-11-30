"""Builder for creating VENT namelists with fluent API."""

from ..core.geometry import Bounds3D, Point3D
from ..core.namelists import Vent
from .base import Builder


class VentBuilder(Builder[Vent]):
    """
    Builder for creating VENT namelists.

    Provides a fluent API for constructing vents including openings,
    HVAC connections, and mesh boundary conditions.

    Examples
    --------
    >>> # Door opening to ambient
    >>> door = VentBuilder() \\
    ...     .bounds(5, 5, 2, 4, 0, 2.1) \\
    ...     .open() \\
    ...     .build()

    >>> # HVAC supply vent
    >>> supply = VentBuilder() \\
    ...     .bounds(2, 3, 2, 3, 3, 3) \\
    ...     .surface("SUPPLY") \\
    ...     .build()

    >>> # Circular burner
    >>> burner = VentBuilder() \\
    ...     .bounds(-0.5, 0.5, -0.5, 0.5, 0, 0) \\
    ...     .surface("FIRE") \\
    ...     .circular(center=(0, 0, 0), radius=0.3) \\
    ...     .build()

    >>> # Mesh boundary vent
    >>> boundary = VentBuilder() \\
    ...     .mesh_boundary("XMIN") \\
    ...     .open() \\
    ...     .build()
    """

    def __init__(self) -> None:
        """Initialize the VentBuilder."""
        super().__init__()
        self._bounds: Bounds3D | None = None
        self._params: dict = {}

    # === Geometry ===

    def bounds(
        self,
        xmin_or_bounds: float | Bounds3D | tuple[float, float, float, float, float, float],
        xmax: float | None = None,
        ymin: float | None = None,
        ymax: float | None = None,
        zmin: float | None = None,
        zmax: float | None = None,
    ) -> "VentBuilder":
        """
        Set vent bounds.

        VENTs must be planar (one dimension has zero thickness).

        Parameters
        ----------
        xmin_or_bounds : float, Bounds3D, or tuple
            Either xmin value, a Bounds3D object, or (xmin, xmax, ymin, ymax, zmin, zmax) tuple
        xmax : float, optional
            Maximum x coordinate
        ymin : float, optional
            Minimum y coordinate
        ymax : float, optional
            Maximum y coordinate
        zmin : float, optional
            Minimum z coordinate
        zmax : float, optional
            Maximum z coordinate

        Returns
        -------
        VentBuilder
            Self for method chaining

        Examples
        --------
        >>> # Floor vent (z thickness = 0)
        >>> vent = VentBuilder().bounds(2, 3, 2, 3, 0, 0).surface("FIRE").build()

        >>> # Wall opening (x thickness = 0)
        >>> door = VentBuilder().bounds(5, 5, 2, 4, 0, 2.1).open().build()
        """
        if isinstance(xmin_or_bounds, Bounds3D):
            self._bounds = xmin_or_bounds
        elif isinstance(xmin_or_bounds, tuple):
            self._bounds = Bounds3D.of(*xmin_or_bounds)
        else:
            if any(v is None for v in [xmax, ymin, ymax, zmin, zmax]):
                raise ValueError("All 6 bounds coordinates required")
            self._bounds = Bounds3D.of(xmin_or_bounds, xmax, ymin, ymax, zmin, zmax)  # type: ignore
        return self

    def mesh_boundary(self, mb: str) -> "VentBuilder":
        """
        Place vent on mesh boundary.

        Parameters
        ----------
        mb : str
            Mesh boundary: 'XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX'

        Returns
        -------
        VentBuilder
            Self for method chaining

        Examples
        --------
        >>> # Open bottom boundary
        >>> boundary = VentBuilder().mesh_boundary("ZMIN").open().build()
        """
        self._params["mb"] = mb
        return self

    def circular(
        self,
        center: tuple[float, float, float] | Point3D,
        radius: float,
        inner_radius: float | None = None,
    ) -> "VentBuilder":
        """
        Set circular geometry.

        Parameters
        ----------
        center : tuple or Point3D
            Center point (x, y, z)
        radius : float
            Outer radius [m]
        inner_radius : float, optional
            Inner radius for annular vent [m]

        Returns
        -------
        VentBuilder
            Self for method chaining

        Examples
        --------
        >>> # Circular burner
        >>> burner = VentBuilder() \\
        ...     .bounds(-0.5, 0.5, -0.5, 0.5, 0, 0) \\
        ...     .surface("FIRE") \\
        ...     .circular(center=(0, 0, 0), radius=0.3) \\
        ...     .build()
        """
        if isinstance(center, tuple):
            center = Point3D.of(*center)
        self._params["xyz"] = center
        self._params["radius"] = radius
        if inner_radius is not None:
            self._params["radius_inner"] = inner_radius
        return self

    def id(self, vent_id: str) -> "VentBuilder":
        """
        Set vent identifier.

        Parameters
        ----------
        vent_id : str
            Unique identifier for the vent

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["id"] = vent_id
        return self

    # === Surface Properties ===

    def surface(self, surf_id: str) -> "VentBuilder":
        """
        Set surface ID.

        Parameters
        ----------
        surf_id : str
            Surface ID for vent properties

        Returns
        -------
        VentBuilder
            Self for method chaining

        Examples
        --------
        >>> vent = VentBuilder().bounds(0,1,0,1,0,0).surface("FIRE").build()
        """
        self._params["surf_id"] = surf_id
        return self

    def open(self) -> "VentBuilder":
        """
        Set as open boundary (passive opening to ambient).

        Returns
        -------
        VentBuilder
            Self for method chaining

        Examples
        --------
        >>> door = VentBuilder().bounds(5,5,2,4,0,2).open().build()
        """
        self._params["surf_id"] = "OPEN"
        return self

    def mirror(self) -> "VentBuilder":
        """
        Set as mirror boundary (symmetry plane).

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["surf_id"] = "MIRROR"
        return self

    def periodic(self) -> "VentBuilder":
        """
        Set as periodic boundary.

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["surf_id"] = "PERIODIC"
        return self

    # === Control ===

    def controlled_by(
        self,
        ctrl_id: str | None = None,
        devc_id: str | None = None,
    ) -> "VentBuilder":
        """
        Set control logic for vent activation.

        Parameters
        ----------
        ctrl_id : str, optional
            Control ID for activation
        devc_id : str, optional
            Device ID for activation

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        if ctrl_id is not None:
            self._params["ctrl_id"] = ctrl_id
        if devc_id is not None:
            self._params["devc_id"] = devc_id
        return self

    def delay(self, seconds: float) -> "VentBuilder":
        """
        Set activation delay.

        Parameters
        ----------
        seconds : float
            Delay before activation [s]

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["delay"] = seconds
        return self

    def activate_at(self, time: float) -> "VentBuilder":
        """
        Set activation time.

        Parameters
        ----------
        time : float
            Time to activate vent [s]

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["t_activate"] = time
        return self

    # === Advanced ===

    def dynamic_pressure(self) -> "VentBuilder":
        """
        Enable dynamic pressure boundary condition.

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["dynamic_pressure"] = True
        return self

    def exterior_temperature(self, tmp: float) -> "VentBuilder":
        """
        Set exterior temperature.

        Parameters
        ----------
        tmp : float
            Exterior temperature [°C]

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["tmp_exterior"] = tmp
        return self

    # === Visualization ===

    def color(self, color: str) -> "VentBuilder":
        """
        Set named color for visualization.

        Parameters
        ----------
        color : str
            Color name (e.g., 'RED', 'BLUE', 'GREEN')

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["color"] = color
        return self

    def rgb(self, r: int, g: int, b: int) -> "VentBuilder":
        """
        Set RGB color for visualization.

        Parameters
        ----------
        r : int
            Red component (0-255)
        g : int
            Green component (0-255)
        b : int
            Blue component (0-255)

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["rgb"] = (r, g, b)
        return self

    def transparency(self, value: float) -> "VentBuilder":
        """
        Set vent transparency.

        Parameters
        ----------
        value : float
            Transparency (0.0 = opaque, 1.0 = transparent)

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["transparency"] = value
        return self

    # === Build ===

    def build(self) -> Vent:
        """
        Build the Vent object.

        Returns
        -------
        Vent
            The constructed Vent namelist object

        Raises
        ------
        ValueError
            If neither bounds (xb) nor mesh boundary (mb) is specified
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        if self._bounds is None and "mb" not in self._params:
            raise ValueError(
                "VentBuilder: either bounds (use .bounds()) or mesh boundary (use .mesh_boundary()) is required"
            )

        params = dict(self._params)
        if self._bounds is not None:
            params["xb"] = self._bounds

        vent = Vent(**params)
        self._mark_built()
        return vent
