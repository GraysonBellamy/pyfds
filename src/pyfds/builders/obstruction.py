"""Builder for creating OBST namelists with fluent API."""

from ..core.geometry import Bounds3D
from ..core.namelists import Obstruction
from .base import Builder


class ObstructionBuilder(Builder[Obstruction]):
    """
    Builder for creating OBST namelists.

    Provides a fluent API for constructing obstructions (solid objects)
    with surface properties, control, and visualization options.

    Examples
    --------
    >>> # Simple obstruction with surface
    >>> box = ObstructionBuilder() \\
    ...     .bounds(0, 1, 0, 1, 0, 0.5) \\
    ...     .surface("FIRE") \\
    ...     .build()

    >>> # Wall with different surfaces per face
    >>> wall = ObstructionBuilder() \\
    ...     .bounds(0, 0.2, 0, 10, 0, 3) \\
    ...     .surfaces(sides="CONCRETE", top="PAINTED", bottom="CONCRETE") \\
    ...     .build()

    >>> # Burnable furniture
    >>> sofa = ObstructionBuilder() \\
    ...     .id("SOFA") \\
    ...     .bounds(2, 4, 2, 3, 0, 0.8) \\
    ...     .surface("FABRIC") \\
    ...     .burn_away(bulk_density=50.0) \\
    ...     .build()

    >>> # Controlled obstruction (door)
    >>> door = ObstructionBuilder() \\
    ...     .bounds(5, 5.1, 2, 3, 0, 2.1) \\
    ...     .surface("WOOD") \\
    ...     .controlled_by(ctrl_id="DOOR_CTRL") \\
    ...     .build()
    """

    def __init__(self) -> None:
        """Initialize the ObstructionBuilder."""
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
    ) -> "ObstructionBuilder":
        """
        Set obstruction bounds.

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
        ObstructionBuilder
            Self for method chaining

        Examples
        --------
        >>> # Using individual coordinates
        >>> obs = ObstructionBuilder().bounds(0, 1, 0, 1, 0, 0.5).build()

        >>> # Using Bounds3D
        >>> obs = ObstructionBuilder().bounds(Bounds3D.of(0, 1, 0, 1, 0, 0.5)).build()

        >>> # Using tuple
        >>> obs = ObstructionBuilder().bounds((0, 1, 0, 1, 0, 0.5)).build()
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

    def id(self, obstruction_id: str) -> "ObstructionBuilder":
        """
        Set obstruction identifier.

        Parameters
        ----------
        obstruction_id : str
            Unique identifier for the obstruction

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["id"] = obstruction_id
        return self

    # === Surface Properties ===

    def surface(self, surf_id: str) -> "ObstructionBuilder":
        """
        Set surface ID for all faces.

        Parameters
        ----------
        surf_id : str
            Surface ID to apply to all faces

        Returns
        -------
        ObstructionBuilder
            Self for method chaining

        Examples
        --------
        >>> obs = ObstructionBuilder().bounds(0,1,0,1,0,1).surface("CONCRETE").build()
        """
        self._params["surf_id"] = surf_id
        return self

    def surfaces(
        self,
        top: str | None = None,
        bottom: str | None = None,
        sides: str | None = None,
    ) -> "ObstructionBuilder":
        """
        Set different surfaces for different faces.

        Parameters
        ----------
        top : str, optional
            Surface ID for top face (+Z)
        bottom : str, optional
            Surface ID for bottom face (-Z)
        sides : str, optional
            Surface ID for side faces

        Returns
        -------
        ObstructionBuilder
            Self for method chaining

        Examples
        --------
        >>> wall = ObstructionBuilder().bounds(0,0.2,0,10,0,3) \\
        ...     .surfaces(top="CAP", bottom="CAP", sides="WALL") \\
        ...     .build()
        """
        if top is not None:
            self._params["surf_id_top"] = top
        if bottom is not None:
            self._params["surf_id_bottom"] = bottom
        if sides is not None:
            self._params["surf_id_sides"] = sides
        return self

    # === Visualization ===

    def color(self, color: str) -> "ObstructionBuilder":
        """
        Set named color for visualization.

        Parameters
        ----------
        color : str
            Color name (e.g., 'RED', 'BLUE', 'GRAY')

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["color"] = color
        return self

    def rgb(self, r: int, g: int, b: int) -> "ObstructionBuilder":
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
        ObstructionBuilder
            Self for method chaining
        """
        self._params["rgb"] = (r, g, b)
        return self

    def transparency(self, value: float) -> "ObstructionBuilder":
        """
        Set obstruction transparency.

        Parameters
        ----------
        value : float
            Transparency (0.0 = opaque, 1.0 = transparent)

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["transparency"] = value
        return self

    def outline(self) -> "ObstructionBuilder":
        """
        Draw as outline only in Smokeview.

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["outline"] = True
        return self

    # === Geometry Flags ===

    def thicken(self) -> "ObstructionBuilder":
        """
        Force obstruction to be at least one cell thick.

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["thicken"] = True
        return self

    def no_holes(self) -> "ObstructionBuilder":
        """
        Prevent HOLEs from punching through this obstruction.

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["permit_hole"] = False
        return self

    def not_removable(self) -> "ObstructionBuilder":
        """
        Prevent removal during simulation.

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["removable"] = False
        return self

    def no_vents(self) -> "ObstructionBuilder":
        """
        Prevent VENTs from attaching to this obstruction.

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["allow_vent"] = False
        return self

    # === Combustion ===

    def burn_away(
        self,
        bulk_density: float | None = None,
        interior_surface: str | None = None,
    ) -> "ObstructionBuilder":
        """
        Enable burn-away behavior.

        Parameters
        ----------
        bulk_density : float, optional
            Combustible mass per unit volume [kg/m³]
        interior_surface : str, optional
            Surface ID for newly exposed surfaces

        Returns
        -------
        ObstructionBuilder
            Self for method chaining

        Examples
        --------
        >>> sofa = ObstructionBuilder().bounds(0,1,0,1,0,0.5) \\
        ...     .surface("FABRIC") \\
        ...     .burn_away(bulk_density=50.0) \\
        ...     .build()
        """
        self._params["burn_away"] = True
        if bulk_density is not None:
            self._params["bulk_density"] = bulk_density
        if interior_surface is not None:
            self._params["surf_id_interior"] = interior_surface
        return self

    # === Control ===

    def controlled_by(
        self,
        ctrl_id: str | None = None,
        devc_id: str | None = None,
    ) -> "ObstructionBuilder":
        """
        Set control logic for obstruction activation.

        Parameters
        ----------
        ctrl_id : str, optional
            Control ID for activation
        devc_id : str, optional
            Device ID for activation

        Returns
        -------
        ObstructionBuilder
            Self for method chaining

        Examples
        --------
        >>> # Door controlled by timer
        >>> door = ObstructionBuilder().bounds(5,5.1,2,3,0,2) \\
        ...     .surface("WOOD") \\
        ...     .controlled_by(ctrl_id="DOOR_TIMER") \\
        ...     .build()
        """
        if ctrl_id is not None:
            self._params["ctrl_id"] = ctrl_id
        if devc_id is not None:
            self._params["devc_id"] = devc_id
        return self

    def mult(self, mult_id: str) -> "ObstructionBuilder":
        """
        Set MULT ID for array replication.

        Parameters
        ----------
        mult_id : str
            Multiplier ID for array pattern

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["mult_id"] = mult_id
        return self

    # === 3D Heat Transfer ===

    def ht3d(
        self,
        matl_id: str | list[str],
        cell_size: float | None = None,
    ) -> "ObstructionBuilder":
        """
        Enable 3D heat conduction.

        Parameters
        ----------
        matl_id : str or list[str]
            Material ID(s) for heat conduction
        cell_size : float, optional
            Interior cell size [m]

        Returns
        -------
        ObstructionBuilder
            Self for method chaining
        """
        self._params["ht3d"] = True
        self._params["matl_id"] = matl_id
        if cell_size is not None:
            self._params["cell_size"] = cell_size
        return self

    # === Build ===

    def build(self) -> Obstruction:
        """
        Build the Obstruction object.

        Returns
        -------
        Obstruction
            The constructed Obstruction namelist object

        Raises
        ------
        ValueError
            If required parameters are missing (bounds is required)
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        if self._bounds is None:
            raise ValueError("ObstructionBuilder: bounds are required (use .bounds())")

        obstruction = Obstruction(xb=self._bounds, **self._params)
        self._mark_built()
        return obstruction
