"""Builder for creating VENT namelists with common configurations."""

from typing import Any

from ..core.geometry import Point3D
from ..core.namelists import Vent
from .base import Builder


class VentBuilder(Builder[Vent]):
    """
    Builder for creating VENT namelists.

    Provides fluent API for creating vents with geometry, control,
    fire spread, and synthetic turbulence parameters.

    Examples
    --------
    >>> # Opening to ambient
    >>> door = VentBuilder("DOOR").opening(xb=(5, 5, 2, 4, 0, 2.1)).build()

    >>> # Circular burner with fire spread
    >>> burner = (
    ...     VentBuilder("BURNER")
    ...     .circular_burner(center=(0, 0, 0), radius=0.5, surf_id='FIRE')
    ...     .with_fire_spread(spread_rate=0.01)
    ...     .build()
    ... )

    >>> # Mesh boundary with synthetic turbulence
    >>> boundary = (
    ...     VentBuilder("BOUNDARY")
    ...     .mesh_boundary(mb='XMIN', surf_id='OPEN')
    ...     .with_synthetic_turbulence(n_eddy=100, l_eddy=0.1, vel_rms=0.5)
    ...     .build()
    ... )
    """

    def __init__(self, id: str | None = None):
        """Initialize VentBuilder."""
        super().__init__()
        self._params: dict[str, Any] = {}
        if id is not None:
            self._params["id"] = id

    def opening(self, xb: tuple[float, float, float, float, float, float]) -> "VentBuilder":
        """
        Configure as an opening to ambient.

        Parameters
        ----------
        xb : tuple[float, float, float, float, float, float]
            Bounding box (xmin, xmax, ymin, ymax, zmin, zmax)

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["xb"] = xb
        self._params["surf_id"] = "OPEN"
        return self

    def circular_burner(
        self, center: Point3D | tuple[float, float, float], radius: float, surf_id: str
    ) -> "VentBuilder":
        """
        Configure as circular burner vent.

        Parameters
        ----------
        center : Point3D or tuple[float, float, float]
            Center point (x, y, z) in meters
        radius : float
            Burner radius in meters
        surf_id : str
            Surface ID defining the fire properties

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        if isinstance(center, tuple):
            center = Point3D.from_tuple(center)

        x, y, z = center.x, center.y, center.z
        # Create bounding box around circle
        xb = (x - radius, x + radius, y - radius, y + radius, z, z)

        self._params["xb"] = xb
        self._params["xyz"] = center
        self._params["radius"] = radius
        self._params["surf_id"] = surf_id
        return self

    def annular_burner(
        self,
        center: Point3D | tuple[float, float, float],
        radius: float,
        radius_inner: float,
        surf_id: str,
    ) -> "VentBuilder":
        """
        Configure as annular (ring-shaped) burner vent.

        Parameters
        ----------
        center : Point3D or tuple[float, float, float]
            Center point (x, y, z) in meters
        radius : float
            Outer radius in meters
        radius_inner : float
            Inner radius in meters
        surf_id : str
            Surface ID defining the fire properties

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        if isinstance(center, tuple):
            center = Point3D.from_tuple(center)

        x, y, z = center.x, center.y, center.z
        # Create bounding box around outer circle
        xb = (x - radius, x + radius, y - radius, y + radius, z, z)

        self._params["xb"] = xb
        self._params["xyz"] = center
        self._params["radius"] = radius
        self._params["radius_inner"] = radius_inner
        self._params["surf_id"] = surf_id
        return self

    def mesh_boundary(
        self, mb: str, surf_id: str = "OPEN", mesh_id: str | None = None
    ) -> "VentBuilder":
        """
        Configure as mesh boundary vent.

        Parameters
        ----------
        mb : str
            Mesh boundary location ('XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX')
        surf_id : str, optional
            Surface ID, default: 'OPEN'
        mesh_id : str, optional
            Mesh identifier for multi-mesh simulations

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["mb"] = mb
        self._params["surf_id"] = surf_id
        if mesh_id is not None:
            self._params["mesh_id"] = mesh_id
        return self

    def door(
        self, x: float, y_min: float, y_max: float, z_min: float = 0.0, z_max: float = 2.1
    ) -> "VentBuilder":
        """
        Configure as a standard door opening.

        Parameters
        ----------
        x : float
            X-location of the door (plane)
        y_min : float
            Minimum Y coordinate
        y_max : float
            Maximum Y coordinate
        z_min : float, optional
            Minimum Z coordinate, default: 0.0 (floor)
        z_max : float, optional
            Maximum Z coordinate, default: 2.1 (standard door height)

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        xb = (x, x, y_min, y_max, z_min, z_max)
        self._params["xb"] = xb
        self._params["surf_id"] = "OPEN"
        return self

    def window(
        self, x: float, y_min: float, y_max: float, z_min: float, z_max: float
    ) -> "VentBuilder":
        """
        Configure as a window opening.

        Parameters
        ----------
        x : float
            X-location of the window (plane)
        y_min : float
            Minimum Y coordinate
        y_max : float
            Maximum Y coordinate
        z_min : float
            Minimum Z coordinate (sill height)
        z_max : float
            Maximum Z coordinate (window top)

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        xb = (x, x, y_min, y_max, z_min, z_max)
        self._params["xb"] = xb
        self._params["surf_id"] = "OPEN"
        return self

    # === PHASE 4: VENT ENHANCEMENTS ===
    def with_geometry_params(
        self,
        db: str | None = None,
        pbx: float | None = None,
        pby: float | None = None,
        pbz: float | None = None,
        ior: int | None = None,
    ) -> "VentBuilder":
        """
        Set geometry parameters for domain boundaries and orientation.

        Parameters
        ----------
        db : str, optional
            Domain boundary location
        pbx, pby, pbz : float, optional
            Plane positions for domain boundaries
        ior : int, optional
            Orientation: +/-1, +/-2, +/-3

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        if db is not None:
            self._params["db"] = db
        if pbx is not None:
            self._params["pbx"] = pbx
        if pby is not None:
            self._params["pby"] = pby
        if pbz is not None:
            self._params["pbz"] = pbz
        if ior is not None:
            self._params["ior"] = ior
        return self

    def with_control_params(
        self, outline: bool = False, mult_id: str | None = None, obst_id: str | None = None
    ) -> "VentBuilder":
        """
        Set control and visualization parameters.

        Parameters
        ----------
        outline : bool, optional
            Draw outline only (default: False)
        mult_id : str, optional
            Multiplier ID for array replication
        obst_id : str, optional
            Associated obstruction ID

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["outline"] = outline
        if mult_id is not None:
            self._params["mult_id"] = mult_id
        if obst_id is not None:
            self._params["obst_id"] = obst_id
        return self

    def with_texture(self, texture_origin: tuple[float, float, float]) -> "VentBuilder":
        """
        Set texture origin for visualization.

        Parameters
        ----------
        texture_origin : tuple[float, float, float]
            Texture origin point (x, y, z)

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["texture_origin"] = texture_origin
        return self

    def with_fire_spread(self, spread_rate: float) -> "VentBuilder":
        """
        Set fire spread rate for circular vents.

        Parameters
        ----------
        spread_rate : float
            Fire spread rate [m/s]

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        self._params["spread_rate"] = spread_rate
        return self

    def with_open_boundary_ramps(
        self, tmp_exterior_ramp: str | None = None, pressure_ramp: str | None = None
    ) -> "VentBuilder":
        """
        Set ramps for open boundary conditions.

        Parameters
        ----------
        tmp_exterior_ramp : str, optional
            Ramp ID for exterior temperature
        pressure_ramp : str, optional
            Ramp ID for dynamic pressure

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        if tmp_exterior_ramp is not None:
            self._params["tmp_exterior_ramp"] = tmp_exterior_ramp
        if pressure_ramp is not None:
            self._params["pressure_ramp"] = pressure_ramp
        return self

    def with_synthetic_turbulence(
        self,
        n_eddy: int | None = None,
        l_eddy: float | None = None,
        l_eddy_ij: list[list[float]] | None = None,
        vel_rms: float | None = None,
        reynolds_stress: list[list[float]] | None = None,
        uvw: tuple[float, float, float] | None = None,
    ) -> "VentBuilder":
        """
        Configure synthetic turbulence parameters.

        Parameters
        ----------
        n_eddy : int, optional
            Number of synthetic eddies
        l_eddy : float, optional
            Eddy length scale [m]
        l_eddy_ij : list[list[float]], optional
            Anisotropic eddy length scales (3x3 matrix)
        vel_rms : float, optional
            RMS velocity fluctuation [m/s]
        reynolds_stress : list[list[float]], optional
            Reynolds stress tensor (3x3 matrix)
        uvw : tuple[float, float, float], optional
            Mean velocity components [m/s]

        Returns
        -------
        VentBuilder
            Self for method chaining
        """
        if n_eddy is not None:
            self._params["n_eddy"] = n_eddy
        if l_eddy is not None:
            self._params["l_eddy"] = l_eddy
        if l_eddy_ij is not None:
            self._params["l_eddy_ij"] = l_eddy_ij
        if vel_rms is not None:
            self._params["vel_rms"] = vel_rms
        if reynolds_stress is not None:
            self._params["reynolds_stress"] = reynolds_stress
        if uvw is not None:
            self._params["uvw"] = uvw
        return self

    def build(self) -> Vent:
        """
        Build and return the Vent object.

        Returns
        -------
        Vent
            The constructed vent object

        Raises
        ------
        RuntimeError
            If builder has already been used
        """
        if self._built:
            raise RuntimeError("Builder has already been used")
        self._built = True
        return Vent(**self._params)
