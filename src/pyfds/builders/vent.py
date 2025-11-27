"""Builder for creating VENT namelists with common configurations."""

from ..core.geometry import Point3D
from ..core.namelists import Vent
from .base import Builder


class VentBuilder(Builder[Vent]):
    """
    Builder for creating VENT namelists.

    Provides factory methods for creating common vent types including
    openings, HVAC vents, circular burners, and mesh boundaries.

    Examples
    --------
    >>> # Opening to ambient
    >>> door = VentBuilder.opening(
    ...     xb=(5, 5, 2, 4, 0, 2.1),
    ...     id='DOOR'
    ... )

    >>> # HVAC vent (define flow on SURF, not VENT)
    >>> hvac_vent = VentBuilder.opening(
    ...     xb=(5, 6, 5, 6, 3, 3),
    ...     id='SUPPLY_VENT'
    ... )

    >>> # Circular burner
    >>> burner = VentBuilder.circular_burner(
    ...     center=(0, 0, 0),
    ...     radius=0.5,
    ...     surf_id='FIRE',
    ...     id='BURNER'
    ... )

    >>> # Mesh boundary
    >>> boundary = VentBuilder.mesh_boundary(
    ...     mb='XMIN',
    ...     surf_id='OPEN'
    ... )
    """

    @classmethod
    def opening(
        cls,
        xb: tuple[float, float, float, float, float, float],
        id: str | None = None,
    ) -> Vent:
        """
        Create an opening to ambient.

        Parameters
        ----------
        xb : tuple[float, float, float, float, float, float]
            Bounding box (xmin, xmax, ymin, ymax, zmin, zmax)
        id : str, optional
            Vent identifier

        Returns
        -------
        Vent
            Opening vent object with SURF_ID='OPEN'

        Examples
        --------
        >>> door = VentBuilder.opening(xb=(5, 5, 2, 4, 0, 2.1), id='DOOR')
        >>> window = VentBuilder.opening(xb=(0, 0, 1, 2, 1, 1.5))
        """
        return Vent(xb=xb, surf_id="OPEN", id=id)

    @classmethod
    def circular_burner(
        cls,
        center: Point3D | tuple[float, float, float],
        radius: float,
        surf_id: str,
        id: str | None = None,
    ) -> Vent:
        """
        Create circular burner vent.

        Parameters
        ----------
        center : Point3D or tuple[float, float, float]
            Center point (x, y, z) in meters
        radius : float
            Burner radius in meters
        surf_id : str
            Surface ID defining the fire properties
        id : str, optional
            Vent identifier

        Returns
        -------
        Vent
            Circular vent object

        Examples
        --------
        >>> burner = VentBuilder.circular_burner(
        ...     center=(0, 0, 0),
        ...     radius=0.5,
        ...     surf_id='FIRE',
        ...     id='BURNER'
        ... )
        """
        if isinstance(center, tuple):
            center = Point3D.from_tuple(center)

        x, y, z = center.x, center.y, center.z
        # Create bounding box around circle
        xb = (x - radius, x + radius, y - radius, y + radius, z, z)

        return Vent(xb=xb, xyz=center, radius=radius, surf_id=surf_id, id=id)

    @classmethod
    def annular_burner(
        cls,
        center: Point3D | tuple[float, float, float],
        radius: float,
        radius_inner: float,
        surf_id: str,
        id: str | None = None,
    ) -> Vent:
        """
        Create annular (ring-shaped) burner vent.

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
        id : str, optional
            Vent identifier

        Returns
        -------
        Vent
            Annular vent object

        Examples
        --------
        >>> burner = VentBuilder.annular_burner(
        ...     center=(0, 0, 0),
        ...     radius=0.5,
        ...     radius_inner=0.3,
        ...     surf_id='FIRE'
        ... )
        """
        if isinstance(center, tuple):
            center = Point3D.from_tuple(center)

        x, y, z = center.x, center.y, center.z
        # Create bounding box around outer circle
        xb = (x - radius, x + radius, y - radius, y + radius, z, z)

        return Vent(
            xb=xb,
            xyz=center,
            radius=radius,
            radius_inner=radius_inner,
            surf_id=surf_id,
            id=id,
        )

    @classmethod
    def mesh_boundary(cls, mb: str, surf_id: str = "OPEN", mesh_id: str | None = None) -> Vent:
        """
        Create mesh boundary vent.

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
        Vent
            Mesh boundary vent object

        Examples
        --------
        >>> # Open boundary at X minimum
        >>> boundary = VentBuilder.mesh_boundary(mb='XMIN', surf_id='OPEN')

        >>> # Periodic boundary
        >>> periodic = VentBuilder.mesh_boundary(mb='XMIN', surf_id='PERIODIC')

        >>> # Mirror boundary
        >>> mirror = VentBuilder.mesh_boundary(mb='ZMIN', surf_id='MIRROR')
        """
        return Vent(mb=mb, surf_id=surf_id, mesh_id=mesh_id)

    @classmethod
    def door(
        cls,
        x: float,
        y_min: float,
        y_max: float,
        z_min: float = 0.0,
        z_max: float = 2.1,
        id: str | None = None,
    ) -> Vent:
        """
        Create a standard door opening.

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
        id : str, optional
            Vent identifier

        Returns
        -------
        Vent
            Door opening vent

        Examples
        --------
        >>> door = VentBuilder.door(x=5.0, y_min=2.0, y_max=3.0, id='DOOR_1')
        """
        xb = (x, x, y_min, y_max, z_min, z_max)
        return Vent(xb=xb, surf_id="OPEN", id=id)

    @classmethod
    def window(
        cls,
        x: float,
        y_min: float,
        y_max: float,
        z_min: float,
        z_max: float,
        id: str | None = None,
    ) -> Vent:
        """
        Create a window opening.

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
        id : str, optional
            Vent identifier

        Returns
        -------
        Vent
            Window opening vent

        Examples
        --------
        >>> window = VentBuilder.window(
        ...     x=0.0, y_min=1.0, y_max=2.0,
        ...     z_min=1.0, z_max=1.5, id='WINDOW_1'
        ... )
        """
        xb = (x, x, y_min, y_max, z_min, z_max)
        return Vent(xb=xb, surf_id="OPEN", id=id)

    def build(self) -> Vent:
        """
        Build method for consistency with Builder pattern.

        Note: VentBuilder uses class methods, so this is not typically used.

        Raises
        ------
        NotImplementedError
            This builder uses factory methods instead
        """
        raise NotImplementedError(
            "VentBuilder uses factory methods (opening, circular_burner, etc.) "
            "instead of the standard build() pattern"
        )
