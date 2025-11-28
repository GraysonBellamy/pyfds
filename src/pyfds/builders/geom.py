"""
Builders for GEOM namelist.

Provides fluent API for creating complex geometry objects.
"""

from typing import Any, Self

from pyfds.builders.base import Builder
from pyfds.core.namelists.geom import Geom


class GeomBuilder(Builder[Geom]):
    """
    Builder for FDS GEOM namelist.

    Provides a fluent API for creating complex geometry objects including
    triangulated surfaces, predefined shapes, and terrain.

    Examples
    --------
    >>> # Simple triangulated surface
    >>> geom = (GeomBuilder("TRIANGLE")
    ...         .with_surface("STEEL")
    ...         .with_vertices([(0,0,0), (1,0,0), (0.5,1,0)])
    ...         .with_faces([(1,2,3,1)])
    ...         .build())

    >>> # Sphere
    >>> sphere = (GeomBuilder("BALL")
    ...          .with_surface("STEEL")
    ...          .sphere(center=(5,5,1), radius=0.5, subdivisions=3)
    ...          .build())

    >>> # Terrain from elevation data
    >>> terrain = (GeomBuilder("LANDSCAPE")
    ...           .with_surface("DIRT")
    ...           .terrain(elevations=[0,1,2,1,0], extend_to_domain=True)
    ...           .build())
    """

    def __init__(self, id: str | None = None):
        """Initialize GeomBuilder."""
        super().__init__()
        self._params: dict[str, Any] = {}
        if id is not None:
            self._params["id"] = id

    def with_surface(
        self,
        surf_id: str,
        surf_ids: tuple[str, str, str] | None = None,
        surf_id6: tuple[str, str, str, str, str, str] | None = None,
    ) -> Self:
        """
        Set surface properties.

        Parameters
        ----------
        surf_id : str
            Default surface ID
        surf_ids : tuple[str, str, str], optional
            Surface IDs for (top, sides, bottom)
        surf_id6 : tuple[str, str, str, str, str, str], optional
            Surface IDs for 6 faces

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["surf_id"] = surf_id
        if surf_ids:
            self._params["surf_ids"] = surf_ids
        if surf_id6:
            self._params["surf_id6"] = surf_id6
        return self

    def with_visualization(
        self,
        color: str | None = None,
        rgb: tuple[int, int, int] | None = None,
        transparency: float = 1.0,
    ) -> Self:
        """
        Set visualization properties.

        Parameters
        ----------
        color : str, optional
            Named color
        rgb : tuple[int, int, int], optional
            RGB color values (0-255)
        transparency : float, optional
            Transparency (0=clear, 1=opaque)

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        if color:
            self._params["color"] = color
        if rgb:
            self._params["rgb"] = rgb
        if transparency != 1.0:
            self._params["transparency"] = transparency
        return self

    def with_vertices(self, vertices: list[tuple[float, float, float]]) -> Self:
        """
        Set vertex coordinates for triangulated surface.

        Parameters
        ----------
        vertices : list[tuple[float, float, float]]
            List of (x,y,z) vertex coordinates

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        # Flatten the vertices into a single list
        self._params["verts"] = [coord for vertex in vertices for coord in vertex]
        return self

    def with_faces(self, faces: list[tuple[int, int, int, int]]) -> Self:
        """
        Set face definitions for triangulated surface.

        Parameters
        ----------
        faces : list[tuple[int, int, int, int]]
            List of (v1,v2,v3,surf_idx) face definitions (1-based vertex indices)

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        # Flatten the faces into a single list
        self._params["faces"] = [idx for face in faces for idx in face]
        return self

    def with_binary_file(self, filename: str) -> Self:
        """
        Load geometry from external binary file.

        Parameters
        ----------
        filename : str
            Path to binary geometry file

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["binary_file"] = filename
        return self

    def with_transformation(self, move_id: str) -> Self:
        """
        Apply a transformation to the geometry.

        Parameters
        ----------
        move_id : str
            ID of MOVE namelist transformation

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["move_id"] = move_id
        return self

    def block(
        self,
        xb: tuple[float, float, float, float, float, float],
        ijk: tuple[int, int, int] | None = None,
    ) -> Self:
        """
        Create a rectangular block geometry.

        Parameters
        ----------
        xb : tuple[float, float, float, float, float, float]
            Block bounds (xmin,xmax,ymin,ymax,zmin,zmax)
        ijk : tuple[int, int, int], optional
            Block subdivisions

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["xb"] = xb
        if ijk:
            self._params["ijk"] = ijk
        return self

    def sphere(
        self,
        center: tuple[float, float, float],
        radius: float,
        subdivisions: int = 2,
        n_lat: int = 6,
        n_long: int = 12,
    ) -> Self:
        """
        Create a spherical geometry.

        Parameters
        ----------
        center : tuple[float, float, float]
            Sphere center coordinates
        radius : float
            Sphere radius
        subdivisions : int, optional
            Number of subdivision levels
        n_lat : int, optional
            Number of latitude divisions
        n_long : int, optional
            Number of longitude divisions

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["sphere_origin"] = center
        self._params["sphere_radius"] = radius
        self._params["n_levels"] = subdivisions
        self._params["n_lat"] = n_lat
        self._params["n_long"] = n_long
        return self

    def cylinder(
        self,
        origin: tuple[float, float, float],
        axis: tuple[float, float, float],
        length: float,
        radius: float,
        nseg_axis: int = 4,
        nseg_theta: int = 8,
    ) -> Self:
        """
        Create a cylindrical geometry.

        Parameters
        ----------
        origin : tuple[float, float, float]
            Cylinder bottom center
        axis : tuple[float, float, float]
            Cylinder axis direction vector
        length : float
            Cylinder length
        radius : float
            Cylinder radius
        nseg_axis : int, optional
            Axial segment count
        nseg_theta : int, optional
            Angular segment count

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["cylinder_origin"] = origin
        self._params["cylinder_axis"] = axis
        self._params["cylinder_length"] = length
        self._params["cylinder_radius"] = radius
        self._params["cylinder_nseg_axis"] = nseg_axis
        self._params["cylinder_nseg_theta"] = nseg_theta
        return self

    def terrain(
        self,
        elevations: list[float],
        zmin: float | None = None,
        extend_to_domain: bool = False,
        horizon: float | None = None,
    ) -> Self:
        """
        Create terrain geometry from elevation data.

        Parameters
        ----------
        elevations : list[float]
            Elevation values for terrain
        zmin : float, optional
            Minimum elevation
        extend_to_domain : bool, optional
            Extend terrain to domain boundaries
        horizon : float, optional
            Horizon elevation

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["zvals"] = elevations
        if zmin is not None:
            self._params["zmin"] = zmin
        self._params["is_terrain"] = True
        if extend_to_domain:
            self._params["extend_terrain"] = True
        if horizon is not None:
            self._params["zval_horizon"] = horizon
        return self

    def extrude_polygon(self, vertices: list[int], distance: float) -> Self:
        """
        Create geometry by extruding a polygon.

        Parameters
        ----------
        vertices : list[int]
            Vertex indices defining the polygon
        distance : float
            Extrusion distance

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["poly"] = vertices
        self._params["extrude"] = distance
        return self

    def thin_geometry(self, orientation: int) -> Self:
        """
        Create thin geometry (surface-like).

        Parameters
        ----------
        orientation : int
            Block direction (+/-1,2,3 for x,y,z axes)

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["cell_block_ior"] = orientation
        return self

    def with_texture(
        self,
        mapping: str = "RECTANGULAR",
        origin: tuple[float, float, float] | None = None,
        scale: float = 1.0,
    ) -> Self:
        """
        Set texture mapping properties.

        Parameters
        ----------
        mapping : str, optional
            Mapping type ("RECTANGULAR" or "SPHERICAL")
        origin : tuple[float, float, float], optional
            Texture origin point
        scale : float, optional
            Texture scale factor

        Returns
        -------
        Self
            Builder instance for method chaining
        """
        self._params["texture_mapping"] = mapping.upper()
        if origin:
            self._params["texture_origin"] = origin
        if scale != 1.0:
            self._params["texture_scale"] = scale
        return self

    def build(self) -> Geom:
        """
        Build and return the Geom object.

        Returns
        -------
        Geom
            The constructed geometry object

        Raises
        ------
        RuntimeError
            If builder has already been used
        """
        if self._built:
            raise RuntimeError("Builder has already been used")
        self._built = True
        return Geom(**self._params)
