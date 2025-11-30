"""
FDS GEOM namelist.

Unstructured geometry using triangulated surfaces (Beta feature).
"""

from pydantic import field_validator, model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase


class Geom(NamelistBase):
    """
    FDS GEOM namelist - unstructured geometry (Beta).

    Allows definition of complex geometry using triangulated surfaces
    or predefined shapes like spheres, cylinders, and terrain.

    Warning
    -------
    This is a Beta feature in FDS. Use with caution.

    Parameters
    ----------
    id : str, optional
        Geometry identifier
    surf_id : str, optional
        Default surface ID
    surf_ids : tuple[str, str, str], optional
        Surface IDs for (top, sides, bottom)
    surf_id6 : tuple[str, ...], optional
        Surface IDs for 6 faces
    verts : list[float], optional
        Vertex coordinates as flat list (x1,y1,z1,x2,y2,z2,...)
    faces : list[int], optional
        Face definitions (v1,v2,v3,surf_idx,...)
    binary_file : str, optional
        External binary geometry file
    move_id : str, optional
        MOVE transformation reference

    Examples
    --------
    >>> # Simple triangle
    >>> geom = Geom(
    ...     id='TRIANGLE',
    ...     surf_id='INERT',
    ...     verts=[0,0,0, 1,0,0, 0.5,1,0],
    ...     faces=[1,2,3,1]
    ... )

    >>> # Sphere
    >>> sphere = Geom(
    ...     id='BALL',
    ...     surf_id='STEEL',
    ...     sphere_origin=(5, 5, 1),
    ...     sphere_radius=0.5,
    ...     n_levels=3
    ... )
    """

    # Basic properties
    id: str | None = FdsField(None, description="Geometry identifier")
    surf_id: str = FdsField("INERT", description="Default surface ID")
    surf_ids: tuple[str, str, str] | None = FdsField(
        None, description="Surface IDs (top, sides, bottom)"
    )
    surf_id6: tuple[str, str, str, str, str, str] | None = FdsField(
        None, description="Surface IDs for 6 faces"
    )

    # Visualization
    color: str | None = FdsField(None, description="Color")
    rgb: tuple[int, int, int] | None = FdsField(None, description="RGB color")
    transparency: float = FdsField(1.0, ge=0, le=1, exclude_if=1.0, description="Transparency")

    # Mesh definition
    verts: list[float] | None = FdsField(None, description="Vertex coordinates")
    faces: list[int] | None = FdsField(None, description="Face definitions")
    binary_file: str | None = FdsField(None, description="Binary geometry file")

    # Transformation
    move_id: str | None = FdsField(None, description="MOVE transformation reference")

    # === Self-generated geometry ===

    # Block
    xb: tuple[float, float, float, float, float, float] | None = FdsField(
        None, description="Block bounds"
    )
    ijk: tuple[int, int, int] | None = FdsField(None, description="Block subdivisions")

    # Sphere
    sphere_origin: tuple[float, float, float] | None = FdsField(None, description="Sphere center")
    sphere_radius: float | None = FdsField(None, gt=0, description="Sphere radius")
    n_levels: int | None = FdsField(None, ge=0, description="Subdivision level")
    n_lat: int | None = FdsField(None, ge=3, description="Latitude divisions")
    n_long: int | None = FdsField(None, ge=3, description="Longitude divisions")

    # Cylinder
    cylinder_origin: tuple[float, float, float] | None = FdsField(
        None, description="Cylinder bottom center"
    )
    cylinder_axis: tuple[float, float, float] | None = FdsField(
        None, description="Cylinder axis direction"
    )
    cylinder_length: float | None = FdsField(None, gt=0, description="Cylinder length")
    cylinder_radius: float | None = FdsField(None, gt=0, description="Cylinder radius")
    cylinder_nseg_axis: int | None = FdsField(None, ge=1, description="Axial segments")
    cylinder_nseg_theta: int | None = FdsField(None, ge=3, description="Angular segments")

    # Terrain
    zvals: list[float] | None = FdsField(None, description="Terrain elevation data")
    zmin: float | None = FdsField(None, description="Terrain bottom elevation")
    is_terrain: bool = FdsField(False, exclude_if=False, description="Mark as terrain geometry")
    extend_terrain: bool = FdsField(
        False, exclude_if=False, description="Extend to domain boundaries"
    )
    zval_horizon: float | None = FdsField(None, description="Horizon elevation")

    # Extrusion
    poly: list[int] | None = FdsField(None, description="Polygon vertex indices")
    extrude: float | None = FdsField(None, description="Extrusion distance")

    # Thin geometry
    cell_block_ior: int | None = FdsField(None, description="Block direction (+/-1,2,3)")

    # Texture mapping
    texture_mapping: str | None = FdsField(None, description="RECTANGULAR or SPHERICAL")
    texture_origin: tuple[float, float, float] | None = FdsField(None, description="Texture origin")
    texture_scale: float = FdsField(1.0, gt=0, exclude_if=1.0, description="Texture scale")

    @field_validator("surf_id6")
    @classmethod
    def validate_surf_id6(cls, v: tuple[str, ...] | None) -> tuple[str, ...] | None:
        """Validate surf_id6 has exactly 6 elements."""
        if v is not None and len(v) != 6:
            raise ValueError("surf_id6 must have exactly 6 surface IDs")
        return v

    @field_validator("verts")
    @classmethod
    def validate_verts(cls, v: list[float] | None) -> list[float] | None:
        """Validate verts is multiple of 3 (x,y,z coordinates)."""
        if v is not None and len(v) % 3 != 0:
            raise ValueError("verts must contain triplets of (x,y,z) coordinates")
        return v

    @field_validator("faces")
    @classmethod
    def validate_faces(cls, v: list[int] | None) -> list[int] | None:
        """Validate faces contains valid face definitions."""
        if v is not None:
            if len(v) % 4 != 0:
                raise ValueError("faces must contain quadruplets of (v1,v2,v3,surf_idx)")
            # Check vertex indices are positive
            for i in range(0, len(v), 4):
                if v[i] < 1 or v[i + 1] < 1 or v[i + 2] < 1:
                    raise ValueError("Face vertex indices must be positive (1-based)")
        return v

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB color values."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in v):
                raise ValueError("RGB values must be in range 0-255")
        return v

    @field_validator("texture_mapping")
    @classmethod
    def validate_texture_mapping(cls, v: str | None) -> str | None:
        """Validate texture mapping type."""
        if v is not None and v.upper() not in ["RECTANGULAR", "SPHERICAL"]:
            raise ValueError("texture_mapping must be 'RECTANGULAR' or 'SPHERICAL'")
        return v.upper() if v else v

    @field_validator("cell_block_ior")
    @classmethod
    def validate_cell_block_ior(cls, v: int | None) -> int | None:
        """Validate cell block orientation."""
        if v is not None and abs(v) not in [1, 2, 3]:
            raise ValueError("cell_block_ior must be +/-1, +/-2, or +/-3")
        return v

    @model_validator(mode="after")
    def validate_geometry_definition(self) -> "Geom":
        """Validate that geometry is properly defined."""
        # Must have either verts/faces, binary_file, or a self-generated shape
        has_custom_mesh = self.verts is not None and self.faces is not None
        has_binary = self.binary_file is not None
        has_block = self.xb is not None
        has_sphere = self.sphere_origin is not None and self.sphere_radius is not None
        has_cylinder = (
            self.cylinder_origin is not None
            and self.cylinder_axis is not None
            and self.cylinder_length is not None
            and self.cylinder_radius is not None
        )
        has_terrain = self.zvals is not None
        has_extrusion = self.poly is not None and self.extrude is not None

        geometry_types = [
            has_custom_mesh,
            has_binary,
            has_block,
            has_sphere,
            has_cylinder,
            has_terrain,
            has_extrusion,
        ]

        if sum(geometry_types) == 0:
            raise ValueError(
                "Must specify geometry via verts/faces, binary_file, "
                "or one of the self-generated shapes (xb, sphere_*, cylinder_*, zvals, poly/extrude)"
            )
        if sum(geometry_types) > 1:
            raise ValueError("Cannot specify multiple geometry definitions simultaneously")

        return self

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "GEOM"
