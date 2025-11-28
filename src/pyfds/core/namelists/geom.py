"""
FDS GEOM namelist.

Unstructured geometry using triangulated surfaces (Beta feature).
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.namelists.base import NamelistBase


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
    id: str | None = Field(None, description="Geometry identifier")
    surf_id: str = Field("INERT", description="Default surface ID")
    surf_ids: tuple[str, str, str] | None = Field(
        None, description="Surface IDs (top, sides, bottom)"
    )
    surf_id6: tuple[str, str, str, str, str, str] | None = Field(
        None, description="Surface IDs for 6 faces"
    )

    # Visualization
    color: str | None = Field(None, description="Color")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    transparency: float = Field(1.0, ge=0, le=1, description="Transparency")

    # Mesh definition
    verts: list[float] | None = Field(None, description="Vertex coordinates")
    faces: list[int] | None = Field(None, description="Face definitions")
    binary_file: str | None = Field(None, description="Binary geometry file")

    # Transformation
    move_id: str | None = Field(None, description="MOVE transformation reference")

    # === Self-generated geometry ===

    # Block
    xb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Block bounds"
    )
    ijk: tuple[int, int, int] | None = Field(None, description="Block subdivisions")

    # Sphere
    sphere_origin: tuple[float, float, float] | None = Field(None, description="Sphere center")
    sphere_radius: float | None = Field(None, gt=0, description="Sphere radius")
    n_levels: int | None = Field(None, ge=0, description="Subdivision level")
    n_lat: int | None = Field(None, ge=3, description="Latitude divisions")
    n_long: int | None = Field(None, ge=3, description="Longitude divisions")

    # Cylinder
    cylinder_origin: tuple[float, float, float] | None = Field(
        None, description="Cylinder bottom center"
    )
    cylinder_axis: tuple[float, float, float] | None = Field(
        None, description="Cylinder axis direction"
    )
    cylinder_length: float | None = Field(None, gt=0, description="Cylinder length")
    cylinder_radius: float | None = Field(None, gt=0, description="Cylinder radius")
    cylinder_nseg_axis: int | None = Field(None, ge=1, description="Axial segments")
    cylinder_nseg_theta: int | None = Field(None, ge=3, description="Angular segments")

    # Terrain
    zvals: list[float] | None = Field(None, description="Terrain elevation data")
    zmin: float | None = Field(None, description="Terrain bottom elevation")
    is_terrain: bool = Field(False, description="Mark as terrain geometry")
    extend_terrain: bool = Field(False, description="Extend to domain boundaries")
    zval_horizon: float | None = Field(None, description="Horizon elevation")

    # Extrusion
    poly: list[int] | None = Field(None, description="Polygon vertex indices")
    extrude: float | None = Field(None, description="Extrusion distance")

    # Thin geometry
    cell_block_ior: int | None = Field(None, description="Block direction (+/-1,2,3)")

    # Texture mapping
    texture_mapping: str | None = Field(None, description="RECTANGULAR or SPHERICAL")
    texture_origin: tuple[float, float, float] | None = Field(None, description="Texture origin")
    texture_scale: float = Field(1.0, gt=0, description="Texture scale")

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

    def to_fds(self) -> str:
        """Generate FDS GEOM namelist."""
        params: dict[str, Any] = {}

        if self.id:
            params["id"] = self.id
        params["surf_id"] = self.surf_id

        if self.surf_ids:
            params["surf_ids"] = self.surf_ids
        if self.surf_id6:
            params["surf_id6"] = self.surf_id6

        if self.color:
            params["color"] = self.color
        if self.rgb:
            params["rgb"] = self.rgb
        if self.transparency != 1.0:
            params["transparency"] = self.transparency

        if self.verts:
            params["verts"] = self.verts
        if self.faces:
            params["faces"] = self.faces
        if self.binary_file:
            params["binary_file"] = self.binary_file

        if self.move_id:
            params["move_id"] = self.move_id

        # Self-generated geometry
        if self.xb:
            params["xb"] = self.xb
        if self.ijk:
            params["ijk"] = self.ijk

        if self.sphere_origin:
            params["sphere_origin"] = self.sphere_origin
        if self.sphere_radius is not None:
            params["sphere_radius"] = self.sphere_radius
        if self.n_levels is not None:
            params["n_levels"] = self.n_levels
        if self.n_lat is not None:
            params["n_lat"] = self.n_lat
        if self.n_long is not None:
            params["n_long"] = self.n_long

        if self.cylinder_origin:
            params["cylinder_origin"] = self.cylinder_origin
        if self.cylinder_axis:
            params["cylinder_axis"] = self.cylinder_axis
        if self.cylinder_length is not None:
            params["cylinder_length"] = self.cylinder_length
        if self.cylinder_radius is not None:
            params["cylinder_radius"] = self.cylinder_radius
        if self.cylinder_nseg_axis is not None:
            params["cylinder_nseg_axis"] = self.cylinder_nseg_axis
        if self.cylinder_nseg_theta is not None:
            params["cylinder_nseg_theta"] = self.cylinder_nseg_theta

        if self.zvals:
            params["zvals"] = self.zvals
        if self.zmin is not None:
            params["zmin"] = self.zmin
        if self.is_terrain:
            params["is_terrain"] = True
        if self.extend_terrain:
            params["extend_terrain"] = True
        if self.zval_horizon is not None:
            params["zval_horizon"] = self.zval_horizon

        if self.poly:
            params["poly"] = self.poly
        if self.extrude is not None:
            params["extrude"] = self.extrude

        if self.cell_block_ior is not None:
            params["cell_block_ior"] = self.cell_block_ior

        if self.texture_mapping:
            params["texture_mapping"] = self.texture_mapping
        if self.texture_origin:
            params["texture_origin"] = self.texture_origin
        if self.texture_scale != 1.0:
            params["texture_scale"] = self.texture_scale

        return self._build_namelist("GEOM", params)
