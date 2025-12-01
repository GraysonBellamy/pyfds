"""FDS OBST namelist for obstructions and solid objects.

Obstructions define solid objects in the computational domain including
walls, furniture, and fire sources with optional 3D heat transfer.

FDS User Guide Reference: Table 22.16 (38 parameters)

Field Groups:
    identification: Obstruction ID
    geometry: Bounds and shape
    surface: Surface properties for faces
    appearance: Color, transparency, outline
    geometry_control: Thicken, overlay, holes
    control: Device and control activation
    replication: Array multiplier reference
    output: Boundary file output control
    heat_transfer: 3D heat transfer parameters
"""

from pydantic import field_validator, model_validator

from pyfds.core.enums import ObstShape
from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Obstruction"]


class Obstruction(NamelistBase):
    """FDS OBST namelist for obstructions and solid objects.

    Obstructions are the primary way to define solid objects in FDS.
    They can be simple rectangular blocks or complex shapes with
    detailed surface properties and 3D heat transfer.

    FDS User Guide Reference: Table 22.16

    Parameters
    ----------
    xb : Bounds3D
        Obstruction bounds (xmin, xmax, ymin, ymax, zmin, zmax).
    id : str, optional
        Unique obstruction identifier.
    surf_id : str, optional
        Surface ID for all faces, default 'INERT'.
    color : str, optional
        Named color for visualization.

    Notes
    -----
    For 3D heat transfer (HT3D on SURF), specify MATL_ID on the OBST.
    The obstruction must have non-zero volume for HT3D to work.

    Examples
    --------
    >>> from pyfds.core.geometry import Bounds3D
    >>> burner = Obstruction(
    ...     xb=Bounds3D.of(4, 6, 4, 6, 0, 0.5),
    ...     surf_id='FIRE'
    ... )

    See Also
    --------
    Surface : Surface properties applied to obstructions.
    Hole : Create openings in obstructions.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "OBST"

    # ==========================================================================
    # Identification
    # ==========================================================================
    id: str | None = FdsField(None, description="Obstruction identifier", group="identification")

    # ==========================================================================
    # Geometry
    # ==========================================================================
    xb: Bounds3D = FdsField(
        ...,
        description="Obstruction bounds (xmin,xmax,ymin,ymax,zmin,zmax)",
        group="geometry",
    )

    # ==========================================================================
    # Surface Properties
    # ==========================================================================
    surf_id: str | None = FdsField(None, description="Surface ID for all faces", group="surface")
    surf_ids: tuple[str, str, str] | None = FdsField(
        None,
        description="Surface IDs for top, sides, bottom (3-element array)",
        group="surface",
    )
    surf_id6: tuple[str, str, str, str, str, str] | None = FdsField(
        None,
        description="Surface IDs for all 6 faces (-x,+x,-y,+y,-z,+z)",
        group="surface",
    )
    surf_id_interior: str | None = FdsField(
        None, description="Interior surface ID for burn-away", group="surface"
    )

    # ==========================================================================
    # Appearance
    # ==========================================================================
    color: str | None = FdsField(None, description="Named color", group="appearance")
    rgb: tuple[int, int, int] | None = FdsField(
        None, description="RGB color (0-255)", group="appearance"
    )
    transparency: float = FdsField(
        1.0,
        ge=0,
        le=1,
        exclude_if=1.0,
        description="Transparency (0=invisible, 1=opaque)",
        group="appearance",
    )
    outline: bool = FdsField(
        False, exclude_if=False, description="Draw outline only in Smokeview", group="appearance"
    )
    texture_origin: tuple[float, float, float] | None = FdsField(
        None, description="Texture origin [m]", group="appearance"
    )

    # ==========================================================================
    # Geometry Control
    # ==========================================================================
    thicken: bool = FdsField(
        False,
        exclude_if=False,
        description="Thicken thin obstructions to one cell",
        group="geometry_control",
    )
    overlay: bool = FdsField(
        True,
        exclude_if=True,
        description="Allow overlay by later obstructions",
        group="geometry_control",
    )
    permit_hole: bool = FdsField(
        True, exclude_if=True, description="Permit holes to be punched", group="geometry_control"
    )
    removable: bool = FdsField(
        True, exclude_if=True, description="Obstruction can be removed", group="geometry_control"
    )
    allow_vent: bool = FdsField(
        True,
        exclude_if=True,
        description="Allow vents on this obstruction",
        group="geometry_control",
    )

    # ==========================================================================
    # Control/Activation
    # ==========================================================================
    ctrl_id: str | None = FdsField(None, description="Control ID for activation", group="control")
    devc_id: str | None = FdsField(None, description="Device ID for activation", group="control")

    # ==========================================================================
    # Array Replication
    # ==========================================================================
    mult_id: str | None = FdsField(
        None, description="Multiplier ID for arrays", group="replication"
    )

    # ==========================================================================
    # Boundary File Output
    # ==========================================================================
    bndf_obst: bool = FdsField(
        True,
        exclude_if=True,
        description="Output boundary file data for this obstruction",
        group="output",
    )
    bndf_face: tuple[bool, bool, bool, bool, bool, bool, bool] | None = FdsField(
        None, description="Boundary file output per face (-3,-2,-1,0,+1,+2,+3)", group="output"
    )

    # ==========================================================================
    # 3D Heat Transfer (HT3D requires these on OBST)
    # ==========================================================================
    matl_id: str | list[str] | None = FdsField(
        None, description="Material ID(s) for HT3D", group="heat_transfer"
    )
    matl_mass_fraction: list[float] | None = FdsField(
        None, description="Material mass fractions", group="heat_transfer"
    )
    bulk_density: float | None = FdsField(
        None, gt=0, description="Bulk density for burn-away [kg/m³]", group="heat_transfer"
    )
    cell_size: float | None = FdsField(
        None, gt=0, description="Interior node spacing for HT3D [m]", group="heat_transfer"
    )
    cell_size_factor: float | None = FdsField(
        None, gt=0, description="Cell size factor for HT3D", group="heat_transfer"
    )
    stretch_factor: float | None = FdsField(
        None, gt=0, description="Stretch factor for HT3D nodes", group="heat_transfer"
    )
    n_layer_cells_max: int | None = FdsField(
        None, ge=1, description="Maximum layer cells for HT3D", group="heat_transfer"
    )
    internal_heat_source: float = FdsField(
        0.0, exclude_if=0.0, description="Internal heat source [kW/m³]", group="heat_transfer"
    )
    ramp_ihs: str | None = FdsField(
        None, description="RAMP for internal heat source", group="heat_transfer"
    )

    # ==========================================================================
    # Shape-Based Geometry (used with MULT)
    # ==========================================================================
    shape: ObstShape | str | None = FdsField(
        None, description="Geometric shape (SPHERE, CYLINDER, CONE, BOX)", group="shape"
    )
    xyz: tuple[float, float, float] | None = FdsField(
        None, description="Shape center point [m]", group="shape"
    )
    radius: float | None = FdsField(None, gt=0, description="Shape radius [m]", group="shape")
    height: float | None = FdsField(None, gt=0, description="Shape height [m]", group="shape")
    length: float | None = FdsField(None, gt=0, description="Shape length [m]", group="shape")
    width: float | None = FdsField(None, gt=0, description="Shape width [m]", group="shape")
    orientation: tuple[float, float, float] | None = FdsField(
        None, description="Shape orientation vector", group="shape"
    )
    theta: float | None = FdsField(None, description="Shape rotation angle [°]", group="shape")

    # ==========================================================================
    # Validators
    # ==========================================================================
    @field_validator("rgb", mode="before")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB color values."""
        if v is None:
            return None
        if len(v) != 3:
            raise ValueError("RGB must be a tuple of 3 integers")
        for component in v:
            if not (0 <= component <= 255):
                raise ValueError("RGB components must be between 0 and 255")
        return v

    @field_validator("surf_ids", mode="before")
    @classmethod
    def validate_surf_ids(cls, v: tuple[str, str, str] | None) -> tuple[str, str, str] | None:
        """Validate SURF_IDS array."""
        if v is None:
            return None
        if len(v) != 3:
            raise ValueError("SURF_IDS must be a tuple of 3 strings (top, sides, bottom)")
        return v

    @field_validator("surf_id6", mode="before")
    @classmethod
    def validate_surf_id6(
        cls, v: tuple[str, str, str, str, str, str] | None
    ) -> tuple[str, str, str, str, str, str] | None:
        """Validate SURF_ID6 array."""
        if v is None:
            return None
        if len(v) != 6:
            raise ValueError("SURF_ID6 must be a tuple of 6 strings")
        return v

    @field_validator("bndf_face", mode="before")
    @classmethod
    def validate_bndf_face(
        cls, v: tuple[bool, bool, bool, bool, bool, bool, bool] | None
    ) -> tuple[bool, bool, bool, bool, bool, bool, bool] | None:
        """Validate BNDF_FACE array."""
        if v is None:
            return None
        if len(v) != 7:
            raise ValueError("BNDF_FACE must be a tuple of 7 booleans (-3 to +3)")
        return v

    @field_validator("shape", mode="before")
    @classmethod
    def validate_shape(cls, v: ObstShape | str | None) -> ObstShape | str | None:
        """Validate shape parameter."""
        if v is None:
            return None
        if isinstance(v, ObstShape):
            return v
        valid_shapes = {"SPHERE", "CYLINDER", "CONE", "BOX"}
        if v.upper() not in valid_shapes:
            raise ValueError(f"Shape must be one of: {', '.join(valid_shapes)}")
        return v.upper()

    @model_validator(mode="after")
    def validate_obstruction(self) -> "Obstruction":
        """Validate obstruction configuration."""
        # Shape requires XYZ unless MULT_ID is specified
        # With MULT_ID, FDS computes shape centers from XB for each instance
        if self.shape is not None and self.xyz is None and self.mult_id is None:
            raise ValueError("SHAPE requires XYZ to be specified (unless using MULT_ID)")

        # SURF_ID conflicts
        surf_count = sum(1 for s in [self.surf_id, self.surf_ids, self.surf_id6] if s is not None)
        if surf_count > 1:
            raise ValueError("Only one of SURF_ID, SURF_IDS, or SURF_ID6 can be specified")

        return self
