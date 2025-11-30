"""
FDS OBST namelist.

Obstructions and solid objects.
"""

from typing import TYPE_CHECKING

from pydantic import field_validator

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import FdsField, NamelistBase

if TYPE_CHECKING:
    from pyfds.builders import ObstructionBuilder


class Obstruction(NamelistBase):
    """
    FDS OBST namelist - obstructions and solid objects.

    Parameters
    ----------
    xb : Bounds3D
        Obstruction bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    surf_id : str, optional
        Surface ID for all faces
    surf_id_top : str, optional
        Top surface ID
    surf_id_bottom : str, optional
        Bottom surface ID
    surf_id_sides : str, optional
        Side surfaces ID
    color : str, optional
        Named color
    rgb : tuple[int, int, int], optional
        RGB color (0-255)
    transparency : float, optional
        Transparency (0-1, default: 1.0)
    outline : bool, optional
        Draw as outline in Smokeview (default: False)
    bulk_density : float, optional
        Combustible mass per unit volume [kg/m³]
    surf_id_interior : str, optional
        Surface ID for newly exposed surfaces during burn-away
    burn_away : bool, optional
        Allow obstruction to burn away (default: False)
    id : str, optional
        Obstruction identifier
    thicken : bool, optional
        Force cell thickness (default: False)
    overlay : bool, optional
        Override overlapping obstructions (default: True)
    permit_hole : bool, optional
        Allow HOLEs to punch through (default: True)
    removable : bool, optional
        Allow removal during simulation (default: True)
    allow_vent : bool, optional
        Allow VENTs to attach (default: True)
    ctrl_id : str, optional
        Control ID for activation
    devc_id : str, optional
        Device ID for activation
    mult_id : str, optional
        Multiplier ID for array replication
    texture_origin : tuple[float, float, float], optional
        Texture origin point
    ht3d : bool, optional
        Enable 3D heat conduction (default: False)
    matl_id : list[str] | str, optional
        Material ID(s) for HT3D
    matl_mass_fraction : list[float], optional
        Mass fractions for HT3D materials
    cell_size : float, optional
        Interior cell size [m]
    cell_size_factor : float, optional
        Cell size multiplier
    stretch_factor : float, optional
        Grid stretching
    n_layer_cells_max : int, optional
        Max cells in layer
    internal_heat_source : float, optional
        Internal heat source [kW/m³]
    ramp_ihs : str, optional
        Ramp for internal heat source
    shape : str, optional
        Shape type: SPHERE, CYLINDER, CONE, BOX
    xyz : tuple[float, float, float], optional
        Center position
    radius : float, optional
        Shape radius
    height : float, optional
        Shape height
    length : float, optional
        Box length
    width : float, optional
        Box width
    orientation : tuple[float, float, float], optional
        Direction vector
    theta : float, optional
        Rotation angle [degrees]

    Examples
    --------
    >>> from pyfds.core.geometry import Bounds3D
    >>> burner = Obstruction(xb=Bounds3D.of(4, 6, 4, 6, 0, 0.5).5), surf_id='FIRE')
    >>> print(burner.to_fds())
    &OBST XB=4,6,4,6,0,0.5, SURF_ID='FIRE' /

    >>> # Wall with HT3D heat transfer
    >>> wall = Obstruction(
    ...     xb=Bounds3D.of(0, 0.2, 0, 10, 0, 3),
    ...     surf_id='CONCRETE',
    ...     ht3d=True,
    ...     matl_id='CONCRETE',
    ...     cell_size=0.05
    ... )
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "OBST"

    @classmethod
    def builder(cls) -> "ObstructionBuilder":
        """Return a fluent builder for Obstruction.

        Returns
        -------
        ObstructionBuilder
            A builder instance for fluent construction

        Examples
        --------
        >>> obst = Obstruction.builder() \\
        ...     .bounds(0, 1, 0, 1, 0, 0.5) \\
        ...     .surface("FIRE") \\
        ...     .build()
        """
        from pyfds.builders import ObstructionBuilder

        return ObstructionBuilder()

    xb: Bounds3D = FdsField(..., description="Obstruction bounds (xmin,xmax,ymin,ymax,zmin,zmax)")

    # Surface properties
    surf_id: str | None = FdsField("INERT", description="Surface ID")
    surf_id_top: str | None = FdsField(None, description="Top surface ID")
    surf_id_bottom: str | None = FdsField(None, description="Bottom surface ID")
    surf_id_sides: str | None = FdsField(None, description="Side surfaces ID")

    # Identification
    id: str | None = FdsField(None, description="Obstruction identifier")

    # Visualization
    color: str | None = FdsField(None, description="Named color")
    rgb: tuple[int, int, int] | None = FdsField(None, description="RGB color (0-255)")
    transparency: float = FdsField(1.0, ge=0, le=1, description="Transparency (0-1)")
    outline: bool = FdsField(False, description="Draw outline only")

    # Geometry control
    thicken: bool = FdsField(False, description="Thicken thin obstructions")
    overlay: bool = FdsField(True, description="Allow overlay")
    permit_hole: bool = FdsField(True, description="Permit holes")
    removable: bool = FdsField(True, description="Removable obstruction")
    allow_vent: bool = FdsField(True, description="Allow vents")

    # Control/activation
    ctrl_id: str | None = FdsField(None, description="Control ID")
    devc_id: str | None = FdsField(None, description="Device ID")

    # Array replication
    mult_id: str | None = FdsField(None, description="Multiplier ID")

    # Texture
    texture_origin: tuple[float, float, float] | None = FdsField(None, description="Texture origin")

    # Combustion
    bulk_density: float | None = FdsField(None, gt=0, description="Bulk density [kg/m³]")
    surf_id_interior: str | None = FdsField(None, description="Interior surface ID")
    burn_away: bool = FdsField(False, description="Burn away obstruction")

    # 3D Heat transfer
    ht3d: bool = FdsField(False, description="3D heat transfer")
    matl_id: str | list[str] | None = FdsField(None, description="Material ID(s)")
    matl_mass_fraction: list[float] | None = FdsField(None, description="Material mass fractions")
    cell_size: float | None = FdsField(None, gt=0, description="Cell size [m]")
    cell_size_factor: float | None = FdsField(None, gt=0, description="Cell size factor")
    stretch_factor: float | None = FdsField(None, gt=0, description="Stretch factor")
    n_layer_cells_max: int | None = FdsField(None, ge=1, description="Max layer cells")
    internal_heat_source: float | None = FdsField(None, description="Internal heat source [kW/m³]")
    ramp_ihs: str | None = FdsField(None, description="Internal heat source ramp")

    # Shape-based geometry
    shape: str | None = FdsField(None, description="Geometric shape")
    xyz: tuple[float, float, float] | None = FdsField(None, description="Shape center")
    radius: float | None = FdsField(None, gt=0, description="Shape radius [m]")
    height: float | None = FdsField(None, gt=0, description="Shape height [m]")
    length: float | None = FdsField(None, gt=0, description="Shape length [m]")
    width: float | None = FdsField(None, gt=0, description="Shape width [m]")
    orientation: tuple[int, int, int] | None = FdsField(None, description="Shape orientation")
    theta: float | None = FdsField(None, description="Shape rotation [°]")

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

    @field_validator("shape", mode="before")
    @classmethod
    def validate_shape(cls, v: str | None) -> str | None:
        """Validate shape parameter."""
        if v is None:
            return None
        valid_shapes = {"SPHERE", "CYLINDER", "CONE", "BOX"}
        if v.upper() not in valid_shapes:
            raise ValueError(f"Shape must be one of: {', '.join(valid_shapes)}")
        return v.upper()
