"""
FDS OBST namelist.

Obstructions and solid objects.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import NamelistBase


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
    >>> burner = Obstruction(xb=Bounds3D(4, 6, 4, 6, 0, 0.5), surf_id='FIRE')
    >>> print(burner.to_fds())
    &OBST XB=4,6,4,6,0,0.5, SURF_ID='FIRE' /

    >>> # Wall with HT3D heat transfer
    >>> wall = Obstruction(
    ...     xb=(0, 0.2, 0, 10, 0, 3),
    ...     surf_id='CONCRETE',
    ...     ht3d=True,
    ...     matl_id='CONCRETE',
    ...     cell_size=0.05
    ... )
    """

    xb: Bounds3D = Field(..., description="Obstruction bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    surf_id: str | None = Field(None, description="Surface ID for all faces")
    surf_id_top: str | None = Field(None, description="Top surface ID")
    surf_id_bottom: str | None = Field(None, description="Bottom surface ID")
    surf_id_sides: str | None = Field(None, description="Side surfaces ID")
    color: str | None = Field(None, description="Named color")
    bulk_density: float | None = Field(
        None, gt=0, description="Combustible mass per unit volume [kg/m³]"
    )
    surf_id_interior: str | None = Field(
        None, description="Surface ID for newly exposed surfaces during burn-away"
    )
    burn_away: bool = Field(False, description="Allow obstruction to burn away")

    # Identification
    id: str | None = Field(None, description="Obstruction identifier")

    # Visualization
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    transparency: float = Field(1.0, ge=0, le=1, description="Transparency")
    outline: bool = Field(False, description="Draw as outline in Smokeview")

    # Geometry Control
    thicken: bool = Field(False, description="Force cell thickness")
    overlay: bool = Field(True, description="Override overlapping obstructions")
    permit_hole: bool = Field(True, description="Allow HOLEs to punch through")
    removable: bool = Field(True, description="Allow removal during simulation")
    allow_vent: bool = Field(True, description="Allow VENTs to attach")

    # Control/Activation
    ctrl_id: str | None = Field(None, description="Control ID for activation")
    devc_id: str | None = Field(None, description="Device ID for activation")

    # Array Replication
    mult_id: str | None = Field(None, description="Multiplier ID")

    # Texture
    texture_origin: tuple[float, float, float] | None = Field(
        None, description="Texture origin point"
    )

    # 3D Heat Transfer (HT3D) - for solid obstructions
    ht3d: bool = Field(False, description="Enable 3D heat conduction")
    matl_id: list[str] | str | None = Field(None, description="Material ID(s) for HT3D")
    matl_mass_fraction: list[float] | None = Field(
        None, description="Mass fractions for HT3D materials"
    )
    cell_size: float | None = Field(None, gt=0, description="Interior cell size [m]")
    cell_size_factor: float | None = Field(None, gt=0, description="Cell size multiplier")
    stretch_factor: float | None = Field(None, gt=0, description="Grid stretching")
    n_layer_cells_max: int | None = Field(None, ge=1, description="Max cells in layer")
    internal_heat_source: float | None = Field(None, description="Internal heat source [kW/m³]")
    ramp_ihs: str | None = Field(None, description="Ramp for internal heat source")

    # Shape-based geometry (used with MULT)
    shape: str | None = Field(None, description="Shape type: SPHERE, CYLINDER, CONE, BOX")
    xyz: tuple[float, float, float] | None = Field(None, description="Center position")
    radius: float | None = Field(None, gt=0, description="Shape radius")
    height: float | None = Field(None, gt=0, description="Shape height")
    length: float | None = Field(None, gt=0, description="Box length")
    width: float | None = Field(None, gt=0, description="Box width")
    orientation: tuple[float, float, float] | None = Field(None, description="Direction vector")
    theta: float | None = Field(None, description="Rotation angle [degrees]")

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Bounds3D:
        """Validate and convert obstruction bounds."""
        if isinstance(v, Bounds3D):
            return v
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        raise ValueError("XB must be a Bounds3D or tuple of 6 floats")

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

    def to_fds(self) -> str:
        """Generate FDS OBST namelist."""
        params: dict[str, Any] = {"xb": self.xb.as_tuple()}

        # Basic surface parameters
        if self.surf_id:
            params["surf_id"] = self.surf_id
        if self.surf_id_top:
            params["surf_id_top"] = self.surf_id_top
        if self.surf_id_bottom:
            params["surf_id_bottom"] = self.surf_id_bottom
        if self.surf_id_sides:
            params["surf_id_sides"] = self.surf_id_sides

        # Identification
        if self.id:
            params["id"] = self.id

        # Visualization
        if self.color:
            params["color"] = self.color
        if self.rgb:
            params["rgb"] = self.rgb
        if self.transparency != 1.0:
            params["transparency"] = self.transparency
        if self.outline:
            params["outline"] = self.outline

        # Geometry Control
        if self.thicken:
            params["thicken"] = self.thicken
        if not self.overlay:
            params["overlay"] = self.overlay
        if not self.permit_hole:
            params["permit_hole"] = self.permit_hole
        if not self.removable:
            params["removable"] = self.removable
        if not self.allow_vent:
            params["allow_vent"] = self.allow_vent

        # Control/Activation
        if self.ctrl_id:
            params["ctrl_id"] = self.ctrl_id
        if self.devc_id:
            params["devc_id"] = self.devc_id

        # Array Replication
        if self.mult_id:
            params["mult_id"] = self.mult_id

        # Texture
        if self.texture_origin:
            params["texture_origin"] = self.texture_origin

        # Combustion
        if self.bulk_density is not None:
            params["bulk_density"] = self.bulk_density
        if self.surf_id_interior:
            params["surf_id_interior"] = self.surf_id_interior
        if self.burn_away:
            params["burn_away"] = self.burn_away

        # 3D Heat Transfer (HT3D)
        if self.ht3d:
            params["ht3d"] = self.ht3d
        if self.matl_id:
            if isinstance(self.matl_id, list):
                params["matl_id"] = self.matl_id
            else:
                params["matl_id"] = self.matl_id
        if self.matl_mass_fraction:
            params["matl_mass_fraction"] = self.matl_mass_fraction
        if self.cell_size is not None:
            params["cell_size"] = self.cell_size
        if self.cell_size_factor is not None:
            params["cell_size_factor"] = self.cell_size_factor
        if self.stretch_factor is not None:
            params["stretch_factor"] = self.stretch_factor
        if self.n_layer_cells_max is not None:
            params["n_layer_cells_max"] = self.n_layer_cells_max
        if self.internal_heat_source is not None:
            params["internal_heat_source"] = self.internal_heat_source
        if self.ramp_ihs:
            params["ramp_ihs"] = self.ramp_ihs

        # Shape-based geometry
        if self.shape:
            params["shape"] = self.shape
        if self.xyz:
            params["xyz"] = self.xyz
        if self.radius is not None:
            params["radius"] = self.radius
        if self.height is not None:
            params["height"] = self.height
        if self.length is not None:
            params["length"] = self.length
        if self.width is not None:
            params["width"] = self.width
        if self.orientation:
            params["orientation"] = tuple(int(x) for x in self.orientation)
        if self.theta is not None:
            params["theta"] = self.theta

        return self._build_namelist("OBST", params)
