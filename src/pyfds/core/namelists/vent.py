"""
FDS VENT namelist.

Boundary conditions and openings including HVAC vents.
"""

from enum import Enum
from typing import Any

import numpy as np
from pydantic import Field, field_validator, model_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import NamelistBase


class VentType(str, Enum):
    """Types of vents in FDS."""

    OPEN = "OPEN"
    HVAC = "HVAC"
    SURFACE = "SURFACE"
    MIRROR = "MIRROR"
    PERIODIC = "PERIODIC"


class VentShape(str, Enum):
    """Vent geometry types."""

    RECTANGULAR = "RECTANGULAR"
    CIRCULAR = "CIRCULAR"
    ANNULAR = "ANNULAR"


class Vent(NamelistBase):
    """
    FDS VENT namelist - boundary conditions and openings.

    Handles various types of vents including openings to ambient, HVAC systems,
    surface patches, and mesh boundary conditions. Supports both rectangular
    and circular geometries.

    Parameters
    ----------
    xb : tuple[float, float, float, float, float, float], optional
        Bounding box coordinates (xmin, xmax, ymin, ymax, zmin, zmax)
    mb : str, optional
        Mesh boundary location ('XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX')
    surf_id : str, optional
        Surface properties ID, default: 'INERT'
    id : str, optional
        Vent identifier

    Geometric Parameters
    -------------------
    xyz : Point3D, optional
        Center point for circular vents
    radius : float, optional
        Radius for circular vents [m]
    radius_inner : float, optional
        Inner radius for annular vents [m]

    Control Parameters
    -----------------
    devc_id : str, optional
        Device ID for activation
    ctrl_id : str, optional
        Control ID for activation
    delay : float, optional
        Activation delay [s], default: 0.0
    t_activate : float, optional
        Activation time [s]

    Advanced Parameters
    ------------------
    dynamic_pressure : bool, optional
        Use dynamic pressure boundary condition, default: False
    tmp_exterior : float, optional
        Exterior temperature [°C]
    color : str, optional
        Named color for visualization
    rgb : tuple[int, int, int], optional
        RGB color values (0-255)
    transparency : float, optional
        Transparency value [0-1], default: 1.0

    Examples
    --------
    >>> # Opening to ambient
    >>> door = Vent(xb=(5, 5, 2, 4, 0, 3), surf_id='OPEN')

    >>> # HVAC vent (flow parameters defined on SURF, not VENT)
    >>> hvac_vent = Vent(xb=(5, 6, 5, 6, 3, 3), surf_id='HVAC')

    >>> # Circular burner
    >>> burner = Vent(
    ...     xb=(-1, 1, -1, 1, 0, 0),
    ...     surf_id='FIRE',
    ...     xyz=(0, 0, 0),
    ...     radius=0.5
    ... )

    >>> # Mesh boundary vent
    >>> boundary = Vent(mb='XMIN', surf_id='OPEN')

    Notes
    -----
    - VENTs must be planar (exactly one dimension in XB must be zero)
    - HVAC flow parameters (VOLUME_FLOW, VEL, MASS_FLOW) are defined on SURF, not VENT
    - Circular vents require both xyz and radius
    - For mesh boundaries, use mb instead of xb
    """

    # Geometry
    xb: Bounds3D | None = Field(None, description="Vent bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    mb: str | None = Field(None, description="Mesh boundary")
    surf_id: str = Field("INERT", description="Surface ID")
    id: str | None = Field(None, description="Vent identifier")

    # Circular geometry
    xyz: Point3D | None = Field(None, description="Center point")
    radius: float | None = Field(None, gt=0, description="Radius [m]")
    radius_inner: float | None = Field(None, gt=0, description="Inner radius [m]")

    # Control
    devc_id: str | None = Field(None, description="Device ID for activation")
    ctrl_id: str | None = Field(None, description="Control ID for activation")
    delay: float = Field(0.0, ge=0, description="Activation delay [s]")
    t_activate: float | None = Field(None, description="Activation time [s]")

    # Mesh reference
    mesh_id: str | None = Field(None, description="Mesh ID for MB")

    # Advanced
    dynamic_pressure: bool = Field(False, description="Dynamic pressure BC")
    tmp_exterior: float | None = Field(None, description="Exterior temperature [°C]")

    # Visualization
    color: str | None = Field(None, description="Named color")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    transparency: float = Field(1.0, ge=0, le=1, description="Transparency [0-1]")

    @field_validator("xyz", mode="before")
    @classmethod
    def validate_xyz(cls, v: Any) -> Any:
        if isinstance(v, tuple):
            return Point3D.from_tuple(v)
        return v

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Any:
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        return v

    @model_validator(mode="after")
    def validate_vent(self) -> "Vent":
        """Validate vent parameters."""
        # Must have either XB or MB
        if not self.xb and not self.mb:
            raise ValueError("Vent must have either XB or MB specified")

        # XB validation - must define a plane
        if self.xb:
            dims = [
                abs(self.xb.xmax - self.xb.xmin),
                abs(self.xb.ymax - self.xb.ymin),
                abs(self.xb.zmax - self.xb.zmin),
            ]
            zero_dims = sum(1 for d in dims if d < 1e-6)
            if zero_dims != 1:
                raise ValueError(
                    f"Vent XB must be a plane (exactly one dimension zero), got dimensions: {dims}"
                )

        # Circular vent validation
        if self.radius:
            if not self.xyz:
                raise ValueError("Circular vent requires both XYZ and RADIUS")
            if self.radius_inner and self.radius_inner >= self.radius:
                raise ValueError(
                    f"RADIUS_INNER ({self.radius_inner}) must be less than RADIUS ({self.radius})"
                )

        # MB validation
        if self.mb:
            valid_mb = ["XMIN", "XMAX", "YMIN", "YMAX", "ZMIN", "ZMAX"]
            if self.mb not in valid_mb:
                raise ValueError(f"MB must be one of {valid_mb}, got '{self.mb}'")

        # RGB validation
        if self.rgb:
            if len(self.rgb) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in self.rgb):
                raise ValueError("RGB values must be in range 0-255")

        return self

    def get_vent_type(self) -> VentType:
        """Determine the type of vent based on surf_id."""
        if self.surf_id == "OPEN":
            return VentType.OPEN
        if self.surf_id == "HVAC":
            return VentType.HVAC
        if self.surf_id == "MIRROR":
            return VentType.MIRROR
        if self.surf_id == "PERIODIC":
            return VentType.PERIODIC
        return VentType.SURFACE

    def get_shape(self) -> VentShape:
        """Determine the shape of the vent."""
        if self.xyz and self.radius:
            if self.radius_inner:
                return VentShape.ANNULAR
            return VentShape.CIRCULAR
        return VentShape.RECTANGULAR

    def get_area(self) -> float | None:
        """
        Calculate vent area based on geometry.

        Returns
        -------
        float or None
            Area in m², or None if cannot be calculated
        """
        shape = self.get_shape()

        if shape == VentShape.RECTANGULAR and self.xb:
            dx = abs(self.xb.xmax - self.xb.xmin)
            dy = abs(self.xb.ymax - self.xb.ymin)
            dz = abs(self.xb.zmax - self.xb.zmin)

            # Area is the non-zero face
            if dx < 1e-6:
                return dy * dz
            if dy < 1e-6:
                return dx * dz
            if dz < 1e-6:
                return dx * dy

        elif shape == VentShape.CIRCULAR and self.radius:
            return float(np.pi * self.radius**2)

        elif shape == VentShape.ANNULAR and self.radius and self.radius_inner:
            return float(np.pi * (self.radius**2 - self.radius_inner**2))

        return None

    def to_fds(self) -> str:
        """Generate FDS VENT namelist."""
        params: dict[str, Any] = {}

        if self.id:
            params["id"] = self.id
        if self.xb:
            params["xb"] = self.xb.as_tuple()
        if self.mb:
            params["mb"] = self.mb
            if self.mesh_id:
                params["mesh_id"] = self.mesh_id

        params["surf_id"] = self.surf_id

        if self.xyz:
            params["xyz"] = self.xyz.as_tuple()
        if self.radius is not None:
            params["radius"] = self.radius
        if self.radius_inner is not None:
            params["radius_inner"] = self.radius_inner

        if self.devc_id:
            params["devc_id"] = self.devc_id
        if self.ctrl_id:
            params["ctrl_id"] = self.ctrl_id
        if self.delay > 0:
            params["delay"] = self.delay
        if self.t_activate is not None:
            params["t_activate"] = self.t_activate

        if self.dynamic_pressure:
            params["dynamic_pressure"] = self.dynamic_pressure
        if self.tmp_exterior is not None:
            params["tmp_exterior"] = self.tmp_exterior

        if self.color:
            params["color"] = self.color
        if self.rgb:
            params["rgb"] = self.rgb
        if self.transparency != 1.0:
            params["transparency"] = self.transparency

        return self._build_namelist("VENT", params)
