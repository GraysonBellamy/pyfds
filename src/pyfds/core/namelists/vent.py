"""
FDS VENT namelist.

Boundary conditions and openings including HVAC vents.
"""

from typing import TYPE_CHECKING

import numpy as np
from pydantic import model_validator

from pyfds.core.enums import VentShape, VentType
from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import FdsField, NamelistBase

if TYPE_CHECKING:
    from pyfds.builders import VentBuilder


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
    >>> door = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id='OPEN')

    >>> # HVAC vent (flow parameters defined on SURF, not VENT)
    >>> hvac_vent = Vent(xb=Bounds3D.of(5, 6, 5, 6, 3, 3), surf_id='HVAC')

    >>> # Circular burner
    >>> burner = Vent(
    ...     xb=Bounds3D.of(-1, 1, -1, 1, 0, 0),
    ...     surf_id='FIRE',
    ...     xyz=Point3D.of(0, 0, 0),
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

    @classmethod
    def builder(cls) -> "VentBuilder":
        """Return a fluent builder for Vent.

        Returns
        -------
        VentBuilder
            A builder instance for fluent construction

        Examples
        --------
        >>> vent = Vent.builder() \\
        ...     .bounds(5, 5, 2, 4, 0, 2) \\
        ...     .open() \\
        ...     .build()
        """
        from pyfds.builders import VentBuilder

        return VentBuilder()

    # Geometry
    xb: Bounds3D | None = FdsField(None, description="Vent bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    mb: str | None = FdsField(None, description="Mesh boundary")
    surf_id: str = FdsField("INERT", description="Surface ID")
    id: str | None = FdsField(None, description="Vent identifier")

    # Circular geometry
    xyz: Point3D | None = FdsField(None, description="Center point")
    radius: float | None = FdsField(None, gt=0, description="Radius [m]")
    radius_inner: float | None = FdsField(None, gt=0, description="Inner radius [m]")

    # Control
    devc_id: str | None = FdsField(None, description="Device ID for activation")
    ctrl_id: str | None = FdsField(None, description="Control ID for activation")
    delay: float = FdsField(0.0, exclude_if=0.0, ge=0, description="Activation delay [s]")
    t_activate: float | None = FdsField(None, description="Activation time [s]")

    # Mesh reference
    mesh_id: str | None = FdsField(None, description="Mesh ID for MB")

    # Advanced
    dynamic_pressure: bool = FdsField(False, exclude_if=False, description="Dynamic pressure BC")
    tmp_exterior: float | None = FdsField(None, description="Exterior temperature [°C]")

    # Visualization
    color: str | None = FdsField(None, description="Named color")
    rgb: tuple[int, int, int] | None = FdsField(None, description="RGB color")
    transparency: float = FdsField(
        1.0, exclude_if=1.0, ge=0, le=1, description="Transparency [0-1]"
    )

    # Geometry parameters
    db: str | None = FdsField(None, description="Domain boundary")
    pbx: float | None = FdsField(None, description="X plane position")
    pby: float | None = FdsField(None, description="Y plane position")
    pbz: float | None = FdsField(None, description="Z plane position")
    ior: int | None = FdsField(None, description="Orientation: +/-1,2,3")

    # Control parameters
    outline: bool = FdsField(False, exclude_if=False, description="Draw outline only")
    mult_id: str | None = FdsField(None, description="Multiplier ID")
    obst_id: str | None = FdsField(None, description="Associated obstruction ID")

    # Texture parameters
    texture_origin: tuple[float, float, float] | None = FdsField(
        None, description="Texture origin point"
    )

    # Fire spread (circular vents)
    spread_rate: float | None = FdsField(None, gt=0, description="Fire spread rate [m/s]")

    # Open boundary parameters
    tmp_exterior_ramp: str | None = FdsField(None, description="Ramp for exterior temperature")
    pressure_ramp: str | None = FdsField(None, description="Ramp for dynamic pressure")

    # Synthetic turbulence parameters
    n_eddy: int | None = FdsField(None, ge=1, description="Number of synthetic eddies")
    l_eddy: float | None = FdsField(None, gt=0, description="Eddy length scale [m]")
    l_eddy_ij: list[list[float]] | None = FdsField(
        None, description="Anisotropic eddy length scales (3x3)"
    )
    vel_rms: float | None = FdsField(None, ge=0, description="RMS velocity fluctuation [m/s]")
    reynolds_stress: list[list[float]] | None = FdsField(
        None, description="Reynolds stress tensor (3x3)"
    )
    uvw: tuple[float, float, float] | None = FdsField(
        None, description="Mean velocity components [m/s]"
    )

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

        # Additional validations
        if self.ior is not None and abs(self.ior) not in [1, 2, 3]:
            raise ValueError("IOR must be +/-1, +/-2, or +/-3")

        if self.l_eddy_ij is not None and (
            len(self.l_eddy_ij) != 3 or any(len(row) != 3 for row in self.l_eddy_ij)
        ):
            raise ValueError("L_EDDY_IJ must be a 3x3 matrix")

        if self.reynolds_stress is not None and (
            len(self.reynolds_stress) != 3 or any(len(row) != 3 for row in self.reynolds_stress)
        ):
            raise ValueError("REYNOLDS_STRESS must be a 3x3 matrix")

        if self.uvw is not None and len(self.uvw) != 3:
            raise ValueError("UVW must have exactly 3 components")

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

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "VENT"
