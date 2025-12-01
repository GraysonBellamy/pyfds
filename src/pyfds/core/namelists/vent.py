"""FDS VENT namelist for boundary conditions and openings.

Vents define boundary conditions including openings to ambient, HVAC systems,
surface patches, and mesh boundary conditions.

Field Groups:
    identification: Vent ID
    geometry: Bounds, point, radius for shape
    surface: Surface properties
    control: Device and control activation
    boundary: Mesh boundary specification
    appearance: Color and transparency
    replication: Array multiplier reference
    combustion: Spread rate control
    turbulence: Turbulence parameters
"""

import numpy as np
from pydantic import model_validator

from pyfds.core.enums import MeshBoundary, VentShape, VentType
from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Vent"]


class Vent(NamelistBase):
    """FDS VENT namelist for boundary conditions and openings.

    Handles various types of vents including openings to ambient, HVAC systems,
    surface patches, and mesh boundary conditions. Supports both rectangular
    and circular geometries.

    Parameters
    ----------
    xb : Bounds3D, optional
        Bounding box coordinates (xmin, xmax, ymin, ymax, zmin, zmax).
    mb : str, optional
        Mesh boundary location ('XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX').
    surf_id : str, optional
        Surface properties ID, default: 'INERT'.
    id : str, optional
        Vent identifier.

    Notes
    -----
    - VENTs must be planar (exactly one dimension in XB must be zero)
    - HVAC flow parameters are defined on SURF, not VENT
    - Circular vents require both xyz and radius
    - For mesh boundaries, use mb instead of xb

    Examples
    --------
    >>> from pyfds.core.geometry import Bounds3D
    >>> door = Vent(xb=Bounds3D.of(5, 5, 2, 4, 0, 3), surf_id='OPEN')

    See Also
    --------
    Surface : Surface properties applied to vents.
    Control : Control logic for vent activation.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "VENT"

    # --- Identification ---
    id: str | None = FdsField(None, description="Vent identifier", group="identification")

    # --- Geometry ---
    xb: Bounds3D | None = FdsField(
        None, description="Vent bounds (xmin,xmax,ymin,ymax,zmin,zmax)", group="geometry"
    )
    mb: MeshBoundary | str | None = FdsField(None, description="Mesh boundary", group="geometry")

    # --- Surface ---
    surf_id: str = FdsField("INERT", description="Surface ID", group="surface")

    # --- Circular Geometry ---
    xyz: Point3D | None = FdsField(None, description="Center point", group="geometry")
    radius: float | None = FdsField(None, gt=0, description="Radius [m]", group="geometry")

    # --- Control ---
    devc_id: str | None = FdsField(None, description="Device ID for activation", group="control")
    ctrl_id: str | None = FdsField(None, description="Control ID for activation", group="control")

    # --- Boundary Conditions ---
    dynamic_pressure: float = FdsField(
        0.0, description="Dynamic pressure at OPEN boundary [Pa]", group="boundary"
    )
    tmp_exterior: float | None = FdsField(
        None, description="Exterior temperature [°C]", group="boundary"
    )

    # --- Appearance ---
    color: str | None = FdsField(None, description="Named color", group="appearance")
    rgb: tuple[int, int, int] | None = FdsField(None, description="RGB color", group="appearance")
    transparency: float = FdsField(
        1.0, exclude_if=1.0, ge=0, le=1, description="Transparency [0-1]", group="appearance"
    )
    outline: bool = FdsField(
        False, exclude_if=False, description="Draw outline only", group="appearance"
    )

    # --- Geometry Parameters ---
    db: str | None = FdsField(None, description="Domain boundary", group="geometry")
    pbx: float | None = FdsField(None, description="X plane position", group="geometry")
    pby: float | None = FdsField(None, description="Y plane position", group="geometry")
    pbz: float | None = FdsField(None, description="Z plane position", group="geometry")
    ior: int | None = FdsField(None, description="Orientation: +/-1,2,3", group="geometry")

    # --- Replication ---
    mult_id: str | None = FdsField(None, description="Multiplier ID", group="replication")
    obst_id: str | None = FdsField(None, description="Associated obstruction ID", group="geometry")

    # --- Texture ---
    texture_origin: tuple[float, float, float] | None = FdsField(
        None, description="Texture origin point", group="appearance"
    )

    # --- Fire Spread ---
    spread_rate: float = FdsField(0.05, description="Fire spread rate [m/s]", group="combustion")
    area_adjust: bool = FdsField(
        False, description="Adjust area for spreading fire", group="combustion"
    )

    # --- Level Set ---
    geom: bool = FdsField(
        False, description="Level set fire spread geometry flag", group="level_set"
    )

    # --- Open Boundary Parameters ---
    tmp_exterior_ramp: str | None = FdsField(
        None, description="Ramp for exterior temperature", group="boundary"
    )
    pressure_ramp: str | None = FdsField(
        None, description="Ramp for dynamic pressure", group="boundary"
    )

    # --- Synthetic Turbulence ---
    n_eddy: int | None = FdsField(
        None, ge=1, description="Number of synthetic eddies", group="turbulence"
    )
    l_eddy: float | None = FdsField(
        None, gt=0, description="Eddy length scale [m]", group="turbulence"
    )
    l_eddy_ij: list[list[float]] | None = FdsField(
        None, description="Anisotropic eddy length scales (3x3)", group="turbulence"
    )
    vel_rms: float | None = FdsField(
        None, ge=0, description="RMS velocity fluctuation [m/s]", group="turbulence"
    )
    reynolds_stress: list[list[float]] | None = FdsField(
        None, description="Reynolds stress tensor (3x3)", group="turbulence"
    )
    uvw: tuple[float, float, float] | None = FdsField(
        None, description="Mean velocity components [m/s]", group="turbulence"
    )

    # --- Validators ---
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
        if self.radius and not self.xyz:
            raise ValueError("Circular vent requires both XYZ and RADIUS")

        # MB validation
        if self.mb:
            valid_mb = ["XMIN", "XMAX", "YMIN", "YMAX", "ZMIN", "ZMAX"]
            mb_value = self.mb.value if isinstance(self.mb, MeshBoundary) else self.mb
            if mb_value not in valid_mb:
                raise ValueError(f"MB must be one of {valid_mb}, got '{self.mb}'")

        # RGB validation
        if self.rgb:
            if len(self.rgb) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in self.rgb):
                raise ValueError("RGB values must be in range 0-255")

        # IOR validation
        if self.ior is not None and abs(self.ior) not in [1, 2, 3]:
            raise ValueError("IOR must be +/-1, +/-2, or +/-3")

        # Matrix validations
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
            return VentShape.CIRCULAR
        return VentShape.RECTANGULAR

    def get_area(self) -> float | None:
        """Calculate vent area based on geometry.

        Returns
        -------
        float | None
            Area in m², or None if cannot be calculated.
        """
        shape = self.get_shape()

        if shape == VentShape.RECTANGULAR and self.xb:
            dx = abs(self.xb.xmax - self.xb.xmin)
            dy = abs(self.xb.ymax - self.xb.ymin)
            dz = abs(self.xb.zmax - self.xb.zmin)

            if dx < 1e-6:
                return dy * dz
            if dy < 1e-6:
                return dx * dz
            if dz < 1e-6:
                return dx * dy

        elif shape == VentShape.CIRCULAR and self.radius:
            return float(np.pi * self.radius**2)

        return None
