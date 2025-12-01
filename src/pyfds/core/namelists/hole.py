"""FDS HOLE namelist for holes in obstructions.

Holes are used to create openings in obstructions such as doors,
windows, and vents.

Field Groups:
    identification: Hole ID
    geometry: Hole bounds (XB)
    control: Device and control activation
    visualization: Color and transparency
    replication: Array multiplier reference
"""

from pydantic import field_validator

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Hole"]


class Hole(NamelistBase):
    """FDS HOLE namelist - holes in obstructions.

    Holes are used to create openings in obstructions such as doors,
    windows, and vents. They can be controlled to open/close during
    the simulation.

    Parameters
    ----------
    xb : Bounds3D
        Hole bounds (xmin, xmax, ymin, ymax, zmin, zmax).
    id : str, optional
        Hole identifier.
    ctrl_id : str, optional
        Control ID for activation/deactivation.
    devc_id : str, optional
        Device ID for activation/deactivation.
    color : str, optional
        Color when hole is closed.
    rgb : tuple[int, int, int], optional
        RGB color when closed (0-255).
    transparency : float, optional
        Transparency when closed (0-1), default: 1.0.
    mult_id : str, optional
        Multiplier ID for array replication.

    Examples
    --------
    >>> from pyfds.core.geometry import Bounds3D
    >>> door = Hole(xb=Bounds3D.of(5, 5.1, 2, 4, 0, 2.1), id='DOOR')

    See Also
    --------
    Obstruction : Solid objects that holes are created in.
    Control : Control logic for dynamic hole activation.
    Multiplier : Array replication of holes.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "HOLE"

    # --- Identification ---
    id: str | None = FdsField(None, description="Hole identifier", group="identification")

    # --- Geometry ---
    xb: Bounds3D = FdsField(
        ..., description="Hole bounds (xmin,xmax,ymin,ymax,zmin,zmax)", group="geometry"
    )

    # --- Control ---
    ctrl_id: str | None = FdsField(
        None, description="Control ID for activation/deactivation", group="control"
    )
    devc_id: str | None = FdsField(
        None, description="Device ID for activation/deactivation", group="control"
    )

    # --- Visualization ---
    color: str | None = FdsField(
        None, description="Color when hole is closed", group="visualization"
    )
    rgb: tuple[int, int, int] | None = FdsField(
        None, description="RGB color when closed (0-255)", group="visualization"
    )
    transparency: float | None = FdsField(
        1.0,
        ge=0,
        le=1,
        exclude_if=1.0,
        description="Transparency when closed (0-1)",
        group="visualization",
    )

    # --- Replication ---
    mult_id: str | None = FdsField(
        None, description="Multiplier ID for array replication", group="replication"
    )

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
