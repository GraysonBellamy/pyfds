"""
FDS INIT namelist.

Initial conditions for temperature, density, and species.
"""

from pydantic import model_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import FdsField, NamelistBase


class Init(NamelistBase):
    """
    FDS INIT namelist - initial conditions.

    Specifies initial conditions for temperature, density, and species
    concentrations in a region.

    Parameters
    ----------
    xb : tuple[float, float, float, float, float, float], optional
        Region bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    xyz : Point3D, optional
        Point location (x, y, z)
    temperature : float, optional
        Initial temperature [°C]
    density : float, optional
        Initial density [kg/m³]
    mass_fraction : list[float], optional
        Species mass fractions
    volume_fraction : list[float], optional
        Species volume fractions
    spec_id : list[str], optional
        Species identifiers

    Examples
    --------
    >>> # Hot gas region
    >>> init = Init(
    ...     xb=Bounds3D.of(0, 10, 0, 10, 0, 0.1),
    ...     temperature=500,
    ...     spec_id=['PROPANE'],
    ...     mass_fraction=[0.05]
    ... )
    """

    xb: Bounds3D | None = FdsField(
        None, description="Initialization bounds (xmin,xmax,ymin,ymax,zmin,zmax)"
    )
    xyz: Point3D | None = FdsField(None, description="Point location")
    temperature: float | None = FdsField(None, description="Temperature [°C]")
    density: float | None = FdsField(None, gt=0, description="Density [kg/m³]")
    mass_fraction: list[float] | None = FdsField(None, description="Mass fractions")
    volume_fraction: list[float] | None = FdsField(None, description="Volume fractions")
    spec_id: list[str] | None = FdsField(None, description="Species IDs")
    id: str | None = FdsField(None, description="INIT identifier")

    @model_validator(mode="after")
    def validate_init(self) -> "Init":
        """Validate initial conditions."""
        # Must specify either XB or XYZ
        if self.xb is None and self.xyz is None:
            raise ValueError("INIT requires either XB or XYZ")

        # If species are specified, need concentrations
        if self.spec_id:
            if not (self.mass_fraction or self.volume_fraction):
                raise ValueError("INIT with SPEC_ID requires MASS_FRACTION or VOLUME_FRACTION")
            if self.mass_fraction and len(self.mass_fraction) != len(self.spec_id):
                raise ValueError("MASS_FRACTION length must match SPEC_ID length")
            if self.volume_fraction and len(self.volume_fraction) != len(self.spec_id):
                raise ValueError("VOLUME_FRACTION length must match SPEC_ID length")

        return self

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "INIT"
