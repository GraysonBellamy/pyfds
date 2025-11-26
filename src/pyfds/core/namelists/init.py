"""
FDS INIT namelist.

Initial conditions for temperature, density, and species.
"""

from typing import Any

from pydantic import Field, model_validator

from pyfds.core.namelists.base import NamelistBase


class Init(NamelistBase):
    """
    FDS INIT namelist - initial conditions.

    Specifies initial conditions for temperature, density, and species
    concentrations in a region.

    Parameters
    ----------
    xb : tuple[float, float, float, float, float, float], optional
        Region bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    xyz : tuple[float, float, float], optional
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
    ...     xb=(0, 10, 0, 10, 0, 0.1),
    ...     temperature=500,
    ...     spec_id=['PROPANE'],
    ...     mass_fraction=[0.05]
    ... )
    """

    xb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Region bounds"
    )
    xyz: tuple[float, float, float] | None = Field(None, description="Point location")
    temperature: float | None = Field(None, description="Temperature [°C]")
    density: float | None = Field(None, gt=0, description="Density [kg/m³]")
    mass_fraction: list[float] | None = Field(None, description="Mass fractions")
    volume_fraction: list[float] | None = Field(None, description="Volume fractions")
    spec_id: list[str] | None = Field(None, description="Species IDs")

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

    def to_fds(self) -> str:
        """Generate FDS INIT namelist."""
        params: dict[str, Any] = {}

        if self.xb:
            params["xb"] = self.xb
        if self.xyz:
            params["xyz"] = self.xyz
        if self.temperature is not None:
            params["temperature"] = self.temperature
        if self.density is not None:
            params["density"] = self.density
        if self.spec_id:
            params["spec_id"] = self.spec_id
        if self.mass_fraction:
            params["mass_fraction"] = self.mass_fraction
        if self.volume_fraction:
            params["volume_fraction"] = self.volume_fraction

        return self._build_namelist("INIT", params)
