"""
FDS SURF namelist.

Surface properties for boundaries and materials.
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.namelists.base import NamelistBase


class Surface(NamelistBase):
    """
    FDS SURF namelist - surface properties.

    Parameters
    ----------
    id : str
        Unique surface identifier
    rgb : Tuple[int, int, int], optional
        RGB color values (0-255)
    color : str, optional
        Named color (e.g., 'RED', 'BLUE')
    hrrpua : float, optional
        Heat release rate per unit area (kW/m²)
    tmp_front : float, optional
        Front surface temperature (°C)
    matl_id : str, optional
        Material identifier
    thickness : float, optional
        Material thickness (m)

    Examples
    --------
    >>> fire_surf = Surface(id='FIRE', hrrpua=1000.0, color='RED')
    >>> print(fire_surf.to_fds())
    &SURF ID='FIRE', HRRPUA=1000.0, COLOR='RED' /
    """

    id: str = Field(..., description="Surface identifier")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    color: str | None = Field(None, description="Named color")
    hrrpua: float | None = Field(None, ge=0, description="Heat release rate per unit area (kW/m²)")
    tmp_front: float | None = Field(None, description="Front surface temperature (°C)")
    matl_id: str | None = Field(None, description="Material identifier")
    thickness: float | None = Field(None, gt=0, description="Material thickness (m)")
    volume_flow: float | None = Field(None, description="Volume flow rate (m³/s)")
    vel: float | None = Field(None, description="Velocity (m/s)")
    mass_flow: float | None = Field(None, description="Mass flow rate (kg/s)")

    # Heat Transfer Parameters (Stage 1.1)
    mlrpua: float | None = Field(None, ge=0, description="Mass flux per unit area (kg/s/m²)")
    mass_flux_total: float | None = Field(None, ge=0, description="Total mass flux (kg/s)")
    convective_heat_flux: float | None = Field(None, description="Convective heat flux (kW/m²)")
    net_heat_flux: float | None = Field(None, description="Net heat flux (kW/m²)")
    external_flux: float | None = Field(None, description="External radiative flux (kW/m²)")

    # Pyrolysis Control Parameters (Stage 1.1)
    heat_of_combustion: float | None = Field(None, gt=0, description="Heat of combustion (kJ/kg)")
    ignition_temperature: float | None = Field(None, description="Ignition temperature (°C)")
    burn_away: bool = Field(False, description="Remove surface when material burns away")
    backing: str | None = Field(None, description="Backing condition: VOID, INSULATED, or EXPOSED")

    # Radiation Properties (Stage 1.1)
    emissivity: float | None = Field(None, ge=0, le=1, description="Surface emissivity")
    absorptivity: float | None = Field(None, ge=0, le=1, description="Surface absorptivity")

    # Time-Dependent Properties (Stage 1.1)
    ramp_q: str | None = Field(None, description="RAMP_ID for heat flux")
    ramp_mf: str | None = Field(None, description="RAMP_ID for mass flux")
    tau_q: float | None = Field(None, gt=0, description="Ramp time for heat flux (s)")
    tau_mf: float | None = Field(None, gt=0, description="Ramp time for mass flux (s)")

    # Particle Generation (Stage 2.2)
    part_id: str | None = Field(None, description="Particle class ID to generate")
    particle_mass_flux: float | None = Field(None, ge=0, description="Particle mass flux (kg/s/m²)")
    nppc: int = Field(1, ge=1, description="Number of particles per cell")

    # Droplet Distribution (Stage 2.2)
    median_diameter: float | None = Field(None, gt=0, description="Median droplet diameter (m)")
    gamma_d: float | None = Field(None, gt=0, description="Distribution shape parameter")
    spray_pattern: str | None = Field(None, description="Spray pattern: UNIFORM, GAUSSIAN")

    # Particle Velocity (Stage 2.2)
    vel_part: float | None = Field(None, description="Particle velocity magnitude (m/s)")
    particle_velocity: tuple[float, float, float] | None = Field(
        None, description="Particle velocity vector (vx, vy, vz)"
    )

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB values are in range 0-255."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in v):
                raise ValueError("RGB values must be in range 0-255")
        return v

    @field_validator("backing")
    @classmethod
    def validate_backing(cls, v: str | None) -> str | None:
        """Validate backing condition."""
        if v is not None:
            valid_backing = ["VOID", "INSULATED", "EXPOSED"]
            if v.upper() not in valid_backing:
                raise ValueError(f"BACKING must be one of {valid_backing}, got '{v}'")
            return v.upper()
        return v

    @field_validator("spray_pattern")
    @classmethod
    def validate_spray_pattern(cls, v: str | None) -> str | None:
        """Validate spray pattern."""
        if v is not None:
            valid_patterns = ["UNIFORM", "GAUSSIAN"]
            if v.upper() not in valid_patterns:
                raise ValueError(f"SPRAY_PATTERN must be one of {valid_patterns}, got '{v}'")
            return v.upper()
        return v

    @model_validator(mode="after")
    def validate_heat_source(self) -> "Surface":
        """Validate mutually exclusive heat source specifications."""
        heat_sources = [
            self.hrrpua is not None,
            self.mlrpua is not None,
            self.mass_flux_total is not None,
            self.convective_heat_flux is not None,
            self.net_heat_flux is not None,
        ]
        if sum(heat_sources) > 1:
            raise ValueError(
                "Only one heat source specification allowed per SURF: "
                "HRRPUA, MLRPUA, MASS_FLUX_TOTAL, CONVECTIVE_HEAT_FLUX, or NET_HEAT_FLUX"
            )
        return self

    def to_fds(self) -> str:
        """Generate FDS SURF namelist."""
        params: dict[str, Any] = {"id": self.id}
        if self.rgb:
            params["rgb"] = self.rgb
        if self.color:
            params["color"] = self.color
        if self.hrrpua is not None:
            params["hrrpua"] = self.hrrpua
        if self.tmp_front is not None:
            params["tmp_front"] = self.tmp_front
        if self.matl_id:
            params["matl_id"] = self.matl_id
        if self.thickness is not None:
            params["thickness"] = self.thickness
        if self.volume_flow is not None:
            params["volume_flow"] = self.volume_flow
        if self.vel is not None:
            params["vel"] = self.vel
        if self.mass_flow is not None:
            params["mass_flow"] = self.mass_flow

        # Heat Transfer Parameters
        if self.mlrpua is not None:
            params["mlrpua"] = self.mlrpua
        if self.mass_flux_total is not None:
            params["mass_flux_total"] = self.mass_flux_total
        if self.convective_heat_flux is not None:
            params["convective_heat_flux"] = self.convective_heat_flux
        if self.net_heat_flux is not None:
            params["net_heat_flux"] = self.net_heat_flux
        if self.external_flux is not None:
            params["external_flux"] = self.external_flux

        # Pyrolysis Control
        if self.heat_of_combustion is not None:
            params["heat_of_combustion"] = self.heat_of_combustion
        if self.ignition_temperature is not None:
            params["ignition_temperature"] = self.ignition_temperature
        if self.burn_away:
            params["burn_away"] = self.burn_away
        if self.backing:
            params["backing"] = self.backing

        # Radiation Properties
        if self.emissivity is not None:
            params["emissivity"] = self.emissivity
        if self.absorptivity is not None:
            params["absorptivity"] = self.absorptivity

        # Time-Dependent Properties
        if self.ramp_q:
            params["ramp_q"] = self.ramp_q
        if self.ramp_mf:
            params["ramp_mf"] = self.ramp_mf
        if self.tau_q is not None:
            params["tau_q"] = self.tau_q
        if self.tau_mf is not None:
            params["tau_mf"] = self.tau_mf

        # Particle Generation
        if self.part_id:
            params["part_id"] = self.part_id
        if self.particle_mass_flux is not None:
            params["particle_mass_flux"] = self.particle_mass_flux
        if self.nppc != 1:
            params["nppc"] = self.nppc

        # Droplet Distribution
        if self.median_diameter is not None:
            params["median_diameter"] = self.median_diameter
        if self.gamma_d is not None:
            params["gamma_d"] = self.gamma_d
        if self.spray_pattern:
            params["spray_pattern"] = self.spray_pattern

        # Particle Velocity
        if self.vel_part is not None:
            params["vel_part"] = self.vel_part
        if self.particle_velocity is not None:
            params["particle_velocity"] = self.particle_velocity

        return self._build_namelist("SURF", params)
