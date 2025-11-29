"""
FDS SURF namelist.

Surface properties for boundaries and materials.
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.enums import BackingCondition, HeatTransferModel, SolidGeometry
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
    matl_id : str, list[str], or list[list[str]], optional
        Material identifier(s): single material, list per layer, or 2D array for multi-component layers
    thickness : float or list[float], optional
        Material thickness(es) (m): single value or list per layer
    matl_mass_fraction : list[list[float]], optional
        Mass fractions for multi-component layers
    delamination_tmp : list[float], optional
        Temperature threshold for layer delamination [°C]
    delamination_density : list[float], optional
        Density threshold for layer delamination [kg/m³]
    tmp_gas_front : float, optional
        Gas temperature for convective heat transfer [°C]
    heat_transfer_coefficient : float, optional
        Fixed heat transfer coefficient [W/(m²·K)]
    ramp_ef : str, optional
        RAMP ID for external flux time history
    tau_ef : float, optional
        Time constant for external flux ramp-up [s]

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
    matl_id: list[list[str]] | list[str] | str | None = Field(
        None, description="Material ID(s): single, list per layer, or 2D array"
    )
    matl_mass_fraction: list[list[float]] | None = Field(
        None, description="Mass fractions for multi-component layers"
    )
    thickness: list[float] | float | None = Field(None, description="Layer thickness(es) [m]")
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
    backing: BackingCondition | str | None = Field(
        None, description="Backing condition: VOID, INSULATED, or EXPOSED"
    )

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

    # Fire Spread Parameters (Priority 1)
    spread_rate: float | None = Field(None, gt=0, description="Radial fire spread rate (m/s)")
    xyz: tuple[float, float, float] | None = Field(
        None, description="Ignition point (x, y, z) for spreading fire"
    )
    area_multiplier: float = Field(1.0, gt=0, description="Surface area correction factor")

    # Species Control (Priority 1)
    spec_id: list[str] | str | None = Field(None, description="Species ID(s) for surface emissions")
    mass_fraction: list[float] | None = Field(
        None, description="Mass fractions for multiple species"
    )

    # Thermally-Thick Specified Burning (Priority 1)
    heat_of_vaporization: float | None = Field(
        None, gt=0, description="Heat of vaporization (kJ/kg)"
    )
    extinction_temperature: float | None = Field(None, description="Extinction temperature (°C)")
    burn_duration: float | None = Field(None, gt=0, description="Burn duration after ignition (s)")

    # SPyro Model Parameters (Priority 1)
    inert_q_ref: bool = Field(False, description="Test data from inert pyrolysis (no combustion)")
    reference_heat_flux: list[float] | float | None = Field(
        None, description="Reference heat flux from test device (kW/m²)"
    )
    reference_thickness: list[float] | float | None = Field(
        None, description="Sample thickness in experiment (m)"
    )
    maximum_scaling_heat_flux: float = Field(
        1500.0, gt=0, description="Upper limit on scaling heat flux (kW/m²)"
    )
    minimum_scaling_heat_flux: float = Field(
        0.0, ge=0, description="Lower limit on scaling heat flux (kW/m²)"
    )
    reference_heat_flux_time_interval: float = Field(
        1.0, gt=0, description="Smoothing window for heat flux (s)"
    )

    # Solid Phase Gas Transport (Priority 1)
    layer_divide: float | None = Field(None, ge=0, description="Layer division for gas transport")

    # Testing Parameters (Priority 1)
    tga_analysis: bool = Field(False, description="Enable TGA analysis mode")
    tga_heating_rate: float = Field(5.0, gt=0, description="TGA heating rate (K/min)")
    tga_final_temperature: float = Field(800.0, description="TGA final temperature (°C)")
    tga_dt: float | None = Field(None, gt=0, description="TGA timestep (s)")
    tga_dump: float | None = Field(None, gt=0, description="TGA output spacing (°C)")
    tga_conversion_factor: float = Field(1.0, description="TGA output multiplier")
    mcc_conversion_factor: float = Field(1.0, description="MCC output multiplier")
    dsc_conversion_factor: float = Field(1.0, description="DSC output multiplier")

    # Liquid Evaporation (Priority 1)
    mass_transfer_coefficient: float | None = Field(
        None, gt=0, description="Mass transfer coefficient (m/s)"
    )

    # Delamination Model (Phase 2.2)
    delamination_tmp: list[float] | None = Field(
        None, description="Temperature threshold for layer delamination [°C]"
    )
    delamination_density: list[float] | None = Field(
        None, description="Density threshold for layer delamination [kg/m³]"
    )

    # Cone Calorimeter Parameters (Phase 2.3)
    tmp_gas_front: float | None = Field(
        None, description="Gas temperature for convective heat transfer [°C]"
    )
    heat_transfer_coefficient: float | None = Field(
        None, gt=0, description="Fixed heat transfer coefficient [W/(m²·K)]"
    )
    ramp_ef: str | None = Field(None, description="RAMP ID for external flux time history")
    tau_ef: float | None = Field(
        None, gt=0, description="Time constant for external flux ramp-up [s]"
    )

    # === TEMPERATURE BOUNDARY CONDITIONS (Phase 3) ===
    tmp_front_initial: float | None = Field(
        None, description="Initial front surface temperature [°C]"
    )
    tmp_inner: float | None = Field(None, description="Initial solid interior temperature [°C]")
    tmp_back: float | None = Field(None, description="Fixed back surface temperature [°C]")
    tmp_gas_back: float | None = Field(None, description="Back gas temperature for convection [°C]")

    # Temperature ramps
    ramp_t: str | None = Field(None, description="Temperature ramp ID")
    ramp_tmp_back: str | None = Field(None, description="Back temperature ramp")
    ramp_tmp_gas_front: str | None = Field(None, description="Front gas temp ramp")
    ramp_tmp_gas_back: str | None = Field(None, description="Back gas temp ramp")
    ramp_t_i: str | None = Field(None, description="Initial temperature profile ramp")

    # === HEAT TRANSFER (Phase 3) ===
    adiabatic: bool = Field(False, description="Adiabatic surface (no heat transfer)")
    heat_transfer_coefficient_back: float | None = Field(
        None, gt=0, description="Back side convection coefficient [W/(m²·K)]"
    )
    heat_transfer_model: HeatTransferModel | str | None = Field(
        None, description="Heat transfer model: LOGLAW, IMPINGING JET"
    )
    convection_length_scale: float = Field(
        1.0, gt=0, description="Characteristic length for convection [m]"
    )
    ramp_heat_transfer_coefficient: str | None = Field(
        None, description="Ramp for heat transfer coefficient"
    )
    ramp_heat_transfer_coefficient_back: str | None = Field(None, description="Ramp for back HTC")
    blowing: bool = Field(False, description="Account for mass flux effect on convection")

    # Custom Nusselt correlation: Nu = C0 + C1 * Re^M * Pr^C2
    nusselt_c0: float | None = Field(None, description="Nusselt correlation C0")
    nusselt_c1: float | None = Field(None, description="Nusselt correlation C1")
    nusselt_c2: float | None = Field(None, description="Nusselt correlation C2")
    nusselt_m: float | None = Field(None, description="Nusselt correlation M (Re exponent)")

    # Impinging jet parameters
    heat_transfer_coefficient_sigma: float | None = Field(
        None, gt=0, description="Impinging jet width [m]"
    )

    # === EMISSIVITY (Phase 3) ===
    emissivity_back: float | None = Field(None, ge=0, le=1, description="Back surface emissivity")

    # === SOLID PHASE GEOMETRY (Phase 3) ===
    geometry: SolidGeometry | str | None = Field(
        None, description="Solid geometry: CARTESIAN, CYLINDRICAL, SPHERICAL, INNER CYLINDRICAL"
    )
    inner_radius: float | None = Field(
        None, gt=0, description="Inner radius for hollow cylinder [m]"
    )
    length: float | None = Field(None, gt=0, description="Cylinder/particle length [m]")
    radius: float | None = Field(None, gt=0, description="Cylinder/particle radius [m]")
    width: float | None = Field(None, gt=0, description="Particle width [m]")
    horizontal: bool = Field(False, description="Horizontal cylinder orientation")

    # === 3D HEAT CONDUCTION (Phase 3) ===
    ht3d: bool = Field(False, description="Enable 3-D heat conduction")
    variable_thickness: bool = Field(False, description="Variable thickness 1-D mode")

    # === NUMERICAL PARAMETERS (Phase 3) ===
    stretch_factor: list[float] | float | None = Field(
        None, description="Node spacing stretch factor per layer"
    )
    cell_size_factor: list[float] | float | None = Field(
        None, description="Cell size multiplier per layer"
    )
    cell_size: list[float] | float | None = Field(
        None, description="Explicit cell size per layer [m]"
    )
    n_layer_cells_max: list[int] | int | None = Field(None, description="Maximum cells per layer")
    time_step_factor: float = Field(10.0, gt=0, description="Maximum time step subdivision factor")
    delta_tmp_max: float = Field(10.0, gt=0, description="Maximum temperature change per step [°C]")
    minimum_layer_thickness: list[float] | float | None = Field(
        None, description="Minimum layer thickness [m]"
    )
    minimum_layer_mass_fraction: list[float] | float | None = Field(
        None, description="Minimum layer mass fraction"
    )
    remesh_ratio: float = Field(0.15, gt=0, description="Trigger ratio for remeshing")

    # === INTERNAL HEAT SOURCE (Phase 3) ===
    internal_heat_source: list[float] | float | None = Field(
        None, description="Internal heat source per layer [kW/m³]"
    )
    ramp_ihs: list[str] | str | None = Field(
        None, description="Ramp for internal heat source per layer"
    )

    # === VISUALIZATION (Phase 3) ===
    default: bool = Field(False, description="Use as default boundary condition")
    texture_map: str | None = Field(None, description="Texture image file")
    texture_width: float = Field(1.0, gt=0, description="Texture width [m]")
    texture_height: float = Field(1.0, gt=0, description="Texture height [m]")
    transparency: float = Field(1.0, ge=0, le=1, description="Surface transparency")

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

    @field_validator("reference_thickness")
    @classmethod
    def validate_reference_thickness(
        cls, v: list[float] | float | None
    ) -> list[float] | float | None:
        """Validate reference thickness values are positive."""
        if v is not None:
            if isinstance(v, list):
                if any(thickness <= 0 for thickness in v):
                    raise ValueError("All REFERENCE_THICKNESS values must be > 0")
            else:
                if v <= 0:
                    raise ValueError("REFERENCE_THICKNESS must be > 0")
        return v

    @field_validator("spec_id")
    @classmethod
    def normalize_spec_id(cls, v: list[str] | str | None) -> list[str] | str | None:
        """Normalize SPEC_ID to list or string."""
        # Keep as-is, will be handled in validation
        return v

    @field_validator("geometry")
    @classmethod
    def validate_geometry(cls, v: str | None) -> str | None:
        """Validate solid phase geometry."""
        if v is not None:
            valid = ["CARTESIAN", "CYLINDRICAL", "SPHERICAL", "INNER CYLINDRICAL"]
            if v.upper() not in valid:
                raise ValueError(f"GEOMETRY must be one of {valid}, got '{v}'")
            return v.upper()
        return v

    @field_validator("heat_transfer_model")
    @classmethod
    def validate_heat_transfer_model(cls, v: str | None) -> str | None:
        """Validate heat transfer model."""
        if v is not None:
            valid = ["LOGLAW", "IMPINGING JET"]
            if v.upper() not in valid:
                raise ValueError(f"HEAT_TRANSFER_MODEL must be one of {valid}, got '{v}'")
            return v.upper()
        return v

    @model_validator(mode="after")
    def validate_surface(self) -> "Surface":
        """Validate surface parameters and dependencies."""
        # Validate mutually exclusive heat source specifications
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

        # Validate SPEC_ID and MASS_FRACTION consistency
        if self.mass_fraction is not None:
            if self.spec_id is None:
                raise ValueError("MASS_FRACTION requires SPEC_ID to be specified")
            if isinstance(self.spec_id, str):
                raise ValueError(
                    "MASS_FRACTION requires SPEC_ID to be a list of species, not a single species"
                )
            if len(self.spec_id) != len(self.mass_fraction):
                raise ValueError(
                    f"SPEC_ID ({len(self.spec_id)} items) and MASS_FRACTION "
                    f"({len(self.mass_fraction)} items) must have same length"
                )
            # Validate mass fractions sum to ~1.0
            total = sum(self.mass_fraction)
            if not (0.99 <= total <= 1.01):
                raise ValueError(f"MASS_FRACTION values must sum to 1.0, got {total:.3f}")

        # Validate fire spread parameters
        if self.spread_rate is not None and self.hrrpua is None and self.mlrpua is None:
            raise ValueError("SPREAD_RATE requires HRRPUA or MLRPUA to be specified")

        # Validate extinction temperature
        if (
            self.extinction_temperature is not None
            and self.ignition_temperature is not None
            and self.extinction_temperature > self.ignition_temperature
        ):
            raise ValueError(
                f"EXTINCTION_TEMPERATURE ({self.extinction_temperature}°C) must be <= "
                f"IGNITION_TEMPERATURE ({self.ignition_temperature}°C)"
            )

        # Validate SPyro model requirements
        spyro_params = [
            self.reference_heat_flux is not None,
            self.ignition_temperature is not None and self.ramp_q is not None,
        ]
        if (
            any(spyro_params)
            and not all([self.hrrpua is not None, self.ignition_temperature is not None])
            and self.reference_heat_flux is not None
            and self.ramp_q is None
        ):
            # SPyro model is being used - check requirements
            raise ValueError(
                "SPyro model (REFERENCE_HEAT_FLUX) requires RAMP_Q to be specified with test data"
            )

        # Validate REFERENCE_HEAT_FLUX and REFERENCE_THICKNESS array consistency
        if self.reference_heat_flux is not None and self.reference_thickness is not None:
            # Convert to lists for validation
            ref_flux = (
                self.reference_heat_flux
                if isinstance(self.reference_heat_flux, list)
                else [self.reference_heat_flux]
            )
            ref_thick = (
                self.reference_thickness
                if isinstance(self.reference_thickness, list)
                else [self.reference_thickness]
            )
            if len(ref_flux) != len(ref_thick):
                raise ValueError(
                    f"REFERENCE_HEAT_FLUX ({len(ref_flux)} values) and REFERENCE_THICKNESS "
                    f"({len(ref_thick)} values) must have same length for multiple experiments"
                )

        # Validate TGA_ANALYSIS requirements
        if self.tga_analysis and self.matl_id is None:
            raise ValueError("TGA_ANALYSIS requires MATL_ID to be specified")

        return self

    @field_validator("thickness", mode="after")
    @classmethod
    def validate_thickness(cls, v: float | list[float] | None) -> float | list[float] | None:
        """Validate thickness values are positive."""
        if v is None:
            return v
        if isinstance(v, (int, float)):
            if v <= 0:
                raise ValueError("Thickness must be positive")
            return v
        if isinstance(v, list):
            if not all(isinstance(t, (int, float)) and t > 0 for t in v):
                raise ValueError("All thickness values must be positive")
            return v
        raise ValueError("Thickness must be a number or list of numbers")

    def _format_multi_layer_params(self) -> dict[str, Any]:
        """Format multi-layer material parameters for FDS output."""
        params: dict[str, Any] = {}

        if self.matl_id:
            if isinstance(self.matl_id, str):
                # Single material
                params["matl_id"] = self.matl_id
            elif isinstance(self.matl_id, list):
                if all(isinstance(layer, str) for layer in self.matl_id):
                    # List of materials per layer - use 2D indexed format (1, layer_index)
                    # FDS requires 2D indexing even for single material per layer
                    for i, matl in enumerate(self.matl_id):
                        params[f"matl_id(1,{i + 1})"] = matl
                elif all(isinstance(layer, list) for layer in self.matl_id):
                    # 2D array: layer x components
                    for i, layer in enumerate(self.matl_id):
                        if layer:  # Non-empty layer
                            for j, matl in enumerate(layer):
                                params[f"matl_id({i + 1},{j + 1})"] = matl

        if self.matl_mass_fraction:
            # 2D array for mass fractions
            for i, layer in enumerate(self.matl_mass_fraction):  # type: ignore
                layer: list[float]  # type: ignore
                if layer:  # Non-empty layer
                    for j, frac in enumerate(layer):
                        frac: float  # type: ignore
                        params[f"matl_mass_fraction({i + 1},{j + 1})"] = frac
        if self.thickness is not None:
            if isinstance(self.thickness, (int, float)):
                params["thickness"] = self.thickness
            elif isinstance(self.thickness, list):
                # Use indexed format for multi-layer
                for i, t in enumerate(self.thickness):
                    params[f"thickness({i + 1})"] = t

        return params

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

        # Multi-layer material parameters
        multi_layer_params = self._format_multi_layer_params()
        params.update(multi_layer_params)

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

        # Fire Spread Parameters
        if self.spread_rate is not None:
            params["spread_rate"] = self.spread_rate
        if self.xyz is not None:
            params["xyz"] = self.xyz
        if self.area_multiplier != 1.0:
            params["area_multiplier"] = self.area_multiplier

        # Species Control
        if self.spec_id is not None:
            params["spec_id"] = self.spec_id
        if self.mass_fraction is not None:
            params["mass_fraction"] = self.mass_fraction

        # Thermally-Thick Specified Burning
        if self.heat_of_vaporization is not None:
            params["heat_of_vaporization"] = self.heat_of_vaporization
        if self.extinction_temperature is not None:
            params["extinction_temperature"] = self.extinction_temperature
        if self.burn_duration is not None:
            params["burn_duration"] = self.burn_duration

        # SPyro Model Parameters
        if self.inert_q_ref:
            params["inert_q_ref"] = self.inert_q_ref
        if self.reference_heat_flux is not None:
            params["reference_heat_flux"] = self.reference_heat_flux
        if self.reference_thickness is not None:
            params["reference_thickness"] = self.reference_thickness
        if self.maximum_scaling_heat_flux != 1500.0:
            params["maximum_scaling_heat_flux"] = self.maximum_scaling_heat_flux
        if self.minimum_scaling_heat_flux != 0.0:
            params["minimum_scaling_heat_flux"] = self.minimum_scaling_heat_flux
        if self.reference_heat_flux_time_interval != 1.0:
            params["reference_heat_flux_time_interval"] = self.reference_heat_flux_time_interval

        # Solid Phase Gas Transport
        if self.layer_divide is not None:
            params["layer_divide"] = self.layer_divide

        # Testing Parameters
        if self.tga_analysis:
            params["tga_analysis"] = self.tga_analysis
        if self.tga_heating_rate != 5.0:
            params["tga_heating_rate"] = self.tga_heating_rate
        if self.tga_final_temperature != 800.0:
            params["tga_final_temperature"] = self.tga_final_temperature
        if self.tga_dt is not None:
            params["tga_dt"] = self.tga_dt
        if self.tga_dump is not None:
            params["tga_dump"] = self.tga_dump
        if self.tga_conversion_factor != 1.0:
            params["tga_conversion_factor"] = self.tga_conversion_factor
        if self.mcc_conversion_factor != 1.0:
            params["mcc_conversion_factor"] = self.mcc_conversion_factor
        if self.dsc_conversion_factor != 1.0:
            params["dsc_conversion_factor"] = self.dsc_conversion_factor

        # Liquid Evaporation
        if self.mass_transfer_coefficient is not None:
            params["mass_transfer_coefficient"] = self.mass_transfer_coefficient

        # Delamination Model
        if self.delamination_tmp is not None:
            params["delamination_tmp"] = self.delamination_tmp
        if self.delamination_density is not None:
            params["delamination_density"] = self.delamination_density

        # Cone Calorimeter Parameters
        if self.tmp_gas_front is not None:
            params["tmp_gas_front"] = self.tmp_gas_front
        if self.heat_transfer_coefficient is not None:
            params["heat_transfer_coefficient"] = self.heat_transfer_coefficient
        if self.ramp_ef is not None:
            params["ramp_ef"] = self.ramp_ef
        if self.tau_ef is not None:
            params["tau_ef"] = self.tau_ef

        # === TEMPERATURE BOUNDARY CONDITIONS (Phase 3) ===
        if self.tmp_front_initial is not None:
            params["tmp_front_initial"] = self.tmp_front_initial
        if self.tmp_inner is not None:
            params["tmp_inner"] = self.tmp_inner
        if self.tmp_back is not None:
            params["tmp_back"] = self.tmp_back
        if self.tmp_gas_back is not None:
            params["tmp_gas_back"] = self.tmp_gas_back

        # Temperature ramps
        if self.ramp_t is not None:
            params["ramp_t"] = self.ramp_t
        if self.ramp_tmp_back is not None:
            params["ramp_tmp_back"] = self.ramp_tmp_back
        if self.ramp_tmp_gas_front is not None:
            params["ramp_tmp_gas_front"] = self.ramp_tmp_gas_front
        if self.ramp_tmp_gas_back is not None:
            params["ramp_tmp_gas_back"] = self.ramp_tmp_gas_back
        if self.ramp_t_i is not None:
            params["ramp_t_i"] = self.ramp_t_i

        # === HEAT TRANSFER (Phase 3) ===
        if self.adiabatic:
            params["adiabatic"] = self.adiabatic
        if self.heat_transfer_coefficient_back is not None:
            params["heat_transfer_coefficient_back"] = self.heat_transfer_coefficient_back
        if self.heat_transfer_model is not None:
            params["heat_transfer_model"] = self.heat_transfer_model
        if self.convection_length_scale != 1.0:
            params["convection_length_scale"] = self.convection_length_scale
        if self.ramp_heat_transfer_coefficient is not None:
            params["ramp_heat_transfer_coefficient"] = self.ramp_heat_transfer_coefficient
        if self.ramp_heat_transfer_coefficient_back is not None:
            params["ramp_heat_transfer_coefficient_back"] = self.ramp_heat_transfer_coefficient_back
        if self.blowing:
            params["blowing"] = self.blowing

        # Custom Nusselt correlation
        if self.nusselt_c0 is not None:
            params["nusselt_c0"] = self.nusselt_c0
        if self.nusselt_c1 is not None:
            params["nusselt_c1"] = self.nusselt_c1
        if self.nusselt_c2 is not None:
            params["nusselt_c2"] = self.nusselt_c2
        if self.nusselt_m is not None:
            params["nusselt_m"] = self.nusselt_m

        # Impinging jet parameters
        if self.heat_transfer_coefficient_sigma is not None:
            params["heat_transfer_coefficient_sigma"] = self.heat_transfer_coefficient_sigma

        # === EMISSIVITY (Phase 3) ===
        if self.emissivity_back is not None:
            params["emissivity_back"] = self.emissivity_back

        # === SOLID PHASE GEOMETRY (Phase 3) ===
        if self.geometry is not None:
            params["geometry"] = self.geometry
        if self.inner_radius is not None:
            params["inner_radius"] = self.inner_radius
        if self.length is not None:
            params["length"] = self.length
        if self.radius is not None:
            params["radius"] = self.radius
        if self.width is not None:
            params["width"] = self.width
        if self.horizontal:
            params["horizontal"] = self.horizontal

        # === 3D HEAT CONDUCTION (Phase 3) ===
        if self.ht3d:
            params["ht3d"] = self.ht3d
        if self.variable_thickness:
            params["variable_thickness"] = self.variable_thickness

        # === NUMERICAL PARAMETERS (Phase 3) ===
        if self.stretch_factor is not None:
            params["stretch_factor"] = self.stretch_factor
        if self.cell_size_factor is not None:
            params["cell_size_factor"] = self.cell_size_factor
        if self.cell_size is not None:
            params["cell_size"] = self.cell_size
        if self.n_layer_cells_max is not None:
            params["n_layer_cells_max"] = self.n_layer_cells_max
        if self.time_step_factor != 10.0:
            params["time_step_factor"] = self.time_step_factor
        if self.delta_tmp_max != 10.0:
            params["delta_tmp_max"] = self.delta_tmp_max
        if self.minimum_layer_thickness is not None:
            params["minimum_layer_thickness"] = self.minimum_layer_thickness
        if self.minimum_layer_mass_fraction is not None:
            params["minimum_layer_mass_fraction"] = self.minimum_layer_mass_fraction
        if self.remesh_ratio != 0.15:
            params["remesh_ratio"] = self.remesh_ratio

        # === INTERNAL HEAT SOURCE (Phase 3) ===
        if self.internal_heat_source is not None:
            params["internal_heat_source"] = self.internal_heat_source
        if self.ramp_ihs is not None:
            params["ramp_ihs"] = self.ramp_ihs

        # === VISUALIZATION (Phase 3) ===
        if self.default:
            params["default"] = self.default
        if self.texture_map is not None:
            params["texture_map"] = self.texture_map
        if self.texture_width != 1.0:
            params["texture_width"] = self.texture_width
        if self.texture_height != 1.0:
            params["texture_height"] = self.texture_height
        if self.transparency != 1.0:
            params["transparency"] = self.transparency

        return self._build_namelist("SURF", params)
