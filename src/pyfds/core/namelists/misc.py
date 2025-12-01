"""FDS MISC namelist for miscellaneous simulation parameters.

The MISC namelist contains global simulation parameters that affect
physics, numerics, and solver behavior.

FDS User Guide Reference: Table 22.15 (95 parameters)

Field Groups:
    ambient: Temperature, pressure, humidity, gravity
    turbulence: LES model and constants
    numerical: CFL and Von Neumann limits, flux limiters
    solver: Physics options (radiation, stratification)
    visibility: Visibility and smoke parameters
    deposition: Particle deposition settings
    hvac: HVAC pressure and transport settings
    wildfire: Level set wildfire mode
    restart: Restart configuration
    output: Output control parameters
    geo: Geographic position
    misc: Other miscellaneous parameters
"""

from pydantic import field_validator, model_validator

from pyfds.core.enums import LESFilterType, SimulationMode, TurbulenceModel
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Misc"]


class Misc(NamelistBase):
    """FDS MISC namelist for miscellaneous parameters.

    Contains global simulation parameters that affect physics, numerics,
    and solver behavior. Only one MISC namelist is allowed per simulation.

    FDS User Guide Reference: Table 22.15

    Parameters
    ----------
    tmpa : float, optional
        Ambient temperature [°C], default: 20.0.
    p_inf : float, optional
        Background pressure [Pa], default: 101325.0.
    humidity : float, optional
        Relative humidity [%], default: 40.0.
    turbulence_model : TurbulenceModel, optional
        LES turbulence model, default: DEARDORFF.

    Notes
    -----
    Only one MISC namelist is allowed per simulation.
    Some parameters have major performance impacts.

    Examples
    --------
    >>> misc = Misc(tmpa=25.0, humidity=70.0)

    See Also
    --------
    Combustion : Additional combustion parameters via COMB namelist.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MISC"

    # ==========================================================================
    # Ambient Conditions
    # ==========================================================================
    tmpa: float = FdsField(
        20.0, exclude_if=20.0, description="Ambient temperature [°C]", group="ambient"
    )
    p_inf: float = FdsField(
        101325.0,
        exclude_if=101325.0,
        gt=0,
        description="Background pressure [Pa]",
        group="ambient",
    )
    humidity: float = FdsField(
        40.0,
        exclude_if=40.0,
        ge=0,
        le=100,
        description="Relative humidity [%]",
        group="ambient",
    )
    gvec: tuple[float, float, float] | None = FdsField(
        None,
        description="Gravity vector [m/s²]",
        group="ambient",
    )
    gamma: float = FdsField(
        1.4, exclude_if=1.4, gt=1, description="Ratio of specific heats", group="ambient"
    )
    constant_specific_heat_ratio: bool = FdsField(
        False,
        exclude_if=False,
        description="Use constant specific heat ratio for all species",
        group="ambient",
    )
    h_f_reference_temperature: float = FdsField(
        25.0,
        exclude_if=25.0,
        description="Reference temperature for enthalpy of formation [°C]",
        group="ambient",
    )
    y_co2_infty: float | None = FdsField(
        None, ge=0, le=1, description="Ambient CO2 mass fraction [kg/kg]", group="ambient"
    )
    y_o2_infty: float | None = FdsField(
        None, ge=0, le=1, description="Ambient O2 mass fraction [kg/kg]", group="ambient"
    )

    # ==========================================================================
    # Turbulence Model
    # ==========================================================================
    turbulence_model: TurbulenceModel = FdsField(
        TurbulenceModel.DEARDORFF,
        exclude_if=TurbulenceModel.DEARDORFF,
        description="Turbulence model",
        group="turbulence",
    )
    simulation_mode: SimulationMode = FdsField(
        SimulationMode.VLES,
        exclude_if=SimulationMode.VLES,
        description="Simulation mode (VLES, LES, DNS, SVLES)",
        group="turbulence",
    )
    c_deardorff: float = FdsField(
        0.1, exclude_if=0.1, ge=0, description="Deardorff constant", group="turbulence"
    )
    c_smagorinsky: float = FdsField(
        0.2, exclude_if=0.2, ge=0, description="Smagorinsky constant", group="turbulence"
    )
    c_vreman: float = FdsField(
        0.07, exclude_if=0.07, ge=0, description="Vreman constant", group="turbulence"
    )
    c_wale: float = FdsField(
        0.6, exclude_if=0.6, ge=0, description="WALE constant", group="turbulence"
    )
    les_filter_type: LESFilterType = FdsField(
        LESFilterType.MEAN,
        exclude_if=LESFilterType.MEAN,
        description="LES filter type (MEAN or MAX)",
        group="turbulence",
    )
    fixed_les_filter_width: float | None = FdsField(
        None, gt=0, description="Fixed LES filter width [m]", group="turbulence"
    )
    pr_t: float = FdsField(
        0.5, exclude_if=0.5, gt=0, description="Turbulent Prandtl number", group="turbulence"
    )
    sc_t: float = FdsField(
        0.5, exclude_if=0.5, gt=0, description="Turbulent Schmidt number", group="turbulence"
    )

    # ==========================================================================
    # Numerical Parameters
    # ==========================================================================
    cfl_max: float = FdsField(
        1.0, exclude_if=1.0, gt=0, description="Maximum CFL number", group="numerical"
    )
    cfl_min: float = FdsField(
        0.8, exclude_if=0.8, gt=0, description="Minimum CFL number", group="numerical"
    )
    cfl_velocity_norm: int | None = FdsField(
        None, ge=0, le=3, description="CFL velocity norm type", group="numerical"
    )
    vn_max: float = FdsField(
        1.0, exclude_if=1.0, gt=0, description="Maximum Von Neumann number", group="numerical"
    )
    vn_min: float = FdsField(
        0.8, exclude_if=0.8, gt=0, description="Minimum Von Neumann number", group="numerical"
    )
    check_vn: bool = FdsField(
        True, exclude_if=True, description="Check Von Neumann stability", group="numerical"
    )
    flux_limiter: int = FdsField(
        2, exclude_if=2, ge=0, le=5, description="Flux limiter type", group="numerical"
    )
    particle_cfl: bool = FdsField(
        False, exclude_if=False, description="Use particle CFL", group="numerical"
    )
    particle_cfl_max: float = FdsField(
        1.0, exclude_if=1.0, gt=0, description="Maximum particle CFL number", group="numerical"
    )
    particle_cfl_min: float = FdsField(
        0.8, exclude_if=0.8, gt=0, description="Minimum particle CFL number", group="numerical"
    )
    check_fo: bool = FdsField(
        False,
        exclude_if=False,
        description="Check Fourier number for solid phase stability",
        group="numerical",
    )
    check_ht: bool = FdsField(
        False,
        exclude_if=False,
        description="Check heat transfer coefficient",
        group="numerical",
    )
    alignment_tolerance: float = FdsField(
        0.001,
        exclude_if=0.001,
        gt=0,
        description="Mesh alignment tolerance",
        group="numerical",
    )

    # ==========================================================================
    # Solver Options
    # ==========================================================================
    solid_phase_only: bool = FdsField(
        False, exclude_if=False, description="Solid phase only (no gas flow)", group="solver"
    )
    isothermal: bool = FdsField(
        False, exclude_if=False, description="Isothermal flow", group="solver"
    )
    radiation: bool = FdsField(
        True, exclude_if=True, description="Include radiation", group="solver"
    )
    stratification: bool = FdsField(
        True, exclude_if=True, description="Include stratification", group="solver"
    )
    freeze_velocity: bool = FdsField(
        False, exclude_if=False, description="Freeze velocity field", group="solver"
    )
    unfreeze_time: float | None = FdsField(
        None, ge=0, description="Time to unfreeze velocity [s]", group="solver"
    )
    noise: bool = FdsField(
        True, exclude_if=True, description="Add numerical noise to initial velocity", group="solver"
    )
    noise_velocity: float = FdsField(
        0.005,
        exclude_if=0.005,
        ge=0,
        description="Noise velocity magnitude [m/s]",
        group="solver",
    )
    rnd_seed: int | None = FdsField(
        None, description="Random number seed for reproducibility", group="solver"
    )

    # ==========================================================================
    # Visibility and Smoke
    # ==========================================================================
    maximum_visibility: float = FdsField(
        30.0,
        exclude_if=30.0,
        gt=0,
        description="Maximum visibility distance [m]",
        group="visibility",
    )
    visibility_factor: float = FdsField(
        3.0, exclude_if=3.0, gt=0, description="Visibility factor", group="visibility"
    )
    smoke_albedo: float = FdsField(
        0.3, exclude_if=0.3, ge=0, le=1, description="Smoke albedo", group="visibility"
    )

    # ==========================================================================
    # Aerosol and Deposition
    # ==========================================================================
    aerosol_al2o3: bool = FdsField(
        False, exclude_if=False, description="Include Al2O3 aerosol", group="deposition"
    )
    aerosol_scrubbing: bool = FdsField(
        False, exclude_if=False, description="Enable aerosol scrubbing", group="deposition"
    )
    agglomeration: bool = FdsField(
        True, exclude_if=True, description="Enable particle agglomeration", group="deposition"
    )
    deposition: bool = FdsField(
        True, exclude_if=True, description="Enable particle deposition", group="deposition"
    )
    gravitational_deposition: bool = FdsField(
        True,
        exclude_if=True,
        description="Enable gravitational deposition",
        group="deposition",
    )
    gravitational_settling: bool = FdsField(
        True, exclude_if=True, description="Enable gravitational settling", group="deposition"
    )
    thermophoretic_deposition: bool = FdsField(
        True,
        exclude_if=True,
        description="Enable thermophoretic deposition",
        group="deposition",
    )
    thermophoretic_settling: bool = FdsField(
        True, exclude_if=True, description="Enable thermophoretic settling", group="deposition"
    )
    turbulent_deposition: bool = FdsField(
        True, exclude_if=True, description="Enable turbulent deposition", group="deposition"
    )
    near_wall_particle_interpolation: bool = FdsField(
        False,
        exclude_if=False,
        description="Use near-wall particle interpolation",
        group="deposition",
    )
    porous_floor: bool = FdsField(
        True,
        exclude_if=True,
        description="Particles removed at floor boundary",
        group="deposition",
    )
    soot_density: float = FdsField(
        1800.0, exclude_if=1800.0, gt=0, description="Soot density [kg/m³]", group="deposition"
    )
    soot_oxidation: bool = FdsField(
        False, exclude_if=False, description="Enable soot oxidation", group="deposition"
    )
    cnf_cutoff: float = FdsField(
        0.005,
        exclude_if=0.005,
        ge=0,
        le=1,
        description="Cumulative number fraction cutoff",
        group="deposition",
    )
    nucleation_sites: float = FdsField(
        1.0e7,
        exclude_if=1.0e7,
        gt=0,
        description="Nucleation sites [#/m³]",
        group="deposition",
    )

    # ==========================================================================
    # HVAC
    # ==========================================================================
    hvac_local_pressure: bool = FdsField(
        True,
        exclude_if=True,
        description="Use local pressure for HVAC nodes",
        group="hvac",
    )
    hvac_pres_relax: float = FdsField(
        1.0,
        exclude_if=1.0,
        ge=0,
        le=1,
        description="HVAC pressure relaxation factor",
        group="hvac",
    )
    hvac_qfan: bool = FdsField(
        False,
        exclude_if=False,
        description="Use quadratic fan curves for HVAC",
        group="hvac",
    )
    hvac_mass_transport_cell_l: float | None = FdsField(
        None, gt=0, description="HVAC mass transport cell length [m]", group="hvac"
    )

    # ==========================================================================
    # Level Set / Wildfire
    # ==========================================================================
    level_set_mode: int = FdsField(
        0, exclude_if=0, ge=0, le=4, description="Level set wildfire mode", group="wildfire"
    )
    level_set_ellipse: bool = FdsField(
        True, exclude_if=True, description="Use elliptical level set", group="wildfire"
    )

    # ==========================================================================
    # Restart
    # ==========================================================================
    restart: bool = FdsField(False, exclude_if=False, description="Enable restart", group="restart")
    restart_chid: str | None = FdsField(None, description="Restart CHID", group="restart")

    # ==========================================================================
    # Output Control
    # ==========================================================================
    bndf_default: bool = FdsField(
        True, exclude_if=True, description="Default BNDF output", group="output"
    )
    iblank_smv: bool = FdsField(
        True, exclude_if=True, description="Write iblank data for Smokeview", group="output"
    )
    overwrite: bool = FdsField(
        True, exclude_if=True, description="Overwrite existing output files", group="output"
    )
    verbose: bool = FdsField(False, exclude_if=False, description="Verbose output", group="output")

    # ==========================================================================
    # Geographic Position
    # ==========================================================================
    origin_lat: float | None = FdsField(
        None, ge=-90, le=90, description="Origin latitude [deg]", group="geo"
    )
    origin_lon: float | None = FdsField(
        None, ge=-180, le=180, description="Origin longitude [deg]", group="geo"
    )
    north_bearing: float | None = FdsField(
        None, ge=0, le=360, description="North bearing angle [deg]", group="geo"
    )

    # ==========================================================================
    # Geometry and Mesh
    # ==========================================================================
    thicken_obstructions: bool = FdsField(
        False,
        exclude_if=False,
        description="Thicken thin obstructions to one cell",
        group="geometry",
    )
    neighbor_separation_distance: float = FdsField(
        0.0,
        exclude_if=0.0,
        ge=0,
        description="Neighbor mesh separation distance [m]",
        group="geometry",
    )
    minimum_zone_volume: float = FdsField(
        0.0,
        exclude_if=0.0,
        ge=0,
        description="Minimum zone volume to fill [m³]",
        group="geometry",
    )
    no_pressure_zones: bool = FdsField(
        False,
        exclude_if=False,
        description="Suppress pressure zones",
        group="geometry",
    )
    max_leak_paths: int = FdsField(
        200, exclude_if=200, ge=1, description="Maximum leak paths/zones", group="geometry"
    )
    texture_origin: tuple[float, float, float] | None = FdsField(
        None, description="Texture origin [m]", group="geometry"
    )

    # ==========================================================================
    # Ramps
    # ==========================================================================
    max_ramps: int = FdsField(
        100, exclude_if=100, ge=1, description="Maximum number of RAMPs", group="ramp"
    )
    tau_default: float = FdsField(
        1.0, exclude_if=1.0, gt=0, description="Default TAU ramp time [s]", group="ramp"
    )
    ramp_gx: str | None = FdsField(None, description="RAMP for gravity X component", group="ramp")
    ramp_gy: str | None = FdsField(None, description="RAMP for gravity Y component", group="ramp")
    ramp_gz: str | None = FdsField(None, description="RAMP for gravity Z component", group="ramp")
    ramp_ux: str | None = FdsField(
        None, description="RAMP for U velocity in X direction", group="ramp"
    )
    ramp_uy: str | None = FdsField(
        None, description="RAMP for U velocity in Y direction", group="ramp"
    )
    ramp_uz: str | None = FdsField(
        None, description="RAMP for U velocity in Z direction", group="ramp"
    )
    ramp_vx: str | None = FdsField(
        None, description="RAMP for V velocity in X direction", group="ramp"
    )
    ramp_vy: str | None = FdsField(
        None, description="RAMP for V velocity in Y direction", group="ramp"
    )
    ramp_vz: str | None = FdsField(
        None, description="RAMP for V velocity in Z direction", group="ramp"
    )
    ramp_wx: str | None = FdsField(
        None, description="RAMP for W velocity in X direction", group="ramp"
    )
    ramp_wy: str | None = FdsField(
        None, description="RAMP for W velocity in Y direction", group="ramp"
    )
    ramp_wz: str | None = FdsField(
        None, description="RAMP for W velocity in Z direction", group="ramp"
    )

    # ==========================================================================
    # Other / Miscellaneous
    # ==========================================================================
    external_filename: str | None = FdsField(
        None, description="External control filename", group="misc"
    )
    mpi_timeout: float = FdsField(
        600.0, exclude_if=600.0, gt=0, description="MPI timeout [s]", group="misc"
    )
    i_max_temp: int = FdsField(
        5000, exclude_if=5000, gt=0, description="Maximum temperature clip [K]", group="misc"
    )
    terrain_image: str | None = FdsField(
        None, description="Terrain image file for wildfire", group="misc"
    )

    # ==========================================================================
    # Validators
    # ==========================================================================
    @field_validator("gvec", mode="before")
    @classmethod
    def validate_gvec(
        cls, v: tuple[float, float, float] | None
    ) -> tuple[float, float, float] | None:
        """Validate gravity vector."""
        if v is None:
            return None
        if len(v) != 3:
            raise ValueError("GVEC must be a 3-element tuple")
        return v

    @field_validator("texture_origin", mode="before")
    @classmethod
    def validate_texture_origin(
        cls, v: tuple[float, float, float] | None
    ) -> tuple[float, float, float] | None:
        """Validate texture origin."""
        if v is None:
            return None
        if len(v) != 3:
            raise ValueError("TEXTURE_ORIGIN must be a 3-element tuple")
        return v

    @model_validator(mode="after")
    def validate_misc(self) -> "Misc":
        """Validate MISC parameters."""
        # Temperature range check
        if not (-273.15 < self.tmpa < 2000):
            raise ValueError(f"TMPA ({self.tmpa}°C) outside reasonable range [-273.15, 2000]")

        # CFL validation
        if self.cfl_min > self.cfl_max:
            raise ValueError(
                f"CFL_MIN ({self.cfl_min}) must be less than or equal to CFL_MAX ({self.cfl_max})"
            )

        # VN validation
        if self.vn_min > self.vn_max:
            raise ValueError(
                f"VN_MIN ({self.vn_min}) must be less than or equal to VN_MAX ({self.vn_max})"
            )

        # Particle CFL validation
        if self.particle_cfl_min > self.particle_cfl_max:
            raise ValueError(
                f"PARTICLE_CFL_MIN ({self.particle_cfl_min}) must be less than or equal to "
                f"PARTICLE_CFL_MAX ({self.particle_cfl_max})"
            )

        # Mode conflicts
        if self.solid_phase_only and self.isothermal:
            raise ValueError("Cannot use both SOLID_PHASE_ONLY and ISOTHERMAL")

        # Freeze/unfreeze consistency
        if self.unfreeze_time is not None and not self.freeze_velocity:
            raise ValueError("UNFREEZE_TIME requires FREEZE_VELOCITY=True")

        return self
