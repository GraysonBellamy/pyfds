"""FDS SURF namelist for surface properties.

Surface properties for boundaries and materials.

Fields are organized by logical groups (use group= metadata for categorization):
- core: Basic identification and geometry
- appearance: Visual properties (color, texture, transparency)
- burning: Fire and combustion parameters
- flow: Flow and mass transfer properties
- heat_transfer: Convective and radiative heat transfer
- layers: Material layer definitions
- particles: Particle generation and spray properties
- radiation: Radiative properties
- temperature: Temperature boundary conditions
- spyro: SPyro model parameters
- testing: TGA/MCC/DSC analysis parameters
- numerical: Numerical solver parameters
- wall_model: Wall boundary layer parameters
- vegetation: Vegetation/boundary fuel model parameters
- level_set: Level set wildland fire spread parameters
- ember: Ember generation and ignition parameters
- leakage: Pressure zone leakage parameters
"""

from typing import Any

from pydantic import field_validator, model_validator

from pyfds.core.enums import BackingCondition, HeatTransferModel, SolidGeometry
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Surface"]


class Surface(NamelistBase):
    """
    FDS SURF namelist - surface properties.

    All parameters are organized into logical groups via the `group` metadata:
    - core: Basic identification (id, default, area_multiplier)
    - appearance: Visual properties (color, rgb, transparency, texture)
    - burning: Fire behavior (hrrpua, mlrpua, ignition_temperature, burn_away)
    - flow: Mass/volume flow (volume_flow, vel, mass_flow)
    - heat_transfer: Convection/conduction (adiabatic, heat_transfer_coefficient)
    - layers: Material layers (matl_id, thickness, backing)
    - particles: Droplet/particle generation (part_id, nppc, spray_pattern)
    - radiation: Radiative properties (emissivity, absorptivity)
    - temperature: Temperature BCs (tmp_front, tmp_back, ramp_t)
    - spyro: SPyro model parameters
    - testing: TGA/MCC/DSC analysis
    - numerical: Solver parameters

    Parameters
    ----------
    id : str
        Unique surface identifier
    hrrpua : float, optional
        Heat release rate per unit area [kW/m²]
    mlrpua : float, optional
        Mass loss rate per unit area [kg/m²/s]
    color : str, optional
        Named color for visualization
    matl_id : str | list, optional
        Material ID(s) for solid surfaces

    Examples
    --------
    >>> fire_surf = Surface(id='FIRE', hrrpua=1000.0, color='RED')
    >>> print(fire_surf.to_fds())
    &SURF ID='FIRE', HRRPUA=1000.0, COLOR='RED' /

    See Also
    --------
    Material : Material properties for layered surfaces.
    Obstruction : Objects that use surface properties.
    Vent : Boundary conditions that use surface properties.
    Ramp : Time-varying surface properties.
    """

    # =========================================================================
    # CORE PARAMETERS
    # =========================================================================
    id: str = FdsField(..., description="Surface identifier", group="core")
    default: bool = FdsField(False, description="Use as default boundary condition", group="core")
    area_multiplier: float = FdsField(
        1.0, description="Surface area correction factor", group="core"
    )

    # =========================================================================
    # APPEARANCE (from AppearanceMixin)
    # =========================================================================
    rgb: tuple[int, int, int] | None = FdsField(
        None, description="RGB color (0-255)", group="appearance"
    )
    color: str | None = FdsField(
        None, description="Named color (RED, GREEN, BLUE, etc.)", group="appearance"
    )
    transparency: float = FdsField(
        1.0, ge=0, le=1, description="Surface transparency [0-1]", group="appearance"
    )
    texture_map: str | None = FdsField(
        None, description="Texture image filename", group="appearance"
    )
    texture_width: float = FdsField(1.0, description="Texture width (m)", group="appearance")
    texture_height: float = FdsField(1.0, description="Texture height (m)", group="appearance")

    # =========================================================================
    # BURNING (from BurningMixin)
    # =========================================================================
    hrrpua: float | None = FdsField(
        None, description="Heat release rate per unit area [kW/m²]", group="burning"
    )
    mlrpua: float | None = FdsField(
        None, ge=0, description="Mass loss rate per unit area [kg/m²/s]", group="burning"
    )
    mass_flux_total: float | None = FdsField(
        None, description="Total mass flux from surface [kg/m²/s]", group="burning"
    )
    convective_heat_flux: float | None = FdsField(
        None, description="Specified convective heat flux [kW/m²]", group="burning"
    )
    net_heat_flux: float | None = FdsField(
        None, description="Specified net heat flux [kW/m²]", group="burning"
    )
    external_flux: float | None = FdsField(
        None, description="External radiative flux [kW/m²]", group="burning"
    )
    ignition_temperature: float | None = FdsField(
        None, description="Ignition temperature [°C]", group="burning"
    )
    burn_away: bool = FdsField(
        False, description="Remove surface when fuel is consumed", group="burning"
    )
    heat_of_combustion: float | None = FdsField(
        None, ge=0, description="Heat of combustion [kJ/kg]", group="burning"
    )
    ramp_q: str | None = FdsField(None, description="RAMP ID for HRR time history", group="burning")
    ramp_mf: list[str] | str | None = FdsField(
        None, description="RAMP ID(s) for mass flux time history", group="burning"
    )
    tau_q: float | None = FdsField(
        None, ge=0, description="Time constant for t² fire [s]", group="burning"
    )
    tau_mf: list[float] | float | None = FdsField(
        None, description="Time constant(s) for mass flux ramp [s]", group="burning"
    )

    # Fire Spread Parameters
    spread_rate: float | None = FdsField(
        None, description="Radial fire spread rate [m/s]", group="burning"
    )
    xyz: tuple[float, float, float] | None = FdsField(
        None, description="Ignition point (x, y, z) for spreading fire", group="burning"
    )

    # Thermally-Thick Specified Burning
    heat_of_vaporization: float | None = FdsField(
        None, description="Heat of vaporization [kJ/kg]", group="burning"
    )
    extinction_temperature: float | None = FdsField(
        None, description="Extinction temperature [°C]", group="burning"
    )
    burn_duration: float | None = FdsField(
        None, description="Burn duration after ignition [s]", group="burning"
    )

    # =========================================================================
    # FLOW (from FlowMixin)
    # =========================================================================
    volume_flow: float | None = FdsField(None, description="Volume flow rate [m³/s]", group="flow")
    vel: float | None = FdsField(
        None, description="Velocity boundary condition [m/s]", group="flow"
    )
    vel_bulk: float | None = FdsField(
        None, description="Bulk velocity for profiles [m/s]", group="flow"
    )
    vel_grad: float | None = FdsField(None, description="Velocity gradient [1/s]", group="flow")
    vel_t: tuple[float, float] | None = FdsField(
        None, description="Tangential velocity components (v_t1, v_t2) [m/s]", group="flow"
    )
    mass_flow: float | None = FdsField(None, description="Mass flow rate [kg/s]", group="flow")
    mass_transfer_coefficient: float | None = FdsField(
        None, description="Mass transfer coefficient [m/s]", group="flow"
    )
    convert_volume_to_mass: bool = FdsField(
        False, description="Convert volume flux to mass flux", group="flow"
    )
    profile: str | None = FdsField(None, description="Velocity profile type", group="flow")

    # Species Control
    spec_id: list[str] | str | None = FdsField(
        None, description="Species ID(s) for surface emissions", group="flow"
    )
    mass_fraction: list[float] | None = FdsField(
        None, description="Mass fractions for multiple species", group="flow"
    )
    mass_flux: list[float] | None = FdsField(
        None, description="Mass flux per species [kg/(m²·s)]", group="flow"
    )
    mass_flux_var: float | None = FdsField(
        None, description="Mass flux variance parameter", group="flow"
    )

    # Velocity ramps
    ramp_v: str | None = FdsField(
        None, description="RAMP ID for velocity time history", group="flow"
    )
    ramp_v_x: str | None = FdsField(
        None, description="RAMP ID for velocity X-profile", group="flow"
    )
    ramp_v_y: str | None = FdsField(
        None, description="RAMP ID for velocity Y-profile", group="flow"
    )
    ramp_v_z: str | None = FdsField(
        None, description="RAMP ID for velocity Z-profile", group="flow"
    )
    tau_v: float = FdsField(1.0, description="Time constant for velocity ramp [s]", group="flow")

    # =========================================================================
    # HEAT TRANSFER (from HeatTransferMixin)
    # =========================================================================
    adiabatic: bool = FdsField(
        False, description="Adiabatic surface (no heat transfer)", group="heat_transfer"
    )
    heat_transfer_coefficient: float | None = FdsField(
        None, description="Fixed heat transfer coefficient [W/(m²·K)]", group="heat_transfer"
    )
    heat_transfer_coefficient_back: float | None = FdsField(
        None, description="Back side convection coefficient [W/(m²·K)]", group="heat_transfer"
    )
    heat_transfer_model: HeatTransferModel | str | None = FdsField(
        None, description="Heat transfer model: LOGLAW, IMPINGING JET", group="heat_transfer"
    )
    convection_length_scale: float = FdsField(
        1.0, description="Characteristic length for convection [m]", group="heat_transfer"
    )
    ramp_heat_transfer_coefficient: str | None = FdsField(
        None, description="Ramp for heat transfer coefficient", group="heat_transfer"
    )
    ramp_heat_transfer_coefficient_back: str | None = FdsField(
        None, description="Ramp for back HTC", group="heat_transfer"
    )
    blowing: bool = FdsField(
        False, description="Account for mass flux effect on convection", group="heat_transfer"
    )

    # Custom Nusselt correlation: Nu = C0 + C1 * Re^M * Pr^C2
    nusselt_c0: float | None = FdsField(
        None, description="Nusselt correlation C0", group="heat_transfer"
    )
    nusselt_c1: float | None = FdsField(
        None, description="Nusselt correlation C1", group="heat_transfer"
    )
    nusselt_c2: float | None = FdsField(
        None, description="Nusselt correlation C2", group="heat_transfer"
    )
    nusselt_m: float | None = FdsField(
        None, description="Nusselt correlation M (Re exponent)", group="heat_transfer"
    )

    # Impinging jet parameters
    heat_transfer_coefficient_sigma: float | None = FdsField(
        None, description="Impinging jet width [m]", group="heat_transfer"
    )

    # Gas temperatures for convection
    tmp_gas_front: float | None = FdsField(
        None, description="Gas temperature for convective heat transfer [°C]", group="heat_transfer"
    )
    tmp_gas_back: float | None = FdsField(
        None, description="Back gas temperature for convection [°C]", group="heat_transfer"
    )
    ramp_tmp_gas_front: str | None = FdsField(
        None, description="Front gas temp ramp", group="heat_transfer"
    )
    ramp_tmp_gas_back: str | None = FdsField(
        None, description="Back gas temp ramp", group="heat_transfer"
    )

    # =========================================================================
    # LAYERS (from LayersMixin)
    # =========================================================================
    matl_id: list[list[str]] | list[str] | str | None = FdsField(
        None, description="Material ID(s): single, list per layer, or 2D array", group="layers"
    )
    matl_mass_fraction: list[list[float]] | None = FdsField(
        None, description="Mass fractions for multi-component layers", group="layers"
    )
    thickness: list[float] | float | None = FdsField(
        None, description="Layer thickness(es) [m]", group="layers"
    )
    backing: BackingCondition | str | None = FdsField(
        None, description="Backing condition: VOID, INSULATED, or EXPOSED", group="layers"
    )
    delamination_tmp: list[float] | None = FdsField(
        None, description="Temperature threshold for layer delamination [°C]", group="layers"
    )
    delamination_density: list[float] | None = FdsField(
        None, description="Density threshold for layer delamination [kg/m³]", group="layers"
    )
    layer_divide: float | None = FdsField(
        None, description="Layer division for gas transport", group="layers"
    )

    # =========================================================================
    # PARTICLES (from ParticlesMixin)
    # =========================================================================
    allow_surface_particles: bool = FdsField(
        True, description="Allow particles to deposit on surface", group="particles"
    )
    allow_underside_particles: bool = FdsField(
        False, description="Allow particles to deposit on underside", group="particles"
    )
    part_id: str | None = FdsField(
        None, description="Particle class ID to generate", group="particles"
    )
    particle_mass_flux: float | None = FdsField(
        None, description="Particle mass flux [kg/s/m²]", group="particles"
    )
    particle_surface_density: float | None = FdsField(
        None, description="Particle surface density [kg/m²]", group="particles"
    )
    particle_extraction_velocity: float | None = FdsField(
        None, description="Velocity for particle extraction [m/s]", group="particles"
    )
    nppc: int = FdsField(1, description="Number of particles per cell", group="particles")
    dt_insert: float = FdsField(
        0.01, description="Particle insertion time interval [s]", group="particles"
    )
    ramp_part: str | None = FdsField(
        None, description="RAMP ID for particle flux time history", group="particles"
    )
    tau_part: float = FdsField(
        1.0, description="Time constant for particle flux ramp [s]", group="particles"
    )

    # Particle Velocity
    vel_part: float | None = FdsField(
        None, description="Particle velocity magnitude [m/s]", group="particles"
    )

    # =========================================================================
    # RADIATION (from RadiationMixin)
    # =========================================================================
    emissivity: float | None = FdsField(
        None, ge=0, le=1, description="Surface emissivity", group="radiation"
    )
    absorptivity: float | None = FdsField(
        None, ge=0, le=1, description="Surface absorptivity", group="radiation"
    )
    emissivity_back: float | None = FdsField(
        None, description="Back surface emissivity", group="radiation"
    )

    # =========================================================================
    # TEMPERATURE (from TemperatureMixin)
    # =========================================================================
    tmp_front: float | None = FdsField(
        None, description="Front surface temperature [°C]", group="temperature"
    )
    tmp_back: float | None = FdsField(
        None, description="Fixed back surface temperature [°C]", group="temperature"
    )
    tmp_front_initial: float | None = FdsField(
        None, description="Initial front surface temperature [°C]", group="temperature"
    )
    tmp_inner: float | None = FdsField(
        None, description="Initial solid interior temperature [°C]", group="temperature"
    )
    ramp_t: str | None = FdsField(None, description="Temperature ramp ID", group="temperature")
    ramp_tmp_back: str | None = FdsField(
        None, description="Back temperature ramp", group="temperature"
    )
    ramp_t_i: str | None = FdsField(
        None, description="Initial temperature profile ramp", group="temperature"
    )
    tau_t: float = FdsField(
        1.0, description="Time constant for temperature ramp [s]", group="temperature"
    )

    # =========================================================================
    # SPYRO MODEL PARAMETERS
    # =========================================================================
    inert_q_ref: bool = FdsField(
        False, description="Test data from inert pyrolysis (no combustion)", group="spyro"
    )
    reference_heat_flux: list[float] | float | None = FdsField(
        None, description="Reference heat flux from test device [kW/m²]", group="spyro"
    )
    reference_thickness: list[float] | float | None = FdsField(
        None, description="Sample thickness in experiment [m]", group="spyro"
    )
    maximum_scaling_heat_flux: float = FdsField(
        1500.0, description="Upper limit on scaling heat flux [kW/m²]", group="spyro"
    )
    minimum_scaling_heat_flux: float = FdsField(
        0.0, description="Lower limit on scaling heat flux [kW/m²]", group="spyro"
    )
    reference_heat_flux_time_interval: float = FdsField(
        1.0, description="Smoothing window for heat flux [s]", group="spyro"
    )
    ramp_ef: str | None = FdsField(
        None, description="RAMP ID for external flux time history", group="spyro"
    )
    tau_ef: float | None = FdsField(
        None, description="Time constant for external flux ramp-up [s]", group="spyro"
    )

    # =========================================================================
    # TESTING PARAMETERS (TGA/MCC/DSC)
    # =========================================================================
    tga_analysis: bool = FdsField(False, description="Enable TGA analysis mode", group="testing")
    tga_heating_rate: float = FdsField(5.0, description="TGA heating rate [K/min]", group="testing")
    tga_final_temperature: float = FdsField(
        800.0, description="TGA final temperature [°C]", group="testing"
    )
    tga_dt: float | None = FdsField(None, description="TGA timestep [s]", group="testing")
    tga_dump: float | None = FdsField(None, description="TGA output spacing [°C]", group="testing")
    tga_conversion_factor: float = FdsField(
        1.0, description="TGA output multiplier", group="testing"
    )
    mcc_conversion_factor: float = FdsField(
        1.0, description="MCC output multiplier", group="testing"
    )
    dsc_conversion_factor: float = FdsField(
        1.0, description="DSC output multiplier", group="testing"
    )

    # =========================================================================
    # SOLID PHASE GEOMETRY
    # =========================================================================
    geometry: SolidGeometry | str | None = FdsField(
        None,
        description="Solid geometry: CARTESIAN, CYLINDRICAL, SPHERICAL, INNER CYLINDRICAL",
        group="geometry",
    )
    inner_radius: float | None = FdsField(
        None, description="Inner radius for hollow cylinder [m]", group="geometry"
    )
    length: float | None = FdsField(
        None, description="Cylinder/particle length [m]", group="geometry"
    )
    radius: float | None = FdsField(
        None, description="Cylinder/particle radius [m]", group="geometry"
    )
    width: float | None = FdsField(None, description="Particle width [m]", group="geometry")
    horizontal: bool = FdsField(
        False, description="Horizontal cylinder orientation", group="geometry"
    )

    # 3D Heat Conduction
    ht3d: bool = FdsField(False, description="Enable 3-D heat conduction", group="geometry")
    variable_thickness: bool = FdsField(
        False, description="Variable thickness 1-D mode", group="geometry"
    )

    # =========================================================================
    # NUMERICAL PARAMETERS
    # =========================================================================
    stretch_factor: list[float] | float | None = FdsField(
        None, description="Node spacing stretch factor per layer", group="numerical"
    )
    cell_size_factor: list[float] | float | None = FdsField(
        None, description="Cell size multiplier per layer", group="numerical"
    )
    cell_size: list[float] | float | None = FdsField(
        None, description="Explicit cell size per layer [m]", group="numerical"
    )
    n_layer_cells_max: list[int] | int | None = FdsField(
        None, description="Maximum cells per layer", group="numerical"
    )
    time_step_factor: float = FdsField(
        10.0, description="Maximum time step subdivision factor", group="numerical"
    )
    delta_tmp_max: float = FdsField(
        10.0, description="Maximum temperature change per step [°C]", group="numerical"
    )
    minimum_layer_thickness: list[float] | float | None = FdsField(
        None, description="Minimum layer thickness [m]", group="numerical"
    )
    minimum_layer_mass_fraction: list[float] | float | None = FdsField(
        None, description="Minimum layer mass fraction", group="numerical"
    )
    remesh_ratio: float = FdsField(
        0.15, description="Trigger ratio for remeshing", group="numerical"
    )
    internal_heat_source: list[float] | float | None = FdsField(
        None, description="Internal heat source per layer [kW/m³]", group="numerical"
    )
    ramp_ihs: list[str] | str | None = FdsField(
        None, description="Ramp for internal heat source per layer", group="numerical"
    )

    # =========================================================================
    # WALL MODEL PARAMETERS
    # =========================================================================
    free_slip: bool = FdsField(
        False, description="Free-slip wall boundary condition", group="wall_model"
    )
    no_slip: bool = FdsField(
        False, description="No-slip wall boundary condition", group="wall_model"
    )
    roughness: float = FdsField(0.0, description="Surface roughness height [m]", group="wall_model")
    near_wall_eddy_viscosity: float | None = FdsField(
        None, description="Near-wall eddy viscosity [m²/s]", group="wall_model"
    )
    near_wall_turbulence_model: str | None = FdsField(
        None, description="Near-wall turbulence model", group="wall_model"
    )

    # =========================================================================
    # SUPPRESSION PARAMETERS
    # =========================================================================
    e_coefficient: float | None = FdsField(
        None, description="Suppression effectiveness coefficient [m²/(kg·s)]", group="suppression"
    )

    # =========================================================================
    # STRATIFICATION/WIND PARAMETERS
    # =========================================================================
    ple: float = FdsField(
        0.3, description="Power law exponent for stratification", group="stratification"
    )
    z0: float = FdsField(
        10.0, description="Reference height for stratification [m]", group="stratification"
    )
    z_0: float = FdsField(
        0.0, description="Aerodynamic roughness length [m]", group="stratification"
    )

    # =========================================================================
    # VEGETATION / BOUNDARY FUEL MODEL PARAMETERS
    # =========================================================================
    drag_coefficient: float = FdsField(
        2.8, description="Drag coefficient for vegetation", group="vegetation"
    )
    shape_factor: float = FdsField(
        0.25, description="Shape factor for cylindrical fuel elements", group="vegetation"
    )
    mass_per_volume: list[float] | None = FdsField(
        None, description="Fuel mass per volume [kg/m³]", group="vegetation"
    )
    surface_volume_ratio: list[float] | None = FdsField(
        None, description="Surface to volume ratio [1/m]", group="vegetation"
    )
    moisture_content: list[float] | None = FdsField(
        None, description="Fuel moisture content (dry mass fraction)", group="vegetation"
    )
    minimum_burnout_time: float = FdsField(
        1000000.0, description="Minimum burnout time for vegetation [s]", group="vegetation"
    )
    init_ids: list[str] | None = FdsField(
        None, description="INIT IDs for vegetation particles", group="vegetation"
    )
    init_per_area: float | None = FdsField(
        None, description="INIT particles per unit area [1/m²]", group="vegetation"
    )

    # =========================================================================
    # EMBER GENERATION PARAMETERS
    # =========================================================================
    ember_generation_height: tuple[float, float] | None = FdsField(
        None, description="Height range for ember generation [m]", group="ember"
    )
    ember_ignition_power_mean: float | None = FdsField(
        None, description="Mean ignition power for embers [kW]", group="ember"
    )
    ember_ignition_power_sigma: float = FdsField(
        0.001, description="Standard deviation of ignition power [kW]", group="ember"
    )
    ember_tracking_ratio: float = FdsField(
        100.0, description="Ratio of embers tracked to generated", group="ember"
    )
    ember_yield: float | None = FdsField(
        None, description="Mass of embers per fuel mass [kg/kg]", group="ember"
    )

    # =========================================================================
    # LEAKAGE PARAMETERS
    # =========================================================================
    leak_path: tuple[int, int] | None = FdsField(
        None, description="Pressure zone IDs for leakage path", group="leakage"
    )
    leak_path_id: tuple[str, str] | None = FdsField(
        None, description="Pressure zone names for leakage path", group="leakage"
    )

    # =========================================================================
    # HVAC PARAMETERS
    # =========================================================================
    node_id: str | None = FdsField(
        None, description="HVAC node ID connected to surface", group="hvac"
    )

    # =========================================================================
    # LEVEL SET WILDLAND FIRE PARAMETERS
    # =========================================================================
    veg_lset_beta: float = FdsField(
        0.01, description="Packing ratio for level set fire", group="level_set"
    )
    veg_lset_char_fraction: float = FdsField(
        0.2, description="Char fraction for level set fire", group="level_set"
    )
    veg_lset_firebase_time: float | None = FdsField(
        None, description="Firebase duration time [s]", group="level_set"
    )
    veg_lset_fuel_index: int | None = FdsField(
        None, description="Fuel model index for level set fire", group="level_set"
    )
    veg_lset_ht: float = FdsField(
        0.2, description="Fuel height for level set fire [m]", group="level_set"
    )
    veg_lset_ignite_time: float | None = FdsField(
        None, description="Ignition time for level set fire [s]", group="level_set"
    )
    veg_lset_m1: float = FdsField(
        0.03, description="1-hour fuel moisture content", group="level_set"
    )
    veg_lset_m10: float = FdsField(
        0.04, description="10-hour fuel moisture content", group="level_set"
    )
    veg_lset_m100: float = FdsField(
        0.05, description="100-hour fuel moisture content", group="level_set"
    )
    veg_lset_mlw: float = FdsField(
        0.70, description="Live woody fuel moisture content", group="level_set"
    )
    veg_lset_mlh: float = FdsField(
        0.70, description="Live herbaceous fuel moisture content", group="level_set"
    )
    veg_lset_qcon: float = FdsField(
        0.0, description="Convective heat flux for level set [kW/m²]", group="level_set"
    )
    veg_lset_ros_00: float = FdsField(
        0.0, description="Zero-wind rate of spread [m/s]", group="level_set"
    )
    veg_lset_ros_back: float = FdsField(
        0.0, description="Backing rate of spread [m/s]", group="level_set"
    )
    veg_lset_ros_flank: float = FdsField(
        0.0, description="Flanking rate of spread [m/s]", group="level_set"
    )
    veg_lset_ros_head: float = FdsField(
        0.0, description="Heading rate of spread [m/s]", group="level_set"
    )
    veg_lset_ros_fixed: bool = FdsField(
        False, description="Use fixed rate of spread", group="level_set"
    )
    veg_lset_sigma: float = FdsField(
        5000.0, description="Surface-to-volume ratio [1/m]", group="level_set"
    )
    veg_lset_surf_load: float = FdsField(
        1.0, description="Surface fuel load [kg/m²]", group="level_set"
    )
    veg_lset_tan2: float | None = FdsField(
        None, description="Slope factor (tan² of slope angle)", group="level_set"
    )
    veg_lset_wind_exp: float = FdsField(1.0, description="Wind speed exponent", group="level_set")
    veg_lset_wind_height: float = FdsField(
        6.1, description="Wind measurement height [m]", group="level_set"
    )
    veg_lset_wind_ramp: str | None = FdsField(
        None, description="RAMP ID for wind time history", group="level_set"
    )

    # =========================================================================
    # VALIDATORS
    # =========================================================================
    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB values are in 0-255 range."""
        if v is not None:
            for i, val in enumerate(v):
                if not 0 <= val <= 255:
                    raise ValueError(f"RGB[{i}] = {val} must be in range 0-255")
        return v

    @field_validator("tmp_front")
    @classmethod
    def validate_tmp_front(cls, v: float | None) -> float | None:
        if v is not None and v < 0:
            raise ValueError("TMP_FRONT must be non-negative")
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

    @field_validator("profile")
    @classmethod
    def validate_profile(cls, v: str | None) -> str | None:
        """Validate velocity profile type."""
        if v is not None:
            valid = ["PARABOLIC", "ATMOSPHERIC", "RAMP"]
            if v.upper() not in valid:
                raise ValueError(f"PROFILE must be one of {valid}, got '{v}'")
            return v.upper()
        return v

    @field_validator("near_wall_turbulence_model")
    @classmethod
    def validate_near_wall_turbulence_model(cls, v: str | None) -> str | None:
        """Validate near-wall turbulence model."""
        if v is not None:
            valid = ["WALE", "DEARDORFF", "VREMAN", "CONSTANT SMAGORINSKY", "DYNAMIC SMAGORINSKY"]
            if v.upper() not in valid:
                raise ValueError(f"NEAR_WALL_TURBULENCE_MODEL must be one of {valid}, got '{v}'")
            return v.upper()
        return v

    @field_validator("tau_mf")
    @classmethod
    def validate_tau_mf(cls, v: list[float] | float | None) -> list[float] | float | None:
        """Validate tau_mf values are non-negative."""
        if v is not None:
            if isinstance(v, list):
                if any(val < 0 for val in v):
                    raise ValueError("All TAU_MF values must be >= 0")
            elif v < 0:
                raise ValueError("TAU_MF must be >= 0")
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

    def _format_multi_layer_params(self) -> dict[str, Any]:
        """Format multi-layer material parameters for FDS output."""
        params: dict[str, Any] = {}

        if self.matl_id:
            if isinstance(self.matl_id, str):
                # Single material
                params["MATL_ID"] = self._format_value(self.matl_id)
            elif isinstance(self.matl_id, list):
                if all(isinstance(layer, str) for layer in self.matl_id):
                    # List of materials per layer - use indexed format MATL_ID(layer_index)
                    for i, matl in enumerate(self.matl_id):
                        params[f"MATL_ID({i + 1})"] = self._format_value(matl)
                elif all(isinstance(layer, list) for layer in self.matl_id):
                    # 2D array: layer x components
                    for i, layer in enumerate(self.matl_id):
                        if layer:  # Non-empty layer
                            for j, matl in enumerate(layer):
                                params[f"MATL_ID({i + 1},{j + 1})"] = self._format_value(matl)

        if self.matl_mass_fraction:
            # 2D array for mass fractions
            for i, layer in enumerate(self.matl_mass_fraction):  # type: ignore
                layer: list[float]  # type: ignore
                if layer:  # Non-empty layer
                    for j, frac in enumerate(layer):
                        frac: float  # type: ignore
                        params[f"MATL_MASS_FRACTION({i + 1},{j + 1})"] = frac
        if self.thickness is not None:
            if isinstance(self.thickness, (int, float)):
                params["THICKNESS"] = self._format_value(self.thickness)
            elif isinstance(self.thickness, list):
                # Use indexed format for multi-layer
                for i, t in enumerate(self.thickness):
                    params[f"THICKNESS({i + 1})"] = self._format_value(t)

        return params

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "SURF"

    def _collect_fds_params(self) -> dict[str, Any]:
        """
        Collect FDS parameters using metadata-driven approach with special multi-layer formatting.

        Returns
        -------
        dict[str, Any]
            Dictionary of parameter name-value pairs
        """
        # Use base class metadata-driven collection
        params = super()._collect_fds_params()

        # Handle multi-layer material parameters with special formatting
        # Remove any multi-layer FdsFields that were handled by base class
        multi_layer_params = self._format_multi_layer_params()
        params.update(multi_layer_params)

        return params
