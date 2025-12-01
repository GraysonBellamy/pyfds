"""FDS INIT namelist for initial conditions.

Specifies initial conditions for temperature, species concentrations,
and particle insertion within specified regions of the domain.

Field Groups:
    identification: INIT identifier and multiplier
    geometry: Region bounds (XB), point (XYZ), shape parameters
    conditions: Temperature, HRRPUV, species
    particles: Particle insertion parameters
    ramps: Time ramps for various parameters
    control: Device and control activation
    trees: Tree/vegetation parameters
"""

from pydantic import model_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Initialization"]


class Initialization(NamelistBase):
    """FDS INIT namelist - initial conditions.

    Specifies initial conditions for temperature, species concentrations,
    and particle insertion within specified regions of the domain.

    Parameters
    ----------
    id : str, optional
        INIT identifier.
    xb : Bounds3D, optional
        Region bounds (xmin, xmax, ymin, ymax, zmin, zmax).
    xyz : Point3D, optional
        Point location (x, y, z) for single particle insertion.
    db : str, optional
        Domain bounds shortcut (e.g., 'WHOLE DOMAIN').
    temperature : float, optional
        Initial temperature [°C].
    spec_id : list[str], optional
        Species identifiers.
    mass_fraction : list[float], optional
        Species mass fractions [kg/kg].
    volume_fraction : list[float], optional
        Species volume fractions [mol/mol].
    hrrpuv : float, optional
        Heat release rate per unit volume [kW/m³].
    radiative_fraction : float, optional
        Radiative fraction for HRRPUV (default: 0.0).
    part_id : str, optional
        Particle class ID for particle insertion.
    n_particles : int, optional
        Number of particles to insert.
    n_particles_per_cell : int, optional
        Number of particles per grid cell.
    shape : str, optional
        Shape for particle region ('BLOCK', 'CYLINDER', 'CONE', 'LINE', 'RING').
    cell_centered : bool, optional
        If True, place particles at cell centers.
    uniform : bool, optional
        If True, distribute particles uniformly (for RING shape).

    Examples
    --------
    >>> # Initialize temperature in a region
    >>> init = Initialization(
    ...     xb=Bounds3D.of(0, 10, 0, 10, 0, 0.1),
    ...     temperature=500
    ... )
    >>> # Initialize species concentrations
    >>> init = Initialization(
    ...     xb=Bounds3D.of(0, 10, 0, 10, 0, 3),
    ...     spec_id=['PROPANE'],
    ...     mass_fraction=[0.05]
    ... )
    >>> # Insert particles in a cone (tree shape)
    >>> init = Initialization(
    ...     xyz=Point3D(0, 0, 0),
    ...     part_id='foliage',
    ...     shape='CONE',
    ...     radius=1.0,
    ...     height=2.0,
    ...     n_particles_per_cell=1
    ... )

    See Also
    --------
    Species : Species definitions for initial concentrations.
    Particle : Particle class definitions.
    Misc : Ambient temperature and pressure.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "INIT"

    # --- Identification ---
    id: str | None = FdsField(None, description="INIT identifier", group="identification")
    mult_id: str | None = FdsField(None, description="Multiplier ID", group="identification")

    # --- Geometry / Location ---
    xb: Bounds3D | None = FdsField(
        None,
        description="Initialization bounds (xmin,xmax,ymin,ymax,zmin,zmax) [m]",
        group="geometry",
    )
    xyz: Point3D | None = FdsField(
        None, description="Point location for single particle [m]", group="geometry"
    )
    db: str | None = FdsField(
        None, description="Domain bounds shortcut (e.g., 'WHOLE DOMAIN')", group="geometry"
    )

    # --- Shape Parameters (for particle regions) ---
    shape: str | None = FdsField(
        None,
        description="Particle region shape: BLOCK, CYLINDER, CONE, LINE, RING",
        group="geometry",
    )
    radius: float | None = FdsField(
        None, gt=0, description="Radius for CYLINDER/CONE/RING shapes [m]", group="geometry"
    )
    inner_radius: float | None = FdsField(
        None, ge=0, description="Inner radius for shell shapes [m]", group="geometry"
    )
    height: float | None = FdsField(
        None, gt=0, description="Height for CYLINDER/CONE shapes [m]", group="geometry"
    )

    # --- Offset Parameters (for line of particles) ---
    dx: float | None = FdsField(
        None, description="X offset between particles [m]", group="geometry"
    )
    dy: float | None = FdsField(
        None, description="Y offset between particles [m]", group="geometry"
    )
    dz: float | None = FdsField(
        None, description="Z offset between particles [m]", group="geometry"
    )

    # --- Temperature & Heat Source ---
    temperature: float | None = FdsField(
        None, description="Initial temperature [°C]", group="conditions"
    )
    hrrpuv: float | None = FdsField(
        None, ge=0, description="Heat release rate per unit volume [kW/m³]", group="conditions"
    )
    radiative_fraction: float | None = FdsField(
        None,
        ge=0,
        le=1,
        description="Radiative fraction for HRRPUV (0-1)",
        group="conditions",
    )

    # --- Species ---
    spec_id: list[str] | None = FdsField(None, description="Species IDs", group="conditions")
    mass_fraction: list[float] | None = FdsField(
        None, description="Species mass fractions [kg/kg]", group="conditions"
    )
    volume_fraction: list[float] | None = FdsField(
        None, description="Species volume fractions [mol/mol]", group="conditions"
    )

    # --- Particle Insertion ---
    part_id: str | None = FdsField(None, description="Particle class ID", group="particles")
    n_particles: int | None = FdsField(
        None, ge=0, description="Number of particles to insert", group="particles"
    )
    n_particles_per_cell: int | None = FdsField(
        None, ge=0, description="Number of particles per grid cell", group="particles"
    )
    cell_centered: bool | None = FdsField(
        None, description="Place particles at cell centers", group="particles"
    )
    uniform: bool | None = FdsField(
        None, description="Uniform particle distribution (for RING)", group="particles"
    )
    mass_per_volume: float | None = FdsField(
        None, gt=0, description="Mass of particles per unit volume [kg/m³]", group="particles"
    )
    mass_per_time: float | None = FdsField(
        None, gt=0, description="Mass insertion rate [kg/s]", group="particles"
    )
    particle_weight_factor: float | None = FdsField(
        None, gt=0, description="Particle weight factor", group="particles"
    )
    diameter: float | None = FdsField(
        None, gt=0, description="Droplet diameter [μm]", group="particles"
    )
    dt_insert: float | None = FdsField(
        None, gt=0, description="Time interval for particle insertion [s]", group="particles"
    )
    uvw: tuple[float, float, float] | None = FdsField(
        None, description="Initial particle velocity (u,v,w) [m/s]", group="particles"
    )

    # --- Ramps ---
    ramp_q: str | None = FdsField(
        None, description="RAMP ID for HRRPUV time variation", group="ramps"
    )
    ramp_tmp_z: str | None = FdsField(
        None, description="RAMP ID for temperature z-profile", group="ramps"
    )
    ramp_mf_z: str | None = FdsField(
        None, description="RAMP ID for mass fraction z-profile", group="ramps"
    )
    ramp_vf_z: str | None = FdsField(
        None, description="RAMP ID for volume fraction z-profile", group="ramps"
    )
    ramp_part: str | None = FdsField(
        None, description="RAMP ID for particle mass variation", group="ramps"
    )
    path_ramp: tuple[str, str, str] | None = FdsField(
        None, description="RAMP IDs for particle path (x,y,z)", group="ramps"
    )
    orientation_ramp: tuple[str, str, str] | None = FdsField(
        None, description="RAMP IDs for particle orientation", group="ramps"
    )

    # --- Control / Activation ---
    devc_id: str | None = FdsField(
        None, description="Device ID for activation control", group="control"
    )
    ctrl_id: str | None = FdsField(
        None, description="Control ID for activation control", group="control"
    )

    # --- HVAC ---
    node_id: str | None = FdsField(
        None, description="HVAC node ID for duct initialization", group="hvac"
    )

    # --- Trees / Vegetation ---
    tree_height: float | None = FdsField(None, gt=0, description="Tree height [m]", group="trees")
    crown_base_height: float | None = FdsField(
        None, ge=0, description="Crown base height [m]", group="trees"
    )
    crown_base_width: float | None = FdsField(
        None, gt=0, description="Crown base width [m]", group="trees"
    )
    dry: bool | None = FdsField(None, description="Pine needles dry flag", group="trees")

    # --- Bulk Density (for vegetation/porous media) ---
    bulk_density_file: str | None = FdsField(
        None, description="Bulk density data file", group="trees"
    )
    bulk_density_factor: float | None = FdsField(
        None, gt=0, description="Bulk density scaling factor", group="trees"
    )

    @model_validator(mode="after")
    def validate_init(self) -> "Initialization":
        """Validate initial conditions."""
        # Must specify location: XB, XYZ, or DB
        if self.xb is None and self.xyz is None and self.db is None:
            raise ValueError("INIT requires XB, XYZ, or DB to specify location")

        # If species are specified, need concentrations
        if self.spec_id:
            if not (self.mass_fraction or self.volume_fraction):
                raise ValueError("INIT with SPEC_ID requires MASS_FRACTION or VOLUME_FRACTION")
            if self.mass_fraction and len(self.mass_fraction) != len(self.spec_id):
                raise ValueError("MASS_FRACTION length must match SPEC_ID length")
            if self.volume_fraction and len(self.volume_fraction) != len(self.spec_id):
                raise ValueError("VOLUME_FRACTION length must match SPEC_ID length")

        # HRRPUV validation
        if self.radiative_fraction is not None and self.hrrpuv is None:
            raise ValueError("RADIATIVE_FRACTION requires HRRPUV")

        # Shape-specific validation
        if self.shape in ("CYLINDER", "CONE") and self.radius is None:
            raise ValueError(f"SHAPE='{self.shape}' requires RADIUS")
        if self.shape in ("CYLINDER", "CONE") and self.height is None:
            raise ValueError(f"SHAPE='{self.shape}' requires HEIGHT")

        return self
