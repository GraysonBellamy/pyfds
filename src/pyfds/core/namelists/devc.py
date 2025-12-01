"""FDS DEVC namelist for measurement devices and sensors.

Devices are used to measure quantities at points or volumes in the domain,
and can trigger control actions based on setpoints.

Field Groups:
    identification: Device ID and references
    measurement: Quantity, species, and statistics
    geometry: Location (XYZ), bounds (XB), orientation
    control: Setpoint, activation, and latching
    output: Output timing, units, and formatting
    hvac: HVAC-related device parameters
    line_file: Line output file parameters
"""

from pydantic import field_validator, model_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Device"]


class Device(NamelistBase):
    """FDS DEVC namelist for measurement devices.

    Devices measure quantities like temperature, velocity, and heat flux
    at specified locations. They can also trigger control actions.

    Parameters
    ----------
    id : str
        Unique device identifier.
    quantity : str
        FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY').
    xyz : Point3D, optional
        Device location (x, y, z) in meters.
    xb : Bounds3D, optional
        Device bounds for spatial averaging.
    setpoint : float, optional
        Value at which device triggers/trips.
    prop_id : str, optional
        Property ID for special devices (sprinklers, detectors).

    Notes
    -----
    Either xyz or xb must be specified, but not both (unless using line output).
    For control purposes, use setpoint and trip_direction.

    Examples
    --------
    >>> from pyfds.core.geometry import Point3D
    >>> temp_sensor = Device(
    ...     id='TEMP1',
    ...     quantity='TEMPERATURE',
    ...     xyz=Point3D.of(2.5, 2.5, 2.0)
    ... )
    >>> # Sprinkler with control
    >>> sprinkler = Device(
    ...     id='SPR1',
    ...     xyz=Point3D.of(3.0, 5.6, 2.3),
    ...     prop_id='K-11',
    ...     setpoint=74.0
    ... )

    See Also
    --------
    Control : Control logic using device outputs.
    Property : Device properties for sprinklers and detectors.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "DEVC"

    # --- Identification ---
    id: str = FdsField(..., description="Device identifier", group="identification")

    # --- Measurement ---
    quantity: str | None = FdsField(None, description="Quantity to measure", group="measurement")
    quantity2: str | None = FdsField(
        None, description="Second quantity for line file", group="measurement"
    )
    spec_id: str | None = FdsField(
        None, description="Species ID for species quantities", group="measurement"
    )
    reac_id: str | None = FdsField(
        None, description="Reaction ID for reaction quantities", group="measurement"
    )
    matl_id: str | None = FdsField(
        None, description="Material ID for material quantities", group="measurement"
    )
    part_id: str | None = FdsField(
        None, description="Particle ID for particle quantities", group="measurement"
    )

    # --- Geometry / Location ---
    xyz: Point3D | None = FdsField(None, description="Device location [m]", group="geometry")
    xb: Bounds3D | None = FdsField(
        None, description="Device bounds (xmin,xmax,ymin,ymax,zmin,zmax) [m]", group="geometry"
    )
    xbp: Bounds3D | None = FdsField(None, description="Line file bounds [m]", group="geometry")
    db: str | None = FdsField(
        None, description="Domain bounds shortcut (e.g., 'WHOLE DOMAIN')", group="geometry"
    )
    ior: int | None = FdsField(None, description="Index of orientation (-3 to 3)", group="geometry")
    orientation: tuple[float, float, float] | None = FdsField(
        None, description="Orientation vector (default: 0,0,-1)", group="geometry"
    )
    rotation: float | None = FdsField(None, description="Rotation angle [deg]", group="geometry")
    depth: float | None = FdsField(
        None, ge=0, description="Depth for solid-phase measurement [m]", group="geometry"
    )

    # --- Control Parameters ---
    setpoint: float | None = FdsField(None, description="Activation setpoint", group="control")
    initial_state: bool | None = FdsField(None, description="Initial device state", group="control")
    latch: bool | None = FdsField(None, description="Latch behavior", group="control")
    trip_direction: int | None = FdsField(
        None, description="Trip direction (-1=below, 0=both, 1=above)", group="control"
    )
    delay: float | None = FdsField(None, ge=0, description="Activation delay [s]", group="control")
    smoothing_factor: float | None = FdsField(
        None, ge=0, description="Smoothing factor for output", group="control"
    )
    smoothing_time: float | None = FdsField(
        None, ge=0, description="Smoothing time [s]", group="control"
    )

    # --- Device Relationships ---
    prop_id: str | None = FdsField(None, description="Property ID", group="identification")
    ctrl_id: str | None = FdsField(None, description="Control ID", group="identification")
    devc_id: str | None = FdsField(None, description="Reference device ID", group="identification")
    init_id: str | None = FdsField(
        None, description="INIT ID for particle tracking", group="identification"
    )
    move_id: str | None = FdsField(
        None, description="MOVE ID for moving device", group="identification"
    )
    surf_id: str | None = FdsField(
        None, description="Surface ID for statistics", group="identification"
    )
    duct_id: str | None = FdsField(None, description="HVAC duct ID", group="hvac")
    node_id: str | tuple[str, str] | None = FdsField(
        None, description="HVAC node ID (single or pair)", group="hvac"
    )

    # --- Statistics ---
    spatial_statistic: str | None = FdsField(
        None, description="Spatial statistic type", group="statistics"
    )
    temporal_statistic: str | None = FdsField(
        None, description="Temporal statistic type", group="statistics"
    )
    statistics_start: float | None = FdsField(
        None, ge=0, description="Statistics start time [s]", group="statistics"
    )
    statistics_end: float | None = FdsField(
        None, ge=0, description="Statistics end time [s]", group="statistics"
    )
    quantity_range: tuple[float, float] | None = FdsField(
        None, description="Quantity range for statistics (min, max)", group="statistics"
    )
    n_intervals: int | None = FdsField(
        None, ge=1, description="Number of intervals for time integral", group="statistics"
    )
    time_period: float | None = FdsField(
        None, gt=0, description="Time period for time integral [s]", group="statistics"
    )

    # --- Output Control ---
    output: bool | None = FdsField(None, description="Include in output file", group="output")
    time_averaged: bool | None = FdsField(None, description="Time-averaged output", group="output")
    time_history: bool | None = FdsField(None, description="Record time history", group="output")
    hide_coordinates: bool | None = FdsField(
        None, description="Hide coordinates in output", group="output"
    )
    units: str | None = FdsField(None, description="Output units label", group="output")
    conversion_factor: float | None = FdsField(
        None, description="Output conversion factor", group="output"
    )
    conversion_addend: float | None = FdsField(
        None, description="Output conversion addend", group="output"
    )
    relative: bool | None = FdsField(
        None, description="Output relative to initial value", group="output"
    )
    absolute_value: bool | None = FdsField(
        None, description="Output absolute value", group="output"
    )

    # --- Freeze/Update Control ---
    no_update_devc_id: str | None = FdsField(
        None, description="Device ID to freeze updates", group="control"
    )
    no_update_ctrl_id: str | None = FdsField(
        None, description="Control ID to freeze updates", group="control"
    )

    # --- Line File Parameters ---
    points: int | None = FdsField(
        None, ge=1, description="Number of points for line output", group="line_file"
    )
    points_array_x: list[float] | None = FdsField(
        None, description="X coordinates for line points [m]", group="line_file"
    )
    points_array_y: list[float] | None = FdsField(
        None, description="Y coordinates for line points [m]", group="line_file"
    )
    points_array_z: list[float] | None = FdsField(
        None, description="Z coordinates for line points [m]", group="line_file"
    )
    dx: float | None = FdsField(None, description="X offset for line [m]", group="line_file")
    dy: float | None = FdsField(None, description="Y offset for line [m]", group="line_file")
    dz: float | None = FdsField(None, description="Z offset for line [m]", group="line_file")
    coord_factor: float | None = FdsField(
        None, description="Coordinate scaling factor", group="line_file"
    )
    x_id: str | None = FdsField(None, description="X coordinate ID for output", group="line_file")
    y_id: str | None = FdsField(None, description="Y coordinate ID for output", group="line_file")
    z_id: str | None = FdsField(None, description="Z coordinate ID for output", group="line_file")
    d_id: str | None = FdsField(None, description="Distance ID for output", group="line_file")
    r_id: str | None = FdsField(None, description="Radius ID for output", group="line_file")
    xyz_units: str | None = FdsField(
        None, description="Coordinate units for output", group="line_file"
    )

    # --- HVAC Parameters ---
    cell_l: float | None = FdsField(None, gt=0, description="HVAC cell length [m]", group="hvac")
    flowrate: float | None = FdsField(
        None, ge=0, description="Aspiration detector flowrate [kg/s]", group="hvac"
    )
    bypass_flowrate: float | None = FdsField(
        None, ge=0, description="Aspiration detector bypass flowrate [kg/s]", group="hvac"
    )

    # --- Special Parameters ---
    dry: bool | None = FdsField(
        None, description="Dry measurement (no water vapor)", group="measurement"
    )
    velo_index: int | None = FdsField(
        None, description="Velocity component index", group="measurement"
    )
    pipe_index: int | None = FdsField(
        None, ge=1, description="Pipe index for sprinkler pressure", group="control"
    )
    lp_tag: int | None = FdsField(None, description="Lagrangian particle tag", group="measurement")
    force_direction: tuple[float, float, float] | None = FdsField(
        None, description="Force direction vector", group="measurement"
    )

    # --- Validators ---
    @field_validator("trip_direction")
    @classmethod
    def validate_trip_direction(cls, v: int | None) -> int | None:
        """Validate trip direction is -1, 0, or 1."""
        if v is not None and v not in [-1, 0, 1]:
            raise ValueError("TRIP_DIRECTION must be -1 (below), 0 (both), or 1 (above)")
        return v

    @field_validator("ior")
    @classmethod
    def validate_ior(cls, v: int | None) -> int | None:
        """Validate IOR is in valid range."""
        if v is not None and v not in [-3, -2, -1, 1, 2, 3]:
            raise ValueError("IOR must be -3, -2, -1, 1, 2, or 3")
        return v

    @model_validator(mode="after")
    def validate_device(self) -> "Device":
        """Validate device configuration."""
        # For most devices, need at least one location specification
        # Exception: devices linked to INIT_ID or with DUCT_ID/NODE_ID for HVAC
        has_location = (
            self.xyz is not None
            or self.xb is not None
            or self.db is not None
            or self.xbp is not None
            or self.init_id is not None
            or self.duct_id is not None
            or self.node_id is not None
        )
        if not has_location:
            raise ValueError("Device requires XYZ, XB, XBP, DB, INIT_ID, DUCT_ID, or NODE_ID")

        # Cannot have both xyz and xb for single point devices
        if self.xyz is not None and self.xb is not None and self.xbp is None:
            raise ValueError("Cannot specify both XYZ and XB for a single device")

        return self
