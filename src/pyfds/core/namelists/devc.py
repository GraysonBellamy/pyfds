"""
FDS DEVC namelist.

Measurement devices and sensors.
"""

from pydantic import field_validator, model_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import FdsField, NamelistBase


class Device(NamelistBase):
    """
    FDS DEVC namelist - measurement devices.

    Parameters
    ----------
    id : str
        Unique device identifier
    quantity : str
        FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY')
    xyz : Point3D, optional
        Device location (x, y, z) in meters
    xb : Tuple[float, float, float, float, float, float], optional
        Device bounds for spatial averaging

    Examples
    --------
    >>> from pyfds.core.geometry import Point3D
    >>> temp_sensor = Device(id='TEMP1', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 2.0))
    >>> print(temp_sensor.to_fds())
    &DEVC ID='TEMP1', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.0 /
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "DEVC"

    id: str = FdsField(..., description="Device identifier")
    quantity: str = FdsField(..., description="Quantity to measure")
    xyz: Point3D | None = FdsField(None, description="Device location")
    xb: Bounds3D | None = FdsField(
        None, description="Device bounds (xmin,xmax,ymin,ymax,zmin,zmax)"
    )

    # Control parameters
    setpoint: float | None = FdsField(None, description="Activation setpoint")
    initial_state: bool = FdsField(True, description="Initial state")
    latch: bool = FdsField(True, description="Latch behavior")
    trip_direction: int = FdsField(1, description="Trip direction (-1, 0, 1)")

    # Time control
    delay: float | None = FdsField(None, description="Activation delay [s]")
    time_history: bool = FdsField(False, description="Record time history")

    # Statistics
    statistics: str | None = FdsField(None, description="Statistical operation")
    statistics_start: float | None = FdsField(None, description="Statistics start time [s]")
    temporal_statistic: str | None = FdsField(None, description="Temporal statistic")
    spatial_statistic: str | None = FdsField(None, description="Spatial statistic")

    # Device relationships
    prop_id: str | None = FdsField(None, description="Property ID")
    ctrl_id: str | None = FdsField(None, description="Control ID")
    devc_id: str | None = FdsField(None, description="Device ID")
    input_id: str | None = FdsField(None, description="Input ID")

    # Orientation
    orientation: tuple[float, float, float] | None = FdsField(
        None, description="Orientation vector"
    )
    rotation: float | None = FdsField(None, description="Rotation angle [°]")

    # Output control
    hide_coordinates: bool = FdsField(False, description="Hide coordinates in output")
    no_update_devc_id: bool = FdsField(False, description="Don't update device ID")

    @field_validator("trip_direction")
    @classmethod
    def validate_trip_direction(cls, v: int) -> int:
        """Validate trip direction is -1, 0, or 1."""
        if v not in [-1, 0, 1]:
            raise ValueError("TRIP_DIRECTION must be -1 (below), 0 (both), or 1 (above)")
        return v

    @field_validator("statistics")
    @classmethod
    def validate_statistics(cls, v: str | None) -> str | None:
        """Validate statistics type."""
        if v is not None:
            valid_stats = ["MIN", "MAX", "MEAN", "RMS", "VARIANCE", "RANGE"]
            if v.upper() not in valid_stats:
                raise ValueError(f"STATISTICS must be one of {valid_stats}, got '{v}'")
            return v.upper()
        return v

    @model_validator(mode="after")
    def validate_control_logic(self) -> "Device":
        """Validate control logic consistency."""
        if self.setpoint is not None and self.quantity is None:
            raise ValueError("QUANTITY required when SETPOINT is specified")
        return self

    @model_validator(mode="after")
    def validate_location(self) -> "Device":
        """Ensure device specifies either XYZ or XB (but not both)."""
        if self.xyz is None and self.xb is None:
            raise ValueError("Either xyz or xb must be specified")
        if self.xyz is not None and self.xb is not None:
            raise ValueError("Cannot specify both xyz and xb")
        return self
