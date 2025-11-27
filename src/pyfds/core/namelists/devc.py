"""
FDS DEVC namelist.

Measurement devices and sensors.
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists.base import NamelistBase


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
    >>> temp_sensor = Device(id='TEMP1', quantity='TEMPERATURE', xyz=Point3D(2.5, 2.5, 2.0))
    >>> print(temp_sensor.to_fds())
    &DEVC ID='TEMP1', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.0 /
    """

    id: str = Field(..., description="Device identifier")
    quantity: str = Field(..., description="Quantity to measure")
    xyz: Point3D | None = Field(None, description="Device location")
    xb: Bounds3D | None = Field(None, description="Device bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    prop_id: str | None = Field(None, description="Device property ID (for sprinklers, detectors)")

    # Control Parameters (Stage 1.2)
    setpoint: float | None = Field(None, description="Activation setpoint value")
    initial_state: bool = Field(False, description="Initial state of controlled device")
    latch: bool = Field(True, description="Latch device state after activation")
    trip_direction: int = Field(1, description="Direction to trip: 1=above, -1=below, 0=both")

    # Time Control (Stage 1.2)
    delay: float | None = Field(None, ge=0, description="Activation delay time (s)")
    time_history: bool = Field(False, description="Output time history")

    # Statistics (Stage 1.2)
    statistics: str | None = Field(
        None, description="Statistical operation: MIN, MAX, MEAN, RMS, etc."
    )
    statistics_start: float | None = Field(None, ge=0, description="Start time for statistics (s)")
    temporal_statistic: str | None = Field(None, description="Temporal statistic type")
    spatial_statistic: str | None = Field(None, description="Spatial statistic type")

    # Device Relationships (Stage 1.2)
    ctrl_id: str | None = Field(None, description="Control function ID")
    devc_id: str | None = Field(None, description="Device ID for control logic")
    input_id: list[str] = Field(default_factory=list, description="Input device IDs")

    # Orientation (Stage 1.2)
    orientation: tuple[float, float, float] | None = Field(
        None, description="Device orientation vector"
    )
    rotation: float | None = Field(None, description="Rotation angle (degrees)")

    # Output Control (Stage 1.2)
    hide_coordinates: bool = Field(False, description="Hide coordinates in output")
    no_update_devc_id: str | None = Field(None, description="Device to prevent updates")

    @field_validator("xyz", mode="before")
    @classmethod
    def validate_xyz(cls, v: Any) -> Any:
        if isinstance(v, tuple):
            return Point3D.from_tuple(v)
        return v

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Any:
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        return v

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

    def to_fds(self) -> str:
        """Generate FDS DEVC namelist."""
        params: dict[str, Any] = {"id": self.id, "quantity": self.quantity}
        if self.xyz:
            params["xyz"] = self.xyz.as_tuple()
        if self.xb:
            params["xb"] = self.xb.as_tuple()
        if self.prop_id:
            params["prop_id"] = self.prop_id

        # Control Parameters
        if self.setpoint is not None:
            params["setpoint"] = self.setpoint
        if self.initial_state:
            params["initial_state"] = self.initial_state
        if not self.latch:  # Only output if False (default is True)
            params["latch"] = self.latch
        if self.trip_direction != 1:  # Only output if not default
            params["trip_direction"] = self.trip_direction

        # Time Control
        if self.delay is not None:
            params["delay"] = self.delay
        if self.time_history:
            params["time_history"] = self.time_history

        # Statistics
        if self.statistics:
            params["statistics"] = self.statistics
        if self.statistics_start is not None:
            params["statistics_start"] = self.statistics_start
        if self.temporal_statistic:
            params["temporal_statistic"] = self.temporal_statistic
        if self.spatial_statistic:
            params["spatial_statistic"] = self.spatial_statistic

        # Device Relationships
        if self.ctrl_id:
            params["ctrl_id"] = self.ctrl_id
        if self.devc_id:
            params["devc_id"] = self.devc_id
        if self.input_id:
            params["input_id"] = self.input_id

        # Orientation
        if self.orientation:
            params["orientation"] = self.orientation
        if self.rotation is not None:
            params["rotation"] = self.rotation

        # Output Control
        if self.hide_coordinates:
            params["hide_coordinates"] = self.hide_coordinates
        if self.no_update_devc_id:
            params["no_update_devc_id"] = self.no_update_devc_id

        return self._build_namelist("DEVC", params)
