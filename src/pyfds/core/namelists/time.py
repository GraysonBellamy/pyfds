"""FDS TIME namelist for time control parameters.

The TIME namelist controls simulation timing including start time,
end time, and optional time step parameters.

Field Groups:
    timing: Start time, end time, basic time step control
    time_step_control: Advanced time step control parameters
    external_control: External control parameters
    solid_phase: Solid phase calculation parameters
"""

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Time"]


class Time(NamelistBase):
    """FDS TIME namelist for time control parameters.

    The TIME namelist is required and defines the simulation duration.
    Optional parameters can control time stepping and advanced features.

    Parameters
    ----------
    t_end : float
        End time for the simulation in seconds (default: 1.0).
    t_begin : float, optional
        Start time for output (default: 0.0). Useful for matching
        time lines of experimental data or video recordings.
    dt : float, optional
        Initial time step in seconds. Normally set automatically based
        on mesh cell size and characteristic velocity.
    lock_time_step : bool, optional
        If True, prevents FDS from automatically changing the time step.
        Intended for diagnostic purposes only.
    restrict_time_step : bool, optional
        If True (default), time step is not allowed to increase above
        its initial value.
    limiting_dt_ratio : float, optional
        Minimum ratio of current DT to initial DT before stopping
        (default: 0.0001).
    dt_end_fill : float, optional
        Controls final time step (default: 1.0e-6 s).
    dt_end_minimum : float, optional
        Minimum final time step (default: 2*EPSILON).
    ramp_time : str, optional
        ID of RAMP for specifying time step sequence.
    time_shrink_factor : float, optional
        Factor to reduce specific heats for steady-state applications
        (default: 1.0).
    wall_increment : int, optional
        Frequency of solid phase updates (default: 2, meaning every
        other time step).
    dt_external : float, optional
        Time interval for external control (default: 0.0).
    dt_external_heartbeat : float, optional
        Heartbeat interval for external control (default: 0.0).
    external_heartbeat_filename : str, optional
        Filename for external heartbeat control.
    heartbeat_fail : bool, optional
        If True (default), simulation fails on heartbeat loss.

    Notes
    -----
    FDS automatically adjusts the time step based on CFL conditions.
    The time step is checked at the end of each predictor-corrector
    update and adjusted up or down by 10% if needed.

    Examples
    --------
    >>> time = Time(t_end=600.0)  # 10 minute simulation
    >>> time = Time(t_end=3600.0, t_begin=100.0)  # Start output at 100s

    See Also
    --------
    Mesh : CFL parameters affect time stepping.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "TIME"

    # --- Basic Timing ---
    t_end: float = FdsField(..., gt=0, description="End time [s]", group="timing")
    t_begin: float | None = FdsField(None, ge=0, description="Begin time [s]", group="timing")

    # --- Time Step Control ---
    dt: float | None = FdsField(
        None, gt=0, description="Initial time step [s]", group="time_step_control"
    )
    lock_time_step: bool | None = FdsField(
        None, description="Lock time step (diagnostic use only)", group="time_step_control"
    )
    restrict_time_step: bool | None = FdsField(
        None,
        description="Restrict time step to not exceed initial value",
        group="time_step_control",
    )
    limiting_dt_ratio: float | None = FdsField(
        None,
        gt=0,
        description="Min DT ratio before stopping calculation",
        group="time_step_control",
    )
    dt_end_fill: float | None = FdsField(
        None, gt=0, description="Final time step fill control [s]", group="time_step_control"
    )
    dt_end_minimum: float | None = FdsField(
        None, gt=0, description="Minimum final time step [s]", group="time_step_control"
    )
    ramp_time: str | None = FdsField(
        None, description="RAMP ID for time step sequence", group="time_step_control"
    )

    # --- Steady-State Applications ---
    time_shrink_factor: float | None = FdsField(
        None,
        gt=0,
        description="Factor to reduce specific heats for steady-state",
        group="timing",
    )

    # --- Solid Phase ---
    wall_increment: int | None = FdsField(
        None,
        ge=1,
        description="Solid phase update frequency (time steps)",
        group="solid_phase",
    )

    # --- External Control ---
    dt_external: float | None = FdsField(
        None, ge=0, description="External control time interval [s]", group="external_control"
    )
    dt_external_heartbeat: float | None = FdsField(
        None, ge=0, description="External heartbeat interval [s]", group="external_control"
    )
    external_heartbeat_filename: str | None = FdsField(
        None, description="External heartbeat control filename", group="external_control"
    )
    heartbeat_fail: bool | None = FdsField(
        None, description="Fail simulation on heartbeat loss", group="external_control"
    )
