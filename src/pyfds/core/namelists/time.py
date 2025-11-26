"""
FDS TIME namelist.

Time control parameters for simulation.
"""

from typing import Any

from pydantic import Field

from pyfds.core.namelists.base import NamelistBase


class Time(NamelistBase):
    """
    FDS TIME namelist - time control parameters.

    Parameters
    ----------
    t_end : float
        End time for the simulation in seconds
    t_begin : float, optional
        Start time for output (default: 0.0)
    dt : float, optional
        Initial time step in seconds
    wall_clock_time : float, optional
        Maximum wall clock time in seconds

    Examples
    --------
    >>> time = Time(t_end=600.0, dt=0.1)
    >>> print(time.to_fds())
    &TIME T_END=600.0, DT=0.1 /
    """

    t_end: float = Field(..., gt=0, description="End time (s)")
    t_begin: float | None = Field(None, ge=0, description="Begin time (s)")
    dt: float | None = Field(None, gt=0, description="Time step (s)")
    wall_clock_time: float | None = Field(None, gt=0, description="Wall clock limit (s)")

    def to_fds(self) -> str:
        """Generate FDS TIME namelist."""
        params: dict[str, Any] = {"t_end": self.t_end}
        if self.t_begin is not None:
            params["t_begin"] = self.t_begin
        if self.dt is not None:
            params["dt"] = self.dt
        if self.wall_clock_time is not None:
            params["wall_clock_time"] = self.wall_clock_time
        return self._build_namelist("TIME", params)
