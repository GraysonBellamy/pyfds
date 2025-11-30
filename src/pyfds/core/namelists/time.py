"""
FDS TIME namelist.

Time control parameters for simulation.
"""

from pyfds.core.namelists.base import FdsField, NamelistBase


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

    t_end: float = FdsField(..., gt=0, description="End time (s)")
    t_begin: float | None = FdsField(None, ge=0, description="Begin time (s)")
    dt: float | None = FdsField(None, gt=0, description="Time step (s)")
    wall_clock_time: float | None = FdsField(None, gt=0, description="Wall clock limit (s)")

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "TIME"
