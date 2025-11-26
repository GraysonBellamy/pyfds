"""
FDS RAMP namelist.

Time-dependent functions.
"""

from typing import Any, cast

import numpy as np
from pydantic import Field, model_validator

from pyfds.core.namelists.base import NamelistBase


class Ramp(NamelistBase):
    """
    FDS RAMP namelist - time-dependent functions.

    Represents time-dependent or temperature-dependent functions for use with
    various FDS parameters. RAMPs are defined by a series of (T, F) points
    where T can represent time or temperature, and F is the function value.

    Parameters
    ----------
    id : str
        Unique ramp identifier
    points : list[tuple[float, float]]
        List of (T, F) points defining the ramp function

    Examples
    --------
    >>> # Simple linear ramp from 0 to 1000 over 300 seconds
    >>> ramp = Ramp(id='FIRE_RAMP', points=[(0, 0), (300, 1000)])
    >>> print(ramp.to_fds())
    &RAMP ID='FIRE_RAMP', T=0.0, F=0.0 /
    &RAMP ID='FIRE_RAMP', T=300.0, F=1000.0 /

    Notes
    -----
    - Points are automatically sorted by T value
    - At least 2 points are required
    - FDS uses linear interpolation between points
    """

    id: str = Field(..., description="Ramp identifier")
    points: list[tuple[float, float]] = Field(default_factory=list, description="(T, F) points")

    @model_validator(mode="after")
    def validate_ramp(self) -> "Ramp":
        """Validate ramp points."""
        if len(self.points) < 2:
            raise ValueError(f"Ramp '{self.id}' requires at least 2 points")

        # Sort points by T value (use object.__setattr__ to avoid recursion)
        sorted_points = sorted(self.points, key=lambda p: p[0])
        object.__setattr__(self, "points", sorted_points)

        # Check for duplicate T values
        t_values = [p[0] for p in self.points]
        if len(t_values) != len(set(t_values)):
            raise ValueError(f"Ramp '{self.id}' has duplicate T values")

        return self

    def add_point(self, t: float, f: float) -> "Ramp":
        """
        Add a point to the ramp function.

        Parameters
        ----------
        t : float
            Time or temperature value
        f : float
            Function value at this point

        Returns
        -------
        Ramp
            Self for method chaining
        """
        self.points.append((t, f))
        self.points = sorted(self.points, key=lambda p: p[0])

        # Revalidate after adding point
        t_values = [p[0] for p in self.points]
        if len(t_values) != len(set(t_values)):
            raise ValueError(f"Ramp '{self.id}' has duplicate T values")

        return self

    def evaluate(self, t: float | np.ndarray) -> float | np.ndarray:
        """
        Evaluate ramp at given time/temperature using linear interpolation.

        Parameters
        ----------
        t : float or np.ndarray
            Time or temperature value(s) to evaluate

        Returns
        -------
        float or np.ndarray
            Interpolated function value(s)
        """
        if not self.points:
            raise ValueError(f"Ramp '{self.id}' has no points")

        t_values = np.array([p[0] for p in self.points])
        f_values = np.array([p[1] for p in self.points])

        return cast("float | np.ndarray", np.interp(t, t_values, f_values))

    def to_fds(self) -> str:
        """
        Generate FDS RAMP namelist strings.

        Returns
        -------
        str
            Multiple RAMP namelist lines, one for each point
        """
        lines = []
        for t, f in self.points:
            params: dict[str, Any] = {"id": self.id, "t": t, "f": f}
            lines.append(self._build_namelist("RAMP", params).rstrip("\n"))
        return "\n".join(lines) + "\n"
