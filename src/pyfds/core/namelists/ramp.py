"""FDS RAMP namelist for time-dependent functions.

Represents time-dependent or temperature-dependent functions for use with
various FDS parameters.

Field Groups:
    identification: Ramp ID and control references
    data: (T, F) point data with optional X/Z coordinates
    external_control: External file control parameters
    interpolation: Interpolation settings
"""

from typing import Any, cast

import numpy as np
from pydantic import model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Ramp"]


class Ramp(NamelistBase):
    """FDS RAMP namelist - time-dependent functions.

    Represents time-dependent or temperature-dependent functions for use with
    various FDS parameters. RAMPs are defined by a series of (T, F) points
    where T can represent time or temperature, and F is the function value.

    Parameters
    ----------
    id : str
        Unique ramp identifier.
    points : list[tuple[float, float]]
        List of (T, F) points defining the ramp function.
    ctrl_id : str, optional
        Control ID to use as independent variable instead of time.
    ctrl_id_dep : str, optional
        Control ID whose output is used as the RAMP output (dependent variable).
    devc_id : str, optional
        Device ID to use as independent variable instead of time.
    devc_id_dep : str, optional
        Device ID whose output is used as the RAMP output (dependent variable).
    external_file : bool, optional
        If True, RAMP value is controlled by external file.
    initial_value : float, optional
        Initial value when using external file control.
    number_interpolation_points : int, optional
        Number of interpolation points (default: 5000).
    x_values : list[float], optional
        X position values for spatially-varying ramps (e.g., gravity in tunnels).
    z_values : list[float], optional
        Z height values for height-varying ramps (e.g., wind profiles).

    Notes
    -----
    - Points are automatically sorted by T value.
    - At least 2 points are required for standard ramps.
    - FDS uses linear interpolation between points.
    - The independent variable can be time (T), position (X), or height (Z).
    - CTRL_ID or DEVC_ID can replace time as the independent variable.
    - CTRL_ID_DEP or DEVC_ID_DEP replaces the RAMP output with device/control output.

    Examples
    --------
    >>> # Standard time-based ramp
    >>> ramp = Ramp(id='FIRE_RAMP', points=[(0, 0), (300, 1000)])

    >>> # Device-controlled ramp (temperature controls blower)
    >>> ramp = Ramp(id='BLOWER_RAMP', points=[(20, 0), (100, 0.5), (200, 1.0)],
    ...             devc_id='TEMP_SENSOR')

    >>> # Height-varying wind profile
    >>> ramp = Ramp(id='WIND_PROFILE', points=[(0, 1.0), (200, 1.5), (500, 1.8)],
    ...             z_values=[0, 200, 500])

    See Also
    --------
    Surface : Uses RAMP for time-varying HRR and temperature.
    Material : Uses RAMP for temperature-dependent properties.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "RAMP"

    # --- Identification ---
    id: str = FdsField(
        ...,
        description="Ramp identifier",
        group="identification",
    )

    ctrl_id: str | None = FdsField(
        None,
        fds_name="CTRL_ID",
        description="Control ID to use as independent variable instead of time",
        group="identification",
    )

    ctrl_id_dep: str | None = FdsField(
        None,
        fds_name="CTRL_ID_DEP",
        description="Control ID whose output is used as the RAMP output",
        group="identification",
    )

    devc_id: str | None = FdsField(
        None,
        fds_name="DEVC_ID",
        description="Device ID to use as independent variable instead of time",
        group="identification",
    )

    devc_id_dep: str | None = FdsField(
        None,
        fds_name="DEVC_ID_DEP",
        description="Device ID whose output is used as the RAMP output",
        group="identification",
    )

    # --- External Control ---
    external_file: bool | None = FdsField(
        None,
        fds_name="EXTERNAL_FILE",
        description="If True, RAMP value is controlled by external file",
        group="external_control",
    )

    initial_value: float | None = FdsField(
        None,
        fds_name="INITIAL_VALUE",
        description="Initial value when using external file control",
        group="external_control",
    )

    # --- Interpolation ---
    number_interpolation_points: int | None = FdsField(
        None,
        fds_name="NUMBER_INTERPOLATION_POINTS",
        description="Number of interpolation points",
        group="interpolation",
    )

    # --- Data ---
    # points is a convenience field that generates T/F pairs
    points: list[tuple[float, float]] = FdsField(
        default_factory=list,
        description="(T, F) points defining the ramp function",
        group="data",
    )

    # X values for spatially-varying ramps (e.g., gravity in tunnels)
    x_values: list[float] | None = FdsField(
        None,
        description="X position values for spatially-varying ramps",
        group="data",
    )

    # Z values for height-varying ramps (e.g., wind profiles)
    z_values: list[float] | None = FdsField(
        None,
        description="Z height values for height-varying ramps",
        group="data",
    )

    @model_validator(mode="after")
    def validate_ramp(self) -> "Ramp":
        """Validate ramp configuration."""
        # Skip point validation if using dependent variable replacement
        if self.ctrl_id_dep is not None or self.devc_id_dep is not None:
            # When using dependent variable, points are optional
            return self

        # Skip point validation if using external file control without points
        if self.external_file is True and len(self.points) == 0:
            return self

        # Standard ramps require at least 2 points
        if len(self.points) < 2:
            raise ValueError(f"Ramp '{self.id}' requires at least 2 points")

        # Sort points by T value
        self.points.sort(key=lambda p: p[0])

        # Check for duplicate T values after sorting
        t_values = [p[0] for p in self.points]
        if len(t_values) != len(set(t_values)):
            raise ValueError(f"Ramp '{self.id}' has duplicate T values")

        # Validate X values if provided
        if self.x_values is not None and len(self.x_values) != len(self.points):
            raise ValueError(
                f"Ramp '{self.id}': x_values length ({len(self.x_values)}) "
                f"must match points length ({len(self.points)})"
            )

        # Validate Z values if provided
        if self.z_values is not None and len(self.z_values) != len(self.points):
            raise ValueError(
                f"Ramp '{self.id}': z_values length ({len(self.z_values)}) "
                f"must match points length ({len(self.points)})"
            )

        return self

    def add_point(self, t: float, f: float) -> "Ramp":
        """Add a point to the ramp function.

        Parameters
        ----------
        t : float
            Time or temperature value.
        f : float
            Function value at this point.

        Returns
        -------
        Ramp
            Self for method chaining.
        """
        self.points.append((t, f))
        # Sort points by T value after adding
        self.points.sort(key=lambda p: p[0])

        # Revalidate after adding point
        t_values = [p[0] for p in self.points]
        if len(t_values) != len(set(t_values)):
            raise ValueError(f"Ramp '{self.id}' has duplicate T values")

        return self

    def evaluate(self, t: float | np.ndarray) -> float | np.ndarray:
        """Evaluate ramp at given time/temperature using linear interpolation.

        Parameters
        ----------
        t : float or np.ndarray
            Time or temperature value(s) to evaluate.

        Returns
        -------
        float or np.ndarray
            Interpolated function value(s).
        """
        if not self.points:
            raise ValueError(f"Ramp '{self.id}' has no points")

        # Ensure points are sorted by T value
        sorted_points = sorted(self.points, key=lambda p: p[0])
        t_values = np.array([p[0] for p in sorted_points])
        f_values = np.array([p[1] for p in sorted_points])

        return cast("float | np.ndarray", np.interp(t, t_values, f_values))

    def to_fds(self) -> str:
        """Generate FDS RAMP namelist strings.

        Returns
        -------
        str
            Multiple RAMP namelist lines, one for each point.

        Notes
        -----
        FDS RAMP uses multiple namelist entries with the same ID.
        Each entry contains T (or X or Z) and F values.
        Additional parameters (CTRL_ID, DEVC_ID, etc.) only need to
        appear on the first line.
        """
        lines = []

        # Handle dependent variable case (no points needed)
        if (self.ctrl_id_dep is not None or self.devc_id_dep is not None) and not self.points:
            params: dict[str, Any] = {"ID": self._format_value(self.id)}
            if self.ctrl_id_dep is not None:
                params["CTRL_ID_DEP"] = self._format_value(self.ctrl_id_dep)
            if self.devc_id_dep is not None:
                params["DEVC_ID_DEP"] = self._format_value(self.devc_id_dep)
            return self._build_namelist("RAMP", params)

        # Handle external file case with no points
        if self.external_file is True and not self.points:
            params = {"ID": self._format_value(self.id), "EXTERNAL_FILE": ".TRUE."}
            if self.initial_value is not None:
                params["INITIAL_VALUE"] = self._format_value(self.initial_value)
            return self._build_namelist("RAMP", params)

        # Standard case: output one line per point
        for i, (t, f) in enumerate(self.points):
            params = {
                "ID": self._format_value(self.id),
                "F": self._format_value(f),
            }

            # Determine independent variable: T, X, or Z
            if self.x_values is not None:
                params["X"] = self._format_value(self.x_values[i])
            elif self.z_values is not None:
                params["Z"] = self._format_value(self.z_values[i])
            else:
                params["T"] = self._format_value(t)

            # Add control/device references on first line only
            if i == 0:
                if self.ctrl_id is not None:
                    params["CTRL_ID"] = self._format_value(self.ctrl_id)
                if self.ctrl_id_dep is not None:
                    params["CTRL_ID_DEP"] = self._format_value(self.ctrl_id_dep)
                if self.devc_id is not None:
                    params["DEVC_ID"] = self._format_value(self.devc_id)
                if self.devc_id_dep is not None:
                    params["DEVC_ID_DEP"] = self._format_value(self.devc_id_dep)
                if self.external_file is not None:
                    params["EXTERNAL_FILE"] = ".TRUE." if self.external_file else ".FALSE."
                if self.initial_value is not None:
                    params["INITIAL_VALUE"] = self._format_value(self.initial_value)
                if self.number_interpolation_points is not None:
                    params["NUMBER_INTERPOLATION_POINTS"] = self._format_value(
                        self.number_interpolation_points
                    )

            lines.append(self._build_namelist("RAMP", params).rstrip("\n"))

        return "\n".join(lines) + "\n"
