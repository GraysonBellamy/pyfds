"""
FDS MULT namelist.

Multiplier for creating arrays of repeated objects.
"""

from typing import Any

from pydantic import Field, model_validator

from pyfds.core.namelists.base import NamelistBase


class Mult(NamelistBase):
    """
    FDS MULT namelist - creates arrays of repeated objects.

    Used with MESH, OBST, VENT, HOLE, and INIT to create regular
    arrays of objects without specifying each one individually.

    Parameters
    ----------
    id : str
        Multiplier identifier (referenced by MULT_ID on other namelists)
    dx, dy, dz : float, optional
        Spacing in each direction [m]
    dx0, dy0, dz0 : float, optional
        Initial offset [m] (default: 0.0)
    i_lower, i_upper : int, optional
        X-direction array bounds (default: 0)
    j_lower, j_upper : int, optional
        Y-direction array bounds (default: 0)
    k_lower, k_upper : int, optional
        Z-direction array bounds (default: 0)
    n_lower, n_upper : int, optional
        Sequential range (alternative to i,j,k bounds)
    i_lower_skip, i_upper_skip : int, optional
        X-direction skip ranges (for creating gaps)
    j_lower_skip, j_upper_skip : int, optional
        Y-direction skip ranges (for creating gaps)
    k_lower_skip, k_upper_skip : int, optional
        Z-direction skip ranges (for creating gaps)
    dxb : tuple[float, float, float, float, float, float], optional
        Incremental XB offsets (xmin, xmax, ymin, ymax, zmin, zmax)

    Examples
    --------
    >>> # 3x3 array of objects spaced 2m apart
    >>> mult = Mult(
    ...     id='ARRAY_3X3',
    ...     dx=2.0, dy=2.0,
    ...     i_lower=0, i_upper=2,
    ...     j_lower=0, j_upper=2
    ... )

    >>> # Linear array of 10 objects
    >>> mult = Mult(
    ...     id='ROW_10',
    ...     dx=1.0,
    ...     n_lower=0, n_upper=9
    ... )

    >>> # Array with gaps (skip indices 2-3)
    >>> mult = Mult(
    ...     id='ARRAY_WITH_GAPS',
    ...     dx=1.0,
    ...     n_lower=0, n_upper=9,
    ...     n_lower_skip=2, n_upper_skip=3
    ... )
    """

    id: str = Field(..., description="Multiplier identifier")

    # Spacing
    dx: float | None = Field(None, description="X spacing [m]")
    dy: float | None = Field(None, description="Y spacing [m]")
    dz: float | None = Field(None, description="Z spacing [m]")

    # Initial offset
    dx0: float = Field(0.0, description="Initial X offset [m]")
    dy0: float = Field(0.0, description="Initial Y offset [m]")
    dz0: float = Field(0.0, description="Initial Z offset [m]")

    # Array bounds (3D mode)
    i_lower: int = Field(0, description="X lower bound")
    i_upper: int = Field(0, description="X upper bound")
    j_lower: int = Field(0, description="Y lower bound")
    j_upper: int = Field(0, description="Y upper bound")
    k_lower: int = Field(0, description="Z lower bound")
    k_upper: int = Field(0, description="Z upper bound")

    # Sequential mode
    n_lower: int | None = Field(None, description="Sequential lower bound")
    n_upper: int | None = Field(None, description="Sequential upper bound")

    # Skip ranges (for creating gaps in arrays)
    i_lower_skip: int | None = Field(None, description="X skip lower")
    i_upper_skip: int | None = Field(None, description="X skip upper")
    j_lower_skip: int | None = Field(None, description="Y skip lower")
    j_upper_skip: int | None = Field(None, description="Y skip upper")
    k_lower_skip: int | None = Field(None, description="Z skip lower")
    k_upper_skip: int | None = Field(None, description="Z skip upper")
    n_lower_skip: int | None = Field(None, description="Sequential skip lower")
    n_upper_skip: int | None = Field(None, description="Sequential skip upper")

    # Incremental XB offsets
    dxb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Incremental XB offsets"
    )

    @model_validator(mode="after")
    def validate_mult(self) -> "Mult":
        """Validate multiplier configuration."""
        # Check that at least one spacing is defined
        if self.dx is None and self.dy is None and self.dz is None and self.dxb is None:
            raise ValueError(f"MULT '{self.id}': Must specify at least one of DX, DY, DZ, or DXB")

        # Check bounds consistency
        if self.i_lower > self.i_upper:
            raise ValueError(f"MULT '{self.id}': I_LOWER > I_UPPER")
        if self.j_lower > self.j_upper:
            raise ValueError(f"MULT '{self.id}': J_LOWER > J_UPPER")
        if self.k_lower > self.k_upper:
            raise ValueError(f"MULT '{self.id}': K_LOWER > K_UPPER")
        if self.n_lower is not None and self.n_upper is not None and self.n_lower > self.n_upper:
            raise ValueError(f"MULT '{self.id}': N_LOWER > N_UPPER")

        return self

    def get_count(self) -> int:
        """Calculate total number of objects this multiplier creates."""
        if self.n_lower is not None and self.n_upper is not None:
            return self.n_upper - self.n_lower + 1

        i_count = self.i_upper - self.i_lower + 1
        j_count = self.j_upper - self.j_lower + 1
        k_count = self.k_upper - self.k_lower + 1

        return i_count * j_count * k_count

    def to_fds(self) -> str:
        """Generate FDS MULT namelist."""
        params: dict[str, Any] = {"id": self.id}

        # Spacing
        if self.dx is not None:
            params["dx"] = self.dx
        if self.dy is not None:
            params["dy"] = self.dy
        if self.dz is not None:
            params["dz"] = self.dz

        # Offsets (only if non-zero)
        if self.dx0 != 0.0:
            params["dx0"] = self.dx0
        if self.dy0 != 0.0:
            params["dy0"] = self.dy0
        if self.dz0 != 0.0:
            params["dz0"] = self.dz0

        # Array bounds
        if self.n_lower is not None and self.n_upper is not None:
            params["n_lower"] = self.n_lower
            params["n_upper"] = self.n_upper
        else:
            if self.i_upper > 0 or self.i_lower != 0:
                params["i_lower"] = self.i_lower
                params["i_upper"] = self.i_upper
            if self.j_upper > 0 or self.j_lower != 0:
                params["j_lower"] = self.j_lower
                params["j_upper"] = self.j_upper
            if self.k_upper > 0 or self.k_lower != 0:
                params["k_lower"] = self.k_lower
                params["k_upper"] = self.k_upper

        # Skip ranges
        if self.i_lower_skip is not None:
            params["i_lower_skip"] = self.i_lower_skip
        if self.i_upper_skip is not None:
            params["i_upper_skip"] = self.i_upper_skip
        if self.j_lower_skip is not None:
            params["j_lower_skip"] = self.j_lower_skip
        if self.j_upper_skip is not None:
            params["j_upper_skip"] = self.j_upper_skip
        if self.k_lower_skip is not None:
            params["k_lower_skip"] = self.k_lower_skip
        if self.k_upper_skip is not None:
            params["k_upper_skip"] = self.k_upper_skip
        if self.n_lower_skip is not None:
            params["n_lower_skip"] = self.n_lower_skip
        if self.n_upper_skip is not None:
            params["n_upper_skip"] = self.n_upper_skip

        # DXB
        if self.dxb is not None:
            params["dxb"] = self.dxb

        return self._build_namelist("MULT", params)
