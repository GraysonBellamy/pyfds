"""FDS MULT namelist for creating arrays of repeated objects.

Used with MESH, OBST, VENT, HOLE, and INIT to create regular
arrays of objects without specifying each one individually.

Field Groups:
    identification: Multiplier ID
    spacing: X, Y, Z spacing distances
    offset: Initial offset values
    bounds: I, J, K array bounds
    sequential: N-based sequential mode
    skip: Skip ranges for gaps
    xb_offset: Incremental XB offsets
"""

from pydantic import model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Multiplier"]


class Multiplier(NamelistBase):
    """FDS MULT namelist - creates arrays of repeated objects.

    Used with MESH, OBST, VENT, HOLE, and INIT to create regular
    arrays of objects without specifying each one individually.

    Parameters
    ----------
    id : str
        Multiplier identifier (referenced by MULT_ID on other namelists).
    dx, dy, dz : float, optional
        Spacing in each direction [m].
    dx0, dy0, dz0 : float, optional
        Initial offset [m] (default: 0.0).
    i_lower, i_upper : int, optional
        X-direction array bounds (default: 0).
    j_lower, j_upper : int, optional
        Y-direction array bounds (default: 0).
    k_lower, k_upper : int, optional
        Z-direction array bounds (default: 0).

    Examples
    --------
    >>> mult = Multiplier(
    ...     id='ARRAY_3X3',
    ...     dx=2.0, dy=2.0,
    ...     i_lower=0, i_upper=2,
    ...     j_lower=0, j_upper=2
    ... )

    See Also
    --------
    Mesh : Meshes that can be replicated.
    Obstruction : Obstructions that can be replicated.
    Vent : Vents that can be replicated.
    Hole : Holes that can be replicated.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MULT"

    # --- Identification ---
    id: str = FdsField(..., description="Multiplier identifier", group="identification")

    # --- Spacing ---
    dx: float | None = FdsField(None, description="X spacing [m]", group="spacing")
    dy: float | None = FdsField(None, description="Y spacing [m]", group="spacing")
    dz: float | None = FdsField(None, description="Z spacing [m]", group="spacing")

    # --- Initial offset ---
    dx0: float = FdsField(0.0, exclude_if=0.0, description="Initial X offset [m]", group="offset")
    dy0: float = FdsField(0.0, exclude_if=0.0, description="Initial Y offset [m]", group="offset")
    dz0: float = FdsField(0.0, exclude_if=0.0, description="Initial Z offset [m]", group="offset")

    # --- Array bounds (3D mode) ---
    i_lower: int = FdsField(0, exclude_if=0, description="X lower bound", group="bounds")
    i_upper: int = FdsField(0, exclude_if=0, description="X upper bound", group="bounds")
    j_lower: int = FdsField(0, exclude_if=0, description="Y lower bound", group="bounds")
    j_upper: int = FdsField(0, exclude_if=0, description="Y upper bound", group="bounds")
    k_lower: int = FdsField(0, exclude_if=0, description="Z lower bound", group="bounds")
    k_upper: int = FdsField(0, exclude_if=0, description="Z upper bound", group="bounds")

    # --- Sequential mode ---
    n_lower: int | None = FdsField(None, description="Sequential lower bound", group="sequential")
    n_upper: int | None = FdsField(None, description="Sequential upper bound", group="sequential")

    # --- Skip ranges (for creating gaps in arrays) ---
    i_lower_skip: int | None = FdsField(None, description="X skip lower", group="skip")
    i_upper_skip: int | None = FdsField(None, description="X skip upper", group="skip")
    j_lower_skip: int | None = FdsField(None, description="Y skip lower", group="skip")
    j_upper_skip: int | None = FdsField(None, description="Y skip upper", group="skip")
    k_lower_skip: int | None = FdsField(None, description="Z skip lower", group="skip")
    k_upper_skip: int | None = FdsField(None, description="Z skip upper", group="skip")
    n_lower_skip: int | None = FdsField(None, description="Sequential skip lower", group="skip")
    n_upper_skip: int | None = FdsField(None, description="Sequential skip upper", group="skip")

    # --- Incremental XB offsets ---
    dxb: tuple[float, float, float, float, float, float] | None = FdsField(
        None, description="Incremental XB offsets", group="xb_offset"
    )

    @model_validator(mode="after")
    def validate_mult(self) -> "Multiplier":
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
