"""Builder for creating MULT namelists with fluent API."""

from typing import Self

from pyfds.builders.base import Builder
from pyfds.core.namelists import Mult


class MultBuilder(Builder[Mult]):
    """
    Builder for creating MULT namelists.

    Provides a fluent API for constructing multipliers that create arrays
    of repeated objects such as meshes, obstructions, vents, and holes.

    Parameters
    ----------
    id : str
        Multiplier identifier

    Examples
    --------
    >>> # 3x3 array of objects spaced 2m apart
    >>> mult = MultBuilder('ARRAY_3X3') \\
    ...     .with_spacing(dx=2.0, dy=2.0) \\
    ...     .with_array_bounds(i_lower=0, i_upper=2, j_lower=0, j_upper=2) \\
    ...     .build()

    >>> # Linear array of 10 objects
    >>> mult = MultBuilder('ROW_10') \\
    ...     .with_spacing(dx=1.0) \\
    ...     .with_sequential_range(n_lower=0, n_upper=9) \\
    ...     .build()

    >>> # Array with gaps (skip indices 2-3)
    >>> mult = MultBuilder('ARRAY_WITH_GAPS') \\
    ...     .with_spacing(dx=1.0) \\
    ...     .with_sequential_range(n_lower=0, n_upper=9) \\
    ...     .with_skip_ranges(n_lower_skip=2, n_upper_skip=3) \\
    ...     .build()
    """

    def __init__(self, id: str):
        """
        Initialize the MultBuilder.

        Parameters
        ----------
        id : str
            Multiplier identifier
        """
        super().__init__()
        self._id = id
        self._dx: float | None = None
        self._dy: float | None = None
        self._dz: float | None = None
        self._dx0: float = 0.0
        self._dy0: float = 0.0
        self._dz0: float = 0.0
        self._i_lower: int = 0
        self._i_upper: int = 0
        self._j_lower: int = 0
        self._j_upper: int = 0
        self._k_lower: int = 0
        self._k_upper: int = 0
        self._n_lower: int | None = None
        self._n_upper: int | None = None
        self._i_lower_skip: int | None = None
        self._i_upper_skip: int | None = None
        self._j_lower_skip: int | None = None
        self._j_upper_skip: int | None = None
        self._k_lower_skip: int | None = None
        self._k_upper_skip: int | None = None
        self._dxb: tuple[float, float, float, float, float, float] | None = None

    def with_spacing(
        self, dx: float | None = None, dy: float | None = None, dz: float | None = None
    ) -> Self:
        """Set spacing in each direction."""
        if dx is not None:
            self._dx = dx
        if dy is not None:
            self._dy = dy
        if dz is not None:
            self._dz = dz
        return self

    def with_offsets(self, dx0: float = 0.0, dy0: float = 0.0, dz0: float = 0.0) -> Self:
        """Set initial offsets."""
        self._dx0 = dx0
        self._dy0 = dy0
        self._dz0 = dz0
        return self

    def with_array_bounds(
        self,
        i_lower: int = 0,
        i_upper: int = 0,
        j_lower: int = 0,
        j_upper: int = 0,
        k_lower: int = 0,
        k_upper: int = 0,
    ) -> Self:
        """Set 3D array bounds."""
        self._i_lower = i_lower
        self._i_upper = i_upper
        self._j_lower = j_lower
        self._j_upper = j_upper
        self._k_lower = k_lower
        self._k_upper = k_upper
        return self

    def with_sequential_range(self, n_lower: int, n_upper: int) -> Self:
        """Set sequential range (alternative to 3D bounds)."""
        self._n_lower = n_lower
        self._n_upper = n_upper
        return self

    def with_skip_ranges(
        self,
        i_lower_skip: int | None = None,
        i_upper_skip: int | None = None,
        j_lower_skip: int | None = None,
        j_upper_skip: int | None = None,
        k_lower_skip: int | None = None,
        k_upper_skip: int | None = None,
        n_lower_skip: int | None = None,
        n_upper_skip: int | None = None,
    ) -> Self:
        """Set skip ranges for creating gaps in arrays."""
        self._i_lower_skip = i_lower_skip
        self._i_upper_skip = i_upper_skip
        self._j_lower_skip = j_lower_skip
        self._j_upper_skip = j_upper_skip
        self._k_lower_skip = k_lower_skip
        self._k_upper_skip = k_upper_skip
        # For sequential mode, map n skips to i skips
        if n_lower_skip is not None:
            self._i_lower_skip = n_lower_skip
        if n_upper_skip is not None:
            self._i_upper_skip = n_upper_skip
        return self

    def with_incremental_offsets(
        self, dxb: tuple[float, float, float, float, float, float]
    ) -> Self:
        """Set incremental XB offsets."""
        self._dxb = dxb
        return self

    def build(self) -> Mult:
        """Build the Mult object."""
        self._check_built()

        mult = Mult(
            id=self._id,
            dx=self._dx,
            dy=self._dy,
            dz=self._dz,
            dx0=self._dx0,
            dy0=self._dy0,
            dz0=self._dz0,
            i_lower=self._i_lower,
            i_upper=self._i_upper,
            j_lower=self._j_lower,
            j_upper=self._j_upper,
            k_lower=self._k_lower,
            k_upper=self._k_upper,
            n_lower=self._n_lower,
            n_upper=self._n_upper,
            i_lower_skip=self._i_lower_skip,
            i_upper_skip=self._i_upper_skip,
            j_lower_skip=self._j_lower_skip,
            j_upper_skip=self._j_upper_skip,
            k_lower_skip=self._k_lower_skip,
            k_upper_skip=self._k_upper_skip,
            dxb=self._dxb,
        )

        self._mark_built()
        return mult
