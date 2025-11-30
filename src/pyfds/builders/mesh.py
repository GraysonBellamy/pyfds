"""Builder for creating MESH namelists with grid and parallel configuration."""

from ..core.geometry import Bounds3D, Grid3D
from ..core.namelists import Mesh
from .base import Builder


class MeshBuilder(Builder[Mesh]):
    """
    Builder for creating MESH namelists.

    Provides a fluent API for constructing computational meshes
    with grid dimensions, bounds, parallel processing, and performance tuning.

    Examples
    --------
    >>> # Simple mesh
    >>> mesh = MeshBuilder() \\
    ...     .with_bounds(Bounds3D.of(0, 10, 0, 10, 0, 3) 3)) \\
    ...     .with_grid(Grid3D.of(100, 100, 30)) \\
    ...     .build()

    >>> # Multi-mesh with MPI
    >>> mesh = MeshBuilder() \\
    ...     .with_id('MESH1') \\
    ...     .with_bounds(Bounds3D.of(0, 10, 0, 10, 0, 3) 3)) \\
    ...     .with_grid(Grid3D.of(50, 50, 15)) \\
    ...     .with_mpi(process=0, n_threads=4) \\
    ...     .build()

    >>> # Cylindrical mesh
    >>> mesh = MeshBuilder() \\
    ...     .with_bounds(Bounds3D.of(0, 1, 0, 1, 0, 3) 3)) \\
    ...     .with_grid(Grid3D.of(50, 50, 100)) \\
    ...     .as_cylindrical() \\
    ...     .build()
    """

    def __init__(self) -> None:
        """Initialize the MeshBuilder."""
        super().__init__()
        self._bounds: Bounds3D | None = None
        self._grid: Grid3D | None = None
        self._id: str | None = None
        self._params: dict = {}

    def with_bounds(
        self, bounds: Bounds3D | tuple[float, float, float, float, float, float]
    ) -> "MeshBuilder":
        """
        Set mesh bounds.

        Parameters
        ----------
        bounds : Bounds3D or tuple
            Domain bounds (xmin, xmax, ymin, ymax, zmin, zmax) in meters

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().with_bounds(Bounds3D.of(0, 10, 0, 10, 0, 3) 3)).build()
        >>> mesh = MeshBuilder().with_bounds((0, 10, 0, 10, 0, 3)).build()
        """
        if isinstance(bounds, tuple):
            bounds = Bounds3D.of(*bounds)
        self._bounds = bounds
        return self

    def with_grid(self, grid: Grid3D | tuple[int, int, int]) -> "MeshBuilder":
        """
        Set grid dimensions.

        Parameters
        ----------
        grid : Grid3D or tuple
            Number of grid cells (i, j, k) in x, y, z directions

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().with_grid(Grid3D.of(100, 100, 50)).build()
        >>> mesh = MeshBuilder().with_grid((100, 100, 50)).build()
        """
        if isinstance(grid, tuple):
            grid = Grid3D.of(*grid)
        self._grid = grid
        return self

    def with_id(self, mesh_id: str) -> "MeshBuilder":
        """
        Set mesh identifier.

        Parameters
        ----------
        mesh_id : str
            Unique mesh identifier for multi-mesh simulations

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().with_id('MESH1').build()
        """
        self._id = mesh_id
        return self

    def with_mpi(self, process: int, n_threads: int | None = None) -> "MeshBuilder":
        """
        Assign mesh to MPI process.

        Parameters
        ----------
        process : int
            MPI process number (0-indexed)
        n_threads : int, optional
            Number of OpenMP threads for this process

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().with_mpi(process=0, n_threads=4).build()
        """
        self._params["mpi_process"] = process
        if n_threads is not None:
            self._params["n_threads"] = n_threads
        return self

    def with_max_iterations(self, max_iter: int) -> "MeshBuilder":
        """
        Set maximum internal pressure iterations.

        Parameters
        ----------
        max_iter : int
            Maximum number of pressure iterations, default: 10

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().with_max_iterations(20).build()
        """
        self._params["maximum_internal_iterations"] = max_iter
        return self

    def with_mult(self, mult_id: str) -> "MeshBuilder":
        """
        Set MULT ID for mesh replication.

        Parameters
        ----------
        mult_id : str
            MULT identifier for replication pattern

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().with_mult('REPLICATE_X').build()
        """
        self._params["mult_id"] = mult_id
        return self

    def as_cylindrical(self) -> "MeshBuilder":
        """
        Use cylindrical coordinates.

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder().as_cylindrical().build()
        """
        self._params["cylindrical"] = True
        return self

    def disable_vn_check(self) -> "MeshBuilder":
        """
        Disable Von Neumann number check.

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Notes
        -----
        Use with caution - disabling stability checks can lead to unstable simulations.
        """
        self._params["check_vn"] = False
        return self

    def disable_time_step_restriction(self) -> "MeshBuilder":
        """
        Disable automatic time step restriction.

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Notes
        -----
        Use with caution - disabling time step restriction can lead to unstable simulations.
        """
        self._params["restrict_time_step"] = False
        return self

    def with_stability_control(
        self,
        cfl_max: float | None = None,
        cfl_min: float | None = None,
        vn_max: float | None = None,
        check_vn: bool | None = None,
        restrict_time_step: bool | None = None,
    ) -> "MeshBuilder":
        """
        Set stability control parameters.

        Parameters
        ----------
        cfl_max : float, optional
            Maximum CFL number, default: 1.0
        cfl_min : float, optional
            Minimum CFL number, default: 0.8
        vn_max : float, optional
            Maximum Von Neumann number, default: 1.0
        check_vn : bool, optional
            Enable Von Neumann check, default: True
        restrict_time_step : bool, optional
            Restrict time step, default: True

        Returns
        -------
        MeshBuilder
            Self for method chaining

        Examples
        --------
        >>> mesh = MeshBuilder() \\
        ...     .with_stability_control(cfl_max=0.95, vn_max=0.9) \\
        ...     .build()
        """
        if cfl_max is not None:
            self._params["cfl_max"] = cfl_max
        if cfl_min is not None:
            self._params["cfl_min"] = cfl_min
        if vn_max is not None:
            self._params["vn_max"] = vn_max
        if check_vn is not None:
            self._params["check_vn"] = check_vn
        if restrict_time_step is not None:
            self._params["restrict_time_step"] = restrict_time_step
        return self

    def build(self) -> Mesh:
        """
        Build the Mesh object.

        Returns
        -------
        Mesh
            The constructed Mesh namelist object

        Raises
        ------
        ValueError
            If required parameters are missing
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        # Validate required parameters
        if self._bounds is None:
            raise ValueError("MeshBuilder: bounds are required (use with_bounds())")
        if self._grid is None:
            raise ValueError("MeshBuilder: grid is required (use with_grid())")

        params = {
            "ijk": self._grid,
            "xb": self._bounds,
            **self._params,
        }

        if self._id is not None:
            params["id"] = self._id

        mesh = Mesh(**params)
        self._mark_built()
        return mesh
