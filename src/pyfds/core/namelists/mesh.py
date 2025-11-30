"""
FDS MESH namelist.

Computational domain definition.
"""

from typing import TYPE_CHECKING, Any

from pydantic import field_validator

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists.base import FdsField, NamelistBase

if TYPE_CHECKING:
    from pyfds.builders import MeshBuilder


class Mesh(NamelistBase):
    """
    FDS MESH namelist - computational domain definition.

    Parameters
    ----------
    ijk : Grid3D
        Number of grid cells in x, y, z directions
    xb : Bounds3D
        Domain bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    id : str, optional
        Mesh identifier for multi-mesh simulations
    mpi_process : int, optional
        MPI process number for this mesh
    cfl_max : float, optional
        Maximum CFL number
    cfl_min : float, optional
        Minimum CFL number
    vn_max : float, optional
        Maximum Von Neumann number
    check_vn : bool, optional
        Enable Von Neumann check
    restrict_time_step : bool, optional
        Restrict time step

    Examples
    --------
    >>> from pyfds.core.geometry import Grid3D, Bounds3D
    >>> mesh = Mesh(ijk=Grid3D.of(100, 100, 50), xb=Bounds3D.of(0, 10, 0, 10, 0, 5) 5))
    >>> print(mesh.to_fds())
    &MESH IJK=100,100,50, XB=0,10,0,10,0,5 /

    Notes
    -----
    Grid cells should ideally be cubic or near-cubic for best accuracy.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MESH"

    @classmethod
    def builder(cls) -> "MeshBuilder":
        """Return a fluent builder for Mesh.

        Returns
        -------
        MeshBuilder
            A builder instance for fluent construction

        Examples
        --------
        >>> mesh = Mesh.builder() \\
        ...     .with_bounds(0, 10, 0, 10, 0, 3) \\
        ...     .with_grid(100, 100, 30) \\
        ...     .build()
        """
        from pyfds.builders import MeshBuilder

        return MeshBuilder()

    ijk: Grid3D = FdsField(..., description="Grid cell counts (i,j,k)")
    xb: Bounds3D = FdsField(..., description="Domain bounds (xmin,xmax,ymin,ymax,zmin,zmax)")

    @field_validator("ijk")
    @classmethod
    def validate_ijk(cls, v: Grid3D) -> Grid3D:
        if any(x <= 0 for x in v.as_tuple()):
            raise ValueError("IJK values must be positive integers")
        return v

    @field_validator("xb")
    @classmethod
    def validate_xb(cls, v: Bounds3D) -> Bounds3D:
        xb_tuple = v.as_tuple()
        if xb_tuple[0] >= xb_tuple[1] or xb_tuple[2] >= xb_tuple[3] or xb_tuple[4] >= xb_tuple[5]:
            raise ValueError("XB bounds must have min < max for each dimension")
        return v

    # Optional ID for multi-mesh simulations
    id: str | None = FdsField(None, description="Mesh identifier for multi-mesh simulations")

    # Stability control parameters
    vn_max: float | None = FdsField(None, description="Maximum Von Neumann number")
    cfl_max: float | None = FdsField(None, description="Maximum CFL number")
    vn_min: float | None = FdsField(None, description="Minimum Von Neumann number")
    cfl_min: float | None = FdsField(None, description="Minimum CFL number")

    # Time stepping
    check_vn: bool = FdsField(True, description="Check Von Neumann stability")
    restrict_time_step: bool = FdsField(True, description="Restrict time step")

    # MPI parameters
    mpi_process: int | None = FdsField(None, description="MPI process number")
    n_threads: int | None = FdsField(None, description="Number of OpenMP threads")

    # Other mesh parameters
    maximum_internal_iterations: int = FdsField(10, description="Maximum pressure iterations")
    mult_id: str | None = FdsField(None, description="Multiplier ID")
    cylindrical: bool = FdsField(False, description="Cylindrical coordinates")

    def _collect_fds_params(self) -> dict[str, Any]:
        """
        Collect FDS parameters, converting geometry objects to tuples.

        Returns
        -------
        dict[str, Any]
            Dictionary of parameter name-value pairs
        """
        params = {}

        for field_name, field_info in self.__class__.model_fields.items():
            value = getattr(self, field_name)

            # Get FDS metadata
            extra = field_info.json_schema_extra
            if not isinstance(extra, dict):
                extra = {}
            fds_name = extra.get("fds_name") or field_name
            fds_format = extra.get("fds_format")
            if not isinstance(fds_format, (str, type(None))):
                fds_format = None
            exclude_if = extra.get("exclude_if")

            # Skip if value matches exclude condition
            if value is None or value == exclude_if:
                continue

            # Skip if value equals the field's default value (but not for required fields)
            if field_info.default != ... and value == field_info.default:
                continue

            # Convert geometry objects to tuples
            if field_name == "ijk" or field_name == "xb":
                formatted_value = self._format_value(value.as_tuple(), fds_format)
            else:
                formatted_value = self._format_value(value, fds_format)

            params[str(fds_name).upper()] = formatted_value

        return params

    def get_cell_size(self) -> tuple[float, float, float]:
        """
        Calculate the cell size in each direction.

        Returns
        -------
        Tuple[float, float, float]
            Cell sizes (dx, dy, dz) in meters
        """
        return self.ijk.cell_sizes(self.xb)
