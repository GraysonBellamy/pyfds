"""
FDS MESH namelist.

Computational domain definition.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists.base import NamelistBase


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

    Examples
    --------
    >>> from pyfds.core.geometry import Grid3D, Bounds3D
    >>> mesh = Mesh(ijk=Grid3D(100, 100, 50), xb=Bounds3D(0, 10, 0, 10, 0, 5))
    >>> print(mesh.to_fds())
    &MESH IJK=100,100,50, XB=0,10,0,10,0,5 /

    Notes
    -----
    Grid cells should ideally be cubic or near-cubic for best accuracy.
    """

    ijk: Grid3D = Field(..., description="Grid cell counts (i,j,k)")
    xb: Bounds3D = Field(..., description="Domain bounds (xmin,xmax,ymin,ymax,zmin,zmax)")
    id: str | None = Field(None, description="Mesh identifier")
    mpi_process: int | None = Field(None, ge=0, description="MPI process number")

    # Parallel Processing (Stage 1.4)
    n_threads: int | None = Field(None, ge=1, description="Number of OpenMP threads")

    # Mesh Refinement (Stage 1.4)
    mult_id: str | None = Field(None, description="MULT ID for mesh replication")

    # Performance Parameters (Stage 1.4)
    maximum_internal_iterations: int = Field(10, ge=1, description="Max pressure iterations")
    check_vn: bool = Field(True, description="Check Von Neumann number")
    restrict_time_step: bool = Field(True, description="Restrict time step for stability")
    vn_max: float = Field(1.0, gt=0, description="Maximum Von Neumann number")
    cfl_max: float = Field(1.0, gt=0, description="Maximum CFL number")
    cfl_min: float = Field(0.8, gt=0, description="Minimum CFL number")

    # Cylindrical Coordinates (Stage 1.4)
    cylindrical: bool = Field(False, description="Use cylindrical coordinates")

    @field_validator("ijk", mode="before")
    @classmethod
    def validate_ijk(cls, v: Any) -> Grid3D:
        """Validate and convert grid dimensions."""
        if isinstance(v, Grid3D):
            return v
        if isinstance(v, tuple):
            return Grid3D.from_tuple(v)
        raise ValueError("IJK must be a Grid3D or tuple of 3 ints")

    @field_validator("xb", mode="before")
    @classmethod
    def validate_xb(cls, v: Any) -> Bounds3D:
        """Validate and convert domain bounds."""
        if isinstance(v, Bounds3D):
            return v
        if isinstance(v, tuple):
            return Bounds3D.from_tuple(v)
        raise ValueError("XB must be a Bounds3D or tuple of 6 floats")

    def to_fds(self) -> str:
        """Generate FDS MESH namelist."""
        params: dict[str, Any] = {"ijk": self.ijk.as_tuple(), "xb": self.xb.as_tuple()}
        if self.id:
            params["id"] = self.id
        if self.mpi_process is not None:
            params["mpi_process"] = self.mpi_process

        # Parallel Processing
        if self.n_threads is not None:
            params["n_threads"] = self.n_threads

        # Mesh Refinement
        if self.mult_id:
            params["mult_id"] = self.mult_id

        # Performance Parameters (only output if not default)
        if self.maximum_internal_iterations != 10:
            params["maximum_internal_iterations"] = self.maximum_internal_iterations
        if not self.check_vn:
            params["check_vn"] = self.check_vn
        if not self.restrict_time_step:
            params["restrict_time_step"] = self.restrict_time_step
        if self.vn_max != 1.0:
            params["vn_max"] = self.vn_max
        if self.cfl_max != 1.0:
            params["cfl_max"] = self.cfl_max
        if self.cfl_min != 0.8:
            params["cfl_min"] = self.cfl_min

        # Cylindrical Coordinates
        if self.cylindrical:
            params["cylindrical"] = self.cylindrical

        return self._build_namelist("MESH", params)

    def get_cell_size(self) -> tuple[float, float, float]:
        """
        Calculate the cell size in each direction.

        Returns
        -------
        Tuple[float, float, float]
            Cell sizes (dx, dy, dz) in meters
        """
        return self.ijk.get_cell_sizes(self.xb)
