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
