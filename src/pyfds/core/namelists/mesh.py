"""
FDS MESH namelist.

Computational domain definition.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.namelists.base import NamelistBase


class Mesh(NamelistBase):
    """
    FDS MESH namelist - computational domain definition.

    Parameters
    ----------
    ijk : Tuple[int, int, int]
        Number of grid cells in x, y, z directions
    xb : Tuple[float, float, float, float, float, float]
        Domain bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    id : str, optional
        Mesh identifier for multi-mesh simulations
    mpi_process : int, optional
        MPI process number for this mesh

    Examples
    --------
    >>> mesh = Mesh(ijk=(100, 100, 50), xb=(0, 10, 0, 10, 0, 5))
    >>> print(mesh.to_fds())
    &MESH IJK=100,100,50, XB=0,10,0,10,0,5 /

    Notes
    -----
    Grid cells should ideally be cubic or near-cubic for best accuracy.
    """

    ijk: tuple[int, int, int] = Field(..., description="Grid cell counts (i,j,k)")
    xb: tuple[float, float, float, float, float, float] = Field(
        ..., description="Domain bounds (xmin,xmax,ymin,ymax,zmin,zmax)"
    )
    id: str | None = Field(None, description="Mesh identifier")
    mpi_process: int | None = Field(None, ge=0, description="MPI process number")

    @field_validator("ijk")
    @classmethod
    def validate_ijk(cls, v: tuple[int, int, int]) -> tuple[int, int, int]:
        """Validate grid dimensions are positive."""
        if len(v) != 3:
            raise ValueError("IJK must have exactly 3 values")
        if any(val <= 0 for val in v):
            raise ValueError("All IJK values must be positive")
        return v

    @field_validator("xb")
    @classmethod
    def validate_xb(cls, v: tuple[float, ...]) -> tuple[float, ...]:
        """Validate domain bounds are valid."""
        if len(v) != 6:
            raise ValueError("XB must have exactly 6 values")
        if v[0] >= v[1] or v[2] >= v[3] or v[4] >= v[5]:
            raise ValueError("XB bounds must satisfy xmin<xmax, ymin<ymax, zmin<zmax")
        return v

    def to_fds(self) -> str:
        """Generate FDS MESH namelist."""
        params: dict[str, Any] = {"ijk": self.ijk, "xb": self.xb}
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
        dx = (self.xb[1] - self.xb[0]) / self.ijk[0]
        dy = (self.xb[3] - self.xb[2]) / self.ijk[1]
        dz = (self.xb[5] - self.xb[4]) / self.ijk[2]
        return (dx, dy, dz)
