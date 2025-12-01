"""FDS MESH namelist for computational domain definition.

The MESH namelist defines the computational grid including dimensions,
cell counts, and parallel processing assignments.

Field Groups:
    identification: Mesh ID
    geometry: Grid cells and domain bounds
    parallel: MPI process assignment
    visualization: Color and appearance
    transform: Grid transformation
"""

from typing import Any

from pydantic import field_validator

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Mesh"]


class Mesh(NamelistBase):
    """FDS MESH namelist for computational domain definition.

    The MESH defines a rectangular computational domain with uniform
    grid spacing. Multiple meshes can be used for parallel simulations.

    Parameters
    ----------
    ijk : Grid3D
        Number of grid cells in x, y, z directions.
    xb : Bounds3D
        Domain bounds (xmin, xmax, ymin, ymax, zmin, zmax).
    id : str, optional
        Mesh identifier for multi-mesh simulations.
    mpi_process : int, optional
        MPI process number for this mesh.

    Notes
    -----
    Grid cells should ideally be cubic or near-cubic for best accuracy.
    For parallel simulations, assign meshes to MPI processes using MPI_PROCESS.

    Examples
    --------
    >>> from pyfds.core.geometry import Grid3D, Bounds3D
    >>> mesh = Mesh(
    ...     ijk=Grid3D.of(100, 100, 50),
    ...     xb=Bounds3D.of(0, 10, 0, 10, 0, 5)
    ... )

    See Also
    --------
    Multiplier : Create arrays of meshes.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MESH"

    # --- Identification ---
    id: str | None = FdsField(
        None, description="Mesh identifier for multi-mesh simulations", group="identification"
    )

    # --- Geometry ---
    ijk: Grid3D = FdsField(..., description="Grid cell counts (i,j,k)", group="geometry")
    xb: Bounds3D = FdsField(
        ..., description="Domain bounds (xmin,xmax,ymin,ymax,zmin,zmax)", group="geometry"
    )

    # --- Coordinate System ---
    cylindrical: bool = FdsField(
        False, exclude_if=False, description="Use cylindrical coordinates", group="geometry"
    )

    # --- Parallel Processing ---
    mpi_process: int | None = FdsField(
        None, ge=0, description="MPI process number", group="parallel"
    )

    # --- Array Replication ---
    mult_id: str | None = FdsField(
        None, description="Multiplier ID for mesh arrays", group="replication"
    )

    # --- Output Control ---
    bndf_mesh: bool = FdsField(
        True, exclude_if=True, description="Output boundary file for this mesh", group="output"
    )

    # --- Mesh Alignment ---
    check_mesh_alignment: bool = FdsField(
        False,
        exclude_if=False,
        description="Check alignment with other meshes",
        group="alignment",
    )

    # --- Visualization ---
    color: str | None = FdsField(
        None, description="Mesh color name for visualization", group="visualization"
    )
    rgb: tuple[int, int, int] | None = FdsField(
        None, description="RGB color triplet (0-255)", group="visualization"
    )

    # --- Grid Transformation ---
    trnx_id: str | None = FdsField(
        None, description="TRNX transformation ID for x-direction", group="transform"
    )
    trny_id: str | None = FdsField(
        None, description="TRNY transformation ID for y-direction", group="transform"
    )
    trnz_id: str | None = FdsField(
        None, description="TRNZ transformation ID for z-direction", group="transform"
    )

    # --- Validators ---
    @field_validator("ijk")
    @classmethod
    def validate_ijk(cls, v: Grid3D) -> Grid3D:
        """Validate IJK values are positive."""
        if any(x <= 0 for x in v.as_tuple()):
            raise ValueError("IJK values must be positive integers")
        return v

    @field_validator("xb")
    @classmethod
    def validate_xb(cls, v: Bounds3D) -> Bounds3D:
        """Validate XB bounds have min < max."""
        xb_tuple = v.as_tuple()
        if xb_tuple[0] >= xb_tuple[1] or xb_tuple[2] >= xb_tuple[3] or xb_tuple[4] >= xb_tuple[5]:
            raise ValueError("XB bounds must have min < max for each dimension")
        return v

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB values are in valid range 0-255."""
        if v is not None and any(x < 0 or x > 255 for x in v):
            raise ValueError("RGB values must be in range 0-255")
        return v

    def _collect_fds_params(self) -> dict[str, Any]:
        """Collect FDS parameters, converting geometry objects to tuples."""
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
            if field_name in ("ijk", "xb"):
                formatted_value = self._format_value(value.as_tuple(), fds_format)
            else:
                formatted_value = self._format_value(value, fds_format)

            params[str(fds_name).upper()] = formatted_value

        return params

    def get_cell_size(self) -> tuple[float, float, float]:
        """Calculate the cell size in each direction.

        Returns
        -------
        tuple[float, float, float]
            Cell sizes (dx, dy, dz) in meters.
        """
        return self.ijk.cell_sizes(self.xb)
