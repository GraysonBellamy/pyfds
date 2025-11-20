"""
Base namelist classes for FDS input file generation.

This module provides the foundation for creating FDS namelist groups
with proper validation and formatting.
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, Optional, Tuple

from pydantic import BaseModel, ConfigDict, Field, field_validator


class NamelistBase(BaseModel, ABC):
    """
    Abstract base class for all FDS namelist groups.

    All FDS namelist groups inherit from this class and implement
    the to_fds() method to generate their FDS format representation.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True, validate_assignment=True)

    @abstractmethod
    def to_fds(self) -> str:
        """
        Convert the namelist to FDS format string.

        Returns
        -------
        str
            FDS namelist format string
        """
        pass

    def _format_value(self, value: Any) -> str:
        """
        Format a Python value for FDS namelist format.

        Parameters
        ----------
        value : Any
            Python value to format

        Returns
        -------
        str
            FDS-formatted string representation
        """
        if isinstance(value, str):
            return f"'{value}'"
        if isinstance(value, bool):
            return ".TRUE." if value else ".FALSE."
        if isinstance(value, (list, tuple)):
            return ",".join(self._format_value(v) for v in value)
        if isinstance(value, (int, float)):
            return str(value)
        if value is None:
            return ""
        return str(value)

    def _build_namelist(self, group_name: str, params: Dict[str, Any]) -> str:
        """
        Build an FDS namelist string from parameters.

        Parameters
        ----------
        group_name : str
            Name of the FDS namelist group (e.g., 'MESH', 'SURF')
        params : Dict[str, Any]
            Dictionary of parameter name-value pairs

        Returns
        -------
        str
            Formatted FDS namelist string
        """
        # Filter out None values and empty strings
        filtered_params = {k: v for k, v in params.items() if v is not None and v != ""}

        if not filtered_params:
            return ""

        # Build parameter string
        param_strings = []
        for key, value in filtered_params.items():
            fds_key = key.upper()
            fds_value = self._format_value(value)
            param_strings.append(f"{fds_key}={fds_value}")

        params_line = ", ".join(param_strings)
        return f"&{group_name} {params_line} /\n"


class Head(NamelistBase):
    """
    FDS HEAD namelist - simulation identification and metadata.

    Parameters
    ----------
    chid : str
        Case identifier (filename prefix for all output files)
    title : str, optional
        Descriptive title for the simulation

    Examples
    --------
    >>> head = Head(chid='room_fire', title='Room Fire Test')
    >>> print(head.to_fds())
    &HEAD CHID='room_fire', TITLE='Room Fire Test' /
    """

    chid: str = Field(..., description="Case identifier")
    title: Optional[str] = Field(None, description="Simulation title")

    @field_validator("chid")
    @classmethod
    def validate_chid(cls, v: str) -> str:
        """Validate CHID has no spaces or special characters."""
        if not v:
            raise ValueError("CHID cannot be empty")
        if " " in v:
            raise ValueError("CHID cannot contain spaces")
        if len(v) > 60:
            raise ValueError("CHID should be 60 characters or less")
        return v

    def to_fds(self) -> str:
        """Generate FDS HEAD namelist."""
        params: Dict[str, Any] = {"chid": self.chid}
        if self.title:
            params["title"] = self.title
        return self._build_namelist("HEAD", params)


class Time(NamelistBase):
    """
    FDS TIME namelist - time control parameters.

    Parameters
    ----------
    t_end : float
        End time for the simulation in seconds
    t_begin : float, optional
        Start time for output (default: 0.0)
    dt : float, optional
        Initial time step in seconds
    wall_clock_time : float, optional
        Maximum wall clock time in seconds

    Examples
    --------
    >>> time = Time(t_end=600.0, dt=0.1)
    >>> print(time.to_fds())
    &TIME T_END=600.0, DT=0.1 /
    """

    t_end: float = Field(..., gt=0, description="End time (s)")
    t_begin: Optional[float] = Field(None, ge=0, description="Begin time (s)")
    dt: Optional[float] = Field(None, gt=0, description="Time step (s)")
    wall_clock_time: Optional[float] = Field(None, gt=0, description="Wall clock limit (s)")

    def to_fds(self) -> str:
        """Generate FDS TIME namelist."""
        params: Dict[str, Any] = {"t_end": self.t_end}
        if self.t_begin is not None:
            params["t_begin"] = self.t_begin
        if self.dt is not None:
            params["dt"] = self.dt
        if self.wall_clock_time is not None:
            params["wall_clock_time"] = self.wall_clock_time
        return self._build_namelist("TIME", params)


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

    ijk: Tuple[int, int, int] = Field(..., description="Grid cell counts (i,j,k)")
    xb: Tuple[float, float, float, float, float, float] = Field(
        ..., description="Domain bounds (xmin,xmax,ymin,ymax,zmin,zmax)"
    )
    id: Optional[str] = Field(None, description="Mesh identifier")
    mpi_process: Optional[int] = Field(None, ge=0, description="MPI process number")

    @field_validator("ijk")
    @classmethod
    def validate_ijk(cls, v: Tuple[int, int, int]) -> Tuple[int, int, int]:
        """Validate grid dimensions are positive."""
        if len(v) != 3:
            raise ValueError("IJK must have exactly 3 values")
        if any(val <= 0 for val in v):
            raise ValueError("All IJK values must be positive")
        return v

    @field_validator("xb")
    @classmethod
    def validate_xb(cls, v: Tuple[float, ...]) -> Tuple[float, ...]:
        """Validate domain bounds are valid."""
        if len(v) != 6:
            raise ValueError("XB must have exactly 6 values")
        if v[0] >= v[1] or v[2] >= v[3] or v[4] >= v[5]:
            raise ValueError("XB bounds must satisfy xmin<xmax, ymin<ymax, zmin<zmax")
        return v

    def to_fds(self) -> str:
        """Generate FDS MESH namelist."""
        params: Dict[str, Any] = {"ijk": self.ijk, "xb": self.xb}
        if self.id:
            params["id"] = self.id
        if self.mpi_process is not None:
            params["mpi_process"] = self.mpi_process
        return self._build_namelist("MESH", params)

    def get_cell_size(self) -> Tuple[float, float, float]:
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


class Surface(NamelistBase):
    """
    FDS SURF namelist - surface properties.

    Parameters
    ----------
    id : str
        Unique surface identifier
    rgb : Tuple[int, int, int], optional
        RGB color values (0-255)
    color : str, optional
        Named color (e.g., 'RED', 'BLUE')
    hrrpua : float, optional
        Heat release rate per unit area (kW/m²)
    tmp_front : float, optional
        Front surface temperature (°C)
    matl_id : str, optional
        Material identifier
    thickness : float, optional
        Material thickness (m)

    Examples
    --------
    >>> fire_surf = Surface(id='FIRE', hrrpua=1000.0, color='RED')
    >>> print(fire_surf.to_fds())
    &SURF ID='FIRE', HRRPUA=1000.0, COLOR='RED' /
    """

    id: str = Field(..., description="Surface identifier")
    rgb: Optional[Tuple[int, int, int]] = Field(None, description="RGB color")
    color: Optional[str] = Field(None, description="Named color")
    hrrpua: Optional[float] = Field(
        None, ge=0, description="Heat release rate per unit area (kW/m²)"
    )
    tmp_front: Optional[float] = Field(None, description="Front surface temperature (°C)")
    matl_id: Optional[str] = Field(None, description="Material identifier")
    thickness: Optional[float] = Field(None, gt=0, description="Material thickness (m)")

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: Optional[Tuple[int, int, int]]) -> Optional[Tuple[int, int, int]]:
        """Validate RGB values are in range 0-255."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in v):
                raise ValueError("RGB values must be in range 0-255")
        return v

    def to_fds(self) -> str:
        """Generate FDS SURF namelist."""
        params: Dict[str, Any] = {"id": self.id}
        if self.rgb:
            params["rgb"] = self.rgb
        if self.color:
            params["color"] = self.color
        if self.hrrpua is not None:
            params["hrrpua"] = self.hrrpua
        if self.tmp_front is not None:
            params["tmp_front"] = self.tmp_front
        if self.matl_id:
            params["matl_id"] = self.matl_id
        if self.thickness is not None:
            params["thickness"] = self.thickness
        return self._build_namelist("SURF", params)


class Obstruction(NamelistBase):
    """
    FDS OBST namelist - obstructions and solid objects.

    Parameters
    ----------
    xb : Tuple[float, float, float, float, float, float]
        Obstruction bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    surf_id : str, optional
        Surface ID for all faces
    surf_ids : Dict[str, str], optional
        Surface IDs for individual faces (e.g., {'top': 'FIRE', 'sides': 'WALL'})
    color : str, optional
        Named color

    Examples
    --------
    >>> burner = Obstruction(xb=(4, 6, 4, 6, 0, 0.5), surf_id='FIRE')
    >>> print(burner.to_fds())
    &OBST XB=4,6,4,6,0,0.5, SURF_ID='FIRE' /
    """

    xb: Tuple[float, float, float, float, float, float] = Field(
        ..., description="Obstruction bounds"
    )
    surf_id: Optional[str] = Field(None, description="Surface ID for all faces")
    surf_id_top: Optional[str] = Field(None, description="Top surface ID")
    surf_id_bottom: Optional[str] = Field(None, description="Bottom surface ID")
    surf_id_sides: Optional[str] = Field(None, description="Side surfaces ID")
    color: Optional[str] = Field(None, description="Named color")

    @field_validator("xb")
    @classmethod
    def validate_xb(cls, v: Tuple[float, ...]) -> Tuple[float, ...]:
        """Validate obstruction bounds."""
        if len(v) != 6:
            raise ValueError("XB must have exactly 6 values")
        # Note: For obstructions, equal bounds are allowed (thin surfaces)
        if v[0] > v[1] or v[2] > v[3] or v[4] > v[5]:
            raise ValueError("XB bounds must satisfy xmin<=xmax, ymin<=ymax, zmin<=zmax")
        return v

    def to_fds(self) -> str:
        """Generate FDS OBST namelist."""
        params: Dict[str, Any] = {"xb": self.xb}
        if self.surf_id:
            params["surf_id"] = self.surf_id
        if self.surf_id_top:
            params["surf_id_top"] = self.surf_id_top
        if self.surf_id_bottom:
            params["surf_id_bottom"] = self.surf_id_bottom
        if self.surf_id_sides:
            params["surf_id_sides"] = self.surf_id_sides
        if self.color:
            params["color"] = self.color
        return self._build_namelist("OBST", params)


class Device(NamelistBase):
    """
    FDS DEVC namelist - measurement devices.

    Parameters
    ----------
    id : str
        Unique device identifier
    quantity : str
        FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY')
    xyz : Tuple[float, float, float], optional
        Device location (x, y, z) in meters
    xb : Tuple[float, float, float, float, float, float], optional
        Device bounds for spatial averaging

    Examples
    --------
    >>> temp_sensor = Device(id='TEMP1', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.0))
    >>> print(temp_sensor.to_fds())
    &DEVC ID='TEMP1', QUANTITY='TEMPERATURE', XYZ=2.5,2.5,2.0 /
    """

    id: str = Field(..., description="Device identifier")
    quantity: str = Field(..., description="Quantity to measure")
    xyz: Optional[Tuple[float, float, float]] = Field(None, description="Device location")
    xb: Optional[Tuple[float, float, float, float, float, float]] = Field(
        None, description="Device bounds"
    )

    def to_fds(self) -> str:
        """Generate FDS DEVC namelist."""
        params: Dict[str, Any] = {"id": self.id, "quantity": self.quantity}
        if self.xyz:
            params["xyz"] = self.xyz
        if self.xb:
            params["xb"] = self.xb
        return self._build_namelist("DEVC", params)
