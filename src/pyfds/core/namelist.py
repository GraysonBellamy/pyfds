"""
Base namelist classes for FDS input file generation.

This module provides the foundation for creating FDS namelist groups
with proper validation and formatting.
"""

from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, cast

import numpy as np
from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator


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

    def _build_namelist(self, group_name: str, params: dict[str, Any]) -> str:
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
    title: str | None = Field(None, description="Simulation title")

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
        params: dict[str, Any] = {"chid": self.chid}
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
    t_begin: float | None = Field(None, ge=0, description="Begin time (s)")
    dt: float | None = Field(None, gt=0, description="Time step (s)")
    wall_clock_time: float | None = Field(None, gt=0, description="Wall clock limit (s)")

    def to_fds(self) -> str:
        """Generate FDS TIME namelist."""
        params: dict[str, Any] = {"t_end": self.t_end}
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
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    color: str | None = Field(None, description="Named color")
    hrrpua: float | None = Field(None, ge=0, description="Heat release rate per unit area (kW/m²)")
    tmp_front: float | None = Field(None, description="Front surface temperature (°C)")
    matl_id: str | None = Field(None, description="Material identifier")
    thickness: float | None = Field(None, gt=0, description="Material thickness (m)")

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB values are in range 0-255."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in v):
                raise ValueError("RGB values must be in range 0-255")
        return v

    def to_fds(self) -> str:
        """Generate FDS SURF namelist."""
        params: dict[str, Any] = {"id": self.id}
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

    xb: tuple[float, float, float, float, float, float] = Field(
        ..., description="Obstruction bounds"
    )
    surf_id: str | None = Field(None, description="Surface ID for all faces")
    surf_id_top: str | None = Field(None, description="Top surface ID")
    surf_id_bottom: str | None = Field(None, description="Bottom surface ID")
    surf_id_sides: str | None = Field(None, description="Side surfaces ID")
    color: str | None = Field(None, description="Named color")

    @field_validator("xb")
    @classmethod
    def validate_xb(cls, v: tuple[float, ...]) -> tuple[float, ...]:
        """Validate obstruction bounds."""
        if len(v) != 6:
            raise ValueError("XB must have exactly 6 values")
        # Note: For obstructions, equal bounds are allowed (thin surfaces)
        if v[0] > v[1] or v[2] > v[3] or v[4] > v[5]:
            raise ValueError("XB bounds must satisfy xmin<=xmax, ymin<=ymax, zmin<=zmax")
        return v

    def to_fds(self) -> str:
        """Generate FDS OBST namelist."""
        params: dict[str, Any] = {"xb": self.xb}
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
    xyz: tuple[float, float, float] | None = Field(None, description="Device location")
    xb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Device bounds"
    )

    def to_fds(self) -> str:
        """Generate FDS DEVC namelist."""
        params: dict[str, Any] = {"id": self.id, "quantity": self.quantity}
        if self.xyz:
            params["xyz"] = self.xyz
        if self.xb:
            params["xb"] = self.xb
        return self._build_namelist("DEVC", params)


class Ramp(NamelistBase):
    """
    FDS RAMP namelist - time-dependent functions.

    Represents time-dependent or temperature-dependent functions for use with
    various FDS parameters. RAMPs are defined by a series of (T, F) points
    where T can represent time or temperature, and F is the function value.

    Parameters
    ----------
    id : str
        Unique ramp identifier
    points : list[tuple[float, float]]
        List of (T, F) points defining the ramp function

    Examples
    --------
    >>> # Simple linear ramp from 0 to 1000 over 300 seconds
    >>> ramp = Ramp(id='FIRE_RAMP', points=[(0, 0), (300, 1000)])
    >>> print(ramp.to_fds())
    &RAMP ID='FIRE_RAMP', T=0.0, F=0.0 /
    &RAMP ID='FIRE_RAMP', T=300.0, F=1000.0 /

    Notes
    -----
    - Points are automatically sorted by T value
    - At least 2 points are required
    - FDS uses linear interpolation between points
    """

    id: str = Field(..., description="Ramp identifier")
    points: list[tuple[float, float]] = Field(default_factory=list, description="(T, F) points")

    @model_validator(mode="after")
    def validate_ramp(self) -> "Ramp":
        """Validate ramp points."""
        if len(self.points) < 2:
            raise ValueError(f"Ramp '{self.id}' requires at least 2 points")

        # Sort points by T value (use object.__setattr__ to avoid recursion)
        sorted_points = sorted(self.points, key=lambda p: p[0])
        object.__setattr__(self, "points", sorted_points)

        # Check for duplicate T values
        t_values = [p[0] for p in self.points]
        if len(t_values) != len(set(t_values)):
            raise ValueError(f"Ramp '{self.id}' has duplicate T values")

        return self

    def add_point(self, t: float, f: float) -> "Ramp":
        """
        Add a point to the ramp function.

        Parameters
        ----------
        t : float
            Time or temperature value
        f : float
            Function value at this point

        Returns
        -------
        Ramp
            Self for method chaining
        """
        self.points.append((t, f))
        self.points = sorted(self.points, key=lambda p: p[0])

        # Revalidate after adding point
        t_values = [p[0] for p in self.points]
        if len(t_values) != len(set(t_values)):
            raise ValueError(f"Ramp '{self.id}' has duplicate T values")

        return self

    def evaluate(self, t: float | np.ndarray) -> float | np.ndarray:
        """
        Evaluate ramp at given time/temperature using linear interpolation.

        Parameters
        ----------
        t : float or np.ndarray
            Time or temperature value(s) to evaluate

        Returns
        -------
        float or np.ndarray
            Interpolated function value(s)
        """
        if not self.points:
            raise ValueError(f"Ramp '{self.id}' has no points")

        t_values = np.array([p[0] for p in self.points])
        f_values = np.array([p[1] for p in self.points])

        return cast("float | np.ndarray", np.interp(t, t_values, f_values))

    def to_fds(self) -> str:
        """
        Generate FDS RAMP namelist strings.

        Returns
        -------
        str
            Multiple RAMP namelist lines, one for each point
        """
        lines = []
        for t, f in self.points:
            params: dict[str, Any] = {"id": self.id, "t": t, "f": f}
            lines.append(self._build_namelist("RAMP", params).rstrip("\n"))
        return "\n".join(lines) + "\n"


class Material(NamelistBase):
    """
    FDS MATL namelist - material properties.

    Represents material properties for heat transfer and pyrolysis modeling.
    Supports constant properties, temperature-dependent properties (via RAMP),
    and multi-reaction pyrolysis.

    Parameters
    ----------
    id : str
        Unique material identifier
    density : float
        Material density [kg/m³]
    conductivity : float, optional
        Thermal conductivity [W/(m·K)]
    conductivity_ramp : str, optional
        RAMP ID for temperature-dependent conductivity
    specific_heat : float, optional
        Specific heat capacity [kJ/(kg·K)]
    specific_heat_ramp : str, optional
        RAMP ID for temperature-dependent specific heat
    emissivity : float, optional
        Surface emissivity [0-1], default: 0.9
    absorption_coefficient : float, optional
        Radiation absorption coefficient [1/m], default: 50000.0
    n_reactions : int, optional
        Number of pyrolysis reactions, default: 1
    reference_temperature : float, optional
        Reference temperature for properties [°C]
    heat_of_reaction : list[float], optional
        Heat of pyrolysis/vaporization per reaction [kJ/kg]
    a : list[float], optional
        Pre-exponential factors for reactions [1/s]
    e : list[float], optional
        Activation energies for reactions [kJ/kmol]
    nu_spec : list[str], optional
        Species produced by each reaction
    nu_matl : list[str], optional
        Residue materials from each reaction

    Examples
    --------
    >>> # Simple material with constant properties
    >>> wood = Material(
    ...     id='PINE',
    ...     density=500.0,
    ...     conductivity=0.13,
    ...     specific_heat=2.5
    ... )

    >>> # Material with temperature-dependent conductivity
    >>> steel = Material(
    ...     id='STEEL',
    ...     density=7850.0,
    ...     conductivity_ramp='STEEL_K',
    ...     specific_heat=0.46
    ... )

    Notes
    -----
    - Either conductivity or conductivity_ramp must be specified
    - Either specific_heat or specific_heat_ramp must be specified
    - For multi-reaction materials, set n_reactions > 1 and provide arrays
    """

    id: str = Field(..., description="Material identifier")
    density: float = Field(..., gt=0, description="Density [kg/m³]")

    # Thermal properties - constant values
    conductivity: float | None = Field(None, gt=0, description="Thermal conductivity [W/(m·K)]")
    specific_heat: float | None = Field(None, gt=0, description="Specific heat [kJ/(kg·K)]")

    # Thermal properties - temperature-dependent via RAMP
    conductivity_ramp: str | None = Field(None, description="Conductivity RAMP ID")
    specific_heat_ramp: str | None = Field(None, description="Specific heat RAMP ID")

    # Radiative properties
    emissivity: float = Field(0.9, ge=0, le=1, description="Surface emissivity")
    absorption_coefficient: float = Field(50000.0, ge=0, description="Absorption coefficient [1/m]")

    # Pyrolysis properties
    n_reactions: int = Field(1, ge=1, description="Number of reactions")
    reference_temperature: float | None = Field(None, description="Reference temperature [°C]")
    heat_of_reaction: list[float] | None = Field(None, description="Heat of reaction [kJ/kg]")

    # Reaction kinetics
    a: list[float] | None = Field(None, description="Pre-exponential factors [1/s]")
    e: list[float] | None = Field(None, description="Activation energies [kJ/kmol]")
    n_s: list[float] | None = Field(None, description="Reaction orders")

    # Product specification
    nu_spec: list[str] | None = Field(None, description="Product species IDs")
    nu_matl: list[str] | None = Field(None, description="Residue material IDs")

    @model_validator(mode="after")
    def validate_material(self) -> "Material":
        """Validate material properties."""
        # Check that thermal conductivity is specified
        if self.conductivity is None and self.conductivity_ramp is None:
            raise ValueError(
                f"Material '{self.id}': Must specify either CONDUCTIVITY or CONDUCTIVITY_RAMP"
            )

        # Check that specific heat is specified
        if self.specific_heat is None and self.specific_heat_ramp is None:
            raise ValueError(
                f"Material '{self.id}': Must specify either SPECIFIC_HEAT or SPECIFIC_HEAT_RAMP"
            )

        # Validate density range
        if not (1.0 <= self.density <= 10000.0):
            raise ValueError(
                f"Material '{self.id}': DENSITY = {self.density} is outside valid range [1.0, 10000.0]"
            )

        # Validate conductivity range if specified
        if self.conductivity is not None and not (0.001 <= self.conductivity <= 1000.0):
            raise ValueError(
                f"Material '{self.id}': CONDUCTIVITY = {self.conductivity} "
                f"is outside valid range [0.001, 1000.0]"
            )

        # Validate specific heat range if specified
        if self.specific_heat is not None and not (0.1 <= self.specific_heat <= 10.0):
            raise ValueError(
                f"Material '{self.id}': SPECIFIC_HEAT = {self.specific_heat} "
                f"is outside valid range [0.1, 10.0]"
            )

        # Validate multi-reaction arrays
        if self.n_reactions > 1:
            for param_name in ["a", "e", "heat_of_reaction"]:
                param_value = getattr(self, param_name)
                if param_value is not None and len(param_value) != self.n_reactions:
                    raise ValueError(
                        f"Material '{self.id}': {param_name.upper()} must have "
                        f"{self.n_reactions} values for N_REACTIONS={self.n_reactions}"
                    )

        return self

    def to_fds(self) -> str:
        """Generate FDS MATL namelist."""
        params: dict[str, Any] = {"id": self.id, "density": self.density}

        # Thermal conductivity
        if self.conductivity is not None:
            params["conductivity"] = self.conductivity
        elif self.conductivity_ramp:
            params["conductivity_ramp"] = self.conductivity_ramp

        # Specific heat
        if self.specific_heat is not None:
            params["specific_heat"] = self.specific_heat
        elif self.specific_heat_ramp:
            params["specific_heat_ramp"] = self.specific_heat_ramp

        # Radiative properties
        if self.emissivity != 0.9:
            params["emissivity"] = self.emissivity
        if self.absorption_coefficient != 50000.0:
            params["absorption_coefficient"] = self.absorption_coefficient

        # Reaction properties
        if self.n_reactions > 1:
            params["n_reactions"] = self.n_reactions
        if self.reference_temperature is not None:
            params["reference_temperature"] = self.reference_temperature
        if self.heat_of_reaction:
            params["heat_of_reaction"] = self.heat_of_reaction
        if self.a:
            params["a"] = self.a
        if self.e:
            params["e"] = self.e
        if self.n_s:
            params["n_s"] = self.n_s
        if self.nu_spec:
            params["nu_spec"] = self.nu_spec
        if self.nu_matl:
            params["nu_matl"] = self.nu_matl

        return self._build_namelist("MATL", params)


class Reaction(NamelistBase):
    """
    FDS REAC namelist - combustion reaction.

    Defines the combustion reaction including fuel composition, heat of combustion,
    and product yields. FDS supports only one reaction per simulation.

    Parameters
    ----------
    fuel : str, optional
        Fuel name for predefined fuels (e.g., 'PROPANE', 'METHANE')
    c : float, optional
        Number of carbon atoms in fuel molecule
    h : float, optional
        Number of hydrogen atoms in fuel molecule
    o : float, optional
        Number of oxygen atoms in fuel molecule
    n : float, optional
        Number of nitrogen atoms in fuel molecule
    heat_of_combustion : float, optional
        Heat of combustion [kJ/kg]
    soot_yield : float, optional
        Soot yield [kg soot/kg fuel], default: 0.01
    co_yield : float, optional
        CO yield [kg CO/kg fuel], default: 0.0
    radiative_fraction : float, optional
        Fraction of energy radiated, default: 0.35
    auto_ignition_temperature : float, optional
        Auto-ignition temperature [°C]

    Examples
    --------
    >>> # Use predefined fuel
    >>> reac = Reaction(fuel='PROPANE')

    >>> # Custom fuel composition
    >>> reac = Reaction(c=7, h=16, heat_of_combustion=44600, soot_yield=0.015)

    Notes
    -----
    - Only one REAC namelist allowed per simulation
    - Yields must sum to less than 1.0
    """

    fuel: str | None = Field(None, description="Fuel name")
    c: float | None = Field(None, gt=0, description="Carbon atoms")
    h: float | None = Field(None, gt=0, description="Hydrogen atoms")
    o: float | None = Field(None, ge=0, description="Oxygen atoms")
    n: float | None = Field(None, ge=0, description="Nitrogen atoms")
    heat_of_combustion: float | None = Field(None, gt=0, description="Heat of combustion [kJ/kg]")
    soot_yield: float = Field(0.01, ge=0, le=1, description="Soot yield")
    co_yield: float = Field(0.0, ge=0, le=1, description="CO yield")
    radiative_fraction: float | None = Field(None, ge=0, le=1, description="Radiative fraction")
    auto_ignition_temperature: float | None = Field(None, description="Auto-ignition temp [°C]")

    @model_validator(mode="after")
    def validate_reaction(self) -> "Reaction":
        """Validate reaction parameters."""
        # Check yields sum
        total_yield = self.soot_yield + self.co_yield
        if total_yield > 1.0:
            raise ValueError(f"Sum of product yields ({total_yield:.2f}) exceeds 1.0")

        return self

    def to_fds(self) -> str:
        """Generate FDS REAC namelist."""
        params: dict[str, Any] = {}

        if self.fuel:
            params["fuel"] = self.fuel
        if self.c is not None:
            params["c"] = self.c
        if self.h is not None:
            params["h"] = self.h
        if self.o and self.o > 0:
            params["o"] = self.o
        if self.n and self.n > 0:
            params["n"] = self.n
        if self.heat_of_combustion:
            params["heat_of_combustion"] = self.heat_of_combustion
        if self.soot_yield != 0.01:
            params["soot_yield"] = self.soot_yield
        if self.co_yield > 0:
            params["co_yield"] = self.co_yield
        if self.radiative_fraction is not None:
            params["radiative_fraction"] = self.radiative_fraction
        if self.auto_ignition_temperature is not None:
            params["auto_ignition_temperature"] = self.auto_ignition_temperature

        return self._build_namelist("REAC", params)


class Prop(NamelistBase):
    """
    FDS PROP namelist - device properties.

    Defines properties for sprinklers, smoke detectors, heat detectors, and
    other devices.

    Parameters
    ----------
    id : str
        Unique property identifier
    quantity : str, optional
        Measured quantity (e.g., 'TEMPERATURE', 'CHAMBER_OBSCURATION')
    activation_temperature : float, optional
        Activation temperature for heat detectors/sprinklers [°C]
    activation_obscuration : float, optional
        Activation obscuration for smoke detectors [%/m]
    rti : float, optional
        Response Time Index [m^0.5·s^0.5]
    flow_rate : float, optional
        Sprinkler flow rate [L/min]
    k_factor : float, optional
        Sprinkler K-factor [(L/min)/bar^0.5]
    spray_angle : tuple[float, float], optional
        Spray angle limits [degrees]

    Examples
    --------
    >>> # Sprinkler property
    >>> sprinkler = Prop(
    ...     id='SPRINKLER',
    ...     activation_temperature=68,
    ...     rti=50,
    ...     flow_rate=60
    ... )

    >>> # Smoke detector property
    >>> detector = Prop(
    ...     id='SMOKE_DETECTOR',
    ...     quantity='CHAMBER_OBSCURATION',
    ...     activation_obscuration=3.28
    ... )
    """

    id: str = Field(..., description="Property identifier")
    quantity: str | None = Field(None, description="Measured quantity")
    activation_temperature: float | None = Field(None, description="Activation temperature [°C]")
    activation_obscuration: float | None = Field(
        None, ge=0, description="Activation obscuration [%/m]"
    )
    rti: float | None = Field(None, gt=0, description="Response Time Index")
    flow_rate: float | None = Field(None, gt=0, description="Flow rate [L/min]")
    k_factor: float | None = Field(None, gt=0, description="K-factor")
    spray_angle: tuple[float, float] | None = Field(None, description="Spray angle [degrees]")

    def to_fds(self) -> str:
        """Generate FDS PROP namelist."""
        params: dict[str, Any] = {"id": self.id}

        if self.quantity:
            params["quantity"] = self.quantity
        if self.activation_temperature is not None:
            params["activation_temperature"] = self.activation_temperature
        if self.activation_obscuration is not None:
            params["activation_obscuration"] = self.activation_obscuration
        if self.rti is not None:
            params["rti"] = self.rti
        if self.flow_rate is not None:
            params["flow_rate"] = self.flow_rate
        if self.k_factor is not None:
            params["k_factor"] = self.k_factor
        if self.spray_angle:
            params["spray_angle"] = self.spray_angle

        return self._build_namelist("PROP", params)


class ControlFunction(str, Enum):
    """Control function types."""

    ANY = "ANY"
    ALL = "ALL"
    ONLY = "ONLY"
    TIME_DELAY = "TIME_DELAY"
    CUSTOM = "CUSTOM"
    KILL = "KILL"
    RESTART = "RESTART"


class Ctrl(NamelistBase):
    """
    FDS CTRL namelist - control logic.

    Implements control logic for devices based on inputs from other devices
    or control functions.

    Parameters
    ----------
    id : str
        Unique control identifier
    function_type : ControlFunction
        Type of control function
    input_id : str | list[str], optional
        Input device or control ID(s)
    delay : float, optional
        Time delay [s], default: 0.0
    initial_state : bool, optional
        Initial state, default: False
    latch : bool, optional
        Whether to latch on activation, default: True

    Examples
    --------
    >>> # ANY logic - activate if any input is true
    >>> ctrl = Ctrl(
    ...     id='SMOKE_ALARM',
    ...     function_type=ControlFunction.ANY,
    ...     input_id=['SD_1', 'SD_2', 'SD_3']
    ... )

    >>> # Time delay
    >>> ctrl = Ctrl(
    ...     id='DELAYED_ACTIVATION',
    ...     function_type=ControlFunction.TIME_DELAY,
    ...     input_id='SPRINKLER_1',
    ...     delay=5.0
    ... )
    """

    id: str = Field(..., description="Control identifier")
    function_type: ControlFunction = Field(..., description="Function type")
    input_id: str | list[str] | None = Field(None, description="Input device ID(s)")
    delay: float = Field(0.0, ge=0, description="Time delay [s]")
    initial_state: bool = Field(False, description="Initial state")
    latch: bool = Field(True, description="Latch on activation")

    @model_validator(mode="after")
    def validate_ctrl(self) -> "Ctrl":
        """Validate control parameters."""
        # ANY and ALL require multiple inputs
        if self.function_type in [ControlFunction.ANY, ControlFunction.ALL] and not isinstance(
            self.input_id, list
        ):
            raise ValueError(
                f"Control '{self.id}': {self.function_type.value} requires multiple INPUT_ID"
            )

        return self

    def to_fds(self) -> str:
        """Generate FDS CTRL namelist."""
        params: dict[str, Any] = {"id": self.id, "function_type": self.function_type.value}

        if self.input_id:
            params["input_id"] = self.input_id
        if self.delay > 0:
            params["delay"] = self.delay
        if self.initial_state:
            params["initial_state"] = self.initial_state
        if not self.latch:
            params["latch"] = self.latch

        return self._build_namelist("CTRL", params)


class Init(NamelistBase):
    """
    FDS INIT namelist - initial conditions.

    Specifies initial conditions for temperature, density, and species
    concentrations in a region.

    Parameters
    ----------
    xb : tuple[float, float, float, float, float, float], optional
        Region bounds (xmin, xmax, ymin, ymax, zmin, zmax)
    xyz : tuple[float, float, float], optional
        Point location (x, y, z)
    temperature : float, optional
        Initial temperature [°C]
    density : float, optional
        Initial density [kg/m³]
    mass_fraction : list[float], optional
        Species mass fractions
    volume_fraction : list[float], optional
        Species volume fractions
    spec_id : list[str], optional
        Species identifiers

    Examples
    --------
    >>> # Hot gas region
    >>> init = Init(
    ...     xb=(0, 10, 0, 10, 0, 0.1),
    ...     temperature=500,
    ...     spec_id=['PROPANE'],
    ...     mass_fraction=[0.05]
    ... )
    """

    xb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Region bounds"
    )
    xyz: tuple[float, float, float] | None = Field(None, description="Point location")
    temperature: float | None = Field(None, description="Temperature [°C]")
    density: float | None = Field(None, gt=0, description="Density [kg/m³]")
    mass_fraction: list[float] | None = Field(None, description="Mass fractions")
    volume_fraction: list[float] | None = Field(None, description="Volume fractions")
    spec_id: list[str] | None = Field(None, description="Species IDs")

    @model_validator(mode="after")
    def validate_init(self) -> "Init":
        """Validate initial conditions."""
        # Must specify either XB or XYZ
        if self.xb is None and self.xyz is None:
            raise ValueError("INIT requires either XB or XYZ")

        # If species are specified, need concentrations
        if self.spec_id:
            if not (self.mass_fraction or self.volume_fraction):
                raise ValueError("INIT with SPEC_ID requires MASS_FRACTION or VOLUME_FRACTION")
            if self.mass_fraction and len(self.mass_fraction) != len(self.spec_id):
                raise ValueError("MASS_FRACTION length must match SPEC_ID length")
            if self.volume_fraction and len(self.volume_fraction) != len(self.spec_id):
                raise ValueError("VOLUME_FRACTION length must match SPEC_ID length")

        return self

    def to_fds(self) -> str:
        """Generate FDS INIT namelist."""
        params: dict[str, Any] = {}

        if self.xb:
            params["xb"] = self.xb
        if self.xyz:
            params["xyz"] = self.xyz
        if self.temperature is not None:
            params["temperature"] = self.temperature
        if self.density is not None:
            params["density"] = self.density
        if self.spec_id:
            params["spec_id"] = self.spec_id
        if self.mass_fraction:
            params["mass_fraction"] = self.mass_fraction
        if self.volume_fraction:
            params["volume_fraction"] = self.volume_fraction

        return self._build_namelist("INIT", params)


class VentType(str, Enum):
    """Types of vents in FDS."""

    OPEN = "OPEN"
    HVAC = "HVAC"
    SURFACE = "SURFACE"
    MIRROR = "MIRROR"
    PERIODIC = "PERIODIC"


class VentShape(str, Enum):
    """Vent geometry types."""

    RECTANGULAR = "RECTANGULAR"
    CIRCULAR = "CIRCULAR"
    ANNULAR = "ANNULAR"


class Vent(NamelistBase):
    """
    FDS VENT namelist - boundary conditions and openings.

    Handles various types of vents including openings to ambient, HVAC systems,
    surface patches, and mesh boundary conditions. Supports both rectangular
    and circular geometries.

    Parameters
    ----------
    xb : tuple[float, float, float, float, float, float], optional
        Bounding box coordinates (xmin, xmax, ymin, ymax, zmin, zmax)
    mb : str, optional
        Mesh boundary location ('XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX')
    surf_id : str, optional
        Surface properties ID, default: 'INERT'
    id : str, optional
        Vent identifier

    Geometric Parameters
    -------------------
    xyz : tuple[float, float, float], optional
        Center point for circular vents
    radius : float, optional
        Radius for circular vents [m]
    radius_inner : float, optional
        Inner radius for annular vents [m]

    HVAC Parameters
    --------------
    volume_flow : float, optional
        Volume flow rate [m³/s] (positive=inflow, negative=outflow)
    mass_flow : float, optional
        Mass flow rate [kg/s]
    vel : float, optional
        Velocity [m/s]

    Control Parameters
    -----------------
    devc_id : str, optional
        Device ID for activation
    ctrl_id : str, optional
        Control ID for activation
    delay : float, optional
        Activation delay [s], default: 0.0
    t_activate : float, optional
        Activation time [s]

    Advanced Parameters
    ------------------
    dynamic_pressure : bool, optional
        Use dynamic pressure boundary condition, default: False
    tmp_exterior : float, optional
        Exterior temperature [°C]
    color : str, optional
        Named color for visualization
    rgb : tuple[int, int, int], optional
        RGB color values (0-255)
    transparency : float, optional
        Transparency value [0-1], default: 1.0

    Examples
    --------
    >>> # Opening to ambient
    >>> door = Vent(xb=(5, 5, 2, 4, 0, 3), surf_id='OPEN')

    >>> # HVAC supply vent
    >>> supply = Vent(xb=(5, 6, 5, 6, 3, 3), surf_id='HVAC', volume_flow=0.5)

    >>> # Circular burner
    >>> burner = Vent(
    ...     xb=(-1, 1, -1, 1, 0, 0),
    ...     surf_id='FIRE',
    ...     xyz=(0, 0, 0),
    ...     radius=0.5
    ... )

    >>> # Mesh boundary vent
    >>> boundary = Vent(mb='XMIN', surf_id='OPEN')

    Notes
    -----
    - VENTs must be planar (exactly one dimension in XB must be zero)
    - HVAC vents can specify only one of: volume_flow, mass_flow, or vel
    - Circular vents require both xyz and radius
    - For mesh boundaries, use mb instead of xb
    """

    # Geometry
    xb: tuple[float, float, float, float, float, float] | None = Field(
        None, description="Bounding box"
    )
    mb: str | None = Field(None, description="Mesh boundary")
    surf_id: str = Field("INERT", description="Surface ID")
    id: str | None = Field(None, description="Vent identifier")

    # Circular geometry
    xyz: tuple[float, float, float] | None = Field(None, description="Center point")
    radius: float | None = Field(None, gt=0, description="Radius [m]")
    radius_inner: float | None = Field(None, gt=0, description="Inner radius [m]")

    # HVAC parameters
    volume_flow: float | None = Field(None, description="Volume flow rate [m³/s]")
    mass_flow: float | None = Field(None, description="Mass flow rate [kg/s]")
    vel: float | None = Field(None, description="Velocity [m/s]")

    # Control
    devc_id: str | None = Field(None, description="Device ID for activation")
    ctrl_id: str | None = Field(None, description="Control ID for activation")
    delay: float = Field(0.0, ge=0, description="Activation delay [s]")
    t_activate: float | None = Field(None, description="Activation time [s]")

    # Mesh reference
    mesh_id: str | None = Field(None, description="Mesh ID for MB")

    # Advanced
    dynamic_pressure: bool = Field(False, description="Dynamic pressure BC")
    tmp_exterior: float | None = Field(None, description="Exterior temperature [°C]")

    # Visualization
    color: str | None = Field(None, description="Named color")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color")
    transparency: float = Field(1.0, ge=0, le=1, description="Transparency [0-1]")

    @model_validator(mode="after")
    def validate_vent(self) -> "Vent":
        """Validate vent parameters."""
        # Must have either XB or MB
        if not self.xb and not self.mb:
            raise ValueError("Vent must have either XB or MB specified")

        # XB validation - must define a plane
        if self.xb:
            dims = [
                abs(self.xb[1] - self.xb[0]),
                abs(self.xb[3] - self.xb[2]),
                abs(self.xb[5] - self.xb[4]),
            ]
            zero_dims = sum(1 for d in dims if d < 1e-6)
            if zero_dims != 1:
                raise ValueError(
                    f"Vent XB must be a plane (exactly one dimension zero), got dimensions: {dims}"
                )

        # Circular vent validation
        if self.radius:
            if not self.xyz:
                raise ValueError("Circular vent requires both XYZ and RADIUS")
            if self.radius_inner and self.radius_inner >= self.radius:
                raise ValueError(
                    f"RADIUS_INNER ({self.radius_inner}) must be less than RADIUS ({self.radius})"
                )

        # HVAC validation - only one flow parameter
        if self.surf_id == "HVAC":
            flow_params = [self.volume_flow, self.mass_flow, self.vel]
            if sum(p is not None for p in flow_params) > 1:
                raise ValueError(
                    "HVAC vent can only specify one of: VOLUME_FLOW, MASS_FLOW, or VEL"
                )

        # MB validation
        if self.mb:
            valid_mb = ["XMIN", "XMAX", "YMIN", "YMAX", "ZMIN", "ZMAX"]
            if self.mb not in valid_mb:
                raise ValueError(f"MB must be one of {valid_mb}, got '{self.mb}'")

        # RGB validation
        if self.rgb:
            if len(self.rgb) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if any(val < 0 or val > 255 for val in self.rgb):
                raise ValueError("RGB values must be in range 0-255")

        return self

    def get_vent_type(self) -> VentType:
        """Determine the type of vent based on surf_id."""
        if self.surf_id == "OPEN":
            return VentType.OPEN
        if self.surf_id == "HVAC":
            return VentType.HVAC
        if self.surf_id == "MIRROR":
            return VentType.MIRROR
        if self.surf_id == "PERIODIC":
            return VentType.PERIODIC
        return VentType.SURFACE

    def get_shape(self) -> VentShape:
        """Determine the shape of the vent."""
        if self.xyz and self.radius:
            if self.radius_inner:
                return VentShape.ANNULAR
            return VentShape.CIRCULAR
        return VentShape.RECTANGULAR

    def get_area(self) -> float | None:
        """
        Calculate vent area based on geometry.

        Returns
        -------
        float or None
            Area in m², or None if cannot be calculated
        """
        shape = self.get_shape()

        if shape == VentShape.RECTANGULAR and self.xb:
            dx = abs(self.xb[1] - self.xb[0])
            dy = abs(self.xb[3] - self.xb[2])
            dz = abs(self.xb[5] - self.xb[4])

            # Area is the non-zero face
            if dx < 1e-6:
                return dy * dz
            if dy < 1e-6:
                return dx * dz
            if dz < 1e-6:
                return dx * dy

        elif shape == VentShape.CIRCULAR and self.radius:
            return float(np.pi * self.radius**2)

        elif shape == VentShape.ANNULAR and self.radius and self.radius_inner:
            return float(np.pi * (self.radius**2 - self.radius_inner**2))

        return None

    def to_fds(self) -> str:
        """Generate FDS VENT namelist."""
        params: dict[str, Any] = {}

        if self.id:
            params["id"] = self.id
        if self.xb:
            params["xb"] = self.xb
        if self.mb:
            params["mb"] = self.mb
            if self.mesh_id:
                params["mesh_id"] = self.mesh_id

        params["surf_id"] = self.surf_id

        if self.xyz:
            params["xyz"] = self.xyz
        if self.radius is not None:
            params["radius"] = self.radius
        if self.radius_inner is not None:
            params["radius_inner"] = self.radius_inner

        if self.volume_flow is not None:
            params["volume_flow"] = self.volume_flow
        if self.mass_flow is not None:
            params["mass_flow"] = self.mass_flow
        if self.vel is not None:
            params["vel"] = self.vel

        if self.devc_id:
            params["devc_id"] = self.devc_id
        if self.ctrl_id:
            params["ctrl_id"] = self.ctrl_id
        if self.delay > 0:
            params["delay"] = self.delay
        if self.t_activate is not None:
            params["t_activate"] = self.t_activate

        if self.dynamic_pressure:
            params["dynamic_pressure"] = self.dynamic_pressure
        if self.tmp_exterior is not None:
            params["tmp_exterior"] = self.tmp_exterior

        if self.color:
            params["color"] = self.color
        if self.rgb:
            params["rgb"] = self.rgb
        if self.transparency != 1.0:
            params["transparency"] = self.transparency

        return self._build_namelist("VENT", params)


class TurbulenceModel(str, Enum):
    """LES turbulence models."""

    DEARDORFF = "DEARDORFF"
    DYNAMIC_SMAGORINSKY = "DYNAMIC SMAGORINSKY"
    VREMAN = "VREMAN"
    WALE = "WALE"


class Misc(NamelistBase):
    """
    FDS MISC namelist - miscellaneous parameters.

    Contains global simulation parameters that affect physics, numerics,
    and solver behavior. Only one MISC namelist is allowed per simulation.

    Ambient Conditions
    -----------------
    tmpa : float, optional
        Ambient temperature [°C], default: 20.0
    p_inf : float, optional
        Background pressure [Pa], default: 101325.0
    humidity : float, optional
        Relative humidity [%], default: 40.0
    gvec : tuple[float, float, float], optional
        Gravity vector [m/s²], default: (0.0, 0.0, -9.81)

    Turbulence Parameters
    --------------------
    turbulence_model : TurbulenceModel, optional
        LES turbulence model, default: DEARDORFF
    c_deardorff : float, optional
        Deardorff model constant, default: 0.1
    c_smagorinsky : float, optional
        Smagorinsky constant, default: 0.2
    c_vreman : float, optional
        Vreman model constant, default: 0.07

    Numerical Parameters
    -------------------
    cfl_max : float, optional
        Maximum CFL number, default: 1.0
    cfl_min : float, optional
        Minimum CFL number, default: 0.8

    Solver Options
    -------------
    solid_phase_only : bool, optional
        Only solve solid heat transfer, default: False
    isothermal : bool, optional
        Isothermal flow calculation, default: False
    radiation : bool, optional
        Include radiation, default: True
    stratification : bool, optional
        Include stratification, default: True

    Special Modes
    ------------
    level_set_mode : int, optional
        Wildfire spread mode (0, 1, or 2)
    particle_cfl : bool, optional
        Use particle CFL, default: True

    Restart
    -------
    restart : bool, optional
        Enable restart capability, default: False
    restart_chid : str, optional
        CHID for restart file

    Examples
    --------
    >>> # Standard ambient conditions
    >>> misc = Misc(tmpa=25.0, humidity=70.0)

    >>> # Solid phase only
    >>> misc = Misc(solid_phase_only=True)

    >>> # Wildfire simulation
    >>> misc = Misc(level_set_mode=1, tmpa=35.0, humidity=15.0)

    >>> # Restart configuration
    >>> misc = Misc(restart=True, restart_chid='previous_run')

    Notes
    -----
    - Only one MISC namelist allowed per simulation
    - Some parameters have major performance impacts
    - Cannot use both solid_phase_only and isothermal
    - Only non-default values are written to FDS file
    """

    # Ambient conditions
    tmpa: float = Field(20.0, description="Ambient temperature [°C]")
    p_inf: float = Field(101325.0, gt=0, description="Background pressure [Pa]")
    humidity: float = Field(40.0, ge=0, le=100, description="Relative humidity [%]")
    gvec: tuple[float, float, float] = Field((0.0, 0.0, -9.81), description="Gravity vector [m/s²]")

    # Turbulence model
    turbulence_model: TurbulenceModel = Field(
        TurbulenceModel.DEARDORFF, description="Turbulence model"
    )
    c_deardorff: float = Field(0.1, ge=0, le=1, description="Deardorff constant")
    c_smagorinsky: float = Field(0.2, ge=0, le=1, description="Smagorinsky constant")
    c_vreman: float = Field(0.07, ge=0, le=1, description="Vreman constant")

    # Numerical parameters
    cfl_max: float = Field(1.0, gt=0.1, le=10, description="Maximum CFL number")
    cfl_min: float = Field(0.8, gt=0.1, le=10, description="Minimum CFL number")

    # Solver options
    solid_phase_only: bool = Field(False, description="Solid phase only")
    isothermal: bool = Field(False, description="Isothermal flow")
    radiation: bool = Field(True, description="Include radiation")
    stratification: bool = Field(True, description="Include stratification")

    # Special modes
    level_set_mode: int | None = Field(None, ge=0, le=2, description="Wildfire mode")
    particle_cfl: bool = Field(True, description="Use particle CFL")

    # Restart
    restart: bool = Field(False, description="Enable restart")
    restart_chid: str | None = Field(None, description="Restart CHID")

    @model_validator(mode="after")
    def validate_misc(self) -> "Misc":
        """Validate MISC parameters."""
        # Temperature range check
        if not (-273.15 < self.tmpa < 2000):
            raise ValueError(f"TMPA ({self.tmpa}°C) outside reasonable range [-273.15, 2000]")

        # CFL validation
        if self.cfl_min > self.cfl_max:
            raise ValueError(
                f"CFL_MIN ({self.cfl_min}) must be less than or equal to CFL_MAX ({self.cfl_max})"
            )

        # Mode conflicts
        if self.solid_phase_only and self.isothermal:
            raise ValueError("Cannot use both SOLID_PHASE_ONLY and ISOTHERMAL")

        return self

    def to_fds(self) -> str:
        """
        Generate FDS MISC namelist.

        Only outputs non-default values to keep the file clean.
        """
        params: dict[str, Any] = {}

        # Ambient conditions (only if non-default)
        if self.tmpa != 20.0:
            params["tmpa"] = self.tmpa
        if self.p_inf != 101325.0:
            params["p_inf"] = self.p_inf
        if self.humidity != 40.0:
            params["humidity"] = self.humidity
        if self.gvec != (0.0, 0.0, -9.81):
            params["gvec"] = self.gvec

        # Turbulence model
        if self.turbulence_model != TurbulenceModel.DEARDORFF:
            params["turbulence_model"] = self.turbulence_model.value
        if self.c_deardorff != 0.1:
            params["c_deardorff"] = self.c_deardorff
        if self.c_smagorinsky != 0.2:
            params["c_smagorinsky"] = self.c_smagorinsky
        if self.c_vreman != 0.07:
            params["c_vreman"] = self.c_vreman

        # CFL parameters
        if self.cfl_max != 1.0:
            params["cfl_max"] = self.cfl_max
        if self.cfl_min != 0.8:
            params["cfl_min"] = self.cfl_min

        # Solver options
        if self.solid_phase_only:
            params["solid_phase_only"] = self.solid_phase_only
        if self.isothermal:
            params["isothermal"] = self.isothermal
        if not self.radiation:
            params["radiation"] = self.radiation
        if not self.stratification:
            params["stratification"] = self.stratification

        # Special modes
        if self.level_set_mode is not None:
            params["level_set_mode"] = self.level_set_mode
        if not self.particle_cfl:
            params["particle_cfl"] = self.particle_cfl

        # Restart
        if self.restart:
            params["restart"] = self.restart
            if self.restart_chid:
                params["restart_chid"] = self.restart_chid

        return self._build_namelist("MISC", params)
