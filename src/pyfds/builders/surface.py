"""Builder for creating SURF namelists with fluent API."""

from ..core.namelists import Surface
from .base import Builder


class SurfaceBuilder(Builder[Surface]):
    """
    Builder for creating SURF namelists.

    Provides a fluent API for constructing surfaces with thermal properties,
    burning behavior, flow conditions, and material layers.

    The builder groups the 150+ SURF parameters into logical method categories:
    - Identification: `.id()`, `.color()`, `.rgb()`
    - Temperature: `.temperature()`, `.adiabatic()`
    - Heat transfer: `.heat_transfer()`
    - Burning/Fire: `.burning()`, `.fire_spread()`
    - Flow/HVAC: `.flow()`
    - Material layers: `.layer()`
    - Radiation: `.emissivity()`

    Examples
    --------
    >>> # Simple fire surface
    >>> fire = SurfaceBuilder() \\
    ...     .id("FIRE") \\
    ...     .burning(hrrpua=1000.0) \\
    ...     .color("RED") \\
    ...     .build()

    >>> # Thermal boundary with fixed temperature
    >>> hot_wall = SurfaceBuilder() \\
    ...     .id("HOT_WALL") \\
    ...     .temperature(tmp_front=200.0) \\
    ...     .build()

    >>> # Multi-layer wall
    >>> wall = SurfaceBuilder() \\
    ...     .id("INSULATED_WALL") \\
    ...     .layer("CONCRETE", thickness=0.2) \\
    ...     .layer("INSULATION", thickness=0.05) \\
    ...     .layer("GYPSUM", thickness=0.013) \\
    ...     .backing("EXPOSED") \\
    ...     .build()

    >>> # HVAC supply vent
    >>> supply = SurfaceBuilder() \\
    ...     .id("SUPPLY") \\
    ...     .flow(volume_flow=0.5) \\
    ...     .temperature(tmp_front=20.0) \\
    ...     .build()

    >>> # Ramped fire growth
    >>> fire = SurfaceBuilder() \\
    ...     .id("GROWING_FIRE") \\
    ...     .burning(hrrpua=2500.0, ramp_q="FIRE_RAMP") \\
    ...     .build()
    """

    def __init__(self) -> None:
        """Initialize the SurfaceBuilder."""
        super().__init__()
        self._params: dict = {}
        self._matl_ids: list[str] = []
        self._thicknesses: list[float] = []

    # === Identification ===

    def id(self, surface_id: str) -> "SurfaceBuilder":
        """
        Set surface identifier (required).

        Parameters
        ----------
        surface_id : str
            Unique surface identifier

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfaceBuilder().id("MY_SURFACE").build()
        """
        self._params["id"] = surface_id
        return self

    def color(self, color: str) -> "SurfaceBuilder":
        """
        Set named color for visualization.

        Parameters
        ----------
        color : str
            Color name (e.g., 'RED', 'BLUE', 'GREEN', 'GRAY')

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfaceBuilder().id("X").color("RED").build()
        """
        self._params["color"] = color
        return self

    def rgb(self, r: int, g: int, b: int) -> "SurfaceBuilder":
        """
        Set RGB color for visualization.

        Parameters
        ----------
        r : int
            Red component (0-255)
        g : int
            Green component (0-255)
        b : int
            Blue component (0-255)

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfaceBuilder().id("X").rgb(128, 128, 128).build()
        """
        self._params["rgb"] = (r, g, b)
        return self

    def transparency(self, value: float) -> "SurfaceBuilder":
        """
        Set surface transparency.

        Parameters
        ----------
        value : float
            Transparency value (0.0 = opaque, 1.0 = fully transparent)

        Returns
        -------
        SurfaceBuilder
            Self for method chaining
        """
        self._params["transparency"] = value
        return self

    # === Temperature Boundary Conditions ===

    def temperature(
        self,
        tmp_front: float | None = None,
        tmp_back: float | None = None,
        tmp_inner: float | None = None,
        ramp_t: str | None = None,
    ) -> "SurfaceBuilder":
        """
        Set temperature boundary conditions.

        Parameters
        ----------
        tmp_front : float, optional
            Front surface temperature [°C]
        tmp_back : float, optional
            Back surface temperature [°C]
        tmp_inner : float, optional
            Initial solid interior temperature [°C]
        ramp_t : str, optional
            Temperature ramp ID for time-varying front temperature

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> # Fixed temperature wall
        >>> wall = SurfaceBuilder().id("HOT").temperature(tmp_front=100.0).build()

        >>> # Temperature ramp
        >>> wall = SurfaceBuilder().id("HEATING").temperature(
        ...     tmp_front=500.0, ramp_t="TEMP_RAMP"
        ... ).build()
        """
        if tmp_front is not None:
            self._params["tmp_front"] = tmp_front
        if tmp_back is not None:
            self._params["tmp_back"] = tmp_back
        if tmp_inner is not None:
            self._params["tmp_inner"] = tmp_inner
        if ramp_t is not None:
            self._params["ramp_t"] = ramp_t
        return self

    def adiabatic(self) -> "SurfaceBuilder":
        """
        Set surface as adiabatic (no heat transfer).

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> wall = SurfaceBuilder().id("INSULATED").adiabatic().build()
        """
        self._params["adiabatic"] = True
        return self

    # === Heat Transfer ===

    def heat_transfer(
        self,
        coefficient: float | None = None,
        coefficient_back: float | None = None,
        model: str | None = None,
        convection_length_scale: float | None = None,
    ) -> "SurfaceBuilder":
        """
        Set heat transfer parameters.

        Parameters
        ----------
        coefficient : float, optional
            Heat transfer coefficient [W/(m²·K)]
        coefficient_back : float, optional
            Back side heat transfer coefficient [W/(m²·K)]
        model : str, optional
            Heat transfer model: 'LOGLAW' or 'IMPINGING JET'
        convection_length_scale : float, optional
            Characteristic length for convection [m]

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> wall = SurfaceBuilder().id("WALL") \\
        ...     .heat_transfer(coefficient=25.0) \\
        ...     .build()
        """
        if coefficient is not None:
            self._params["heat_transfer_coefficient"] = coefficient
        if coefficient_back is not None:
            self._params["heat_transfer_coefficient_back"] = coefficient_back
        if model is not None:
            self._params["heat_transfer_model"] = model
        if convection_length_scale is not None:
            self._params["convection_length_scale"] = convection_length_scale
        return self

    # === Burning/Fire Properties ===

    def burning(
        self,
        hrrpua: float | None = None,
        mlrpua: float | None = None,
        heat_of_combustion: float | None = None,
        ignition_temperature: float | None = None,
        ramp_q: str | None = None,
        tau_q: float | None = None,
        burn_away: bool | None = None,
    ) -> "SurfaceBuilder":
        """
        Set burning/fire properties.

        Parameters
        ----------
        hrrpua : float, optional
            Heat release rate per unit area [kW/m²]
        mlrpua : float, optional
            Mass loss rate per unit area [kg/(s·m²)]
        heat_of_combustion : float, optional
            Heat of combustion [kJ/kg]
        ignition_temperature : float, optional
            Ignition temperature [°C]
        ramp_q : str, optional
            RAMP ID for heat release rate time history
        tau_q : float, optional
            Time constant for t² fire growth [s]
        burn_away : bool, optional
            Allow surface to burn away when fuel depleted

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> # Constant HRR fire
        >>> fire = SurfaceBuilder().id("FIRE") \\
        ...     .burning(hrrpua=1000.0) \\
        ...     .build()

        >>> # Ramped fire
        >>> fire = SurfaceBuilder().id("FIRE") \\
        ...     .burning(hrrpua=2500.0, ramp_q="FIRE_RAMP") \\
        ...     .build()

        >>> # t² fire growth
        >>> fire = SurfaceBuilder().id("FIRE") \\
        ...     .burning(hrrpua=1000.0, tau_q=300.0) \\
        ...     .build()
        """
        if hrrpua is not None:
            self._params["hrrpua"] = hrrpua
        if mlrpua is not None:
            self._params["mlrpua"] = mlrpua
        if heat_of_combustion is not None:
            self._params["heat_of_combustion"] = heat_of_combustion
        if ignition_temperature is not None:
            self._params["ignition_temperature"] = ignition_temperature
        if ramp_q is not None:
            self._params["ramp_q"] = ramp_q
        if tau_q is not None:
            self._params["tau_q"] = tau_q
        if burn_away is not None:
            self._params["burn_away"] = burn_away
        return self

    def fire_spread(
        self,
        spread_rate: float,
        ignition_point: tuple[float, float, float] | None = None,
    ) -> "SurfaceBuilder":
        """
        Set fire spread parameters.

        Parameters
        ----------
        spread_rate : float
            Radial fire spread rate [m/s]
        ignition_point : tuple[float, float, float], optional
            Ignition point (x, y, z) coordinates

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> fire = SurfaceBuilder().id("SPREADING_FIRE") \\
        ...     .burning(hrrpua=500.0) \\
        ...     .fire_spread(spread_rate=0.01, ignition_point=(5.0, 5.0, 0.0)) \\
        ...     .build()
        """
        self._params["spread_rate"] = spread_rate
        if ignition_point is not None:
            self._params["xyz"] = ignition_point
        return self

    def external_flux(
        self,
        flux: float,
        ramp_ef: str | None = None,
    ) -> "SurfaceBuilder":
        """
        Set external radiative flux (e.g., for cone calorimeter).

        Parameters
        ----------
        flux : float
            External heat flux [kW/m²]
        ramp_ef : str, optional
            Ramp ID for external flux time history

        Returns
        -------
        SurfaceBuilder
            Self for method chaining
        """
        self._params["external_flux"] = flux
        if ramp_ef is not None:
            self._params["ramp_ef"] = ramp_ef
        return self

    # === Flow/HVAC Properties ===

    def flow(
        self,
        vel: float | None = None,
        volume_flow: float | None = None,
        mass_flow: float | None = None,
    ) -> "SurfaceBuilder":
        """
        Set flow properties for HVAC surfaces.

        Parameters
        ----------
        vel : float, optional
            Velocity magnitude [m/s] (negative = into domain)
        volume_flow : float, optional
            Volume flow rate [m³/s]
        mass_flow : float, optional
            Mass flow rate [kg/s]

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Notes
        -----
        Only one of vel, volume_flow, or mass_flow should be specified.

        Examples
        --------
        >>> # Supply vent at 2 m/s
        >>> supply = SurfaceBuilder().id("SUPPLY").flow(vel=2.0).build()

        >>> # Exhaust at 0.5 m³/s
        >>> exhaust = SurfaceBuilder().id("EXHAUST").flow(volume_flow=-0.5).build()
        """
        if vel is not None:
            self._params["vel"] = vel
        if volume_flow is not None:
            self._params["volume_flow"] = volume_flow
        if mass_flow is not None:
            self._params["mass_flow"] = mass_flow
        return self

    # === Material Layers ===

    def layer(
        self,
        matl_id: str,
        thickness: float,
    ) -> "SurfaceBuilder":
        """
        Add a material layer.

        Call multiple times for multi-layer surfaces. Layers are ordered
        from front (exposed) to back.

        Parameters
        ----------
        matl_id : str
            Material ID (must be defined in simulation)
        thickness : float
            Layer thickness [m]

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> # Single layer wall
        >>> wall = SurfaceBuilder().id("WALL") \\
        ...     .layer("CONCRETE", thickness=0.2) \\
        ...     .build()

        >>> # Multi-layer wall (front to back)
        >>> wall = SurfaceBuilder().id("COMPOSITE_WALL") \\
        ...     .layer("GYPSUM", thickness=0.013) \\
        ...     .layer("INSULATION", thickness=0.1) \\
        ...     .layer("CONCRETE", thickness=0.2) \\
        ...     .build()
        """
        self._matl_ids.append(matl_id)
        self._thicknesses.append(thickness)
        return self

    def backing(self, condition: str) -> "SurfaceBuilder":
        """
        Set backing condition for solid.

        Parameters
        ----------
        condition : str
            Backing condition:
            - 'VOID': Back surface at ambient
            - 'INSULATED': Adiabatic back surface
            - 'EXPOSED': Back surface exposed to gas phase

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> wall = SurfaceBuilder().id("WALL") \\
        ...     .layer("CONCRETE", 0.2) \\
        ...     .backing("INSULATED") \\
        ...     .build()
        """
        self._params["backing"] = condition
        return self

    # === Radiation ===

    def emissivity(
        self,
        front: float,
        back: float | None = None,
    ) -> "SurfaceBuilder":
        """
        Set surface emissivity.

        Parameters
        ----------
        front : float
            Front surface emissivity (0-1)
        back : float, optional
            Back surface emissivity (0-1)

        Returns
        -------
        SurfaceBuilder
            Self for method chaining

        Examples
        --------
        >>> wall = SurfaceBuilder().id("WALL").emissivity(front=0.9).build()
        """
        self._params["emissivity"] = front
        if back is not None:
            self._params["emissivity_back"] = back
        return self

    # === Advanced Options ===

    def default(self) -> "SurfaceBuilder":
        """
        Mark as default surface for unmarked boundaries.

        Returns
        -------
        SurfaceBuilder
            Self for method chaining
        """
        self._params["default"] = True
        return self

    def ht3d(self) -> "SurfaceBuilder":
        """
        Enable 3D heat conduction.

        Returns
        -------
        SurfaceBuilder
            Self for method chaining
        """
        self._params["ht3d"] = True
        return self

    def tga_analysis(
        self,
        heating_rate: float = 5.0,
        final_temperature: float = 800.0,
    ) -> "SurfaceBuilder":
        """
        Configure for TGA (Thermogravimetric Analysis) mode.

        Parameters
        ----------
        heating_rate : float, optional
            Heating rate [K/min], default: 5.0
        final_temperature : float, optional
            Final temperature [°C], default: 800.0

        Returns
        -------
        SurfaceBuilder
            Self for method chaining
        """
        self._params["tga_analysis"] = True
        self._params["tga_heating_rate"] = heating_rate
        self._params["tga_final_temperature"] = final_temperature
        return self

    def geometry(
        self,
        shape: str,
        radius: float | None = None,
        length: float | None = None,
        inner_radius: float | None = None,
    ) -> "SurfaceBuilder":
        """
        Set solid phase geometry.

        Parameters
        ----------
        shape : str
            Geometry type: 'CARTESIAN', 'CYLINDRICAL', 'SPHERICAL', 'INNER CYLINDRICAL'
        radius : float, optional
            Radius for cylindrical/spherical geometry [m]
        length : float, optional
            Length for cylindrical geometry [m]
        inner_radius : float, optional
            Inner radius for hollow cylinder [m]

        Returns
        -------
        SurfaceBuilder
            Self for method chaining
        """
        self._params["geometry"] = shape
        if radius is not None:
            self._params["radius"] = radius
        if length is not None:
            self._params["length"] = length
        if inner_radius is not None:
            self._params["inner_radius"] = inner_radius
        return self

    # === Build ===

    def build(self) -> Surface:
        """
        Build the Surface object.

        Returns
        -------
        Surface
            The constructed Surface namelist object

        Raises
        ------
        ValueError
            If required parameters are missing (id is required)
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        if "id" not in self._params:
            raise ValueError("SurfaceBuilder: id is required (use .id())")

        # Handle material layers
        if self._matl_ids:
            if len(self._matl_ids) == 1:
                self._params["matl_id"] = self._matl_ids[0]
                self._params["thickness"] = self._thicknesses[0]
            else:
                self._params["matl_id"] = self._matl_ids
                self._params["thickness"] = self._thicknesses

        surface = Surface(**self._params)
        self._mark_built()
        return surface
