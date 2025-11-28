"""Builder for creating SURF namelists with fire and heat transfer properties."""

from ..core.namelists import Surface
from .base import Builder


class SurfBuilder(Builder[Surface]):
    """
    Builder for creating SURF namelists.

    Provides a fluent API for constructing surface boundary conditions
    with fire properties, heat transfer, and pyrolysis parameters.

    Parameters
    ----------
    id : str
        Unique identifier for the surface

    Examples
    --------
    >>> # Simple fire surface with constant HRR
    >>> fire = SurfBuilder('FIRE') \\
    ...     .with_heat_release(500.0) \\
    ...     .with_color('RED') \\
    ...     .build()

    >>> # Fire with ramped heat release
    >>> ramped_fire = SurfBuilder('RAMPED_FIRE') \\
    ...     .with_heat_release(1000.0, ramp_id='fire_ramp') \\
    ...     .with_radiation(emissivity=0.9) \\
    ...     .build()

    >>> # Surface with mass flux and ignition
    >>> burner = SurfBuilder('BURNER') \\
    ...     .with_mass_flux(0.01, ramp_id='fuel_ramp') \\
    ...     .with_ignition(temperature=300.0, burn_away=True) \\
    ...     .build()
    """

    def __init__(self, id: str):
        """
        Initialize the SurfBuilder.

        Parameters
        ----------
        id : str
            Unique identifier for the surface
        """
        super().__init__()
        self._id = id
        self._params: dict = {}

    def with_color(self, color: str) -> "SurfBuilder":
        """
        Set surface color for visualization.

        Parameters
        ----------
        color : str
            Named color (e.g., 'RED', 'BLUE', 'GREEN')

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["color"] = color
        return self

    def with_rgb(self, r: int, g: int, b: int) -> "SurfBuilder":
        """
        Set RGB color values.

        Parameters
        ----------
        r : int
            Red value (0-255)
        g : int
            Green value (0-255)
        b : int
            Blue value (0-255)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["rgb"] = (r, g, b)
        return self

    def with_heat_release(self, hrrpua: float, ramp_id: str | None = None) -> "SurfBuilder":
        """
        Set heat release rate per unit area.

        Parameters
        ----------
        hrrpua : float
            Heat release rate per unit area (kW/m²)
        ramp_id : str, optional
            RAMP_ID for time-dependent heat release

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('FIRE').with_heat_release(500.0).build()
        >>> surf = SurfBuilder('GROWING_FIRE').with_heat_release(1000.0, ramp_id='t2_fast').build()
        """
        self._params["hrrpua"] = hrrpua
        if ramp_id:
            self._params["ramp_q"] = ramp_id
        return self

    def with_mass_flux(self, mlrpua: float, ramp_id: str | None = None) -> "SurfBuilder":
        """
        Set mass flux per unit area.

        Parameters
        ----------
        mlrpua : float
            Mass loss rate per unit area (kg/s/m²)
        ramp_id : str, optional
            RAMP_ID for time-dependent mass flux

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('BURNER').with_mass_flux(0.01).build()
        >>> surf = SurfBuilder('FUEL').with_mass_flux(0.02, ramp_id='fuel_ramp').build()
        """
        self._params["mlrpua"] = mlrpua
        if ramp_id:
            self._params["ramp_mf"] = ramp_id
        return self

    def with_temperature(self, tmp_front: float) -> "SurfBuilder":
        """
        Set front surface temperature.

        Parameters
        ----------
        tmp_front : float
            Front surface temperature (°C)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["tmp_front"] = tmp_front
        return self

    def with_material(self, matl_id: str, thickness: float) -> "SurfBuilder":
        """
        Set material and thickness for heat transfer.

        Parameters
        ----------
        matl_id : str
            Material identifier
        thickness : float
            Material thickness (m)

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('WALL').with_material('CONCRETE', 0.2).build()
        """
        self._params["matl_id"] = matl_id
        self._params["thickness"] = thickness
        return self

    def with_multi_layer_material(
        self,
        layers: list[dict],
        backing: str = "EXPOSED",
    ) -> "SurfBuilder":
        """
        Configure multi-layer material stack.

        Parameters
        ----------
        layers : list[dict]
            List of layer definitions. Each dict should contain:
            - 'matl_id': str or list[str] - Material ID(s) for this layer
            - 'thickness': float - Layer thickness in meters
            - 'mass_fraction': list[float], optional - Mass fractions for multi-component
        backing : str, optional
            Backing condition: 'VOID', 'INSULATED', or 'EXPOSED' (default)

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> wall = SurfBuilder("COMPOSITE_WALL").with_multi_layer_material(
        ...     layers=[
        ...         {"matl_id": "GYPSUM", "thickness": 0.013},
        ...         {"matl_id": "INSULATION", "thickness": 0.1},
        ...         {"matl_id": "GYPSUM", "thickness": 0.013},
        ...     ],
        ...     backing="EXPOSED"
        ... ).build()

        >>> # Multi-component layer
        >>> composite = SurfBuilder("MULTI_COMP").with_multi_layer_material(
        ...     layers=[
        ...         {
        ...             "matl_id": ["CALCIUM_SILICATE", "ITE"],
        ...             "thickness": 0.025,
        ...             "mass_fraction": [0.68, 0.32]
        ...         },
        ...     ]
        ... ).build()
        """
        if not layers:
            raise ValueError("At least one layer must be specified")

        matl_ids = []
        thicknesses = []
        mass_fractions = []
        has_multi_component = False

        for i, layer in enumerate(layers):
            if "matl_id" not in layer:
                raise ValueError(f"Layer {i + 1}: 'matl_id' is required")
            if "thickness" not in layer:
                raise ValueError(f"Layer {i + 1}: 'thickness' is required")

            matl_id = layer["matl_id"]
            thickness = layer["thickness"]
            mass_frac = layer.get("mass_fraction")

            if isinstance(matl_id, list):
                has_multi_component = True
                matl_ids.append(matl_id)
                if mass_frac is None:
                    raise ValueError(
                        f"Layer {i + 1}: 'mass_fraction' required for multi-component layer"
                    )
                if len(mass_frac) != len(matl_id):
                    raise ValueError(
                        f"Layer {i + 1}: mass_fraction length must match matl_id length"
                    )
                mass_fractions.append(mass_frac)
            else:
                matl_ids.append(matl_id)
                if mass_frac:
                    mass_fractions.append(mass_frac)

            thicknesses.append(thickness)

        self._params["matl_id"] = matl_ids
        self._params["thickness"] = thicknesses
        self._params["backing"] = backing

        if has_multi_component:
            self._params["matl_mass_fraction"] = mass_fractions

        return self

    def with_ignition(self, temperature: float, burn_away: bool = False) -> "SurfBuilder":
        """
        Set ignition properties.

        Parameters
        ----------
        temperature : float
            Ignition temperature (°C)
        burn_away : bool, optional
            Remove surface when material burns away, default: False

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('WOOD').with_ignition(300.0, burn_away=True).build()
        """
        self._params["ignition_temperature"] = temperature
        self._params["burn_away"] = burn_away
        return self

    def with_radiation(self, emissivity: float, absorptivity: float | None = None) -> "SurfBuilder":
        """
        Set radiation properties.

        Parameters
        ----------
        emissivity : float
            Surface emissivity (0-1)
        absorptivity : float, optional
            Surface absorptivity (0-1)

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('STEEL').with_radiation(emissivity=0.7).build()
        >>> surf = SurfBuilder('BLACK').with_radiation(emissivity=0.9, absorptivity=0.9).build()
        """
        self._params["emissivity"] = emissivity
        if absorptivity is not None:
            self._params["absorptivity"] = absorptivity
        return self

    def with_backing(self, backing: str) -> "SurfBuilder":
        """
        Set backing condition.

        Parameters
        ----------
        backing : str
            Backing condition: 'VOID', 'INSULATED', or 'EXPOSED'

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('WALL').with_backing('INSULATED').build()
        """
        self._params["backing"] = backing
        return self

    def with_heat_of_combustion(self, hoc: float) -> "SurfBuilder":
        """
        Set heat of combustion.

        Parameters
        ----------
        hoc : float
            Heat of combustion (kJ/kg)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["heat_of_combustion"] = hoc
        return self

    def with_convective_heat_flux(self, flux: float) -> "SurfBuilder":
        """
        Set convective heat flux.

        Parameters
        ----------
        flux : float
            Convective heat flux (kW/m²)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["convective_heat_flux"] = flux
        return self

    def with_velocity(self, vel: float) -> "SurfBuilder":
        """
        Set surface velocity.

        Parameters
        ----------
        vel : float
            Velocity (m/s)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["vel"] = vel
        return self

    def with_volume_flow(self, flow: float) -> "SurfBuilder":
        """
        Set volume flow rate.

        Parameters
        ----------
        flow : float
            Volume flow rate (m³/s)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["volume_flow"] = flow
        return self

    def with_particle_generation(
        self,
        part_id: str,
        mass_flux: float | None = None,
        nppc: int = 1,
    ) -> "SurfBuilder":
        """
        Configure particle generation from this surface.

        Parameters
        ----------
        part_id : str
            Particle class ID to generate (must match a PART namelist ID)
        mass_flux : float, optional
            Particle mass flux (kg/s/m²)
        nppc : int, optional
            Number of particles per cell, default: 1

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('SPRINKLER').with_particle_generation('WATER_DROP', mass_flux=0.01).build()
        """
        self._params["part_id"] = part_id
        if mass_flux is not None:
            self._params["particle_mass_flux"] = mass_flux
        if nppc != 1:
            self._params["nppc"] = nppc
        return self

    def with_droplet_distribution(
        self,
        median_diameter: float,
        gamma_d: float | None = None,
        spray_pattern: str | None = None,
    ) -> "SurfBuilder":
        """
        Configure droplet size distribution.

        Parameters
        ----------
        median_diameter : float
            Median droplet diameter (m)
        gamma_d : float, optional
            Distribution shape parameter (log-normal distribution)
        spray_pattern : str, optional
            Spray pattern: 'UNIFORM' or 'GAUSSIAN'

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('NOZZLE').with_droplet_distribution(
        ...     median_diameter=0.001,
        ...     gamma_d=2.4,
        ...     spray_pattern='GAUSSIAN'
        ... ).build()
        """
        self._params["median_diameter"] = median_diameter
        if gamma_d is not None:
            self._params["gamma_d"] = gamma_d
        if spray_pattern is not None:
            self._params["spray_pattern"] = spray_pattern
        return self

    def with_particle_velocity(
        self,
        velocity: float | tuple[float, float, float],
    ) -> "SurfBuilder":
        """
        Set particle velocity.

        Parameters
        ----------
        velocity : float or tuple
            Either velocity magnitude (m/s) or velocity vector (vx, vy, vz)

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> # Velocity magnitude
        >>> surf = SurfBuilder('SPRAY').with_particle_velocity(5.0).build()
        >>> # Velocity vector
        >>> surf = SurfBuilder('SPRAY').with_particle_velocity((1.0, 0.0, -2.0)).build()
        """
        if isinstance(velocity, tuple):
            self._params["particle_velocity"] = velocity
        else:
            self._params["vel_part"] = velocity
        return self

    def as_sprinkler(
        self,
        part_id: str,
        mass_flux: float,
        median_diameter: float,
        velocity: float,
    ) -> "SurfBuilder":
        """
        Configure as a sprinkler surface.

        Convenience method that sets up common sprinkler parameters.

        Parameters
        ----------
        part_id : str
            Particle class ID (typically water droplet)
        mass_flux : float
            Particle mass flux (kg/s/m²)
        median_diameter : float
            Median droplet diameter (m)
        velocity : float
            Droplet velocity magnitude (m/s)

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> surf = SurfBuilder('SPRINKLER_HEAD').as_sprinkler(
        ...     part_id='WATER_DROP',
        ...     mass_flux=0.01,
        ...     median_diameter=0.001,
        ...     velocity=5.0
        ... ).build()
        """
        self._params.update(
            {
                "part_id": part_id,
                "particle_mass_flux": mass_flux,
                "median_diameter": median_diameter,
                "vel_part": velocity,
            }
        )
        return self

    # === TEMPERATURE BOUNDARY CONDITIONS (Phase 3) ===
    def with_initial_temperature(
        self,
        tmp_front_initial: float | None = None,
        tmp_inner: float | None = None,
        tmp_back: float | None = None,
        tmp_gas_back: float | None = None,
    ) -> "SurfBuilder":
        """
        Set initial and boundary temperatures.

        Parameters
        ----------
        tmp_front_initial : float, optional
            Initial front surface temperature [°C]
        tmp_inner : float, optional
            Initial solid interior temperature [°C]
        tmp_back : float, optional
            Fixed back surface temperature [°C]
        tmp_gas_back : float, optional
            Back gas temperature for convection [°C]

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        if tmp_front_initial is not None:
            self._params["tmp_front_initial"] = tmp_front_initial
        if tmp_inner is not None:
            self._params["tmp_inner"] = tmp_inner
        if tmp_back is not None:
            self._params["tmp_back"] = tmp_back
        if tmp_gas_back is not None:
            self._params["tmp_gas_back"] = tmp_gas_back
        return self

    def with_temperature_ramps(
        self,
        ramp_t: str | None = None,
        ramp_tmp_back: str | None = None,
        ramp_tmp_gas_front: str | None = None,
        ramp_tmp_gas_back: str | None = None,
        ramp_t_i: str | None = None,
    ) -> "SurfBuilder":
        """
        Set temperature ramps for time-varying boundary conditions.

        Parameters
        ----------
        ramp_t : str, optional
            Temperature ramp ID
        ramp_tmp_back : str, optional
            Back temperature ramp
        ramp_tmp_gas_front : str, optional
            Front gas temperature ramp
        ramp_tmp_gas_back : str, optional
            Back gas temperature ramp
        ramp_t_i : str, optional
            Initial temperature profile ramp

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        if ramp_t is not None:
            self._params["ramp_t"] = ramp_t
        if ramp_tmp_back is not None:
            self._params["ramp_tmp_back"] = ramp_tmp_back
        if ramp_tmp_gas_front is not None:
            self._params["ramp_tmp_gas_front"] = ramp_tmp_gas_front
        if ramp_tmp_gas_back is not None:
            self._params["ramp_tmp_gas_back"] = ramp_tmp_gas_back
        if ramp_t_i is not None:
            self._params["ramp_t_i"] = ramp_t_i
        return self

    # === HEAT TRANSFER (Phase 3) ===
    def with_adiabatic(self, adiabatic: bool = True) -> "SurfBuilder":
        """
        Make surface adiabatic (no heat transfer).

        Parameters
        ----------
        adiabatic : bool, optional
            Whether surface is adiabatic (default: True)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["adiabatic"] = adiabatic
        return self

    def with_heat_transfer_coefficient_back(
        self, htc_back: float, ramp_id: str | None = None
    ) -> "SurfBuilder":
        """
        Set back side heat transfer coefficient.

        Parameters
        ----------
        htc_back : float
            Back side convection coefficient [W/(m²·K)]
        ramp_id : str, optional
            Ramp ID for time-varying HTC

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["heat_transfer_coefficient_back"] = htc_back
        if ramp_id is not None:
            self._params["ramp_heat_transfer_coefficient_back"] = ramp_id
        return self

    def with_heat_transfer_model(
        self,
        model: str,
        length_scale: float = 1.0,
        ramp_htc: str | None = None,
        blowing: bool = False,
    ) -> "SurfBuilder":
        """
        Set heat transfer model and parameters.

        Parameters
        ----------
        model : str
            Heat transfer model: 'LOGLAW' or 'IMPINGING JET'
        length_scale : float, optional
            Characteristic length for convection [m] (default: 1.0)
        ramp_htc : str, optional
            Ramp ID for heat transfer coefficient
        blowing : bool, optional
            Account for mass flux effect on convection (default: False)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["heat_transfer_model"] = model
        self._params["convection_length_scale"] = length_scale
        if ramp_htc is not None:
            self._params["ramp_heat_transfer_coefficient"] = ramp_htc
        self._params["blowing"] = blowing
        return self

    def with_nusselt_correlation(self, c0: float, c1: float, c2: float, m: float) -> "SurfBuilder":
        """
        Set custom Nusselt correlation: Nu = C0 + C1 * Re^M * Pr^C2.

        Parameters
        ----------
        c0, c1, c2 : float
            Nusselt correlation coefficients
        m : float
            Reynolds number exponent

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["nusselt_c0"] = c0
        self._params["nusselt_c1"] = c1
        self._params["nusselt_c2"] = c2
        self._params["nusselt_m"] = m
        return self

    def with_impinging_jet(self, sigma: float) -> "SurfBuilder":
        """
        Configure impinging jet heat transfer.

        Parameters
        ----------
        sigma : float
            Jet width [m]

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["heat_transfer_coefficient_sigma"] = sigma
        return self

    # === EMISSIVITY (Phase 3) ===
    def with_emissivity_back(self, emissivity_back: float) -> "SurfBuilder":
        """
        Set back surface emissivity.

        Parameters
        ----------
        emissivity_back : float
            Back surface emissivity (0-1)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["emissivity_back"] = emissivity_back
        return self

    # === SOLID PHASE GEOMETRY (Phase 3) ===
    def with_geometry(
        self,
        geometry: str,
        inner_radius: float | None = None,
        length: float | None = None,
        radius: float | None = None,
        width: float | None = None,
        horizontal: bool = False,
    ) -> "SurfBuilder":
        """
        Set solid phase geometry parameters.

        Parameters
        ----------
        geometry : str
            Geometry type: 'CARTESIAN', 'CYLINDRICAL', 'SPHERICAL', 'INNER CYLINDRICAL'
        inner_radius : float, optional
            Inner radius for hollow cylinder [m]
        length : float, optional
            Cylinder/particle length [m]
        radius : float, optional
            Cylinder/particle radius [m]
        width : float, optional
            Particle width [m]
        horizontal : bool, optional
            Horizontal cylinder orientation (default: False)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["geometry"] = geometry
        if inner_radius is not None:
            self._params["inner_radius"] = inner_radius
        if length is not None:
            self._params["length"] = length
        if radius is not None:
            self._params["radius"] = radius
        if width is not None:
            self._params["width"] = width
        self._params["horizontal"] = horizontal
        return self

    # === 3D HEAT CONDUCTION (Phase 3) ===
    def with_3d_heat_conduction(self, variable_thickness: bool = False) -> "SurfBuilder":
        """
        Enable 3-D heat conduction.

        Parameters
        ----------
        variable_thickness : bool, optional
            Variable thickness 1-D mode (default: False)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["ht3d"] = True
        self._params["variable_thickness"] = variable_thickness
        return self

    # === NUMERICAL PARAMETERS (Phase 3) ===
    def with_numerical_params(
        self,
        stretch_factor: list[float] | float | None = None,
        cell_size_factor: list[float] | float | None = None,
        cell_size: list[float] | float | None = None,
        n_layer_cells_max: list[int] | int | None = None,
        time_step_factor: float = 10.0,
        delta_tmp_max: float = 10.0,
        minimum_layer_thickness: list[float] | float | None = None,
        minimum_layer_mass_fraction: list[float] | float | None = None,
        remesh_ratio: float = 0.15,
    ) -> "SurfBuilder":
        """
        Set numerical parameters for solid phase heat conduction.

        Parameters
        ----------
        stretch_factor : list[float] | float, optional
            Node spacing stretch factor per layer
        cell_size_factor : list[float] | float, optional
            Cell size multiplier per layer
        cell_size : list[float] | float, optional
            Explicit cell size per layer [m]
        n_layer_cells_max : list[int] | int, optional
            Maximum cells per layer
        time_step_factor : float, optional
            Maximum time step subdivision factor (default: 10.0)
        delta_tmp_max : float, optional
            Maximum temperature change per step [°C] (default: 10.0)
        minimum_layer_thickness : list[float] | float, optional
            Minimum layer thickness [m]
        minimum_layer_mass_fraction : list[float] | float, optional
            Minimum layer mass fraction
        remesh_ratio : float, optional
            Trigger ratio for remeshing (default: 0.15)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        if stretch_factor is not None:
            self._params["stretch_factor"] = stretch_factor
        if cell_size_factor is not None:
            self._params["cell_size_factor"] = cell_size_factor
        if cell_size is not None:
            self._params["cell_size"] = cell_size
        if n_layer_cells_max is not None:
            self._params["n_layer_cells_max"] = n_layer_cells_max
        self._params["time_step_factor"] = time_step_factor
        self._params["delta_tmp_max"] = delta_tmp_max
        if minimum_layer_thickness is not None:
            self._params["minimum_layer_thickness"] = minimum_layer_thickness
        if minimum_layer_mass_fraction is not None:
            self._params["minimum_layer_mass_fraction"] = minimum_layer_mass_fraction
        self._params["remesh_ratio"] = remesh_ratio
        return self

    # === INTERNAL HEAT SOURCE (Phase 3) ===
    def with_internal_heat_source(
        self, heat_source: list[float] | float, ramp_id: str | None = None
    ) -> "SurfBuilder":
        """
        Set internal heat source per layer.

        Parameters
        ----------
        heat_source : list[float] | float
            Internal heat source per layer [kW/m³]
        ramp_id : str, optional
            Ramp ID for time-varying heat source

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["internal_heat_source"] = heat_source
        if ramp_id is not None:
            self._params["ramp_ihs"] = ramp_id
        return self

    # === VISUALIZATION (Phase 3) ===
    def as_default(self) -> "SurfBuilder":
        """
        Mark surface as default boundary condition.

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["default"] = True
        return self

    def with_texture(
        self, texture_map: str, width: float = 1.0, height: float = 1.0, transparency: float = 1.0
    ) -> "SurfBuilder":
        """
        Set texture mapping for visualization.

        Parameters
        ----------
        texture_map : str
            Texture image file path
        width : float, optional
            Texture width [m] (default: 1.0)
        height : float, optional
            Texture height [m] (default: 1.0)
        transparency : float, optional
            Surface transparency (0-1) (default: 1.0)

        Returns
        -------
        SurfBuilder
            Self for method chaining
        """
        self._params["texture_map"] = texture_map
        self._params["texture_width"] = width
        self._params["texture_height"] = height
        self._params["transparency"] = transparency
        return self

    def with_spyro_model(
        self,
        reference_heat_flux: float | list[float],
        ramp_q: str,
        reference_thickness: float | list[float] | None = None,
        inert_q_ref: bool = False,
        maximum_scaling_heat_flux: float = 1500.0,
        minimum_scaling_heat_flux: float = 0.0,
        reference_heat_flux_time_interval: float = 1.0,
    ) -> "SurfBuilder":
        """
        Configure SPyro (Scaling Pyrolysis) model from cone calorimeter data.

        SPyro scales experimental HRR data from cone calorimeter tests to
        different heat flux conditions. The RAMP_Q should contain normalized
        HRR data from the reference test.

        Parameters
        ----------
        reference_heat_flux : float or list[float]
            Heat flux in cone calorimeter test [kW/m²]
            Use list for multiple experiments at different fluxes
        ramp_q : str
            RAMP ID containing normalized HRR(t) from test
        reference_thickness : float or list[float], optional
            Sample thickness in experiment [m]. Defaults to surface THICKNESS.
        inert_q_ref : bool, optional
            True if test data is from inert pyrolysis (no combustion)
        maximum_scaling_heat_flux : float, optional
            Upper limit on scaling heat flux [kW/m²] (default: 1500)
        minimum_scaling_heat_flux : float, optional
            Lower limit on scaling heat flux [kW/m²] (default: 0)
        reference_heat_flux_time_interval : float, optional
            Smoothing window for heat flux [s] (default: 1.0)

        Returns
        -------
        SurfBuilder
            Self for method chaining

        Examples
        --------
        >>> # Using cone calorimeter data at 50 kW/m²
        >>> plywood = SurfBuilder("PLYWOOD") \\
        ...     .with_material("WOOD", thickness=0.012) \\
        ...     .with_spyro_model(
        ...         reference_heat_flux=50.0,
        ...         ramp_q="PLYWOOD_HRR_50",
        ...         reference_thickness=0.012
        ...     ) \\
        ...     .with_ignition(temperature=300) \\
        ...     .build()

        >>> # Multiple experiments at different heat fluxes
        >>> composite = SurfBuilder("COMPOSITE") \\
        ...     .with_multi_layer_material([...]) \\
        ...     .with_spyro_model(
        ...         reference_heat_flux=[35.0, 50.0, 75.0],
        ...         ramp_q="COMPOSITE_HRR",
        ...         reference_thickness=[0.01, 0.01, 0.01]
        ...     ) \\
        ...     .build()
        """
        self._params["reference_heat_flux"] = reference_heat_flux
        self._params["ramp_q"] = ramp_q

        if reference_thickness is not None:
            self._params["reference_thickness"] = reference_thickness
        if inert_q_ref:
            self._params["inert_q_ref"] = inert_q_ref
        if maximum_scaling_heat_flux != 1500.0:
            self._params["maximum_scaling_heat_flux"] = maximum_scaling_heat_flux
        if minimum_scaling_heat_flux != 0.0:
            self._params["minimum_scaling_heat_flux"] = minimum_scaling_heat_flux
        if reference_heat_flux_time_interval != 1.0:
            self._params["reference_heat_flux_time_interval"] = reference_heat_flux_time_interval

        return self

    def build(self) -> Surface:
        """
        Build the Surface object.

        Returns
        -------
        Surface
            The constructed Surface namelist object

        Raises
        ------
        RuntimeError
            If the builder has already been used
        """
        self._check_built()
        params = {"id": self._id, **self._params}
        surf = Surface(**params)
        self._mark_built()
        return surf
