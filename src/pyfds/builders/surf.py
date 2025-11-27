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
