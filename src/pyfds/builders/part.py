"""Builder for creating PART namelists with particle properties."""

from pyfds.builders.base import Builder
from pyfds.core.namelists import Particle


class PartBuilder(Builder[Particle]):
    """
    Builder for creating PART namelists.

    Provides a fluent API for constructing Lagrangian particle classes
    including water droplets, aerosols, and other particle types.

    Parameters
    ----------
    part_id : str, optional
        Unique particle class identifier. Can also be set via with_id().

    Examples
    --------
    >>> # Water droplet for sprinkler
    >>> droplet = (PartBuilder("WATER_DROP")
    ...           .as_water_droplet(diameter=0.001, temp=20.0)
    ...           .with_breakup(True)
    ...           .with_color("BLUE")
    ...           .build())

    >>> # Smoke aerosol
    >>> smoke = (PartBuilder("SMOKE")
    ...         .as_aerosol(diameter=0.00001, spec_id="SOOT")
    ...         .with_color("GRAY")
    ...         .build())

    >>> # Custom particle
    >>> particle = (PartBuilder("CUSTOM")
    ...            .with_diameter(0.0005)
    ...            .with_density(800.0)
    ...            .with_lifetime(60.0)
    ...            .build())

    >>> # Using with_id() method
    >>> particle = (PartBuilder()
    ...            .with_id("TRACER")
    ...            .as_tracer()
    ...            .build())
    """

    def __init__(self, part_id: str | None = None):
        """
        Initialize the PartBuilder.

        Parameters
        ----------
        part_id : str, optional
            Unique particle class identifier
        """
        super().__init__()
        self._id = part_id
        self._params: dict = {}

    def with_id(self, part_id: str) -> "PartBuilder":
        """
        Set the particle class identifier.

        Parameters
        ----------
        part_id : str
            Unique particle class identifier

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._id = part_id
        return self

    def as_water_droplet(self, diameter: float, temp: float = 20.0) -> "PartBuilder":
        """
        Configure as water droplet.

        Sets standard water droplet properties including density,
        boiling temperature, and heat of vaporization.

        Parameters
        ----------
        diameter : float
            Droplet diameter (m)
        temp : float, optional
            Initial temperature (°C), default: 20.0

        Returns
        -------
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> droplet = PartBuilder("WATER").as_water_droplet(0.001, temp=20.0).build()
        """
        self._params.update(
            {
                "liquid_droplet": True,
                "diameter": diameter,
                "density": 1000.0,
                "initial_temperature": temp,
                "boiling_temperature": 100.0,
                "heat_of_vaporization": 2260.0,
            }
        )
        return self

    def as_aerosol(self, diameter: float, spec_id: str) -> "PartBuilder":
        """
        Configure as aerosol particle.

        Parameters
        ----------
        diameter : float
            Particle diameter (m)
        spec_id : str
            Gas species ID for aerosol

        Returns
        -------
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> aerosol = PartBuilder("SMOKE").as_aerosol(0.00001, "SOOT").build()
        """
        self._params.update(
            {
                "diameter": diameter,
                "spec_id": spec_id,
                "massless": False,
            }
        )
        return self

    def as_tracer(self) -> "PartBuilder":
        """
        Configure as massless tracer particle.

        Returns
        -------
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> tracer = PartBuilder("TRACER").as_tracer().with_color("GREEN").build()
        """
        self._params["massless"] = True
        return self

    def with_diameter(self, diameter: float) -> "PartBuilder":
        """
        Set particle diameter.

        Parameters
        ----------
        diameter : float
            Particle diameter (m)

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["diameter"] = diameter
        return self

    def with_density(self, density: float) -> "PartBuilder":
        """
        Set particle density.

        Parameters
        ----------
        density : float
            Particle density (kg/m³)

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["density"] = density
        return self

    def with_mass(self, mass: float) -> "PartBuilder":
        """
        Set particle mass.

        Parameters
        ----------
        mass : float
            Particle mass (kg)

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["mass"] = mass
        return self

    def with_breakup(self, enabled: bool = True) -> "PartBuilder":
        """
        Enable or disable droplet breakup.

        Parameters
        ----------
        enabled : bool, optional
            Enable droplet breakup, default: True

        Returns
        -------
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> droplet = PartBuilder("WATER").as_water_droplet(0.001).with_breakup(True).build()
        """
        self._params["breakup"] = enabled
        return self

    def with_breakup_parameters(self, cns_min: float, cns_max: float) -> "PartBuilder":
        """
        Set breakup CNS parameters.

        Parameters
        ----------
        cns_min : float
            Minimum CNS for breakup
        cns_max : float
            Maximum CNS for breakup

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["breakup_cns_min"] = cns_min
        self._params["breakup_cns_max"] = cns_max
        return self

    def with_color(self, color: str) -> "PartBuilder":
        """
        Set particle color for visualization.

        Parameters
        ----------
        color : str
            Color name (e.g., "BLUE", "RED", "GREEN")

        Returns
        -------
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> particle = PartBuilder("SMOKE").with_color("GRAY").build()
        """
        self._params["color"] = color
        return self

    def with_rgb(self, r: int, g: int, b: int) -> "PartBuilder":
        """
        Set particle RGB color.

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
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> particle = PartBuilder("CUSTOM").with_rgb(128, 128, 128).build()
        """
        self._params["rgb"] = (r, g, b)
        return self

    def with_lifetime(self, lifetime: float) -> "PartBuilder":
        """
        Set particle lifetime.

        Parameters
        ----------
        lifetime : float
            Particle lifetime (s)

        Returns
        -------
        PartBuilder
            Self for method chaining

        Examples
        --------
        >>> particle = PartBuilder("TEMP").with_lifetime(60.0).build()
        """
        self._params["lifetime"] = lifetime
        return self

    def with_initial_age(self, age: float) -> "PartBuilder":
        """
        Set initial particle age.

        Parameters
        ----------
        age : float
            Initial age (s)

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["age"] = age
        return self

    def with_drag_law(self, drag_law: str) -> "PartBuilder":
        """
        Set drag law.

        Parameters
        ----------
        drag_law : str
            Drag law: "SPHERE", "CYLINDER", or "SCREEN"

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["drag_law"] = drag_law.upper()
        return self

    def with_sampling_factor(self, factor: int) -> "PartBuilder":
        """
        Set statistical sampling factor.

        Parameters
        ----------
        factor : int
            Sampling factor (>= 1)

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["sampling_factor"] = factor
        return self

    def as_static(self) -> "PartBuilder":
        """
        Configure as static particle (doesn't move).

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["static"] = True
        return self

    def with_orientation(self, x: float, y: float, z: float) -> "PartBuilder":
        """
        Set particle orientation vector.

        Parameters
        ----------
        x : float
            X component
        y : float
            Y component
        z : float
            Z component

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["orientation"] = (x, y, z)
        return self

    def with_surface(self, surf_id: str) -> "PartBuilder":
        """
        Set surface ID for particle interaction.

        Parameters
        ----------
        surf_id : str
            Surface ID

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["surf_id"] = surf_id
        return self

    def with_property(self, prop_id: str) -> "PartBuilder":
        """
        Set property ID.

        Parameters
        ----------
        prop_id : str
            Property ID

        Returns
        -------
        PartBuilder
            Self for method chaining
        """
        self._params["prop_id"] = prop_id
        return self

    def _validate(self) -> list[str]:
        """Validate builder state before building."""
        errors = []
        if self._id is None:
            errors.append("id is required (use constructor or with_id())")
        return errors

    def _create(self) -> Particle:
        """Create the Particle object."""
        params = {"id": self._id, **self._params}
        return Particle(**params)
