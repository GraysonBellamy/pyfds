"""FDS PART namelist for Lagrangian particle properties.

Defines properties for particle tracking including water droplets,
aerosols, and other Lagrangian particles in fire simulations.

Field Groups:
    identification: Particle class ID
    physical: Diameter, density, mass
    type: Sampling, static, massless flags
    droplet: Liquid droplet model properties
    aerosol: Gas species and vaporization
    drag: Drag law and coefficients
    visualization: Color display
    breakup: Droplet breakup parameters
    surface: Surface interaction
    lifetime: Age and lifetime limits
    orientation: Particle orientation
    radiation: Radiative properties
    distribution: Size distribution
"""

from typing import Literal

from pydantic import ValidationInfo, field_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Particle"]


class Particle(NamelistBase):
    """FDS PART namelist - Lagrangian particle class properties.

    Defines properties for particle tracking including water droplets,
    aerosols, and other Lagrangian particles in fire simulations.

    Parameters
    ----------
    id : str
        Unique particle class ID.
    diameter : float, optional
        Particle diameter [m].
    density : float, optional
        Particle density [kg/m³].
    liquid_droplet : bool, optional
        Use water droplet model.

    Examples
    --------
    >>> droplet = Particle(
    ...     id="WATER_DROP",
    ...     liquid_droplet=True,
    ...     diameter=0.001,
    ...     density=1000.0
    ... )

    See Also
    --------
    Property : Device properties including sprinkler nozzles.
    Surface : Surfaces that generate particles.
    Device : Devices that track particle positions.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "PART"

    # --- Identification ---
    id: str = FdsField(..., description="Unique particle class ID", group="identification")

    # --- Physical Properties ---
    diameter: float | None = FdsField(
        None, gt=0, description="Particle diameter (m)", group="physical"
    )
    density: float | None = FdsField(
        None, gt=0, description="Particle density (kg/m³)", group="physical"
    )
    mass: float | None = FdsField(None, gt=0, description="Particle mass (kg)", group="physical")

    # --- Particle Type ---
    sampling_factor: int = FdsField(
        1, exclude_if=1, ge=1, description="Statistical sampling factor", group="type"
    )
    static: bool = FdsField(
        False, exclude_if=False, description="Particles don't move (static)", group="type"
    )
    massless: bool = FdsField(
        False, exclude_if=False, description="Massless tracer particles", group="type"
    )

    # --- Droplet Properties ---
    liquid_droplet: bool = FdsField(
        False, exclude_if=False, description="Use water droplet model", group="droplet"
    )
    initial_temperature: float | None = FdsField(
        None, description="Initial temperature (°C)", group="droplet"
    )
    boiling_temperature: float | None = FdsField(
        None, description="Boiling temperature (°C)", group="droplet"
    )
    heat_of_vaporization: float | None = FdsField(
        None, gt=0, description="Heat of vaporization (kJ/kg)", group="droplet"
    )

    # --- Aerosol Properties ---
    spec_id: str | None = FdsField(
        None, description="Gas species ID produced by particle", group="aerosol"
    )
    vaporization_temperature: float | None = FdsField(
        None, description="Vaporization temperature (°C)", group="aerosol"
    )

    # --- Drag ---
    drag_law: Literal["SPHERE", "CYLINDER", "SCREEN"] = FdsField(
        "SPHERE", exclude_if="SPHERE", description="Drag law for particle", group="drag"
    )
    drag_coefficient: list[float] | None = FdsField(
        None, description="Custom drag coefficients", group="drag"
    )

    # --- Visualization ---
    color: str | None = FdsField(
        None, description="Particle color for Smokeview", group="visualization"
    )
    rgb: tuple[int, int, int] | None = FdsField(
        None, description="RGB color values (0-255)", group="visualization"
    )

    # --- Breakup ---
    breakup: bool = FdsField(
        False, exclude_if=False, description="Enable droplet breakup", group="breakup"
    )
    breakup_cns_min: float = FdsField(
        0.5, exclude_if=0.5, gt=0, description="Minimum CNS for breakup", group="breakup"
    )
    breakup_cns_max: float = FdsField(
        12.0, exclude_if=12.0, gt=0, description="Maximum CNS for breakup", group="breakup"
    )

    # --- Surface Interaction ---
    surf_id: str | None = FdsField(
        None, description="Surface ID for particle interaction", group="surface"
    )
    prop_id: str | None = FdsField(None, description="Property ID for mass/energy", group="surface")

    # --- Age and Lifetime ---
    age: float | None = FdsField(
        None, ge=0, description="Initial particle age (s)", group="lifetime"
    )
    lifetime: float | None = FdsField(
        None, gt=0, description="Particle lifetime (s)", group="lifetime"
    )

    # --- Orientation ---
    orientation: tuple[float, float, float] | None = FdsField(
        None, description="Particle orientation vector", group="orientation"
    )

    # --- Radiative Properties ---
    radiative_property_table: str | None = FdsField(
        None, description="Radiative property table ID", group="radiation"
    )

    # --- Monodisperse ---
    monodisperse: bool = FdsField(
        False,
        exclude_if=False,
        description="All particles have same diameter",
        group="distribution",
    )

    # --- Check Arrays ---
    check_distribution: bool = FdsField(
        True, exclude_if=True, description="Check particle size distribution", group="distribution"
    )

    @field_validator("rgb")
    @classmethod
    def validate_rgb(cls, v: tuple[int, int, int] | None) -> tuple[int, int, int] | None:
        """Validate RGB values are in range 0-255."""
        if v is not None:
            if len(v) != 3:
                raise ValueError("RGB must have exactly 3 values")
            if not all(0 <= val <= 255 for val in v):
                raise ValueError("RGB values must be in range 0-255")
        return v

    @field_validator("breakup_cns_max")
    @classmethod
    def validate_cns_range(cls, v: float, info: ValidationInfo) -> float:
        """Validate CNS max is greater than CNS min."""
        if "breakup_cns_min" in info.data and v <= info.data["breakup_cns_min"]:
            raise ValueError("BREAKUP_CNS_MAX must be greater than BREAKUP_CNS_MIN")
        return v
