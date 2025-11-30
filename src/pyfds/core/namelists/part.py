"""PART namelist - Lagrangian particle properties for FDS simulations."""

from typing import Literal

from pydantic import ValidationInfo, field_validator

from pyfds.core.namelists.base import FdsField

from .base import NamelistBase


class Part(NamelistBase):
    """
    FDS PART namelist - Lagrangian particle class properties.

    Defines properties for particle tracking including water droplets,
    aerosols, and other Lagrangian particles in fire simulations.

    Examples
    --------
    >>> # Water droplet
    >>> droplet = Part(
    ...     id="WATER_DROP",
    ...     liquid_droplet=True,
    ...     diameter=0.001,
    ...     density=1000.0,
    ...     initial_temperature=20.0
    ... )

    >>> # Aerosol particle
    >>> aerosol = Part(
    ...     id="SMOKE",
    ...     diameter=0.00001,
    ...     density=1200.0,
    ...     spec_id="SOOT"
    ... )
    """

    # Identification
    id: str = FdsField(..., description="Unique particle class ID")

    # Physical Properties
    diameter: float | None = FdsField(None, gt=0, description="Particle diameter (m)")
    density: float | None = FdsField(None, gt=0, description="Particle density (kg/m³)")
    mass: float | None = FdsField(None, gt=0, description="Particle mass (kg)")

    # Particle Type
    sampling_factor: int = FdsField(
        1, exclude_if=1, ge=1, description="Statistical sampling factor"
    )
    static: bool = FdsField(False, exclude_if=False, description="Particles don't move (static)")
    massless: bool = FdsField(False, exclude_if=False, description="Massless tracer particles")

    # Droplet Properties
    liquid_droplet: bool = FdsField(False, exclude_if=False, description="Use water droplet model")
    initial_temperature: float | None = FdsField(None, description="Initial temperature (°C)")
    boiling_temperature: float | None = FdsField(None, description="Boiling temperature (°C)")
    heat_of_vaporization: float | None = FdsField(
        None, gt=0, description="Heat of vaporization (kJ/kg)"
    )

    # Aerosol Properties
    spec_id: str | None = FdsField(None, description="Gas species ID produced by particle")
    vaporization_temperature: float | None = FdsField(
        None, description="Vaporization temperature (°C)"
    )

    # Drag
    drag_law: Literal["SPHERE", "CYLINDER", "SCREEN"] = FdsField(
        "SPHERE", exclude_if="SPHERE", description="Drag law for particle"
    )
    drag_coefficient: list[float] | None = FdsField(None, description="Custom drag coefficients")

    # Visualization
    color: str | None = FdsField(None, description="Particle color for Smokeview")
    rgb: tuple[int, int, int] | None = FdsField(None, description="RGB color values (0-255)")

    # Breakup
    breakup: bool = FdsField(False, exclude_if=False, description="Enable droplet breakup")
    breakup_cns_min: float = FdsField(
        0.5, exclude_if=0.5, gt=0, description="Minimum CNS for breakup"
    )
    breakup_cns_max: float = FdsField(
        12.0, exclude_if=12.0, gt=0, description="Maximum CNS for breakup"
    )

    # Surface Interaction
    surf_id: str | None = FdsField(None, description="Surface ID for particle interaction")
    prop_id: str | None = FdsField(None, description="Property ID for mass/energy")

    # Age and Lifetime
    age: float | None = FdsField(None, ge=0, description="Initial particle age (s)")
    lifetime: float | None = FdsField(None, gt=0, description="Particle lifetime (s)")

    # Orientation
    orientation: tuple[float, float, float] | None = FdsField(
        None, description="Particle orientation vector"
    )

    # Radiative Properties
    radiative_property_table: str | None = FdsField(None, description="Radiative property table ID")

    # Monodisperse
    monodisperse: bool = FdsField(
        False, exclude_if=False, description="All particles have same diameter"
    )

    # Check Arrays
    check_distribution: bool = FdsField(
        True, exclude_if=True, description="Check particle size distribution"
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

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "PART"
