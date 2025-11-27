"""PART namelist - Lagrangian particle properties for FDS simulations."""

from typing import Literal

from pydantic import Field, ValidationInfo, field_validator

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
    id: str = Field(..., description="Unique particle class ID")

    # Physical Properties
    diameter: float | None = Field(None, gt=0, description="Particle diameter (m)")
    density: float | None = Field(None, gt=0, description="Particle density (kg/m³)")
    mass: float | None = Field(None, gt=0, description="Particle mass (kg)")

    # Particle Type
    sampling_factor: int = Field(1, ge=1, description="Statistical sampling factor")
    static: bool = Field(False, description="Particles don't move (static)")
    massless: bool = Field(False, description="Massless tracer particles")

    # Droplet Properties
    liquid_droplet: bool = Field(False, description="Use water droplet model")
    initial_temperature: float | None = Field(None, description="Initial temperature (°C)")
    boiling_temperature: float | None = Field(None, description="Boiling temperature (°C)")
    heat_of_vaporization: float | None = Field(
        None, gt=0, description="Heat of vaporization (kJ/kg)"
    )

    # Aerosol Properties
    spec_id: str | None = Field(None, description="Gas species ID produced by particle")
    vaporization_temperature: float | None = Field(
        None, description="Vaporization temperature (°C)"
    )

    # Drag
    drag_law: Literal["SPHERE", "CYLINDER", "SCREEN"] = Field(
        "SPHERE", description="Drag law for particle"
    )
    drag_coefficient: list[float] | None = Field(None, description="Custom drag coefficients")

    # Visualization
    color: str | None = Field(None, description="Particle color for Smokeview")
    rgb: tuple[int, int, int] | None = Field(None, description="RGB color values (0-255)")

    # Breakup
    breakup: bool = Field(False, description="Enable droplet breakup")
    breakup_cns_min: float = Field(0.5, gt=0, description="Minimum CNS for breakup")
    breakup_cns_max: float = Field(12.0, gt=0, description="Maximum CNS for breakup")

    # Surface Interaction
    surf_id: str | None = Field(None, description="Surface ID for particle interaction")
    prop_id: str | None = Field(None, description="Property ID for mass/energy")

    # Age and Lifetime
    age: float | None = Field(None, ge=0, description="Initial particle age (s)")
    lifetime: float | None = Field(None, gt=0, description="Particle lifetime (s)")

    # Orientation
    orientation: tuple[float, float, float] | None = Field(
        None, description="Particle orientation vector"
    )

    # Radiative Properties
    radiative_property_table: str | None = Field(None, description="Radiative property table ID")

    # Monodisperse
    monodisperse: bool = Field(False, description="All particles have same diameter")

    # Check Arrays
    check_distribution: bool = Field(True, description="Check particle size distribution")

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

    def to_fds(self) -> str:
        """
        Generate FDS input file text for PART namelist.

        Returns
        -------
        str
            FDS namelist text

        Examples
        --------
        >>> part = Part(id="WATER", diameter=0.001, density=1000.0)
        >>> fds_text = part.to_fds()
        >>> "&PART" in fds_text
        True
        """
        lines = ["&PART"]

        # ID (required)
        lines.append(f"  ID='{self.id}'")

        # Physical properties
        if self.diameter is not None:
            lines.append(f"  DIAMETER={self.diameter}")
        if self.density is not None:
            lines.append(f"  DENSITY={self.density}")
        if self.mass is not None:
            lines.append(f"  MASS={self.mass}")

        # Particle type
        if self.sampling_factor != 1:
            lines.append(f"  SAMPLING_FACTOR={self.sampling_factor}")
        if self.static:
            lines.append("  STATIC=.TRUE.")
        if self.massless:
            lines.append("  MASSLESS=.TRUE.")

        # Droplet properties
        if self.liquid_droplet:
            lines.append("  LIQUID_DROPLET=.TRUE.")
        if self.initial_temperature is not None:
            lines.append(f"  INITIAL_TEMPERATURE={self.initial_temperature}")
        if self.boiling_temperature is not None:
            lines.append(f"  BOILING_TEMPERATURE={self.boiling_temperature}")
        if self.heat_of_vaporization is not None:
            lines.append(f"  HEAT_OF_VAPORIZATION={self.heat_of_vaporization}")

        # Aerosol properties
        if self.spec_id is not None:
            lines.append(f"  SPEC_ID='{self.spec_id}'")
        if self.vaporization_temperature is not None:
            lines.append(f"  VAPORIZATION_TEMPERATURE={self.vaporization_temperature}")

        # Drag
        if self.drag_law != "SPHERE":
            lines.append(f"  DRAG_LAW='{self.drag_law}'")
        if self.drag_coefficient is not None:
            coef_str = ",".join(str(c) for c in self.drag_coefficient)
            lines.append(f"  DRAG_COEFFICIENT={coef_str}")

        # Visualization
        if self.color is not None:
            lines.append(f"  COLOR='{self.color}'")
        if self.rgb is not None:
            rgb_str = ",".join(str(v) for v in self.rgb)
            lines.append(f"  RGB={rgb_str}")

        # Breakup
        if self.breakup:
            lines.append("  BREAKUP=.TRUE.")
        if self.breakup_cns_min != 0.5:
            lines.append(f"  BREAKUP_CNS_MIN={self.breakup_cns_min}")
        if self.breakup_cns_max != 12.0:
            lines.append(f"  BREAKUP_CNS_MAX={self.breakup_cns_max}")

        # Surface interaction
        if self.surf_id is not None:
            lines.append(f"  SURF_ID='{self.surf_id}'")
        if self.prop_id is not None:
            lines.append(f"  PROP_ID='{self.prop_id}'")

        # Age and lifetime
        if self.age is not None:
            lines.append(f"  AGE={self.age}")
        if self.lifetime is not None:
            lines.append(f"  LIFETIME={self.lifetime}")

        # Orientation
        if self.orientation is not None:
            orient_str = ",".join(str(v) for v in self.orientation)
            lines.append(f"  ORIENTATION={orient_str}")

        # Radiative properties
        if self.radiative_property_table is not None:
            lines.append(f"  RADIATIVE_PROPERTY_TABLE='{self.radiative_property_table}'")

        # Monodisperse
        if self.monodisperse:
            lines.append("  MONODISPERSE=.TRUE.")

        # Check distribution
        if not self.check_distribution:
            lines.append("  CHECK_DISTRIBUTION=.FALSE.")

        lines.append("/")
        return "\n".join(lines)
