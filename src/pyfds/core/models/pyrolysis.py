"""Pyrolysis reaction definitions for FDS materials."""

from pydantic import BaseModel, Field, model_validator

__all__ = ["PyrolysisProduct", "PyrolysisReaction"]


class PyrolysisProduct(BaseModel):
    """
    Product specification for a pyrolysis reaction.

    Represents either a gaseous species, solid residue, or particle
    produced by a pyrolysis reaction.
    """

    # Gas product
    spec_id: str | None = Field(None, description="Gas species ID")
    nu_spec: float | None = Field(None, ge=0, le=1, description="Gas yield fraction")
    heat_of_combustion: float | None = Field(None, description="Heat of combustion [kJ/kg]")

    # Solid residue
    matl_id: str | None = Field(None, description="Residue material ID")
    nu_matl: float | None = Field(None, ge=0, le=1, description="Residue yield fraction")

    # Particle product
    part_id: str | None = Field(None, description="Particle class ID")
    nu_part: float | None = Field(None, ge=0, le=1, description="Particle yield fraction")

    @model_validator(mode="after")
    def validate_product(self) -> "PyrolysisProduct":
        """Ensure at least one product type is specified."""
        has_gas = self.spec_id is not None
        has_solid = self.matl_id is not None
        has_particle = self.part_id is not None

        if not (has_gas or has_solid or has_particle):
            raise ValueError("Product must specify spec_id, matl_id, or part_id")

        # Validate yields are specified with IDs
        if has_gas and self.nu_spec is None:
            raise ValueError("spec_id requires nu_spec yield fraction")
        if has_solid and self.nu_matl is None:
            raise ValueError("matl_id requires nu_matl yield fraction")
        if has_particle and self.nu_part is None:
            raise ValueError("part_id requires nu_part yield fraction")

        return self


class PyrolysisReaction(BaseModel):
    """
    Single pyrolysis reaction definition.

    Represents one decomposition reaction for a material. A material
    can have multiple reactions (e.g., moisture evaporation,
    primary pyrolysis, char oxidation).

    Kinetic Specification Methods (mutually exclusive):
    1. Arrhenius: Specify both A and E
    2. Simplified with rate: Specify REFERENCE_TEMPERATURE and REFERENCE_RATE
    3. Simplified with range: Specify REFERENCE_TEMPERATURE and PYROLYSIS_RANGE
    4. Auto-derive: Specify only REFERENCE_TEMPERATURE (FDS derives A and E)
    """

    # Reaction enthalpy (optional, default 0.0)
    heat_of_reaction: float = Field(0.0, description="Heat of reaction [kJ/kg]")

    # Products (required)
    products: list[PyrolysisProduct] = Field(..., min_length=1, description="Reaction products")

    # Arrhenius kinetics (Method 1)
    a: float | None = Field(None, gt=0, description="Pre-exponential factor [1/s]")
    e: float | None = Field(None, ge=0, description="Activation energy [kJ/kmol]")

    # Simplified kinetics (Methods 2-4, alternative to A, E)
    reference_temperature: float | None = Field(None, description="Peak reaction temperature [°C]")
    reference_rate: float | None = Field(
        None, gt=0, description="Normalized mass loss rate at reference temperature [1/s]"
    )
    pyrolysis_range: float | None = Field(
        None, gt=0, description="Temperature width of reaction [°C]"
    )
    heating_rate: float = Field(5.0, gt=0, description="TGA heating rate [K/min]")

    # Reaction order parameters
    n_s: float = Field(1.0, description="Reaction order")
    n_t: float = Field(0.0, description="Temperature exponent")
    n_o2: float = Field(0.0, ge=0, description="Oxygen reaction order")

    # Advanced parameters
    gas_diffusion_depth: float | None = Field(
        None, gt=0, description="Gas diffusion length scale [m]"
    )
    max_reaction_rate: float | None = Field(
        None, gt=0, description="Maximum reaction rate [kg/(m³·s)]"
    )

    @model_validator(mode="after")
    def validate_kinetics(self) -> "PyrolysisReaction":
        """
        Validate kinetic parameter combinations.

        FDS User Guide states:
        - Do not specify A and E if you specify REFERENCE_TEMPERATURE
        - Do not specify PYROLYSIS_RANGE if you specify REFERENCE_RATE
        """
        has_arrhenius = self.a is not None or self.e is not None
        has_simplified = self.reference_temperature is not None
        has_reference_rate = self.reference_rate is not None
        has_pyrolysis_range = self.pyrolysis_range is not None

        # Cannot mix Arrhenius and simplified kinetics
        if has_arrhenius and has_simplified:
            raise ValueError(
                "Cannot specify both Arrhenius (A, E) and simplified "
                "(REFERENCE_TEMPERATURE) kinetics"
            )

        # If using Arrhenius, both A and E required
        if has_arrhenius and (self.a is None or self.e is None):
            raise ValueError("Arrhenius kinetics requires both A and E")

        # Cannot specify both REFERENCE_RATE and PYROLYSIS_RANGE
        if has_reference_rate and has_pyrolysis_range:
            raise ValueError(
                "Cannot specify both REFERENCE_RATE and PYROLYSIS_RANGE. "
                "Use REFERENCE_RATE for known rates, or PYROLYSIS_RANGE to estimate from temperature range."
            )

        # REFERENCE_RATE and PYROLYSIS_RANGE only valid with REFERENCE_TEMPERATURE
        if (has_reference_rate or has_pyrolysis_range) and not has_simplified:
            raise ValueError(
                "REFERENCE_RATE and PYROLYSIS_RANGE require REFERENCE_TEMPERATURE to be specified"
            )

        # Validate total yield
        total_yield = sum(
            (p.nu_spec or 0) + (p.nu_matl or 0) + (p.nu_part or 0) for p in self.products
        )
        if total_yield > 1.01:  # Allow small floating point tolerance
            raise ValueError(f"Total product yield ({total_yield:.3f}) exceeds 1.0")

        return self

    def get_gas_products(self) -> list[PyrolysisProduct]:
        """Get all gaseous products from this reaction."""
        return [p for p in self.products if p.spec_id is not None]

    def get_solid_products(self) -> list[PyrolysisProduct]:
        """Get all solid residue products from this reaction."""
        return [p for p in self.products if p.matl_id is not None]

    def get_particle_products(self) -> list[PyrolysisProduct]:
        """Get all particle products from this reaction."""
        return [p for p in self.products if p.part_id is not None]
