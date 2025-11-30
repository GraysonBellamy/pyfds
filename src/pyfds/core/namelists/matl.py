"""
FDS MATL namelist.

Material properties for heat transfer and pyrolysis.

Fields are organized by logical groups (use group= metadata for categorization):
- core: Basic identification (id)
- thermal: Core thermal transport properties (density, conductivity, specific_heat)
- radiation: Radiative properties (emissivity, absorption_coefficient)
- pyrolysis: Reaction kinetics (Arrhenius and simplified)
- products: Gas species and solid residue products
- liquid: Liquid fuel evaporation model parameters
"""

from typing import TYPE_CHECKING

from pydantic import field_validator, model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase
from pyfds.core.namelists.pyrolysis import PyrolysisReaction

if TYPE_CHECKING:
    from pyfds.builders import MaterialBuilder


class Material(NamelistBase):
    """
    FDS MATL namelist - material properties.

    All parameters are organized into logical groups via the `group` metadata:
    - core: Basic identification (id)
    - thermal: Density, conductivity, specific heat
    - radiation: Emissivity, absorption coefficient
    - pyrolysis: Arrhenius kinetics, simplified kinetics, reaction parameters
    - products: Gas species, solid residue, particle products
    - liquid: Liquid fuel evaporation model

    Parameters
    ----------
    id : str
        Unique material identifier
    density : float
        Material density [kg/m³]
    conductivity : float, optional
        Thermal conductivity [W/(m·K)]
    specific_heat : float, optional
        Specific heat capacity [kJ/(kg·K)]

    Examples
    --------
    >>> matl = Material(id='CONCRETE', density=2300, conductivity=1.0, specific_heat=0.88)
    >>> print(matl.to_fds())
    &MATL ID='CONCRETE', DENSITY=2300.0, CONDUCTIVITY=1.0, SPECIFIC_HEAT=0.88 /
    """

    @classmethod
    def builder(cls) -> "MaterialBuilder":
        """Return a fluent builder for Material.

        Returns
        -------
        MaterialBuilder
            A builder instance for fluent construction

        Examples
        --------
        >>> matl = Material.builder() \\
        ...     .id("CONCRETE") \\
        ...     .density(2300) \\
        ...     .thermal_conductivity(1.0) \\
        ...     .specific_heat(0.88) \\
        ...     .build()
        """
        from pyfds.builders import MaterialBuilder

        return MaterialBuilder()

    # =========================================================================
    # CORE PARAMETERS
    # =========================================================================
    id: str = FdsField(..., description="Material identifier", group="core")

    # NEW: Structured reactions (preferred)
    reactions: list[PyrolysisReaction] | None = FdsField(
        None,
        description="Pyrolysis reactions (structured format, preferred over parallel arrays)",
        group="core",
    )

    # =========================================================================
    # THERMAL PROPERTIES (from ThermalPropertiesMixin)
    # =========================================================================
    density: float = FdsField(..., gt=0, description="Material density [kg/m³]", group="thermal")

    # Thermal conductivity - constant OR ramp
    conductivity: float | None = FdsField(
        None, gt=0, description="Thermal conductivity [W/(m·K)]", group="thermal"
    )
    conductivity_ramp: str | None = FdsField(
        None, description="RAMP ID for temperature-dependent conductivity", group="thermal"
    )

    # Specific heat - constant OR ramp
    specific_heat: float | None = FdsField(
        None, gt=0, description="Specific heat capacity [kJ/(kg·K)]", group="thermal"
    )
    specific_heat_ramp: str | None = FdsField(
        None, description="RAMP ID for temperature-dependent specific heat", group="thermal"
    )

    # =========================================================================
    # RADIATION PROPERTIES (from ThermalPropertiesMixin)
    # =========================================================================
    emissivity: float = FdsField(
        0.9, ge=0, le=1, description="Surface emissivity [0-1]", group="radiation"
    )
    absorption_coefficient: float = FdsField(
        50000.0, ge=0, description="Radiation absorption coefficient [1/m]", group="radiation"
    )

    # =========================================================================
    # PYROLYSIS KINETICS (from PyrolysisMixin)
    # =========================================================================
    # Arrhenius kinetics
    a: list[float] | None = FdsField(
        None, description="Pre-exponential factors [1/s]", group="pyrolysis"
    )
    e: list[float] | None = FdsField(
        None, description="Activation energies [kJ/kmol]", group="pyrolysis"
    )

    # Reaction orders
    n_s: list[float] | None = FdsField(
        None, description="Reaction orders (default: 1.0)", group="pyrolysis"
    )
    n_t: list[float] | None = FdsField(
        None, description="Temperature exponents in reaction rate", group="pyrolysis"
    )
    n_o2: list[float] | None = FdsField(
        None, description="Oxygen reaction orders", group="pyrolysis"
    )

    # Simplified kinetics (alternative to A, E)
    reference_temperature: list[float] | float | None = FdsField(
        None, description="Reference temperature(s) for reactions [°C]", group="pyrolysis"
    )
    reference_rate: float | None = FdsField(
        None, gt=0, description="Reference reaction rate [1/s]", group="pyrolysis"
    )
    pyrolysis_range: float | None = FdsField(
        None, gt=0, description="Temperature width of mass loss curve [°C]", group="pyrolysis"
    )
    heating_rate: float = FdsField(
        5.0, gt=0, description="TGA heating rate for kinetics derivation [K/min]", group="pyrolysis"
    )

    # Reaction enthalpy
    heat_of_reaction: list[float] | float | None = FdsField(
        None, description="Heat of pyrolysis/vaporization [kJ/kg]", group="pyrolysis"
    )

    # Advanced reaction parameters
    gas_diffusion_depth: list[float] | None = FdsField(
        None, description="Gas diffusion length scale [m]", group="pyrolysis"
    )
    max_reaction_rate: list[float] | None = FdsField(
        None, description="Maximum reaction rate [kg/(m³·s)]", group="pyrolysis"
    )
    reac_rate_delta: float | None = FdsField(
        None, description="Reaction rate delta for remeshing control", group="pyrolysis"
    )

    # Physical behavior during reactions
    allow_shrinking: bool = FdsField(
        True, description="Allow material shrinking during pyrolysis", group="pyrolysis"
    )
    allow_swelling: bool = FdsField(
        True, description="Allow material swelling during pyrolysis", group="pyrolysis"
    )

    # Energy conservation
    adjust_h: bool = FdsField(
        True, description="Adjust enthalpies for energy conservation", group="pyrolysis"
    )
    reference_enthalpy: float | None = FdsField(
        None, description="Reference enthalpy [kJ/kg]", group="pyrolysis"
    )
    reference_enthalpy_temperature: float | None = FdsField(
        None, description="Temperature for reference enthalpy [°C]", group="pyrolysis"
    )
    x_o2_pyro: float | None = FdsField(
        None, ge=0, le=1, description="O2 concentration for oxidation kinetics", group="pyrolysis"
    )

    # =========================================================================
    # PRODUCTS (from MechanicalPropertiesMixin)
    # =========================================================================
    # Gas species products
    spec_id: list[list[str]] | list[str] | str | None = FdsField(
        None, description="Gas species ID(s) per reaction", group="products"
    )
    nu_spec: list[list[float]] | list[float] | float | None = FdsField(
        None, description="Gas species yields NU_SPEC(species, reaction)", group="products"
    )

    # Solid residue products
    matl_id_products: list[list[str]] | list[str] | str | None = FdsField(
        None, alias="matl_id", description="Residue material ID(s) per reaction", group="products"
    )
    nu_matl: list[list[float]] | list[float] | float | None = FdsField(
        None, description="Residue yields NU_MATL(material, reaction)", group="products"
    )

    # Heat of combustion per product
    heat_of_combustion_array: list[list[float]] | list[float] | float | None = FdsField(
        None,
        alias="heat_of_combustion",
        description="Heat of combustion per species per reaction [kJ/kg]",
        group="products",
    )

    # Particle products
    part_id: list[list[str]] | list[str] | str | None = FdsField(
        None, description="Particle class ID(s) per reaction", group="products"
    )
    nu_part: list[list[float]] | list[float] | float | None = FdsField(
        None, description="Particle yields NU_PART(particle, reaction)", group="products"
    )

    # =========================================================================
    # LIQUID FUEL (from LiquidFuelMixin)
    # =========================================================================
    boiling_temperature: float | None = FdsField(
        None, description="Boiling point [°C] - triggers liquid model", group="liquid"
    )
    mw: float | None = FdsField(
        None, gt=0, description="Molecular weight for liquid [g/mol]", group="liquid"
    )
    heat_of_vaporization: float | None = FdsField(
        None, gt=0, description="Heat of vaporization [kJ/kg]", group="liquid"
    )

    # =========================================================================
    # VALIDATORS
    # =========================================================================
    @field_validator("density")
    @classmethod
    def validate_density_range(cls, v: float) -> float:
        """Validate density is in physically reasonable range."""
        if not (0.1 <= v <= 25000.0):
            raise ValueError(f"DENSITY = {v} outside valid range [0.1, 25000.0] kg/m³")
        return v

    @field_validator("conductivity")
    @classmethod
    def validate_conductivity_range(cls, v: float | None) -> float | None:
        """Validate conductivity range if specified."""
        if v is not None and not (0.001 <= v <= 2000.0):
            raise ValueError(f"CONDUCTIVITY = {v} outside valid range [0.001, 2000.0] W/(m·K)")
        return v

    @field_validator("specific_heat")
    @classmethod
    def validate_specific_heat_range(cls, v: float | None) -> float | None:
        """Validate specific heat range if specified."""
        if v is not None and not (0.05 <= v <= 50.0):
            raise ValueError(f"SPECIFIC_HEAT = {v} outside valid range [0.05, 50.0] kJ/(kg·K)")
        return v

    @property
    def n_reactions(self) -> int:
        """Number of reactions (computed from reactions list or array parameters)."""
        if self.reactions:
            return len(self.reactions)

        # Check array parameters for multi-reaction indicators
        array_params = [
            self.a,
            self.e,
            self.heat_of_reaction,
            self.n_t,
            self.n_o2,
            self.gas_diffusion_depth,
            self.max_reaction_rate,
            self.part_id,
            self.nu_part,
        ]

        for param in array_params:
            if isinstance(param, list) and len(param) > 1:
                return len(param)

        return 1

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

        # Validate density range (allow low-density foams)
        if not (0.1 <= self.density <= 25000.0):
            raise ValueError(
                f"Material '{self.id}': DENSITY = {self.density} "
                f"is outside valid range [0.1, 25000.0] kg/m³"
            )

        # Validate conductivity range if specified
        if self.conductivity is not None and not (0.001 <= self.conductivity <= 2000.0):
            raise ValueError(
                f"Material '{self.id}': CONDUCTIVITY = {self.conductivity} "
                f"is outside valid range [0.001, 2000.0] W/(m·K)"
            )

        # Validate specific heat range if specified
        if self.specific_heat is not None and not (0.05 <= self.specific_heat <= 50.0):
            raise ValueError(
                f"Material '{self.id}': SPECIFIC_HEAT = {self.specific_heat} "
                f"is outside valid range [0.05, 50.0] kJ/(kg·K)"
            )

        # Validate multi-reaction arrays
        if self.n_reactions > 1:
            for param_name in [
                "a",
                "e",
                "heat_of_reaction",
                "n_t",
                "n_o2",
                "gas_diffusion_depth",
                "max_reaction_rate",
                "reference_temperature",
            ]:
                param_value = getattr(self, param_name)
                if param_value is not None and len(param_value) != self.n_reactions:
                    raise ValueError(
                        f"Material '{self.id}': {param_name.upper()} must have "
                        f"{self.n_reactions} values for N_REACTIONS={self.n_reactions}"
                    )

        # Validate liquid fuel model
        if self.boiling_temperature is not None:
            if self.n_reactions > 1:
                raise ValueError(
                    f"Material '{self.id}': BOILING_TEMPERATURE (liquid model) "
                    f"cannot be used with N_REACTIONS > 1"
                )
            if self.spec_id is None:
                raise ValueError(
                    f"Material '{self.id}': BOILING_TEMPERATURE requires SPEC_ID "
                    f"to specify the gaseous fuel species"
                )

        # Validate REFERENCE_TEMPERATURE and (A, E) mutual exclusivity
        if self.reference_temperature is not None and (self.a is not None or self.e is not None):
            raise ValueError(
                f"Material '{self.id}': Cannot specify both REFERENCE_TEMPERATURE "
                f"and (A, E). Use one or the other."
            )

        # Validate PYROLYSIS_RANGE and REFERENCE_RATE mutual exclusivity
        if self.pyrolysis_range is not None and self.reference_rate is not None:
            raise ValueError(
                f"Material '{self.id}': Cannot specify both PYROLYSIS_RANGE and REFERENCE_RATE"
            )

        # Validate REFERENCE_ENTHALPY requires REFERENCE_ENTHALPY_TEMPERATURE
        if self.reference_enthalpy is not None and self.reference_enthalpy_temperature is None:
            raise ValueError(
                f"Material '{self.id}': REFERENCE_ENTHALPY requires REFERENCE_ENTHALPY_TEMPERATURE"
            )

        # Validate single reaction REFERENCE_TEMPERATURE list length
        if (
            self.n_reactions == 1
            and self.reference_temperature is not None
            and isinstance(self.reference_temperature, list)
            and len(self.reference_temperature) != 1
        ):
            raise ValueError(
                f"Material '{self.id}': REFERENCE_TEMPERATURE list must have length 1 "
                f"for single reaction, got {len(self.reference_temperature)}"
            )

        # Validate yield sums for each reaction
        if self.nu_spec or self.nu_matl:
            for reaction_idx in range(self.n_reactions):
                total_yield = 0.0

                if self.nu_spec:
                    if isinstance(self.nu_spec, (int, float)):
                        total_yield += self.nu_spec
                    elif isinstance(self.nu_spec, list) and reaction_idx < len(self.nu_spec):
                        reaction_spec = self.nu_spec[reaction_idx]
                        if isinstance(reaction_spec, (int, float)):
                            total_yield += reaction_spec
                        elif isinstance(reaction_spec, list):
                            total_yield += sum(reaction_spec)

                if self.nu_matl:
                    if isinstance(self.nu_matl, (int, float)):
                        total_yield += self.nu_matl
                    elif isinstance(self.nu_matl, list) and reaction_idx < len(self.nu_matl):
                        reaction_matl = self.nu_matl[reaction_idx]
                        if isinstance(reaction_matl, (int, float)):
                            total_yield += reaction_matl
                        elif isinstance(reaction_matl, list):
                            total_yield += sum(reaction_matl)

                if total_yield > 1.0:
                    raise ValueError(
                        f"Material '{self.id}': Total yield for reaction {reaction_idx + 1} "
                        f"is {total_yield:.3f}, which exceeds 1.0"
                    )

        # Validate that user doesn't mix structured and array formats
        has_structured = self.reactions is not None and len(self.reactions) > 0
        has_arrays = any(
            [
                self.a is not None,
                self.e is not None,
                self.reference_temperature is not None,
            ]
        )

        if has_structured and has_arrays:
            raise ValueError(
                f"Material '{self.id}': Cannot mix 'reactions' list with "
                f"array parameters (a, e, reference_temperature, etc.). "
                f"Use one format or the other."
            )

        return self

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MATL"
