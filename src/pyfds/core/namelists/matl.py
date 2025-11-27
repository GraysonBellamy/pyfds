"""
FDS MATL namelist.

Material properties for heat transfer and pyrolysis.
"""

from typing import Any

from pydantic import Field, model_validator

from pyfds.core.namelists.base import NamelistBase


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
    reference_rate: float | None = Field(None, gt=0, description="Reference reaction rate [1/s]")

    # Product specification (multi-reaction)
    nu_spec: list[str] | None = Field(None, description="Product species IDs")
    nu_matl: list[str] | None = Field(None, description="Residue material IDs")

    # Single-reaction product specification (Stage 2.4)
    spec_id: str | None = Field(None, description="Gas species ID for single reaction")
    yield_fraction: float | None = Field(
        None, ge=0, le=1, description="Fraction of material yielded"
    )

    # Heat of combustion (Stage 2.4)
    heat_of_combustion: float | None = Field(None, gt=0, description="Heat of combustion (kJ/kg)")

    # Liquid Fuel Properties (Priority 1)
    boiling_temperature: float | None = Field(
        None, description="Boiling temperature for liquid fuels (°C)"
    )
    mw: float | None = Field(
        None, gt=0, description="Molecular weight for liquid components (g/mol)"
    )

    # Advanced Reaction Parameters (Priority 1)
    n_t: list[float] | None = Field(None, description="Temperature exponents in reaction rate")
    n_o2: list[float] | None = Field(None, description="Oxygen reaction orders")
    gas_diffusion_depth: list[float] | None = Field(
        None, description="Gas diffusion length scale (m)"
    )
    max_reaction_rate: list[float] | None = Field(
        None, description="Maximum reaction rate (kg/m³/s)"
    )

    # Kinetic Parameter Estimation (Priority 1)
    pyrolysis_range: float | None = Field(
        None, gt=0, description="Width of mass loss rate curve (°C or K)"
    )
    heating_rate: float = Field(5.0, gt=0, description="TGA heating rate (K/min)")

    # Material Behavior (Priority 1)
    allow_shrinking: bool = Field(True, description="Allow material shrinking")
    allow_swelling: bool = Field(True, description="Allow material swelling")

    # Energy Conservation (Priority 1)
    adjust_h: bool = Field(True, description="Adjust enthalpies for energy conservation")
    reference_enthalpy: float | None = Field(None, description="Reference enthalpy (kJ/kg)")
    reference_enthalpy_temperature: float | None = Field(
        None, description="Temperature for reference enthalpy (°C)"
    )
    x_o2_pyro: float | None = Field(
        None, ge=0, le=1, description="Oxygen concentration for kinetics (volume fraction)"
    )

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
            for param_name in [
                "a",
                "e",
                "heat_of_reaction",
                "n_t",
                "n_o2",
                "gas_diffusion_depth",
                "max_reaction_rate",
            ]:
                param_value = getattr(self, param_name)
                if param_value is not None and len(param_value) != self.n_reactions:
                    raise ValueError(
                        f"Material '{self.id}': {param_name.upper()} must have "
                        f"{self.n_reactions} values for N_REACTIONS={self.n_reactions}"
                    )

        # Validate liquid fuel model
        if self.boiling_temperature is not None:
            # BOILING_TEMPERATURE triggers liquid pyrolysis model
            # This automatically sets N_REACTIONS=1
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

        # For multi-reaction materials, FDS expects indexed parameters
        # Format arrays as A(1)=val1, A(2)=val2, etc. instead of A=val1,val2
        if self.n_reactions > 1:
            # Build indexed parameters manually for multi-reaction case
            result_parts = []
            result_parts.append(f"&MATL ID='{self.id}', DENSITY={self.density}")

            if self.conductivity is not None:
                result_parts.append(f"CONDUCTIVITY={self.conductivity}")
            elif self.conductivity_ramp:
                result_parts.append(f"CONDUCTIVITY_RAMP='{self.conductivity_ramp}'")

            if self.specific_heat is not None:
                result_parts.append(f"SPECIFIC_HEAT={self.specific_heat}")
            elif self.specific_heat_ramp:
                result_parts.append(f"SPECIFIC_HEAT_RAMP='{self.specific_heat_ramp}'")

            if self.emissivity != 0.9:
                result_parts.append(f"EMISSIVITY={self.emissivity}")
            if self.absorption_coefficient != 50000.0:
                result_parts.append(f"ABSORPTION_COEFFICIENT={self.absorption_coefficient}")

            result_parts.append(f"N_REACTIONS={self.n_reactions}")

            # Add indexed arrays for each reaction
            if self.heat_of_reaction:
                for i, val in enumerate(self.heat_of_reaction, 1):
                    result_parts.append(f"HEAT_OF_REACTION({i})={val}")
            if self.a:
                for i, val in enumerate(self.a, 1):
                    result_parts.append(f"A({i})={val}")
            if self.e:
                for i, val in enumerate(self.e, 1):
                    result_parts.append(f"E({i})={val}")
            if self.n_s:
                for i, val in enumerate(self.n_s, 1):
                    result_parts.append(f"N_S({i})={val}")

            # Handle 2D arrays like NU_SPEC and NU_MATL
            # These use notation like NU_SPEC(i,j) where i is species index, j is reaction index
            if self.nu_spec:
                for j, spec_list in enumerate(self.nu_spec, 1):
                    if isinstance(spec_list, (list, tuple)):
                        for i, val in enumerate(spec_list, 1):
                            if val:  # Only add non-empty values
                                result_parts.append(f"SPEC_ID({i},{j})='{val}'")
                                result_parts.append(f"NU_SPEC({i},{j})=1.0")
            if self.nu_matl:
                for j, matl_list in enumerate(self.nu_matl, 1):
                    if isinstance(matl_list, (list, tuple)):
                        for i, val in enumerate(matl_list, 1):
                            if val:  # Only add non-empty values
                                result_parts.append(f"MATL_ID({i},{j})='{val}'")
                                result_parts.append(f"NU_MATL({i},{j})=1.0")

            return ", ".join(result_parts) + " /\n"
        # Single reaction or no reactions - use standard format
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
        if self.reference_rate is not None:
            params["reference_rate"] = self.reference_rate
        if self.nu_spec:
            params["nu_spec"] = self.nu_spec
        if self.nu_matl:
            params["nu_matl"] = self.nu_matl

        # Single-reaction product specification (Stage 2.4)
        if self.spec_id:
            params["spec_id"] = self.spec_id
        if self.yield_fraction is not None:
            params["yield_fraction"] = self.yield_fraction

        # Heat of combustion (Stage 2.4)
        if self.heat_of_combustion is not None:
            params["heat_of_combustion"] = self.heat_of_combustion

        # Liquid Fuel Properties (Priority 1)
        if self.boiling_temperature is not None:
            params["boiling_temperature"] = self.boiling_temperature
        if self.mw is not None:
            params["mw"] = self.mw

        # Advanced Reaction Parameters (Priority 1)
        if self.n_t:
            params["n_t"] = self.n_t
        if self.n_o2:
            params["n_o2"] = self.n_o2
        if self.gas_diffusion_depth:
            params["gas_diffusion_depth"] = self.gas_diffusion_depth
        if self.max_reaction_rate:
            params["max_reaction_rate"] = self.max_reaction_rate

        # Kinetic Parameter Estimation (Priority 1)
        if self.pyrolysis_range is not None:
            params["pyrolysis_range"] = self.pyrolysis_range
        if self.heating_rate != 5.0:
            params["heating_rate"] = self.heating_rate

        # Material Behavior (Priority 1)
        if not self.allow_shrinking:  # Only output if False
            params["allow_shrinking"] = self.allow_shrinking
        if not self.allow_swelling:  # Only output if False
            params["allow_swelling"] = self.allow_swelling

        # Energy Conservation (Priority 1)
        if not self.adjust_h:  # Only output if False
            params["adjust_h"] = self.adjust_h
        if self.reference_enthalpy is not None:
            params["reference_enthalpy"] = self.reference_enthalpy
        if self.reference_enthalpy_temperature is not None:
            params["reference_enthalpy_temperature"] = self.reference_enthalpy_temperature
        if self.x_o2_pyro is not None:
            params["x_o2_pyro"] = self.x_o2_pyro

        return self._build_namelist("MATL", params)
