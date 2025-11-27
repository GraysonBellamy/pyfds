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
    nu_spec : list[list[float]] or list[float], optional
        Species yields: NU_SPEC(species_idx, reaction_idx) for multi-reaction
    nu_matl : list[list[float]] or list[float], optional
        Residue yields: NU_MATL(material_idx, reaction_idx) for multi-reaction
    spec_id : str, list[str], or list[list[str]], optional
        Gas species ID(s): single, per reaction, or 2D array for multi-species per reaction
    matl_id_products : str, list[str], or list[list[str]], optional
        Residue material ID(s) per reaction (alias: matl_id)
    heat_of_combustion_array : list[list[float]] or list[float], optional
        Heat of combustion per species per reaction [kJ/kg]
    part_id : list[list[str]] or list[str], optional
        Particle class ID(s) produced by reactions
    nu_part : list[list[float]] or list[float], optional
        Particle yields: NU_PART(particle_idx, reaction_idx)

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
    heat_of_reaction: list[float] | float | None = Field(
        None, description="Heat of reaction [kJ/kg]"
    )

    # Reaction kinetics
    a: list[float] | None = Field(None, description="Pre-exponential factors [1/s]")
    e: list[float] | None = Field(None, description="Activation energies [kJ/kmol]")
    n_s: list[float] | None = Field(None, description="Reaction orders")
    reference_rate: float | None = Field(None, gt=0, description="Reference reaction rate [1/s]")

    # Product specification (multi-reaction)
    spec_id: list[list[str]] | list[str] | str | None = Field(
        None, description="Gas species ID(s) per reaction"
    )
    nu_spec: list[list[float]] | list[float] | float | None = Field(
        None, description="Species yields: NU_SPEC(species_idx, reaction_idx)"
    )
    matl_id_products: list[list[str]] | list[str] | str | None = Field(
        None, alias="matl_id", description="Residue material ID(s) per reaction"
    )
    nu_matl: list[list[float]] | list[float] | float | None = Field(
        None, description="Residue yields: NU_MATL(material_idx, reaction_idx)"
    )
    heat_of_combustion_array: list[list[float]] | list[float] | float | None = Field(
        None,
        alias="heat_of_combustion",
        description="Heat of combustion per species per reaction [kJ/kg]",
    )

    # Particle products (Phase 2.1)
    part_id: list[list[str]] | list[str] | str | None = Field(
        None, description="Particle class ID(s) produced by reactions"
    )
    nu_part: list[list[float]] | list[float] | float | None = Field(
        None, description="Particle yields: NU_PART(particle_idx, reaction_idx)"
    )

    # Liquid Fuel Properties (Priority 1)
    boiling_temperature: float | None = Field(
        None, description="Boiling temperature for liquid fuels (°C)"
    )
    mw: float | None = Field(
        None, gt=0, description="Molecular weight for liquid components (g/mol)"
    )
    heat_of_vaporization: float | None = Field(
        None, gt=0, description="Heat of vaporization for liquid fuels (kJ/kg)"
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

    # Delamination (Priority 1)
    delamination_tmp: float | None = Field(
        None, gt=0, description="Temperature for delamination (°C)"
    )
    delamination_density: float | None = Field(
        None, gt=0, description="Density for delamination (kg/m³)"
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

        # Validate yield sums for each reaction
        if self.nu_spec or self.nu_matl:
            for reaction_idx in range(self.n_reactions):
                total_yield = 0.0

                # Sum nu_spec for this reaction
                if self.nu_spec:
                    if isinstance(self.nu_spec, (int, float)):
                        total_yield += self.nu_spec
                    elif isinstance(self.nu_spec, list) and reaction_idx < len(self.nu_spec):
                        reaction_spec = self.nu_spec[reaction_idx]
                        if isinstance(reaction_spec, (int, float)):
                            total_yield += reaction_spec
                        elif isinstance(reaction_spec, list):
                            total_yield += sum(reaction_spec)

                # Sum nu_matl for this reaction
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

        # Physical reasonableness checks (warnings, not errors)
        if self.e is not None:
            for _i, e_val in enumerate(self.e):
                if e_val < 10000 or e_val > 500000:
                    # This would be a warning in a real implementation
                    # For now, we'll just pass (could log or add to warnings list)
                    pass

        if self.a is not None:
            for _i, a_val in enumerate(self.a):
                if a_val < 1e3 or a_val > 1e20:
                    # This would be a warning in a real implementation
                    pass

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
            if self.heat_of_reaction and isinstance(self.heat_of_reaction, list):
                for i, val in enumerate(self.heat_of_reaction, 1):
                    result_parts.append(f"HEAT_OF_REACTION({i})={val}")
            if self.a and isinstance(self.a, list):
                for i, val in enumerate(self.a, 1):
                    result_parts.append(f"A({i})={val}")
            if self.e and isinstance(self.e, list):
                for i, val in enumerate(self.e, 1):
                    result_parts.append(f"E({i})={val}")
            if self.n_s and isinstance(self.n_s, list):
                for i, val in enumerate(self.n_s, 1):
                    result_parts.append(f"N_S({i})={val}")

            # Handle 2D arrays for products
            # SPEC_ID(i,j) where i=species index, j=reaction index
            if self.spec_id:
                if isinstance(self.spec_id, str):
                    result_parts.append(f"SPEC_ID='{self.spec_id}'")
                elif isinstance(self.spec_id, list):
                    if all(isinstance(item, str) for item in self.spec_id):
                        # List per reaction
                        for j, spec in enumerate(self.spec_id, 1):
                            result_parts.append(f"SPEC_ID({j})='{spec}'")
                    elif all(isinstance(item, list) for item in self.spec_id):
                        # 2D array
                        for j, reaction_specs in enumerate(self.spec_id, 1):
                            if reaction_specs:
                                spec_count = len(reaction_specs)
                                spec_str = ",".join(f"'{s}'" for s in reaction_specs)
                                result_parts.append(f"SPEC_ID(1:{spec_count},{j})={spec_str}")

            if self.nu_spec:
                if isinstance(self.nu_spec, (int, float)):
                    result_parts.append(f"NU_SPEC={self.nu_spec}")
                elif isinstance(self.nu_spec, list):
                    if all(isinstance(item, (int, float)) for item in self.nu_spec):
                        # List per reaction
                        for j, nu in enumerate(self.nu_spec, 1):
                            result_parts.append(f"NU_SPEC({j})={nu}")
                    elif all(isinstance(item, list) for item in self.nu_spec):
                        # 2D array
                        for j, reaction_nus in enumerate(self.nu_spec, 1):
                            if isinstance(reaction_nus, list) and reaction_nus:
                                nu_count = len(reaction_nus)
                                result_parts.append(
                                    f"NU_SPEC(1:{nu_count},{j})={','.join(str(n) for n in reaction_nus)}"
                                )

            if self.matl_id_products:
                if isinstance(self.matl_id_products, str):
                    result_parts.append(f"MATL_ID='{self.matl_id_products}'")
                elif isinstance(self.matl_id_products, list):
                    if all(isinstance(item, str) for item in self.matl_id_products):
                        # List per reaction
                        for j, matl in enumerate(self.matl_id_products, 1):
                            result_parts.append(f"MATL_ID({j})='{matl}'")
                    elif all(isinstance(item, list) for item in self.matl_id_products):
                        # 2D array
                        for j, reaction_matls in enumerate(self.matl_id_products, 1):
                            if isinstance(reaction_matls, list) and reaction_matls:
                                matl_count = len(reaction_matls)
                                matl_str = ",".join(f"'{m}'" for m in reaction_matls)
                                result_parts.append(f"MATL_ID(1:{matl_count},{j})={matl_str}")

            if self.nu_matl:
                if isinstance(self.nu_matl, (int, float)):
                    result_parts.append(f"NU_MATL={self.nu_matl}")
                elif isinstance(self.nu_matl, list):
                    if all(isinstance(item, (int, float)) for item in self.nu_matl):
                        # List per reaction
                        for j, nu in enumerate(self.nu_matl, 1):
                            result_parts.append(f"NU_MATL({j})={nu}")
                    elif all(isinstance(item, list) for item in self.nu_matl):
                        # 2D array
                        for j, reaction_nus in enumerate(self.nu_matl, 1):
                            if isinstance(reaction_nus, list) and reaction_nus:
                                nu_count = len(reaction_nus)
                                result_parts.append(
                                    f"NU_MATL(1:{nu_count},{j})={','.join(str(n) for n in reaction_nus)}"
                                )

            if self.heat_of_combustion_array:
                if isinstance(self.heat_of_combustion_array, (int, float)):
                    result_parts.append(f"HEAT_OF_COMBUSTION={self.heat_of_combustion_array}")
                elif isinstance(self.heat_of_combustion_array, list):
                    if all(
                        isinstance(item, (int, float)) for item in self.heat_of_combustion_array
                    ):
                        # List per reaction
                        for j, hoc in enumerate(self.heat_of_combustion_array, 1):
                            result_parts.append(f"HEAT_OF_COMBUSTION({j})={hoc}")
                    elif all(isinstance(item, list) for item in self.heat_of_combustion_array):
                        # 2D array
                        for j, reaction_hocs in enumerate(self.heat_of_combustion_array, 1):
                            if isinstance(reaction_hocs, list) and reaction_hocs:
                                hoc_count = len(reaction_hocs)
                                result_parts.append(
                                    f"HEAT_OF_COMBUSTION(1:{hoc_count},{j})={','.join(str(h) for h in reaction_hocs)}"
                                )

            # Handle particle products
            if self.part_id:
                if isinstance(self.part_id, str):
                    result_parts.append(f"PART_ID='{self.part_id}'")
                elif isinstance(self.part_id, list):
                    if all(isinstance(item, str) for item in self.part_id):
                        # List per reaction
                        for j, part in enumerate(self.part_id, 1):
                            result_parts.append(f"PART_ID({j})='{part}'")
                    elif all(isinstance(item, list) for item in self.part_id):
                        # 2D array
                        for j, reaction_parts in enumerate(self.part_id, 1):
                            if reaction_parts:
                                part_count = len(reaction_parts)
                                part_str = ",".join(f"'{p}'" for p in reaction_parts)
                                result_parts.append(f"PART_ID(1:{part_count},{j})={part_str}")

            if self.nu_part:
                if isinstance(self.nu_part, (int, float)):
                    result_parts.append(f"NU_PART={self.nu_part}")
                elif isinstance(self.nu_part, list):
                    if all(isinstance(item, (int, float)) for item in self.nu_part):
                        # List per reaction
                        for j, nu in enumerate(self.nu_part, 1):
                            result_parts.append(f"NU_PART({j})={nu}")
                    elif all(isinstance(item, list) for item in self.nu_part):
                        # 2D array
                        for j, reaction_nus in enumerate(self.nu_part, 1):
                            if isinstance(reaction_nus, list) and reaction_nus:
                                nu_count = len(reaction_nus)
                                result_parts.append(
                                    f"NU_PART(1:{nu_count},{j})={','.join(str(n) for n in reaction_nus)}"
                                )

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

        # Single-reaction product specification
        if self.spec_id:
            if isinstance(self.spec_id, str):
                params["spec_id"] = self.spec_id
            elif isinstance(self.spec_id, list):
                # Multiple species for single reaction
                spec_str = ",".join(f"'{s}'" for s in self.spec_id)
                params["spec_id"] = spec_str
        if self.matl_id_products and isinstance(self.matl_id_products, str):
            params["matl_id"] = self.matl_id_products
        if self.heat_of_combustion_array and isinstance(
            self.heat_of_combustion_array, (int, float)
        ):
            params["heat_of_combustion"] = self.heat_of_combustion_array

        # Particle products (single reaction)
        if self.part_id and isinstance(self.part_id, str):
            params["part_id"] = self.part_id
        if self.nu_part and isinstance(self.nu_part, (int, float)):
            params["nu_part"] = self.nu_part

        # Liquid Fuel Properties (Priority 1)
        if self.boiling_temperature is not None:
            params["boiling_temperature"] = self.boiling_temperature
        if self.mw is not None:
            params["mw"] = self.mw
        if self.heat_of_vaporization is not None:
            params["heat_of_vaporization"] = self.heat_of_vaporization

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

        # Delamination (Priority 1)
        if self.delamination_tmp is not None:
            params["delamination_tmp"] = self.delamination_tmp
        if self.delamination_density is not None:
            params["delamination_density"] = self.delamination_density

        return self._build_namelist("MATL", params)
