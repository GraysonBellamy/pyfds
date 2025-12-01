"""FDS MATL namelist for material properties.

Defines thermal, radiative, and pyrolysis properties for solid materials
used in heat transfer calculations and burning behavior.

Field Groups:
    core: Basic identification (id) and reactions
    thermal: Thermal transport properties (density, conductivity, specific_heat)
    radiation: Radiative properties (emissivity, absorption_coefficient)
    pyrolysis: Reaction kinetics via structured PyrolysisReaction objects
    liquid: Liquid fuel evaporation model
"""

from typing import Any

from pydantic import field_validator, model_validator

from pyfds.core.models.pyrolysis import PyrolysisReaction
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Material"]


class Material(NamelistBase):
    """FDS MATL namelist - material properties.

    Parameters
    ----------
    id : str
        Unique material identifier.
    density : float
        Material density in kg/m³.
    conductivity : float, optional
        Thermal conductivity in W/(m·K).
    conductivity_ramp : str, optional
        RAMP ID for temperature-dependent conductivity.
    specific_heat : float, optional
        Specific heat capacity in kJ/(kg·K).
    specific_heat_ramp : str, optional
        RAMP ID for temperature-dependent specific heat.
    reactions : list[PyrolysisReaction], optional
        Pyrolysis reactions for decomposition behavior.

    Examples
    --------
    Simple inert material:

    >>> matl = Material(id='CONCRETE', density=2300, conductivity=1.0, specific_heat=0.88)
    >>> print(matl.to_fds())
    &MATL ID='CONCRETE', DENSITY=2300, CONDUCTIVITY=1.0, SPECIFIC_HEAT=0.88 /

    Material with pyrolysis reaction:

    >>> from pyfds.core.models import PyrolysisProduct, PyrolysisReaction
    >>> wood = Material(
    ...     id='WOOD',
    ...     density=500.0,
    ...     conductivity=0.13,
    ...     specific_heat=2.5,
    ...     reactions=[
    ...         PyrolysisReaction(
    ...             heat_of_reaction=1800.0,
    ...             a=1e10,
    ...             e=100000.0,
    ...             products=[
    ...                 PyrolysisProduct(spec_id='WOOD_GAS', nu_spec=0.75),
    ...                 PyrolysisProduct(matl_id='CHAR', nu_matl=0.25),
    ...             ],
    ...         )
    ...     ],
    ... )

    See Also
    --------
    Surface : Surface properties that reference materials.
    PyrolysisReaction : Structured pyrolysis reaction definition.
    """

    # =========================================================================
    # CORE PARAMETERS
    # =========================================================================
    id: str = FdsField(..., description="Material identifier", group="core")

    reactions: list[PyrolysisReaction] | None = FdsField(
        None,
        description="Pyrolysis reactions for decomposition behavior",
        group="pyrolysis",
    )

    # =========================================================================
    # THERMAL PROPERTIES
    # =========================================================================
    density: float = FdsField(..., gt=0, description="Material density [kg/m³]", group="thermal")

    conductivity: float | None = FdsField(
        None, gt=0, description="Thermal conductivity [W/(m·K)]", group="thermal"
    )
    conductivity_ramp: str | None = FdsField(
        None,
        description="RAMP ID for temperature-dependent conductivity",
        group="thermal",
    )

    specific_heat: float | None = FdsField(
        None, gt=0, description="Specific heat capacity [kJ/(kg·K)]", group="thermal"
    )
    specific_heat_ramp: str | None = FdsField(
        None,
        description="RAMP ID for temperature-dependent specific heat",
        group="thermal",
    )

    # =========================================================================
    # RADIATION PROPERTIES
    # =========================================================================
    emissivity: float = FdsField(
        0.9, ge=0, le=1, description="Surface emissivity [0-1]", group="radiation"
    )
    absorption_coefficient: float = FdsField(
        50000.0,
        ge=0,
        description="Radiation absorption coefficient [1/m]",
        group="radiation",
    )

    # =========================================================================
    # LIQUID FUEL PROPERTIES
    # =========================================================================
    boiling_temperature: float | None = FdsField(
        None, description="Boiling point [°C] - triggers liquid model", group="liquid"
    )
    mw: float | None = FdsField(
        None, gt=0, description="Molecular weight for liquid [g/mol]", group="liquid"
    )

    # =========================================================================
    # SHRINKING/SWELLING PARAMETERS
    # =========================================================================
    allow_shrinking: bool = FdsField(
        True, description="Allow material to shrink during pyrolysis", group="shrink_swell"
    )
    allow_swelling: bool = FdsField(
        True, description="Allow material to swell during pyrolysis", group="shrink_swell"
    )

    # =========================================================================
    # ENERGY CONSERVATION PARAMETERS
    # =========================================================================
    adjust_h: bool = FdsField(
        True, description="Adjust enthalpy for energy conservation", group="energy"
    )
    reference_enthalpy: float | None = FdsField(
        None, description="Reference enthalpy [kJ/kg]", group="energy"
    )
    reference_enthalpy_temperature: float | None = FdsField(
        None, description="Reference enthalpy temperature [K]", group="energy"
    )

    # =========================================================================
    # REACTION CONTROL PARAMETERS
    # =========================================================================
    reac_rate_delta: float = FdsField(
        0.05, description="Fractional change in pyrolysis rate before renoding", group="reaction"
    )

    # =========================================================================
    # OXIDATION MODEL PARAMETERS
    # =========================================================================
    surface_oxidation_model: bool = FdsField(
        False, description="Use surface oxidation model (vegetation)", group="oxidation"
    )
    x_o2_pyro: bool | None = FdsField(
        None, description="Include O2 transport in pyrolysis region", group="oxidation"
    )

    # =========================================================================
    # VALIDATORS
    # =========================================================================
    @field_validator("density")
    @classmethod
    def validate_density_range(cls, v: float) -> float:
        """Validate density is in physically reasonable range.

        Checks:
        - Density is between 0.1 and 25000 kg/m³
        - Allows low-density foams and high-density metals
        """
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
        """Number of pyrolysis reactions defined for this material."""
        if self.reactions:
            return len(self.reactions)
        return 0

    @model_validator(mode="after")
    def validate_material(self) -> "Material":
        """Validate material property consistency.

        Checks:
        - Either conductivity or conductivity_ramp must be specified
        - Either specific_heat or specific_heat_ramp must be specified
        - Liquid fuel model requirements (boiling_temperature needs spec_id in reactions)
        """
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

        # Validate liquid fuel model
        if self.boiling_temperature is not None:
            if self.n_reactions > 1:
                raise ValueError(
                    f"Material '{self.id}': BOILING_TEMPERATURE (liquid model) "
                    f"cannot be used with multiple reactions"
                )
            # Check that we have a gas species product for evaporation
            has_gas_product = False
            if self.reactions:
                for rxn in self.reactions:
                    for product in rxn.products:
                        if product.spec_id is not None:
                            has_gas_product = True
                            break
            if not has_gas_product:
                raise ValueError(
                    f"Material '{self.id}': BOILING_TEMPERATURE requires a reaction "
                    f"with a gas species product (spec_id) for the evaporated fuel"
                )

        return self

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "MATL"

    def _collect_fds_params(self) -> dict[str, Any]:
        """Collect FDS parameters, converting structured reactions to arrays.

        Returns
        -------
        dict[str, Any]
            Dictionary of parameter name-value pairs for FDS output.
        """
        params = super()._collect_fds_params()

        # Remove 'reactions' from output - we convert to FDS array format
        params.pop("REACTIONS", None)

        if self.reactions:
            reaction_params = self._reactions_to_fds_arrays()
            params.update(reaction_params)

        return params

    def _reactions_to_fds_arrays(self) -> dict[str, str]:
        """Convert PyrolysisReaction list to FDS parallel array format.

        FDS uses parallel arrays for multi-reaction materials:
        - A(1:N_REACTIONS), E(1:N_REACTIONS), etc. for kinetic parameters
        - SPEC_ID(1:N_SPEC, 1:N_REACTIONS), NU_SPEC(...) for products

        Returns
        -------
        dict[str, str]
            Dictionary of FDS parameter strings.
        """
        if not self.reactions:
            return {}

        n_reactions = len(self.reactions)
        params: dict[str, str] = {}

        # Collect kinetic parameters from all reactions
        a_values: list[float] = []
        e_values: list[float] = []
        reference_temperature_values: list[float] = []
        reference_rate_values: list[float] = []
        pyrolysis_range_values: list[float] = []
        heating_rate_values: list[float] = []
        heat_of_reaction_values: list[float] = []
        n_s_values: list[float] = []
        n_t_values: list[float] = []
        n_o2_values: list[float] = []
        gas_diffusion_depth_values: list[float] = []
        max_reaction_rate_values: list[float] = []

        # Collect products per reaction
        spec_ids: list[list[str]] = []
        nu_specs: list[list[float]] = []
        heat_of_combustion_values: list[list[float]] = []
        matl_ids: list[list[str]] = []
        nu_matls: list[list[float]] = []
        part_ids: list[list[str]] = []
        nu_parts: list[list[float]] = []

        for rxn in self.reactions:
            # Kinetic parameters
            if rxn.a is not None:
                a_values.append(rxn.a)
            if rxn.e is not None:
                e_values.append(rxn.e)
            if rxn.reference_temperature is not None:
                reference_temperature_values.append(rxn.reference_temperature)
            if rxn.reference_rate is not None:
                reference_rate_values.append(rxn.reference_rate)
            if rxn.pyrolysis_range is not None:
                pyrolysis_range_values.append(rxn.pyrolysis_range)

            heating_rate_values.append(rxn.heating_rate)
            heat_of_reaction_values.append(rxn.heat_of_reaction)
            n_s_values.append(rxn.n_s)
            n_t_values.append(rxn.n_t)
            n_o2_values.append(rxn.n_o2)

            if rxn.gas_diffusion_depth is not None:
                gas_diffusion_depth_values.append(rxn.gas_diffusion_depth)
            if rxn.max_reaction_rate is not None:
                max_reaction_rate_values.append(rxn.max_reaction_rate)

            # Collect products for this reaction
            rxn_spec_ids: list[str] = []
            rxn_nu_specs: list[float] = []
            rxn_heat_of_combustion: list[float] = []
            rxn_matl_ids: list[str] = []
            rxn_nu_matls: list[float] = []
            rxn_part_ids: list[str] = []
            rxn_nu_parts: list[float] = []

            for product in rxn.products:
                if product.spec_id is not None:
                    rxn_spec_ids.append(product.spec_id)
                    rxn_nu_specs.append(product.nu_spec or 0.0)
                    if product.heat_of_combustion is not None:
                        rxn_heat_of_combustion.append(product.heat_of_combustion)
                if product.matl_id is not None:
                    rxn_matl_ids.append(product.matl_id)
                    rxn_nu_matls.append(product.nu_matl or 0.0)
                if product.part_id is not None:
                    rxn_part_ids.append(product.part_id)
                    rxn_nu_parts.append(product.nu_part or 0.0)

            spec_ids.append(rxn_spec_ids)
            nu_specs.append(rxn_nu_specs)
            heat_of_combustion_values.append(rxn_heat_of_combustion)
            matl_ids.append(rxn_matl_ids)
            nu_matls.append(rxn_nu_matls)
            part_ids.append(rxn_part_ids)
            nu_parts.append(rxn_nu_parts)

        # Build FDS parameter dictionary
        if n_reactions > 1:
            params["N_REACTIONS"] = str(n_reactions)

        # Kinetic parameters (only output if values exist)
        if a_values:
            params["A"] = self._format_value(a_values)
        if e_values:
            params["E"] = self._format_value(e_values)
        if reference_temperature_values:
            params["REFERENCE_TEMPERATURE"] = self._format_value(reference_temperature_values)
        if reference_rate_values:
            params["REFERENCE_RATE"] = self._format_value(reference_rate_values)
        if pyrolysis_range_values:
            params["PYROLYSIS_RANGE"] = self._format_value(pyrolysis_range_values)

        # Only output non-default values
        if any(v != 5.0 for v in heating_rate_values):
            params["HEATING_RATE"] = self._format_value(heating_rate_values)
        if any(v != 0.0 for v in heat_of_reaction_values):
            params["HEAT_OF_REACTION"] = self._format_value(heat_of_reaction_values)
        if any(v != 1.0 for v in n_s_values):
            params["N_S"] = self._format_value(n_s_values)
        if any(v != 0.0 for v in n_t_values):
            params["N_T"] = self._format_value(n_t_values)
        if any(v != 0.0 for v in n_o2_values):
            params["N_O2"] = self._format_value(n_o2_values)

        if gas_diffusion_depth_values:
            params["GAS_DIFFUSION_DEPTH"] = self._format_value(gas_diffusion_depth_values)
        if max_reaction_rate_values:
            params["MAX_REACTION_RATE"] = self._format_value(max_reaction_rate_values)

        # Format gas species products - flatten for FDS output
        all_spec_ids: list[str] = []
        all_nu_specs: list[float] = []
        for rxn_specs, rxn_nus in zip(spec_ids, nu_specs, strict=True):
            all_spec_ids.extend(rxn_specs)
            all_nu_specs.extend(rxn_nus)

        if all_spec_ids:
            params["SPEC_ID"] = self._format_value(all_spec_ids)
            params["NU_SPEC"] = self._format_value(all_nu_specs)

        # Format heat of combustion if specified
        all_hoc: list[float] = []
        for rxn_hoc in heat_of_combustion_values:
            all_hoc.extend(rxn_hoc)
        if all_hoc:
            params["HEAT_OF_COMBUSTION"] = self._format_value(all_hoc)

        # Format solid residue products
        all_matl_ids: list[str] = []
        all_nu_matls: list[float] = []
        for rxn_matls, rxn_nus in zip(matl_ids, nu_matls, strict=True):
            all_matl_ids.extend(rxn_matls)
            all_nu_matls.extend(rxn_nus)

        if all_matl_ids:
            params["MATL_ID"] = self._format_value(all_matl_ids)
            params["NU_MATL"] = self._format_value(all_nu_matls)

        # Format particle products
        all_part_ids: list[str] = []
        all_nu_parts: list[float] = []
        for rxn_parts, rxn_nus in zip(part_ids, nu_parts, strict=True):
            all_part_ids.extend(rxn_parts)
            all_nu_parts.extend(rxn_nus)

        if all_part_ids:
            params["PART_ID"] = self._format_value(all_part_ids)
            params["NU_PART"] = self._format_value(all_nu_parts)

        return params
