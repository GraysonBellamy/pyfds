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

    # Product specification
    nu_spec: list[str] | None = Field(None, description="Product species IDs")
    nu_matl: list[str] | None = Field(None, description="Residue material IDs")

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
            for param_name in ["a", "e", "heat_of_reaction"]:
                param_value = getattr(self, param_name)
                if param_value is not None and len(param_value) != self.n_reactions:
                    raise ValueError(
                        f"Material '{self.id}': {param_name.upper()} must have "
                        f"{self.n_reactions} values for N_REACTIONS={self.n_reactions}"
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
        if self.nu_spec:
            params["nu_spec"] = self.nu_spec
        if self.nu_matl:
            params["nu_matl"] = self.nu_matl

        return self._build_namelist("MATL", params)
