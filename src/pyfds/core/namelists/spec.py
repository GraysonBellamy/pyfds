"""
FDS SPEC namelist.

Gas species definition for combustion and pyrolysis.
"""

from typing import Any

from pydantic import Field, field_validator, model_validator

from pyfds.core.namelists.base import NamelistBase


class Species(NamelistBase):
    """
    FDS SPEC namelist - gas species definition.

    Defines gas species for combustion and pyrolysis modeling. Species can be
    predefined (from FDS database), user-defined with chemical composition,
    or lumped components for simplified modeling.

    Parameters
    ----------
    id : str
        Unique species identifier
    fuel : str, optional
        Fuel name for user-defined species with chemical composition
    c : float, optional
        Number of carbon atoms in molecule
    h : float, optional
        Number of hydrogen atoms in molecule
    o : float, optional
        Number of oxygen atoms in molecule
    n : float, optional
        Number of nitrogen atoms in molecule
    mw : float, optional
        Molecular weight [g/mol]
    mass_fraction_0 : float, optional
        Initial mass fraction in ambient (e.g., 0.23 for oxygen)
    lumped_component_only : bool, optional
        Species is a lumped component (predefined properties), default: False
    spec_id : list[str], optional
        Component species IDs for mixture species
    mass_fraction : list[float], optional
        Mass fractions of components for mixture species
    volume_fraction : list[float], optional
        Volume fractions of components for mixture species
    enthalpy : float, optional
        Enthalpy of formation [kJ/kg]
    specific_heat : float, optional
        Specific heat capacity [kJ/(kg·K)]
    conductivity : float, optional
        Thermal conductivity [W/(m·K)]
    viscosity : float, optional
        Dynamic viscosity [kg/(m·s)]
    diffusivity : float, optional
        Mass diffusivity [m²/s]
    aerosol : bool, optional
        Species is an aerosol, default: False
    density_solid : float, optional
        Density of aerosol particles [kg/m³]
    mean_diameter : float, optional
        Mean diameter of aerosol particles [m]

    Examples
    --------
    >>> # Predefined species (oxygen)
    >>> o2 = Species(id='OXYGEN', mass_fraction_0=0.23)

    >>> # User-defined fuel species
    >>> fuel = Species(fuel='MY_FUEL', c=7, h=16)

    >>> # Lumped component
    >>> toluene = Species(id='TOLUENE', lumped_component_only=True)

    >>> # Mixture species
    >>> mix = Species(
    ...     id='FUEL_MIX',
    ...     spec_id=['PROPANE', 'ETHANE'],
    ...     mass_fraction=[0.7, 0.3]
    ... )

    Notes
    -----
    - Either ID or FUEL must be specified
    - For user-defined species, specify chemical composition (C, H, O, N) or MW
    - SPEC_ID and MASS_FRACTION/VOLUME_FRACTION must have matching lengths
    - See FDS User Guide Appendix for list of predefined species
    """

    id: str | None = Field(None, description="Species identifier")
    fuel: str | None = Field(None, description="Fuel name for composition-defined species")

    # Chemical composition for user-defined species
    c: float | None = Field(None, ge=0, description="Carbon atoms")
    h: float | None = Field(None, ge=0, description="Hydrogen atoms")
    o: float | None = Field(None, ge=0, description="Oxygen atoms")
    n: float | None = Field(None, ge=0, description="Nitrogen atoms")
    mw: float | None = Field(None, gt=0, description="Molecular weight [g/mol]")

    # Ambient composition
    mass_fraction_0: float | None = Field(
        None, ge=0, le=1, description="Initial mass fraction in ambient"
    )
    volume_fraction_0: float | None = Field(
        None, ge=0, le=1, description="Initial volume fraction in ambient"
    )

    # Predefined species flags
    lumped_component_only: bool = Field(False, description="Lumped component flag")

    # Mixture species
    spec_id: list[str] | None = Field(None, description="Component species IDs for mixture")
    mass_fraction: list[float] | None = Field(
        None, description="Component mass fractions for mixture"
    )
    volume_fraction: list[float] | None = Field(
        None, description="Component volume fractions for mixture"
    )

    # Thermophysical properties (optional overrides)
    enthalpy: float | None = Field(None, description="Enthalpy of formation [kJ/kg]")
    specific_heat: float | None = Field(None, gt=0, description="Specific heat [kJ/(kg·K)]")
    conductivity: float | None = Field(None, gt=0, description="Thermal conductivity [W/(m·K)]")
    viscosity: float | None = Field(None, gt=0, description="Dynamic viscosity [kg/(m·s)]")
    diffusivity: float | None = Field(None, gt=0, description="Mass diffusivity [m²/s]")

    # Aerosol properties
    aerosol: bool = Field(False, description="Species is an aerosol")
    density_solid: float | None = Field(None, gt=0, description="Aerosol particle density [kg/m³]")
    mean_diameter: float | None = Field(None, gt=0, description="Aerosol mean diameter [m]")

    # Additional properties
    pr: float | None = Field(None, gt=0, description="Prandtl number")
    sc: float | None = Field(None, gt=0, description="Schmidt number")

    @field_validator("mass_fraction_0")
    @classmethod
    def validate_mass_fraction_0(cls, v: float | None) -> float | None:
        """Validate initial mass fraction."""
        if v is not None and not (0.0 <= v <= 1.0):
            raise ValueError(f"MASS_FRACTION_0 must be between 0 and 1, got {v}")
        return v

    @field_validator("volume_fraction_0")
    @classmethod
    def validate_volume_fraction_0(cls, v: float | None) -> float | None:
        """Validate initial volume fraction."""
        if v is not None and not (0.0 <= v <= 1.0):
            raise ValueError(f"VOLUME_FRACTION_0 must be between 0 and 1, got {v}")
        return v

    @model_validator(mode="after")
    def validate_species(self) -> "Species":
        """Validate species definition."""
        # Check that either ID or FUEL is specified
        if self.id is None and self.fuel is None:
            raise ValueError("Either ID or FUEL must be specified for SPEC")

        # Check mixture species consistency
        if self.spec_id is not None:
            if self.mass_fraction is not None and self.volume_fraction is not None:
                raise ValueError("Cannot specify both MASS_FRACTION and VOLUME_FRACTION")

            if self.mass_fraction is not None:
                if len(self.spec_id) != len(self.mass_fraction):
                    raise ValueError(
                        f"SPEC_ID ({len(self.spec_id)} items) and MASS_FRACTION "
                        f"({len(self.mass_fraction)} items) must have same length"
                    )
                # Validate mass fractions sum to ~1.0
                total = sum(self.mass_fraction)
                if not (0.99 <= total <= 1.01):
                    raise ValueError(f"MASS_FRACTION values must sum to 1.0, got {total:.3f}")

            if self.volume_fraction is not None:
                if len(self.spec_id) != len(self.volume_fraction):
                    raise ValueError(
                        f"SPEC_ID ({len(self.spec_id)} items) and VOLUME_FRACTION "
                        f"({len(self.volume_fraction)} items) must have same length"
                    )
                # Validate volume fractions sum to ~1.0
                total = sum(self.volume_fraction)
                if not (0.99 <= total <= 1.01):
                    raise ValueError(f"VOLUME_FRACTION values must sum to 1.0, got {total:.3f}")

        # Check aerosol consistency
        if self.aerosol and self.density_solid is None:
            raise ValueError("DENSITY_SOLID must be specified for aerosol species")

        # Validate ambient fractions are mutually exclusive
        if self.mass_fraction_0 is not None and self.volume_fraction_0 is not None:
            raise ValueError("Cannot specify both MASS_FRACTION_0 and VOLUME_FRACTION_0")

        return self

    def to_fds(self) -> str:
        """Generate FDS SPEC namelist."""
        params: dict[str, Any] = {}

        # Use FUEL if specified, otherwise ID
        if self.fuel:
            params["fuel"] = self.fuel
        elif self.id:
            params["id"] = self.id

        # Chemical composition
        if self.c is not None:
            params["c"] = self.c
        if self.h is not None:
            params["h"] = self.h
        if self.o is not None and self.o > 0:
            params["o"] = self.o
        if self.n is not None and self.n > 0:
            params["n"] = self.n
        if self.mw is not None:
            params["mw"] = self.mw

        # Ambient composition
        if self.mass_fraction_0 is not None:
            params["mass_fraction_0"] = self.mass_fraction_0
        if self.volume_fraction_0 is not None:
            params["volume_fraction_0"] = self.volume_fraction_0

        # Flags
        if self.lumped_component_only:
            params["lumped_component_only"] = self.lumped_component_only

        # Mixture components
        if self.spec_id:
            params["spec_id"] = self.spec_id
        if self.mass_fraction:
            params["mass_fraction"] = self.mass_fraction
        if self.volume_fraction:
            params["volume_fraction"] = self.volume_fraction

        # Thermophysical properties
        if self.enthalpy is not None:
            params["enthalpy"] = self.enthalpy
        if self.specific_heat is not None:
            params["specific_heat"] = self.specific_heat
        if self.conductivity is not None:
            params["conductivity"] = self.conductivity
        if self.viscosity is not None:
            params["viscosity"] = self.viscosity
        if self.diffusivity is not None:
            params["diffusivity"] = self.diffusivity

        # Aerosol properties
        if self.aerosol:
            params["aerosol"] = self.aerosol
        if self.density_solid is not None:
            params["density_solid"] = self.density_solid
        if self.mean_diameter is not None:
            params["mean_diameter"] = self.mean_diameter

        # Additional properties
        if self.pr is not None:
            params["pr"] = self.pr
        if self.sc is not None:
            params["sc"] = self.sc

        return self._build_namelist("SPEC", params)
