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
    formula : str, optional
        Chemical formula (e.g., 'C2H6O2')
    alt_id : str, optional
        Alternative species ID for lookups
    background : bool, optional
        Use as background species, default: False
    primitive : bool, optional
        Treat duplicate species as primitive, default: False
    copy_lumped : bool, optional
        Create copy of lumped species, default: False
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
    volume_fraction_0 : float, optional
        Initial volume fraction in ambient
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
    reference_temperature : float, optional
        Reference temperature [°C]
    reference_enthalpy : float, optional
        Enthalpy at reference temp [kJ/kg]
    enthalpy_of_formation : float, optional
        Formation enthalpy [kJ/mol]
    polynomial_coeff : list[list[float]], optional
        NASA polynomial coefficients (2 sets of 7 coefficients each)
    polynomial_temp : list[float], optional
        Polynomial temperature ranges [K] (3 values: T_low, T_mid, T_high)
    specific_heat : float, optional
        Specific heat capacity [kJ/(kg·K)]
    ramp_cp : str, optional
        Specific heat ramp ID
    conductivity : float, optional
        Thermal conductivity [W/(m·K)]
    ramp_k : str, optional
        Conductivity ramp ID
    viscosity : float, optional
        Dynamic viscosity [kg/(m·s)]
    ramp_mu : str, optional
        Viscosity ramp ID
    diffusivity : float, optional
        Mass diffusivity [m²/s]
    ramp_d : str, optional
        Diffusivity ramp ID
    ramp_g_f : str, optional
        Gibbs free energy ramp ID
    aerosol : bool, optional
        Species is an aerosol, default: False
    density_solid : float, optional
        Density of aerosol particles [kg/m³]
    mean_diameter : float, optional
        Mean diameter of aerosol particles [m]
    radcal_id : str, optional
        RadCal surrogate species for absorption
    pr : float, optional
        Prandtl number
    pr_gas : float, optional
        Gas phase Prandtl number
    sc : float, optional
        Schmidt number
    turbulent_schmidt_number : float, optional
        Turbulent Schmidt number
    sigmalj : float, optional
        Lennard-Jones sigma [Å]
    epsilonklj : float, optional
        Lennard-Jones epsilon/k [K]
    gamma : float, optional
        Ratio of specific heats
    boiling_temperature : float, optional
        Boiling temperature [°C]
    vaporization_temperature : float, optional
        Vaporization temperature [°C]
    heat_of_vaporization : float, optional
        Heat of vaporization [kJ/kg]
    density_liquid : float, optional
        Liquid density [kg/m³]
    specific_heat_liquid : float, optional
        Liquid specific heat [kJ/(kg·K)]
    conductivity_liquid : float, optional
        Liquid conductivity [W/(m·K)]
    viscosity_liquid : float, optional
        Liquid viscosity [kg/(m·s)]
    surface_tension : float, optional
        Surface tension [N/m]
    melting_temperature : float, optional
        Melting temperature [°C]
    h_v_reference_temperature : float, optional
        Reference temp for vaporization [°C]
    Examples
    --------
    >>> # Predefined species (oxygen)
    >>> o2 = Species(id='OXYGEN', mass_fraction_0=0.23)

    >>> # User-defined fuel species with formula
    >>> fuel = Species(id='MY_FUEL', formula='C3H8O3N4', mw=92.0)

    >>> # Background species
    >>> air = Species(id='AIR', background=True, spec_id=['N2', 'O2'], volume_fraction=[0.79, 0.21])

    >>> # Species with temperature-dependent properties
    >>> gas = Species(id='HOT_GAS', ramp_cp='CP_RAMP', ramp_mu='MU_RAMP')

    >>> # Species with Lennard-Jones parameters
    >>> lj_species = Species(id='LJ_SPEC', sigmalj=3.5, epsilonklj=150.0)

    >>> # Species with NASA polynomials
    >>> thermo_spec = Species(
    ...     id='THERMO_SPEC',
    ...     polynomial_coeff=[
    ...         [2.5, 1.2e-3, -5.1e-7, 1.0e-9, -8.5e-13, -1.2e3, 3.5],
    ...         [3.2, 1.5e-3, -4.8e-7, 9.2e-10, -7.1e-13, -1.5e3, 4.2]
    ...     ],
    ...     polynomial_temp=[300.0, 1000.0, 5000.0]
    ... )

    >>> # Liquid properties for droplet simulation
    >>> liquid = Species(
    ...     id='WATER_LIQUID',
    ...     density_liquid=1000.0,
    ...     boiling_temperature=100.0,
    ...     heat_of_vaporization=2257.0
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

    # Species identification
    formula: str | None = Field(None, description="Chemical formula (e.g., 'C2H6O2')")
    alt_id: str | None = Field(None, description="Alternative species ID for lookups")
    background: bool = Field(False, description="Use as background species")
    primitive: bool = Field(False, description="Treat duplicate species as primitive")
    copy_lumped: bool = Field(False, description="Create copy of lumped species")

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

    # Reference enthalpy
    reference_temperature: float | None = Field(None, description="Reference temperature [°C]")
    reference_enthalpy: float | None = Field(None, description="Enthalpy at reference temp [kJ/kg]")
    enthalpy_of_formation: float | None = Field(None, description="Formation enthalpy [kJ/mol]")

    # NASA polynomials (2 sets of 7 coefficients)
    polynomial_coeff: list[list[float]] | None = Field(
        None, description="NASA polynomial coefficients"
    )
    polynomial_temp: list[float] | None = Field(
        None, description="Polynomial temperature ranges [K]"
    )

    # Temperature-dependent property ramps
    ramp_k: str | None = Field(None, description="Conductivity ramp ID")
    ramp_d: str | None = Field(None, description="Diffusivity ramp ID")
    ramp_mu: str | None = Field(None, description="Viscosity ramp ID")
    ramp_cp: str | None = Field(None, description="Specific heat ramp ID")
    ramp_g_f: str | None = Field(None, description="Gibbs free energy ramp ID")

    # Aerosol properties
    aerosol: bool = Field(False, description="Species is an aerosol")
    density_solid: float | None = Field(None, gt=0, description="Aerosol particle density [kg/m³]")
    mean_diameter: float | None = Field(None, gt=0, description="Aerosol mean diameter [m]")

    # Radiation properties
    radcal_id: str | None = Field(None, description="RadCal surrogate species for absorption")

    # Additional properties
    pr: float | None = Field(None, gt=0, description="Prandtl number")
    sc: float | None = Field(None, gt=0, description="Schmidt number")

    # Lennard-Jones potential parameters
    sigmalj: float | None = Field(None, gt=0, description="Lennard-Jones sigma [Å]")
    epsilonklj: float | None = Field(None, gt=0, description="Lennard-Jones epsilon/k [K]")

    # Gas phase properties
    pr_gas: float | None = Field(None, gt=0, description="Gas phase Prandtl number")
    turbulent_schmidt_number: float | None = Field(
        None, gt=0, description="Turbulent Schmidt number"
    )
    gamma: float | None = Field(None, gt=1, description="Ratio of specific heats")

    # Liquid properties for droplet simulations
    boiling_temperature: float | None = Field(None, description="Boiling temperature [°C]")
    vaporization_temperature: float | None = Field(
        None, description="Vaporization temperature [°C]"
    )
    heat_of_vaporization: float | None = Field(
        None, gt=0, description="Heat of vaporization [kJ/kg]"
    )
    density_liquid: float | None = Field(None, gt=0, description="Liquid density [kg/m³]")
    specific_heat_liquid: float | None = Field(
        None, gt=0, description="Liquid specific heat [kJ/(kg·K)]"
    )
    conductivity_liquid: float | None = Field(
        None, gt=0, description="Liquid conductivity [W/(m·K)]"
    )
    viscosity_liquid: float | None = Field(None, gt=0, description="Liquid viscosity [kg/(m·s)]")
    surface_tension: float | None = Field(None, gt=0, description="Surface tension [N/m]")
    melting_temperature: float | None = Field(None, description="Melting temperature [°C]")
    h_v_reference_temperature: float | None = Field(
        None, description="Reference temp for vaporization [°C]"
    )

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

        # Background species validation
        if self.background and self.lumped_component_only:
            raise ValueError("Background species cannot have LUMPED_COMPONENT_ONLY=True")

        # NASA polynomial validation
        if self.polynomial_coeff is not None:
            if len(self.polynomial_coeff) != 2:
                raise ValueError("POLYNOMIAL_COEFF requires exactly 2 sets of coefficients")
            for i, coeffs in enumerate(self.polynomial_coeff):
                if len(coeffs) != 7:
                    raise ValueError(
                        f"Polynomial set {i + 1} requires 7 coefficients, got {len(coeffs)}"
                    )

        if self.polynomial_temp is not None and len(self.polynomial_temp) != 3:
            raise ValueError("POLYNOMIAL_TEMP requires 3 temperature values [T_low, T_mid, T_high]")

        return self

    def to_fds(self) -> str:
        """Generate FDS SPEC namelist."""
        params: dict[str, Any] = {}

        # Use FUEL if specified, otherwise ID
        if self.fuel:
            params["fuel"] = self.fuel
        elif self.id:
            params["id"] = self.id

        # Species identification
        if self.formula is not None:
            params["formula"] = self.formula
        if self.alt_id is not None:
            params["alt_id"] = self.alt_id
        if self.background:
            params["background"] = self.background
        if self.primitive:
            params["primitive"] = self.primitive
        if self.copy_lumped:
            params["copy_lumped"] = self.copy_lumped

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

        # Temperature-dependent ramps
        if self.ramp_k is not None:
            params["ramp_k"] = self.ramp_k
        if self.ramp_d is not None:
            params["ramp_d"] = self.ramp_d
        if self.ramp_mu is not None:
            params["ramp_mu"] = self.ramp_mu
        if self.ramp_cp is not None:
            params["ramp_cp"] = self.ramp_cp
        if self.ramp_g_f is not None:
            params["ramp_g_f"] = self.ramp_g_f

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

        # Lennard-Jones parameters
        if self.sigmalj is not None:
            params["sigmalj"] = self.sigmalj
        if self.epsilonklj is not None:
            params["epsilonklj"] = self.epsilonklj

        # Gas phase properties
        if self.pr_gas is not None:
            params["pr_gas"] = self.pr_gas
        if self.turbulent_schmidt_number is not None:
            params["turbulent_schmidt_number"] = self.turbulent_schmidt_number
        if self.gamma is not None:
            params["gamma"] = self.gamma

        # Reference enthalpy
        if self.reference_temperature is not None:
            params["reference_temperature"] = self.reference_temperature
        if self.reference_enthalpy is not None:
            params["reference_enthalpy"] = self.reference_enthalpy
        if self.enthalpy_of_formation is not None:
            params["enthalpy_of_formation"] = self.enthalpy_of_formation

        # NASA polynomials
        if self.polynomial_coeff is not None:
            params["polynomial_coeff"] = self.polynomial_coeff
        if self.polynomial_temp is not None:
            params["polynomial_temp"] = self.polynomial_temp

        # Radiation properties
        if self.radcal_id is not None:
            params["radcal_id"] = self.radcal_id

        # Liquid properties
        if self.boiling_temperature is not None:
            params["boiling_temperature"] = self.boiling_temperature
        if self.vaporization_temperature is not None:
            params["vaporization_temperature"] = self.vaporization_temperature
        if self.heat_of_vaporization is not None:
            params["heat_of_vaporization"] = self.heat_of_vaporization
        if self.density_liquid is not None:
            params["density_liquid"] = self.density_liquid
        if self.specific_heat_liquid is not None:
            params["specific_heat_liquid"] = self.specific_heat_liquid
        if self.conductivity_liquid is not None:
            params["conductivity_liquid"] = self.conductivity_liquid
        if self.viscosity_liquid is not None:
            params["viscosity_liquid"] = self.viscosity_liquid
        if self.surface_tension is not None:
            params["surface_tension"] = self.surface_tension
        if self.melting_temperature is not None:
            params["melting_temperature"] = self.melting_temperature
        if self.h_v_reference_temperature is not None:
            params["h_v_reference_temperature"] = self.h_v_reference_temperature

        return self._build_namelist("SPEC", params)
