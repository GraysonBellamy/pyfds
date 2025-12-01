"""FDS SPEC namelist for gas species definition.

Defines gas species for combustion and pyrolysis modeling.

Field Groups:
    identification: Species ID and type
    composition: Elemental composition and formula
    ambient: Initial mass/volume fractions
    mixture: Lumped species components
    thermophysical: Enthalpy and polynomial coefficients
    polynomial: NASA polynomial temperature ranges
    ramps: Temperature-dependent property ramps
    aerosol: Aerosol particle properties
    radiation: Radiative absorption properties
    transport: Viscosity, conductivity, diffusivity
    lennard_jones: Molecular interaction parameters
    liquid: Liquid phase properties
    toxicity: FED/FIC toxicity parameters
    condensation: Condensation and refractive properties
    agglomeration: Particle agglomeration properties
    ode: ODE solver parameters for detailed chemistry
"""

from typing import Literal

from pydantic import field_validator, model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Species"]


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
    mass_fraction_cond_0 : float, optional
        Initial condensed mass fraction
    lumped_component_only : bool, optional
        Species is a lumped component (predefined properties), default: False
    spec_id : list[str], optional
        Component species IDs for mixture species
    mass_fraction : list[float], optional
        Mass fractions of components for mixture species
    volume_fraction : list[float], optional
        Volume fractions of components for mixture species
    specific_heat : float, optional
        Specific heat capacity [kJ/(kg·K)]
    conductivity : float, optional
        Thermal conductivity [W/(m·K)]
    conductivity_solid : float, optional
        Solid phase conductivity for aerosols [W/(m·K)]
    viscosity : float, optional
        Dynamic viscosity [kg/(m·s)]
    diffusivity : float, optional
        Mass diffusivity [m²/s]
    reference_temperature : float, optional
        Reference temperature [°C]
    reference_enthalpy : float, optional
        Enthalpy at reference temp [kJ/kg]
    enthalpy_of_formation : float, optional
        Formation enthalpy [kJ/mol]
    polynomial : str, optional
        Polynomial type ('NASA7' or 'NASA9')
    polynomial_coeff : list[list[float]], optional
        NASA polynomial coefficients (2 sets)
    polynomial_temp : list[float], optional
        Polynomial temperature ranges [K] (3 values: T_low, T_mid, T_high)
    ramp_cp : str, optional
        Specific heat ramp ID
    ramp_cp_l : str, optional
        Liquid specific heat ramp ID
    ramp_k : str, optional
        Conductivity ramp ID
    ramp_d : str, optional
        Diffusivity ramp ID
    ramp_mu : str, optional
        Viscosity ramp ID
    ramp_g_f : str, optional
        Gibbs free energy ramp ID
    aerosol : bool, optional
        Species is an aerosol, default: False
    density_solid : float, optional
        Density of aerosol particles [kg/m³]
    mean_diameter : float, optional
        Mean diameter of aerosol particles [m]
    thermophoretic_diameter : float, optional
        Thermophoretic diameter [m]
    radcal_id : str, optional
        RadCal surrogate species for absorption
    mass_extinction_coefficient : float, optional
        Mass extinction coefficient for smoke
    pr_gas : float, optional
        Gas phase Prandtl number
    turbulent_schmidt_number : float, optional
        Turbulent Schmidt number
    sigmalj : float, optional
        Lennard-Jones sigma [Å]
    epsilonklj : float, optional
        Lennard-Jones epsilon/k [K]
    vaporization_temperature : float, optional
        Vaporization temperature [°C]
    heat_of_vaporization : float, optional
        Heat of vaporization [kJ/kg]
    h_v_reference_temperature : float, optional
        Reference temp for vaporization [°C]
    beta_liquid : float, optional
        Liquid thermal expansion coefficient [1/K]
    density_liquid : float, optional
        Liquid density [kg/m³]
    specific_heat_liquid : float, optional
        Liquid specific heat [kJ/(kg·K)]
    conductivity_liquid : float, optional
        Liquid conductivity [W/(m·K)]
    viscosity_liquid : float, optional
        Liquid viscosity [kg/(m·s)]
    melting_temperature : float, optional
        Melting temperature [°C]
    fic_concentration : float, optional
        FIC concentration for FED calculations [ppm]
    fld_lethal_dose : float, optional
        FLD lethal dose for FED calculations [ppm*min]
    real_refractive_index : float, optional
        Real part of refractive index for condensation
    complex_refractive_index : float, optional
        Complex part of refractive index for condensation
    min_diameter : float, optional
        Minimum diameter for agglomeration [m]
    max_diameter : float, optional
        Maximum diameter for agglomeration [m]
    n_bins : int, optional
        Number of bins for agglomeration
    ode_abs_error : float, optional
        ODE absolute error tolerance for detailed chemistry
    ode_rel_error : float, optional
        ODE relative error tolerance for detailed chemistry

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

    See Also
    --------
    Reaction : Combustion reaction using species.
    Material : Solid materials that produce gas species.
    Initialization : Initial species concentrations.

    >>> # Species with NASA polynomials
    >>> thermo_spec = Species(
    ...     id='THERMO_SPEC',
    ...     polynomial='NASA7',
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
    ...     vaporization_temperature=100.0,
    ...     heat_of_vaporization=2257.0
    ... )

    Notes
    -----
    - ID must be specified for SPEC
    - For user-defined species, specify chemical composition (C, H, O, N) or MW
    - SPEC_ID and MASS_FRACTION/VOLUME_FRACTION must have matching lengths
    - POLYNOMIAL can be 'NASA7' (7 coefficients) or 'NASA9' (9 coefficients)
    - See FDS User Guide Appendix for list of predefined species
    """

    # Species identification
    id: str = FdsField(..., description="Species identifier", group="identification")
    formula: str | None = FdsField(
        None, description="Chemical formula (e.g., 'C2H6O2')", group="identification"
    )
    alt_id: str | None = FdsField(
        None, description="Alternative species ID for lookups", group="identification"
    )
    background: bool = FdsField(
        False, exclude_if=False, description="Use as background species", group="identification"
    )
    primitive: bool = FdsField(
        False,
        exclude_if=False,
        description="Treat duplicate species as primitive",
        group="identification",
    )
    copy_lumped: bool = FdsField(
        False, exclude_if=False, description="Create copy of lumped species", group="identification"
    )
    lumped_component_only: bool = FdsField(
        False, exclude_if=False, description="Lumped component flag", group="identification"
    )

    # Chemical composition for user-defined species
    c: float | None = FdsField(None, ge=0, description="Carbon atoms", group="composition")
    h: float | None = FdsField(None, ge=0, description="Hydrogen atoms", group="composition")
    o: float | None = FdsField(
        None, ge=0, exclude_if=0, description="Oxygen atoms", group="composition"
    )
    n: float | None = FdsField(
        None, ge=0, exclude_if=0, description="Nitrogen atoms", group="composition"
    )
    mw: float | None = FdsField(
        None, gt=0, description="Molecular weight [g/mol]", group="composition"
    )

    # Ambient composition
    mass_fraction_0: float | None = FdsField(
        None, ge=0, le=1, description="Initial mass fraction in ambient", group="ambient"
    )
    mass_fraction_cond_0: float | None = FdsField(
        None, ge=0, description="Initial condensed mass fraction", group="ambient"
    )

    # Mixture species
    spec_id: list[str] | None = FdsField(
        None, description="Component species IDs for mixture", group="mixture"
    )
    mass_fraction: list[float] | None = FdsField(
        None, description="Component mass fractions for mixture", group="mixture"
    )
    volume_fraction: list[float] | None = FdsField(
        None, description="Component volume fractions for mixture", group="mixture"
    )

    # Thermophysical properties
    specific_heat: float | None = FdsField(
        None, gt=0, description="Specific heat [kJ/(kg·K)]", group="thermophysical"
    )
    conductivity: float | None = FdsField(
        None, gt=0, description="Thermal conductivity [W/(m·K)]", group="thermophysical"
    )
    conductivity_solid: float | None = FdsField(
        None, gt=0, description="Solid phase conductivity [W/(m·K)]", group="thermophysical"
    )
    viscosity: float | None = FdsField(
        None, gt=0, description="Dynamic viscosity [kg/(m·s)]", group="thermophysical"
    )
    diffusivity: float | None = FdsField(
        None, gt=0, description="Mass diffusivity [m²/s]", group="thermophysical"
    )
    reference_temperature: float | None = FdsField(
        None, description="Reference temperature [°C]", group="thermophysical"
    )
    reference_enthalpy: float | None = FdsField(
        None, description="Enthalpy at reference temp [kJ/kg]", group="thermophysical"
    )
    enthalpy_of_formation: float | None = FdsField(
        None, description="Formation enthalpy [kJ/mol]", group="thermophysical"
    )

    # NASA polynomials
    polynomial: Literal["NASA7", "NASA9"] | None = FdsField(
        None, description="NASA polynomial type", group="polynomial"
    )
    polynomial_coeff: list[list[float]] | None = FdsField(
        None, description="NASA polynomial coefficients", group="polynomial"
    )
    polynomial_temp: list[float] | None = FdsField(
        None, description="Polynomial temperature ranges [K]", group="polynomial"
    )

    # Temperature-dependent property ramps
    ramp_cp: str | None = FdsField(None, description="Specific heat ramp ID", group="ramps")
    ramp_cp_l: str | None = FdsField(
        None, description="Liquid specific heat ramp ID", group="ramps"
    )
    ramp_k: str | None = FdsField(None, description="Conductivity ramp ID", group="ramps")
    ramp_d: str | None = FdsField(None, description="Diffusivity ramp ID", group="ramps")
    ramp_mu: str | None = FdsField(None, description="Viscosity ramp ID", group="ramps")
    ramp_g_f: str | None = FdsField(None, description="Gibbs free energy ramp ID", group="ramps")

    # Aerosol properties
    aerosol: bool = FdsField(
        False, exclude_if=False, description="Species is an aerosol", group="aerosol"
    )
    density_solid: float | None = FdsField(
        None, gt=0, description="Aerosol particle density [kg/m³]", group="aerosol"
    )
    mean_diameter: float | None = FdsField(
        None, gt=0, description="Aerosol mean diameter [m]", group="aerosol"
    )
    thermophoretic_diameter: float | None = FdsField(
        None, gt=0, description="Thermophoretic diameter [m]", group="aerosol"
    )

    # Radiation properties
    radcal_id: str | None = FdsField(
        None, description="RadCal surrogate species for absorption", group="radiation"
    )
    mass_extinction_coefficient: float | None = FdsField(
        None, ge=0, description="Mass extinction coefficient", group="radiation"
    )

    # Transport properties
    pr_gas: float | None = FdsField(
        None, gt=0, description="Gas phase Prandtl number", group="transport"
    )
    turbulent_schmidt_number: float | None = FdsField(
        None, gt=0, description="Turbulent Schmidt number", group="transport"
    )

    # Lennard-Jones potential parameters
    sigmalj: float | None = FdsField(
        None, ge=0, description="Lennard-Jones sigma [Å]", group="lennard_jones"
    )
    epsilonklj: float | None = FdsField(
        None, ge=0, description="Lennard-Jones epsilon/k [K]", group="lennard_jones"
    )

    # Liquid properties
    vaporization_temperature: float | None = FdsField(
        None, description="Vaporization temperature [°C]", group="liquid"
    )
    heat_of_vaporization: float | None = FdsField(
        None, gt=0, description="Heat of vaporization [kJ/kg]", group="liquid"
    )
    h_v_reference_temperature: float | None = FdsField(
        None, description="Reference temp for vaporization [°C]", group="liquid"
    )
    beta_liquid: float | None = FdsField(
        None, description="Liquid thermal expansion coefficient [1/K]", group="liquid"
    )
    density_liquid: float | None = FdsField(
        None, gt=0, description="Liquid density [kg/m³]", group="liquid"
    )
    specific_heat_liquid: float | None = FdsField(
        None, gt=0, description="Liquid specific heat [kJ/(kg·K)]", group="liquid"
    )
    conductivity_liquid: float | None = FdsField(
        None, gt=0, description="Liquid conductivity [W/(m·K)]", group="liquid"
    )
    viscosity_liquid: float | None = FdsField(
        None, gt=0, description="Liquid viscosity [kg/(m·s)]", group="liquid"
    )
    melting_temperature: float | None = FdsField(
        None, description="Melting temperature [°C]", group="liquid"
    )

    # Toxicity properties (FED/FIC)
    fic_concentration: float | None = FdsField(
        None, ge=0, description="FIC concentration [ppm]", group="toxicity"
    )
    fld_lethal_dose: float | None = FdsField(
        None, ge=0, description="FLD lethal dose [ppm*min]", group="toxicity"
    )

    # Condensation and refractive properties
    real_refractive_index: float | None = FdsField(
        None, description="Real part of refractive index", group="condensation"
    )
    complex_refractive_index: float | None = FdsField(
        None, description="Complex part of refractive index", group="condensation"
    )

    # Agglomeration properties
    min_diameter: float | None = FdsField(
        None, gt=0, description="Minimum diameter for agglomeration [m]", group="agglomeration"
    )
    max_diameter: float | None = FdsField(
        None, gt=0, description="Maximum diameter for agglomeration [m]", group="agglomeration"
    )
    n_bins: int | None = FdsField(
        None, gt=0, description="Number of bins for agglomeration", group="agglomeration"
    )

    # ODE solver parameters for detailed chemistry
    ode_abs_error: float | None = FdsField(
        None, gt=0, description="ODE absolute error tolerance", group="ode"
    )
    ode_rel_error: float | None = FdsField(
        None, gt=0, description="ODE relative error tolerance", group="ode"
    )

    @field_validator("mass_fraction_0")
    @classmethod
    def validate_mass_fraction_0(cls, v: float | None) -> float | None:
        """Validate initial mass fraction."""
        if v is not None and not (0.0 <= v <= 1.0):
            raise ValueError(f"MASS_FRACTION_0 must be between 0 and 1, got {v}")
        return v

    @model_validator(mode="after")
    def validate_species(self) -> "Species":
        """Validate species definition."""
        # Validate that FORMULA and elemental composition are mutually exclusive
        has_formula = self.formula is not None
        has_elements = any(x is not None for x in [self.c, self.h, self.o, self.n])
        if has_formula and has_elements:
            raise ValueError("Cannot specify both FORMULA and elemental composition (C, H, O, N)")

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

        # Background species validation
        if self.background and self.lumped_component_only:
            raise ValueError("Background species cannot have LUMPED_COMPONENT_ONLY=True")

        # NASA polynomial validation
        if self.polynomial_coeff is not None:
            if self.polynomial is None:
                raise ValueError("POLYNOMIAL type must be specified when using POLYNOMIAL_COEFF")
            if len(self.polynomial_coeff) != 2:
                raise ValueError("POLYNOMIAL_COEFF requires exactly 2 sets of coefficients")

            expected_len = 7 if self.polynomial == "NASA7" else 9
            for i, coeffs in enumerate(self.polynomial_coeff):
                if len(coeffs) != expected_len:
                    raise ValueError(
                        f"Polynomial set {i + 1} requires {expected_len} coefficients "
                        f"for {self.polynomial}, got {len(coeffs)}"
                    )

        if self.polynomial_temp is not None and len(self.polynomial_temp) != 3:
            raise ValueError("POLYNOMIAL_TEMP requires 3 temperature values [T_low, T_mid, T_high]")

        # Agglomeration validation
        if self.n_bins is not None:
            if self.min_diameter is None or self.max_diameter is None:
                raise ValueError(
                    "MIN_DIAMETER and MAX_DIAMETER must be specified when using N_BINS"
                )
            if self.min_diameter >= self.max_diameter:
                raise ValueError("MIN_DIAMETER must be less than MAX_DIAMETER")

        return self

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "SPEC"
