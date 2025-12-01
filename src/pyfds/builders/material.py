"""Builder for creating MATL namelists with thermal and pyrolysis properties."""

from typing import TYPE_CHECKING

from pyfds.builders.base import Builder
from pyfds.core.namelists import Material

if TYPE_CHECKING:
    from pyfds.core.models import PyrolysisReaction


class MaterialBuilder(Builder[Material]):
    """
    Builder for creating MATL namelists.

    Provides a fluent API for constructing materials with thermal properties
    and optional multi-reaction pyrolysis.

    Parameters
    ----------
    id : str
        Unique identifier for the material

    Examples
    --------
    >>> # Simple material with constant properties
    >>> wood = MaterialBuilder('PINE') \\
    ...     .density(500) \\
    ...     .thermal_conductivity(0.13) \\
    ...     .specific_heat(2.5) \\
    ...     .build()

    >>> # Temperature-dependent properties using RAMPs
    >>> steel = MaterialBuilder('STEEL') \\
    ...     .density(7850) \\
    ...     .thermal_conductivity_ramp('STEEL_K') \\
    ...     .specific_heat(0.46) \\
    ...     .emissivity(0.7) \\
    ...     .build()

    >>> # Multi-reaction pyrolysis material
    >>> polymer = MaterialBuilder('POLYURETHANE') \\
    ...     .density(40) \\
    ...     .thermal_conductivity(0.04) \\
    ...     .specific_heat(1.5) \\
    ...     .add_reaction(PyrolysisReaction(
    ...         a=1e10, e=80000, heat_of_reaction=1000,
    ...         products=[PyrolysisProduct(spec_id='FUEL_VAPOR', nu_spec=1.0)]
    ...     )) \\
    ...     .build()

    >>> # Use predefined materials
    >>> concrete = MaterialBuilder.concrete()
    >>> gypsum = MaterialBuilder.gypsum()
    >>> steel = MaterialBuilder.steel()
    """

    def __init__(self, id: str | None = None):
        """
        Initialize the MaterialBuilder.

        Parameters
        ----------
        id : str, optional
            Unique identifier for the material. Can also be set via .id() method.
        """
        super().__init__()
        self._id = id
        self._density: float | None = None
        self._conductivity: float | None = None
        self._conductivity_ramp: str | None = None
        self._specific_heat: float | None = None
        self._specific_heat_ramp: str | None = None
        self._emissivity: float = 0.9
        self._absorption_coefficient: float = 50000.0
        self._structured_reactions: list[PyrolysisReaction] = []
        # Liquid fuel parameters
        self._boiling_temperature: float | None = None
        self._mw: float | None = None

    def id(self, material_id: str) -> "MaterialBuilder":
        """
        Set material identifier.

        Parameters
        ----------
        material_id : str
            Unique identifier for the material

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        self._id = material_id
        return self

    def density(self, value: float) -> "MaterialBuilder":
        """
        Set material density.

        Parameters
        ----------
        value : float
            Density in kg/m³

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        self._density = value
        return self

    def thermal_conductivity(self, value: float) -> "MaterialBuilder":
        """
        Set constant thermal conductivity.

        Parameters
        ----------
        value : float
            Thermal conductivity in W/(m·K)

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Notes
        -----
        Use either thermal_conductivity or thermal_conductivity_ramp, not both.
        """
        self._conductivity = value
        self._conductivity_ramp = None
        return self

    def thermal_conductivity_ramp(self, ramp_id: str) -> "MaterialBuilder":
        """
        Use temperature-dependent conductivity via RAMP.

        Parameters
        ----------
        ramp_id : str
            ID of the RAMP defining conductivity vs temperature

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Notes
        -----
        Use either thermal_conductivity or thermal_conductivity_ramp, not both.
        """
        self._conductivity_ramp = ramp_id
        self._conductivity = None
        return self

    def specific_heat(self, value: float) -> "MaterialBuilder":
        """
        Set constant specific heat capacity.

        Parameters
        ----------
        value : float
            Specific heat in kJ/(kg·K)

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Notes
        -----
        Use either specific_heat or specific_heat_ramp, not both.
        """
        self._specific_heat = value
        self._specific_heat_ramp = None
        return self

    def specific_heat_ramp(self, ramp_id: str) -> "MaterialBuilder":
        """
        Use temperature-dependent specific heat via RAMP.

        Parameters
        ----------
        ramp_id : str
            ID of the RAMP defining specific heat vs temperature

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Notes
        -----
        Use either specific_heat or specific_heat_ramp, not both.
        """
        self._specific_heat_ramp = ramp_id
        self._specific_heat = None
        return self

    def emissivity(self, value: float) -> "MaterialBuilder":
        """
        Set surface emissivity.

        Parameters
        ----------
        value : float
            Emissivity in range [0, 1], default: 0.9

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        self._emissivity = value
        return self

    def absorption_coefficient(self, value: float) -> "MaterialBuilder":
        """
        Set radiation absorption coefficient.

        Parameters
        ----------
        value : float
            Absorption coefficient in 1/m, default: 50000.0

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        self._absorption_coefficient = value
        return self

    def add_reaction(self, reaction: "PyrolysisReaction") -> "MaterialBuilder":
        """
        Add a structured pyrolysis reaction to the material.

        Parameters
        ----------
        reaction : PyrolysisReaction
            The structured pyrolysis reaction to add

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Examples
        --------
        >>> from pyfds.core.models import PyrolysisReaction, PyrolysisProduct
        >>> reaction = PyrolysisReaction(
        ...     a=1e10, e=1.5e5, heat_of_reaction=500,
        ...     products=[PyrolysisProduct(spec_id="GAS", nu_spec=0.8)]
        ... )
        >>> mat = MaterialBuilder('WOOD').add_reaction(reaction).build()
        """
        self._structured_reactions.append(reaction)
        return self

    def add_pyrolysis_reaction(
        self,
        a: float,
        e: float,
        heat_of_reaction: float,
        product_species: str | None = None,
        residue_material: str | None = None,
        yield_fraction: float = 1.0,
    ) -> "MaterialBuilder":
        """
        Add a pyrolysis reaction using simplified parameters.

        Creates a PyrolysisReaction from basic kinetic parameters.
        For more complex reactions, prefer using add_reaction() with
        PyrolysisReaction objects directly.

        Parameters
        ----------
        a : float
            Pre-exponential factor [1/s]
        e : float
            Activation energy [J/mol]
        heat_of_reaction : float
            Heat of reaction [kJ/kg]
        product_species : str, optional
            Gas species produced
        residue_material : str, optional
            Solid residue material produced
        yield_fraction : float, optional
            Yield fraction (default: 1.0)

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        from pyfds.core.models import PyrolysisProduct, PyrolysisReaction

        products = []
        if product_species:
            products.append(PyrolysisProduct(spec_id=product_species, nu_spec=yield_fraction))
        if residue_material:
            products.append(PyrolysisProduct(matl_id=residue_material, nu_matl=yield_fraction))

        reaction = PyrolysisReaction(
            a=a,
            e=e,
            heat_of_reaction=heat_of_reaction,
            products=products,
        )
        self._structured_reactions.append(reaction)
        return self

    def as_liquid_fuel(
        self,
        boiling_temperature: float,
        spec_id: str,
        mw: float | None = None,
        heat_of_vaporization: float | None = None,
        absorption_coefficient: float | None = None,
        nu_spec: float = 1.0,
    ) -> "MaterialBuilder":
        """
        Configure as liquid fuel with evaporation properties.

        Liquid fuels in FDS evaporate at their boiling temperature
        and produce gaseous fuel species for combustion.

        Parameters
        ----------
        boiling_temperature : float
            Boiling point in °C (triggers liquid model in FDS)
        spec_id : str
            Gaseous species ID produced by evaporation
        mw : float, optional
            Molecular weight in g/mol
        heat_of_vaporization : float, optional
            Heat of vaporization in kJ/kg
        absorption_coefficient : float, optional
            Radiation absorption coefficient in 1/m
        nu_spec : float, optional
            Yield fraction (default: 1.0 for pure liquid)

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Examples
        --------
        >>> ethanol = MaterialBuilder("ETHANOL_LIQUID") \\
        ...     .density(794) \\
        ...     .thermal_conductivity(0.17) \\
        ...     .specific_heat(2.44) \\
        ...     .as_liquid_fuel(
        ...         boiling_temperature=78.5,
        ...         spec_id="ETHANOL",
        ...         mw=46.07,
        ...         heat_of_vaporization=837,
        ...         absorption_coefficient=1140
        ...     ) \\
        ...     .build()

        >>> methanol = MaterialBuilder("METHANOL_LIQUID") \\
        ...     .density(792) \\
        ...     .thermal_conductivity(0.2) \\
        ...     .specific_heat(2.51) \\
        ...     .as_liquid_fuel(
        ...         boiling_temperature=64.7,
        ...         spec_id="METHANOL",
        ...         mw=32.04
        ...     ) \\
        ...     .build()
        """
        from pyfds.core.models import PyrolysisProduct, PyrolysisReaction

        self._boiling_temperature = boiling_temperature
        if mw is not None:
            self._mw = mw
        if absorption_coefficient is not None:
            self._absorption_coefficient = absorption_coefficient

        # Create a structured reaction for the liquid evaporation
        # In FDS, heat_of_vaporization is specified as heat_of_reaction on the reaction
        evap_reaction = PyrolysisReaction(
            heat_of_reaction=heat_of_vaporization if heat_of_vaporization is not None else 0.0,
            products=[PyrolysisProduct(spec_id=spec_id, nu_spec=nu_spec)],
        )
        self._structured_reactions.append(evap_reaction)

        return self

    def _validate(self) -> list[str]:
        """Validate builder state before building."""
        errors = []
        if self._density is None:
            errors.append("density is required")
        return errors

    def _create(self) -> Material:
        """Create the Material object."""
        # Build parameter dict
        params: dict = {
            "id": self._id,
            "density": self._density,
            "emissivity": self._emissivity,
            "absorption_coefficient": self._absorption_coefficient,
        }

        # Thermal properties
        if self._conductivity is not None:
            params["conductivity"] = self._conductivity
        if self._conductivity_ramp is not None:
            params["conductivity_ramp"] = self._conductivity_ramp
        if self._specific_heat is not None:
            params["specific_heat"] = self._specific_heat
        if self._specific_heat_ramp is not None:
            params["specific_heat_ramp"] = self._specific_heat_ramp

        # Liquid fuel parameters
        if self._boiling_temperature is not None:
            params["boiling_temperature"] = self._boiling_temperature
        if self._mw is not None:
            params["mw"] = self._mw

        # Pyrolysis reactions
        if self._structured_reactions:
            params["reactions"] = self._structured_reactions

        return Material(**params)
