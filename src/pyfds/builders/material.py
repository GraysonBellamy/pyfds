"""Builder for creating MATL namelists with thermal and pyrolysis properties."""

from typing import TYPE_CHECKING

from ..core.namelists import Material
from .base import Builder

if TYPE_CHECKING:
    from ..core.namelists.pyrolysis import PyrolysisReaction


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
    ...     .add_pyrolysis_reaction(
    ...         a=1e10, e=80000, heat_of_reaction=1000,
    ...         product_species='FUEL_VAPOR'
    ...     ) \\
    ...     .add_pyrolysis_reaction(
    ...         a=5e8, e=120000, heat_of_reaction=1500,
    ...         residue_material='CHAR'
    ...     ) \\
    ...     .build()

    >>> # Use predefined materials
    >>> concrete = MaterialBuilder.concrete()
    >>> gypsum = MaterialBuilder.gypsum()
    >>> steel = MaterialBuilder.steel()
    """

    def __init__(self, id: str):
        """
        Initialize the MaterialBuilder.

        Parameters
        ----------
        id : str
            Unique identifier for the material
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
        self._reference_temperature: float | None = None
        self._reactions: list[dict] = []
        self._structured_reactions: list[PyrolysisReaction] = []
        # Stage 2.4 enhancements
        self._spec_id: str | None = None
        self._yield_fraction: float | None = None
        self._heat_of_combustion: float | None = None
        self._reference_rate: float | None = None
        # Liquid fuel parameters
        self._boiling_temperature: float | None = None
        self._mw: float | None = None
        self._heat_of_vaporization: float | None = None

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

    def reference_temperature(self, temp: float) -> "MaterialBuilder":
        """
        Set reference temperature for properties.

        Parameters
        ----------
        temp : float
            Reference temperature in °C

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        self._reference_temperature = temp
        return self

    def with_pyrolysis_product(
        self, spec_id: str, yield_fraction: float | None = None
    ) -> "MaterialBuilder":
        """
        Set pyrolysis product for single-reaction materials (Stage 2.4).

        Parameters
        ----------
        spec_id : str
            Gas species ID produced by pyrolysis
        yield_fraction : float, optional
            Fraction of material yielded (0-1)

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Examples
        --------
        >>> mat = MaterialBuilder('PMMA') \\
        ...     .density(1200) \\
        ...     .thermal_conductivity(0.19) \\
        ...     .specific_heat(1.4) \\
        ...     .with_pyrolysis_product('MMA_VAPOR', yield_fraction=1.0) \\
        ...     .build()
        """
        self._spec_id = spec_id
        if yield_fraction is not None:
            self._yield_fraction = yield_fraction
        return self

    def with_heat_of_combustion(self, value: float) -> "MaterialBuilder":
        """
        Set heat of combustion (Stage 2.4).

        Parameters
        ----------
        value : float
            Heat of combustion in kJ/kg

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Examples
        --------
        >>> mat = MaterialBuilder('FUEL') \\
        ...     .density(800) \\
        ...     .thermal_conductivity(0.1) \\
        ...     .specific_heat(2.0) \\
        ...     .with_heat_of_combustion(25000) \\
        ...     .build()
        """
        self._heat_of_combustion = value
        return self

    def with_reference_rate(self, rate: float) -> "MaterialBuilder":
        """
        Set reference reaction rate (Stage 2.4).

        Parameters
        ----------
        rate : float
            Reference reaction rate in 1/s

        Returns
        -------
        MaterialBuilder
            Self for method chaining
        """
        self._reference_rate = rate
        return self

    def add_pyrolysis_reaction(
        self,
        a: float,
        e: float,
        heat_of_reaction: float,
        product_species: str | None = None,
        residue_material: str | None = None,
        reaction_order: float = 1.0,
    ) -> "MaterialBuilder":
        """
        Add a pyrolysis reaction to the material.

        Parameters
        ----------
        a : float
            Pre-exponential factor in 1/s
        e : float
            Activation energy in kJ/kmol
        heat_of_reaction : float
            Heat of pyrolysis/vaporization in kJ/kg
        product_species : str, optional
            Gaseous product species ID
        residue_material : str, optional
            Solid residue material ID
        reaction_order : float, optional
            Reaction order, default: 1.0

        Returns
        -------
        MaterialBuilder
            Self for method chaining

        Examples
        --------
        >>> mat = MaterialBuilder('WOOD') \\
        ...     .density(500) \\
        ...     .thermal_conductivity(0.13) \\
        ...     .specific_heat(2.5) \\
        ...     .add_pyrolysis_reaction(
        ...         a=1e10, e=100000, heat_of_reaction=1800,
        ...         product_species='WOOD_VAPOR'
        ...     ) \\
        ...     .build()
        """
        self._reactions.append(
            {
                "a": a,
                "e": e,
                "heat_of_reaction": heat_of_reaction,
                "product_species": product_species,
                "residue_material": residue_material,
                "reaction_order": reaction_order,
            }
        )
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
        >>> from pyfds.core.namelists.pyrolysis import PyrolysisReaction, PyrolysisProduct
        >>> reaction = PyrolysisReaction(
        ...     a=1e10, e=1.5e5, heat_of_reaction=500,
        ...     products=[PyrolysisProduct(species="GAS", mass_fraction=0.8)]
        ... )
        >>> mat = MaterialBuilder('WOOD').add_reaction(reaction).build()
        """
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
        self._boiling_temperature = boiling_temperature
        self._spec_id = spec_id
        self._yield_fraction = nu_spec

        if mw is not None:
            self._mw = mw
        if heat_of_vaporization is not None:
            self._heat_of_vaporization = heat_of_vaporization
        if absorption_coefficient is not None:
            self._absorption_coefficient = absorption_coefficient

        return self

    def build(self) -> Material:
        """
        Build the Material object.

        Returns
        -------
        Material
            The constructed Material namelist object

        Raises
        ------
        ValueError
            If required parameters are missing
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        # Validate required parameters
        if self._density is None:
            raise ValueError(f"MaterialBuilder '{self._id}': density is required")

        # Validate reaction API consistency
        if self._structured_reactions and self._reactions:
            raise ValueError(
                f"MaterialBuilder '{self._id}': Cannot mix structured reactions "
                "(add_reaction()) with legacy reactions (add_pyrolysis_reaction())"
            )

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

        # Reference temperature
        if self._reference_temperature is not None:
            params["reference_temperature"] = self._reference_temperature

        # Stage 2.4 enhancements
        if self._spec_id is not None:
            params["spec_id"] = self._spec_id
        if self._yield_fraction is not None:
            params["nu_spec"] = self._yield_fraction
        if self._heat_of_combustion is not None:
            params["heat_of_combustion"] = self._heat_of_combustion
        if self._reference_rate is not None:
            params["reference_rate"] = self._reference_rate

        # Liquid fuel parameters
        if self._boiling_temperature is not None:
            params["boiling_temperature"] = self._boiling_temperature
        if self._mw is not None:
            params["mw"] = self._mw
        if self._heat_of_vaporization is not None:
            params["heat_of_vaporization"] = self._heat_of_vaporization

        # Pyrolysis reactions
        if self._structured_reactions:
            # Use structured PyrolysisReaction objects
            params["reactions"] = self._structured_reactions
        elif self._reactions:
            # Use legacy array-based parameters
            params["n_reactions"] = len(self._reactions)
            params["a"] = [r["a"] for r in self._reactions]
            params["e"] = [r["e"] for r in self._reactions]
            params["heat_of_reaction"] = [r["heat_of_reaction"] for r in self._reactions]
            params["n_s"] = [r["reaction_order"] for r in self._reactions]

            # Species and materials (filter out None values)
            spec_ids = [r.get("product_species") for r in self._reactions]
            matl_ids = [r.get("residue_material") for r in self._reactions]

            # Only include if there are non-None values
            if any(s is not None for s in spec_ids):
                # Set species IDs
                params["spec_id"] = [s if s is not None else "" for s in spec_ids]
                # Set default yields of 1.0 for each species
                params["nu_spec"] = [1.0 if s is not None else 0.0 for s in spec_ids]
            if any(m is not None for m in matl_ids):
                # Set residue material IDs
                params["matl_id"] = [m if m is not None else "" for m in matl_ids]
                # Set default yields of 1.0 for each residue material
                params["nu_matl"] = [1.0 if m is not None else 0.0 for m in matl_ids]

        material = Material(**params)
        self._mark_built()
        return material

    # Predefined common materials
    @classmethod
    def concrete(cls) -> Material:
        """
        Create standard concrete material.

        Returns
        -------
        Material
            Concrete with typical thermal properties

        Examples
        --------
        >>> concrete = MaterialBuilder.concrete()
        """
        return (
            cls("CONCRETE")
            .density(2400)
            .thermal_conductivity(1.6)
            .specific_heat(0.88)
            .emissivity(0.9)
            .build()
        )

    @classmethod
    def gypsum(cls) -> Material:
        """
        Create standard gypsum board (drywall) material.

        Returns
        -------
        Material
            Gypsum board with typical thermal properties

        Examples
        --------
        >>> gypsum = MaterialBuilder.gypsum()
        """
        return (
            cls("GYPSUM")
            .density(930)
            .thermal_conductivity(0.48)
            .specific_heat(0.84)
            .emissivity(0.9)
            .build()
        )

    @classmethod
    def steel(cls) -> Material:
        """
        Create standard structural steel material.

        Returns
        -------
        Material
            Steel with typical thermal properties

        Examples
        --------
        >>> steel = MaterialBuilder.steel()
        """
        return (
            cls("STEEL")
            .density(7850)
            .thermal_conductivity(45.8)
            .specific_heat(0.46)
            .emissivity(0.7)
            .build()
        )

    @classmethod
    def aluminum(cls) -> Material:
        """
        Create standard aluminum material.

        Returns
        -------
        Material
            Aluminum with typical thermal properties

        Examples
        --------
        >>> aluminum = MaterialBuilder.aluminum()
        """
        return (
            cls("ALUMINUM")
            .density(2700)
            .thermal_conductivity(237)
            .specific_heat(0.90)
            .emissivity(0.2)
            .build()
        )

    @classmethod
    def brick(cls) -> Material:
        """
        Create standard brick material.

        Returns
        -------
        Material
            Brick with typical thermal properties

        Examples
        --------
        >>> brick = MaterialBuilder.brick()
        """
        return (
            cls("BRICK")
            .density(1920)
            .thermal_conductivity(0.69)
            .specific_heat(0.84)
            .emissivity(0.9)
            .build()
        )

    @classmethod
    def wood(cls) -> Material:
        """
        Create standard wood material (pine).

        Returns
        -------
        Material
            Wood with typical thermal properties

        Examples
        --------
        >>> wood = MaterialBuilder.wood()
        """
        return (
            cls("WOOD")
            .density(500)
            .thermal_conductivity(0.13)
            .specific_heat(2.5)
            .emissivity(0.9)
            .build()
        )
