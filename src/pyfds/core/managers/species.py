"""
SpeciesManager - Manages gas species definitions and combustion parameters.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Species
    from ..namelists.comb import Combustion


class SpeciesManager(BaseManager):
    """
    Manages gas species definitions and combustion parameters.

    This manager handles species definitions, lumped species mixtures,
    background species, and global combustion parameters.
    """

    def __init__(self) -> None:
        """Initialize the species manager."""
        super().__init__()
        self._species: list[Species] = []
        self._combustion: Combustion | None = None

    @property
    def species(self) -> list[Species]:
        """Get the list of species."""
        return self._species

    @property
    def combustion(self) -> Combustion | None:
        """Get combustion parameters."""
        return self._combustion

    def add_species(self, species: Species) -> None:
        """
        Add a species to the manager.

        Parameters
        ----------
        species : Species
            Species object to add
        """
        self._species.append(species)

    def get_species(self, id: str) -> Species | None:
        """
        Get a species by ID.

        Parameters
        ----------
        id : str
            Species identifier

        Returns
        -------
        Species or None
            The species if found, None otherwise
        """
        for spec in self._species:
            if spec.id == id or (spec.fuel and spec.fuel == id):
                return spec
        return None

    def get_background_species(self) -> Species | None:
        """Get the background species if defined."""
        for spec in self._species:
            if spec.background:
                return spec
        return None

    def set_combustion(self, combustion: Combustion | None = None, **kwargs: Any) -> None:
        """
        Set COMB parameters for the simulation.

        Parameters
        ----------
        combustion : Combustion, optional
            Combustion object to set
        **kwargs
            Keyword arguments to create Combustion object
        """
        if combustion is None:
            from ..namelists.comb import Combustion

            combustion = Combustion(**kwargs)
        self._combustion = combustion

    def validate(self) -> list[str]:
        """
        Validate species configuration and cross-validate with combustion parameters.

        Returns
        -------
        list[str]
            List of validation warnings
        """
        warnings = []

        # Basic species validation
        # Check for duplicate IDs
        ids = [s.id for s in self._species if s.id]
        if len(ids) != len(set(ids)):
            warnings.append("Duplicate species IDs detected")

        # Check for multiple background species
        backgrounds = [s for s in self._species if s.background]
        if len(backgrounds) > 1:
            warnings.append("Multiple background species defined; only one allowed")

        # Validate lumped species references
        all_ids = set(ids)
        for spec in self._species:
            if spec.spec_id:
                for component_id in spec.spec_id:
                    if component_id not in all_ids:
                        warnings.append(
                            f"Lumped species '{spec.id}' references unknown "
                            f"component '{component_id}'"
                        )

        # Cross-validation with combustion parameters
        if self._combustion:
            combustion_warnings = self._validate_combustion_cross_references()
            warnings.extend(combustion_warnings)

        return warnings

    def _validate_combustion_cross_references(self) -> list[str]:
        """
        Validate combustion parameters against species definitions.

        Returns
        -------
        list[str]
            List of validation warnings
        """
        warnings: list[str] = []
        combustion = self._combustion

        if combustion is None:
            return warnings

        # Get all species IDs for reference checking
        species_ids = {s.id for s in self._species if s.id}
        species_ids.update({s.fuel for s in self._species if s.fuel})

        # Validate ramp references in combustion parameters
        if combustion.ramp_zeta_0 and not isinstance(combustion.ramp_zeta_0, str):
            warnings.append("RAMP_ZETA_0 must be a valid ramp ID string")

        # Check for species-dependent combustion parameters
        # Note: Most combustion parameters are global and don't reference specific species

        # Validate extinction model requirements
        if combustion.extinction_model:
            # Extinction models may require specific species to be defined
            # This is model-dependent and would need FDS documentation reference
            pass

        # Validate finite rate temperature against species properties
        if combustion.finite_rate_min_temp is not None:
            # Check if temperature is reasonable (above absolute zero, below typical combustion temps)
            if combustion.finite_rate_min_temp <= -273.15:
                warnings.append("FINITE_RATE_MIN_TEMP must be above absolute zero")
            elif combustion.finite_rate_min_temp > 2000:
                warnings.append("FINITE_RATE_MIN_TEMP seems unusually high (>2000°C)")

        # Validate mixing time bounds
        if (
            combustion.tau_chem is not None
            and combustion.tau_flame is not None
            and combustion.tau_chem >= combustion.tau_flame
        ):
            warnings.append("TAU_CHEM should typically be less than TAU_FLAME")

        return warnings
