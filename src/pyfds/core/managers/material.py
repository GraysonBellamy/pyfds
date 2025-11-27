"""
MaterialManager - Manages materials, surfaces, and ramps.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Material, Surface


class MaterialManager(BaseManager):
    """
    Manages material-related simulation components.

    This manager handles materials, surface definitions, and ramps
    that define material properties and boundary conditions.
    """

    def __init__(self) -> None:
        """Initialize the material manager."""
        super().__init__()
        self._materials: list[Material] = []
        self._surfaces: list[Surface] = []

    @property
    def materials(self) -> list[Material]:
        """Get the list of materials."""
        return self._materials

    @property
    def surfaces(self) -> list[Surface]:
        """Get the list of surfaces."""
        return self._surfaces

    def add_material(self, material: Material) -> None:
        """
        Add a Material object to the simulation.

        Parameters
        ----------
        material : Material
            Material object to add
        """
        self._materials.append(material)

    def add_surface(self, surface: Surface) -> None:
        """
        Add a Surface object to the simulation.

        Parameters
        ----------
        surface : Surface
            Surface object to add
        """
        self._surfaces.append(surface)

    def validate(self) -> list[str]:
        """
        Validate material configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        # Material-specific validation can be added here
        return []

    def validate_surface_references(self, referenced_ids: set[str]) -> list[str]:
        """
        Validate that referenced surface IDs exist.

        Parameters
        ----------
        referenced_ids : set[str]
            Set of surface IDs referenced by obstructions/vents

        Returns
        -------
        List[str]
            List of validation warnings
        """
        warnings = []
        surface_ids = {s.id for s in self._surfaces}
        predefined_surfaces = {"INERT", "OPEN", "MIRROR"}

        for ref_id in referenced_ids:
            if ref_id not in surface_ids and ref_id not in predefined_surfaces:
                warnings.append(f"Referenced undefined surface '{ref_id}'")

        return warnings
