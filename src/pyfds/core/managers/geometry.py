"""
GeometryManager - Manages spatial components (meshes, obstructions, vents).
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Mesh, Obstruction, Vent


class GeometryManager(BaseManager):
    """
    Manages geometry-related simulation components.

    This manager handles meshes, obstructions, and vents, which define
    the spatial structure of the simulation domain.
    """

    def __init__(self) -> None:
        """Initialize the geometry manager."""
        super().__init__()
        self._meshes: list[Mesh] = []
        self._obstructions: list[Obstruction] = []
        self._vents: list[Vent] = []

    @property
    def meshes(self) -> list[Mesh]:
        """Get the list of meshes."""
        return self._meshes

    @property
    def obstructions(self) -> list[Obstruction]:
        """Get the list of obstructions."""
        return self._obstructions

    @property
    def vents(self) -> list[Vent]:
        """Get the list of vents."""
        return self._vents

    def add_mesh(self, mesh: Mesh) -> None:
        """
        Add a Mesh object to the simulation.

        Parameters
        ----------
        mesh : Mesh
            Mesh object to add
        """
        self._meshes.append(mesh)

    def add_obstruction(self, obstruction: Obstruction) -> None:
        """
        Add an Obstruction object to the simulation.

        Parameters
        ----------
        obstruction : Obstruction
            Obstruction object to add
        """
        self._obstructions.append(obstruction)

    def add_vent(self, vent: Vent) -> None:
        """
        Add a Vent object to the simulation.

        Parameters
        ----------
        vent : Vent
            Vent object to add
        """
        self._vents.append(vent)

    def validate(self) -> list[str]:
        """
        Validate geometry configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        warnings = []

        # Check for required meshes
        if not self._meshes:
            warnings.append("No meshes defined - at least one mesh is required")

        # Check mesh cell aspect ratios
        for i, mesh in enumerate(self._meshes):
            dx, dy, dz = mesh.get_cell_size()
            max_ratio = max(dx / dy, dy / dx, dx / dz, dz / dx, dy / dz, dz / dy)
            if max_ratio > 2.0:
                warnings.append(
                    f"Mesh {i} has non-cubic cells (aspect ratio {max_ratio:.2f}). "
                    "Consider using more cubic cells for better accuracy."
                )

        return warnings
