"""Simulation-level validation.

This module validates complete simulation configurations before
writing or running.
"""

import re
from pathlib import Path
from typing import TYPE_CHECKING, ClassVar

from pyfds.core.enums import Severity
from pyfds.exceptions import ValidationError
from pyfds.validation.base import Issue, ValidationResult
from pyfds.validation.utils import flatten_to_list

if TYPE_CHECKING:
    from pyfds.core.registry import SimulationRegistry
    from pyfds.core.simulation import Simulation


class SimulationValidator:
    """Validates complete simulation configurations.

    Validation is performed in order:
    1. Required components (HEAD, TIME, MESH)
    2. Cross-reference validation (SURF_ID, MATL_ID, etc.)
    3. Geometry quality checks
    4. Physical reasonableness checks
    """

    # Built-in surfaces that don't need to be defined
    BUILTIN_SURFACES: ClassVar[set[str]] = {"INERT", "OPEN", "MIRROR", "PERIODIC"}

    # Built-in species that don't need to be defined
    BUILTIN_SPECIES: ClassVar[set[str]] = {"AIR", "PRODUCTS", "SOOT", "WATER VAPOR"}

    def __init__(self, simulation: "Simulation | SimulationRegistry") -> None:
        if hasattr(simulation, "_registry"):
            self._registry = simulation._registry
        else:
            self._registry = simulation

    def validate(self) -> ValidationResult:
        """Run all validations.

        Returns
        -------
        ValidationResult
            Result containing all validation issues
        """
        issues: list[Issue] = []

        issues.extend(self._check_required())
        issues.extend(self._check_cross_references())
        issues.extend(self._check_geometry())
        issues.extend(self._check_materials())
        issues.extend(self._check_species())
        issues.extend(self._check_physics())
        issues.extend(self._check_physical_bounds())

        return ValidationResult(issues=issues)

    def _check_required(self) -> list[Issue]:
        """Check required components exist."""
        issues = []

        if self._registry.head is None:
            issues.append(Issue(Severity.ERROR, "HEAD namelist required", "HEAD"))

        if self._registry.time is None:
            issues.append(Issue(Severity.ERROR, "TIME namelist required", "TIME"))

        if len(self._registry.meshes.list_items()) == 0:
            issues.append(Issue(Severity.ERROR, "At least one MESH required", "MESH"))

        return issues

    def _check_cross_references(self) -> list[Issue]:
        """Validate cross-references between components."""
        issues = []

        surface_ids = set(self._registry.surfaces.list_ids())
        valid_surfaces = surface_ids | self.BUILTIN_SURFACES

        # Check obstruction SURF_ID references
        for obst in self._registry.obstructions.list_items():
            for surf_id in [
                obst.surf_id,
                obst.surf_id_top,
                obst.surf_id_bottom,
                obst.surf_id_sides,
            ]:
                if surf_id and surf_id not in valid_surfaces:
                    issues.append(
                        Issue(
                            Severity.ERROR,
                            f"References undefined surface '{surf_id}'",
                            "OBST",
                            "surf_id",
                        )
                    )

        # Check species references in materials
        species_ids = set(self._registry.species.list_ids())
        for reaction in self._registry.reactions.list_items():
            if reaction.fuel:
                species_ids.add(reaction.fuel)
        species_ids.update(self.BUILTIN_SPECIES)

        for matl in self._registry.materials.list_items():
            if matl.spec_id:
                for spec_id in flatten_to_list(matl.spec_id):
                    if spec_id and spec_id not in species_ids:
                        issues.append(
                            Issue(
                                Severity.WARNING,
                                f"References undefined species '{spec_id}'",
                                "MATL",
                                "spec_id",
                            )
                        )

        return issues

    def _check_materials(self) -> list[Issue]:
        """Validate materials and surface references."""
        issues = []

        material_ids = set(self._registry.materials.list_ids())
        for surf in self._registry.surfaces.list_items():
            if surf.matl_id:
                for matl_id in flatten_to_list(surf.matl_id):
                    if matl_id and matl_id not in material_ids:
                        issues.append(
                            Issue(
                                Severity.ERROR,
                                f"Surface '{surf.id}' references undefined material '{matl_id}'",
                                "SURF",
                                "matl_id",
                            )
                        )

        return issues

    def _check_geometry(self) -> list[Issue]:
        """Check geometry configuration: meshes, obstructions, etc."""
        issues = []

        meshes = self._registry.meshes.list_items()
        if not meshes:
            issues.append(Issue(Severity.ERROR, "No meshes defined", "MESH"))
            return issues

        # Check mesh quality (aspect ratios)
        for mesh in meshes:
            try:
                dx, dy, dz = mesh.get_cell_size()
                max_ratio = max(
                    dx / dy if dy else 1,
                    dy / dx if dx else 1,
                    dx / dz if dz else 1,
                    dz / dx if dx else 1,
                    dy / dz if dz else 1,
                    dz / dy if dy else 1,
                )
                if max_ratio > 2.0:
                    mesh_id = mesh.id or "unnamed"
                    issues.append(
                        Issue(
                            Severity.WARNING,
                            f"Non-cubic cells (aspect ratio {max_ratio:.2f}) in mesh '{mesh_id}'",
                            "MESH",
                        )
                    )
            except Exception:
                continue

        return issues

    def _check_species(self) -> list[Issue]:
        """Check species configuration."""
        issues = []

        # Check for multiple background species
        background_count = sum(1 for s in self._registry.species.list_items() if s.background)
        if background_count > 1:
            issues.append(
                Issue(
                    Severity.ERROR,
                    f"Multiple background species defined ({background_count}). Only one allowed.",
                    "SPEC",
                    "background",
                )
            )

        # Check lumped species references
        defined_ids = set(self._registry.species.list_ids())
        for species in self._registry.species.list_items():
            if species.spec_id:
                spec_ids = flatten_to_list(species.spec_id)
                for ref_id in spec_ids:
                    if ref_id and ref_id not in defined_ids:
                        issues.append(
                            Issue(
                                Severity.ERROR,
                                f"References unknown component species '{ref_id}'",
                                "SPEC",
                                "spec_id",
                            )
                        )

        return issues

    def _check_physics(self) -> list[Issue]:
        """Validate physics configuration."""
        issues: list[Issue] = []
        # Physics validation logic can be added here as needed
        return issues

    def _check_physical_bounds(self) -> list[Issue]:
        """Check physical reasonableness."""
        issues = []

        # Check time parameters
        if self._registry.time:
            time = self._registry.time
            if time.t_end <= 0:
                issues.append(Issue(Severity.ERROR, "T_END must be positive", "TIME", "t_end"))
            if time.t_begin is not None and time.t_begin >= time.t_end:
                issues.append(
                    Issue(Severity.ERROR, "T_BEGIN must be less than T_END", "TIME", "t_begin")
                )

        # Check mesh sizes
        for mesh in self._registry.meshes.list_items():
            total_cells = mesh.ijk.total_cells
            if total_cells > 10_000_000:
                issues.append(
                    Issue(
                        Severity.WARNING,
                        f"Mesh has {total_cells:,} cells which may be computationally expensive",
                        "MESH",
                        "ijk",
                    )
                )

        return issues


# Backward compatibility alias
Validator = SimulationValidator


def validate_fds_file(filepath: Path) -> bool:
    """Validate an existing FDS input file.

    Parameters
    ----------
    filepath : Path
        Path to FDS input file

    Returns
    -------
    bool
        True if file is valid

    Raises
    ------
    ValidationError
        If validation fails
    """
    if not filepath.exists():
        raise ValidationError(f"File not found: {filepath}")

    if filepath.suffix != ".fds":
        raise ValidationError(f"File must have .fds extension: {filepath}")

    content = filepath.read_text()

    # Basic syntax checks
    if not re.search(r"&HEAD", content, re.IGNORECASE):
        raise ValidationError("Missing &HEAD namelist")

    if not re.search(r"&TAIL", content, re.IGNORECASE):
        raise ValidationError("Missing &TAIL namelist")

    # Check for balanced namelists
    namelist_starts = len(re.findall(r"&\w+", content))
    namelist_ends = len(re.findall(r"/", content))

    if namelist_starts != namelist_ends:
        raise ValidationError(
            f"Unbalanced namelists: {namelist_starts} starts, {namelist_ends} ends"
        )

    return True


__all__ = ["SimulationValidator", "Validator", "validate_fds_file"]
