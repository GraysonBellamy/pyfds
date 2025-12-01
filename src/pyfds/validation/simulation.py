"""Simulation-level validation.

This module validates complete simulation configurations before
writing or running.
"""

import re
from pathlib import Path
from typing import TYPE_CHECKING

from pyfds.core.enums import Severity
from pyfds.exceptions import ValidationError
from pyfds.validation.base import Issue, ValidationResult
from pyfds.validation.cross_references import CrossReferenceValidator
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
        """Validate cross-references between components.

        Delegates to CrossReferenceValidator for comprehensive
        reference checking.
        """
        validator = CrossReferenceValidator(self._registry)
        result = validator.validate()
        return result.issues

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
        """Check physics configuration for consistency."""
        issues: list[Issue] = []

        # Check combustion configuration
        issues.extend(self._check_combustion())

        # Check species mass fractions
        issues.extend(self._check_species_fractions())

        return issues

    def _check_combustion(self) -> list[Issue]:
        """Validate combustion/reaction configuration."""
        issues: list[Issue] = []
        reactions = self._registry.reactions.list_items()

        if not reactions:
            # No reactions defined - check if fire surfaces exist
            surfaces = self._registry.surfaces.list_items()
            fire_surfaces = [
                s for s in surfaces if getattr(s, "hrrpua", None) or getattr(s, "mlrpua", None)
            ]

            if fire_surfaces:
                issues.append(
                    Issue(
                        Severity.WARNING,
                        "Fire surfaces defined but no REAC namelist. "
                        "Simple chemistry will be used.",
                        "REAC",
                    )
                )

        for reac in reactions:
            # Check stoichiometry - fuel must have C or H atoms
            c_val = getattr(reac, "c", None)
            h_val = getattr(reac, "h", None)
            if c_val is not None and h_val is not None and c_val == 0 and h_val == 0:
                reac_id = getattr(reac, "id", None) or getattr(reac, "fuel", "unknown")
                issues.append(
                    Issue(
                        Severity.ERROR,
                        f"Reaction '{reac_id}' has no fuel composition (C=0, H=0)",
                        "REAC",
                        reac_id,
                    )
                )

        return issues

    def _check_species_fractions(self) -> list[Issue]:
        """Validate species mass fractions sum to 1.0 where applicable."""
        issues: list[Issue] = []

        # Check INIT namelists with species specifications
        inits = self._registry.inits.list_items()

        for init in inits:
            mass_fraction = getattr(init, "mass_fraction", None)
            if mass_fraction:
                total = sum(mass_fraction)
                if abs(total - 1.0) > 0.01:
                    init_id = getattr(init, "id", None)
                    issues.append(
                        Issue(
                            Severity.WARNING,
                            f"INIT mass fractions sum to {total:.3f}, expected 1.0",
                            "INIT",
                            init_id,
                        )
                    )

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


# Convenient alias for SimulationValidator
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
