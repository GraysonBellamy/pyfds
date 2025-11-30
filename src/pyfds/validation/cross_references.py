"""Cross-reference validation between FDS namelists.

This module provides detailed validation of references between
different namelist types (SURF→MATL, MATL→SPEC, SURF→RAMP, etc.).
"""

from typing import TYPE_CHECKING, ClassVar

from pyfds.core.enums import Severity
from pyfds.validation.base import Issue
from pyfds.validation.utils import flatten_to_list, get_surface_ids_from_obstruction

if TYPE_CHECKING:
    from pyfds.core.registry import SimulationRegistry
    from pyfds.core.simulation import Simulation


class CrossReferenceValidator:
    """Validates cross-references between FDS namelists.

    Provides more detailed reference checking than the main
    SimulationValidator, including:
    - RAMP references from SURF and MATL
    - BURN_AWAY surface configurations
    - HT3D configurations
    """

    # SURF attributes that can reference RAMPs
    SURF_RAMP_ATTRIBUTES: ClassVar[list[str]] = [
        "ramp_q",
        "ramp_mf",
        "ramp_t",
        "ramp_tmp_back",
        "ramp_t_i",
        "ramp_ef",
        "ramp_heat_transfer_coefficient",
        "ramp_heat_transfer_coefficient_back",
        "ramp_tmp_gas_front",
        "ramp_tmp_gas_back",
        "ramp_ihs",
    ]

    # MATL attributes that can reference RAMPs
    MATL_RAMP_ATTRIBUTES: ClassVar[list[str]] = [
        "conductivity_ramp",
        "specific_heat_ramp",
    ]

    # Built-in species that don't need to be defined
    BUILTIN_SPECIES: ClassVar[set[str]] = {"AIR", "PRODUCTS", "SOOT", "WATER VAPOR"}

    def __init__(self, simulation: "Simulation | SimulationRegistry") -> None:
        if hasattr(simulation, "_registry"):
            self._registry = simulation._registry
        else:
            self._registry = simulation
        self._ramp_ids: set[str] = set()
        self._surface_ids: set[str] = set()
        self._material_ids: set[str] = set()
        self._species_ids: set[str] = set()

    def validate(self) -> list[Issue]:
        """Run all cross-reference validations.

        Returns
        -------
        list[Issue]
            All validation issues found
        """
        # Cache IDs for efficiency
        self._cache_ids()

        issues: list[Issue] = []
        issues.extend(self._check_surface_material_refs())
        issues.extend(self._check_ramp_refs())
        issues.extend(self._check_material_species_refs())
        issues.extend(self._check_burn_away_config())
        issues.extend(self._check_ht3d_config())

        return issues

    def _cache_ids(self) -> None:
        """Cache all registered IDs for efficient lookup."""
        self._ramp_ids = set(self._registry.ramps.list_ids())
        self._surface_ids = set(self._registry.surfaces.list_ids())
        self._material_ids = set(self._registry.materials.list_ids())
        self._species_ids = set(self._registry.species.list_ids())

        # Add fuel species from reactions
        for reac in self._registry.reactions.list_items():
            if reac.fuel:
                self._species_ids.add(reac.fuel)

        # Add built-in species
        self._species_ids.update(self.BUILTIN_SPECIES)

    def _check_surface_material_refs(self) -> list[Issue]:
        """Check SURF→MATL references."""
        issues: list[Issue] = []

        for surf in self._registry.surfaces.list_items():
            if surf.matl_id:
                for matl_id in flatten_to_list(surf.matl_id):
                    if matl_id and matl_id not in self._material_ids:
                        issues.append(
                            Issue(
                                Severity.ERROR,
                                f"Surface '{surf.id}' references undefined material '{matl_id}'",
                                "SURF",
                                "matl_id",
                            )
                        )

        return issues

    def _check_ramp_refs(self) -> list[Issue]:
        """Check RAMP references from SURF and MATL."""
        issues: list[Issue] = []

        # Check SURF ramp references
        for surf in self._registry.surfaces.list_items():
            for attr in self.SURF_RAMP_ATTRIBUTES:
                ramp_id = getattr(surf, attr, None)
                if ramp_id and ramp_id not in self._ramp_ids:
                    issues.append(
                        Issue(
                            Severity.WARNING,
                            f"Surface '{surf.id}' references undefined ramp '{ramp_id}'",
                            "SURF",
                            attr,
                        )
                    )

        # Check MATL ramp references
        for matl in self._registry.materials.list_items():
            for attr in self.MATL_RAMP_ATTRIBUTES:
                ramp_id = getattr(matl, attr, None)
                if ramp_id and ramp_id not in self._ramp_ids:
                    issues.append(
                        Issue(
                            Severity.WARNING,
                            f"Material '{matl.id}' references undefined ramp '{ramp_id}'",
                            "MATL",
                            attr,
                        )
                    )

        return issues

    def _check_material_species_refs(self) -> list[Issue]:
        """Check MATL→SPEC references."""
        issues = []

        for matl in self._registry.materials.list_items():
            if matl.spec_id:
                for spec_id in flatten_to_list(matl.spec_id):
                    if spec_id and spec_id not in self._species_ids:
                        issues.append(
                            Issue(
                                Severity.WARNING,
                                f"Material '{matl.id}' references undefined species '{spec_id}'",
                                "MATL",
                                "spec_id",
                            )
                        )

        return issues

    def _check_burn_away_config(self) -> list[Issue]:
        """Check BURN_AWAY surface and obstruction configuration."""
        issues: list[Issue] = []

        # Find surfaces with BURN_AWAY=True
        burn_away_surfaces = {
            surf.id
            for surf in self._registry.surfaces.list_items()
            if getattr(surf, "burn_away", False)
        }

        if not burn_away_surfaces:
            return issues

        # Check that obstructions using burn-away surfaces have BULK_DENSITY
        for obst in self._registry.obstructions.list_items():
            surf_ids = get_surface_ids_from_obstruction(obst)
            uses_burn_away = any(sid in burn_away_surfaces for sid in surf_ids)

            if uses_burn_away:
                bulk_density = getattr(obst, "bulk_density", None)
                if bulk_density is None:
                    obst_id = obst.id or "unnamed"
                    issues.append(
                        Issue(
                            Severity.WARNING,
                            f"Obstruction '{obst_id}' uses BURN_AWAY surface but has no BULK_DENSITY",
                            "OBST",
                            "bulk_density",
                        )
                    )

        return issues

    def _check_ht3d_config(self) -> list[Issue]:
        """Check HT3D (3D heat transfer) configuration."""
        issues: list[Issue] = []

        # Find surfaces with HT3D=True
        ht3d_surfaces = {
            surf.id for surf in self._registry.surfaces.list_items() if getattr(surf, "ht3d", False)
        }

        if not ht3d_surfaces:
            return issues

        # Check that VENTs don't use HT3D surfaces (not valid)
        for vent in self._registry.vents.list_items():
            if vent.surf_id in ht3d_surfaces:
                vent_id = vent.id or "unnamed"
                issues.append(
                    Issue(
                        Severity.ERROR,
                        f"Vent '{vent_id}' uses HT3D surface - HT3D surfaces cannot be used on VENTs",
                        "VENT",
                        "surf_id",
                    )
                )

        # Check that obstructions using HT3D have MATL_ID
        for obst in self._registry.obstructions.list_items():
            if getattr(obst, "ht3d", False) and not obst.matl_id:
                obst_id = obst.id or "unnamed"
                issues.append(
                    Issue(
                        Severity.ERROR,
                        f"Obstruction '{obst_id}' has HT3D=True but no MATL_ID",
                        "OBST",
                        "matl_id",
                    )
                )

        return issues


__all__ = ["CrossReferenceValidator"]
