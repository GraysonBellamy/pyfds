# src/pyfds/core/validators/cross_references.py
"""Cross-namelist reference validation."""

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from pyfds.core.namelists.obst import Obstruction
    from pyfds.core.simulation import Simulation


class CrossReferenceValidator:
    """Validates references between namelists."""

    def __init__(self, simulation: "Simulation"):
        self.sim = simulation
        self.errors: list[str] = []
        self.warnings: list[str] = []

    def validate_all(self) -> tuple[list[str], list[str]]:
        """Run all cross-reference validations."""
        self._validate_surf_matl_references()
        self._validate_ramp_references()
        self._validate_spec_references()
        self._validate_burn_away_configuration()
        self._validate_ht3d_configuration()
        return self.errors, self.warnings

    def _validate_surf_matl_references(self) -> None:
        """Verify MATL_ID in SURF references existing materials."""
        material_ids = {m.id for m in self.sim.material_mgr.materials}

        for surf in self.sim.material_mgr.surfaces:
            if surf.matl_id is None:
                continue

            # Flatten matl_id to list of IDs
            matl_ids = self._flatten_matl_id(surf.matl_id)

            for matl_id in matl_ids:
                if matl_id and matl_id not in material_ids:
                    self.errors.append(
                        f"SURF '{surf.id}': MATL_ID '{matl_id}' not found in materials"
                    )

    def _validate_ramp_references(self) -> None:
        """Verify RAMP_ID references exist."""
        ramp_ids = {r.id for r in self.sim.ramps.ramps}

        # Check SURF ramps
        for surf in self.sim.material_mgr.surfaces:
            ramp_attrs = [
                "ramp_q",
                "ramp_mf",
                "ramp_ef",
                "ramp_t",
                "ramp_tmp_back",
                "ramp_tmp_gas_front",
                "ramp_tmp_gas_back",
                "ramp_t_i",
                "ramp_heat_transfer_coefficient",
                "ramp_heat_transfer_coefficient_back",
                "ramp_ihs",
            ]
            for attr in ramp_attrs:
                ramp_id = getattr(surf, attr, None)
                if ramp_id and ramp_id not in ramp_ids:
                    self.warnings.append(
                        f"SURF '{surf.id}': {attr.upper()}='{ramp_id}' not found in ramps"
                    )

        # Check MATL ramps
        for matl in self.sim.material_mgr.materials:
            for attr in ["conductivity_ramp", "specific_heat_ramp"]:
                ramp_id = getattr(matl, attr, None)
                if ramp_id and ramp_id not in ramp_ids:
                    self.warnings.append(
                        f"MATL '{matl.id}': {attr.upper()}='{ramp_id}' not found in ramps"
                    )

    def _validate_spec_references(self) -> None:
        """Verify SPEC_ID references exist or have corresponding REAC."""
        # Get defined species and reaction fuels
        # TODO: Implement species manager
        # spec_ids = {s.id for s in self.sim.species} if hasattr(self.sim, 'species') else set()
        spec_ids: set[str] = set()
        reac_fuels = {r.fuel for r in self.sim.physics.reactions}
        valid_specs = spec_ids | reac_fuels | {"AIR", "PRODUCTS"}  # Built-in species

        for matl in self.sim.material_mgr.materials:
            if matl.spec_id is None:
                continue

            specs = self._flatten_spec_id(matl.spec_id)
            for spec in specs:
                if spec and spec not in valid_specs:
                    self.warnings.append(
                        f"MATL '{matl.id}': SPEC_ID '{spec}' not found in species/reactions"
                    )

    def _validate_burn_away_configuration(self) -> None:
        """Verify BURN_AWAY surfaces have proper OBST configuration."""
        burn_away_surfs = {s.id for s in self.sim.material_mgr.surfaces if s.burn_away}

        if not burn_away_surfs:
            return

        # Check that OBSTs using these SURFs have BULK_DENSITY set
        for obst in self.sim.geometry.obstructions:
            surf_ids = self._get_obst_surf_ids(obst)
            for surf_id in surf_ids:
                if surf_id in burn_away_surfs and obst.bulk_density is None:
                    self.warnings.append(
                        f"OBST '{obst.id or 'unnamed'}': Uses BURN_AWAY surface "
                        f"'{surf_id}' but BULK_DENSITY not set"
                    )

    def _validate_ht3d_configuration(self) -> None:
        """Verify HT3D surfaces are used with OBST, not standalone."""
        ht3d_surfs = {s.id for s in self.sim.material_mgr.surfaces if s.ht3d}

        if not ht3d_surfs:
            return

        # Warn if HT3D surface is used on VENT (not valid)
        for vent in self.sim.geometry.vents:
            if vent.surf_id in ht3d_surfs:
                self.errors.append(
                    f"VENT '{vent.id or 'unnamed'}': HT3D surfaces cannot be used on VENTs"
                )

    @staticmethod
    def _flatten_matl_id(matl_id: str | list) -> list[str]:
        """Flatten nested matl_id to list of strings."""
        if isinstance(matl_id, str):
            return [matl_id]
        if isinstance(matl_id, list):
            result = []
            for item in matl_id:
                if isinstance(item, str):
                    result.append(item)
                elif isinstance(item, list):
                    result.extend(item)
            return result
        return []

    @staticmethod
    def _flatten_spec_id(spec_id: str | list) -> list[str]:
        """Flatten nested spec_id to list of strings."""
        if isinstance(spec_id, str):
            return [spec_id]
        if isinstance(spec_id, list):
            result = []
            for item in spec_id:
                if isinstance(item, str):
                    result.append(item)
                elif isinstance(item, list):
                    result.extend(item)
            return result
        return []

    @staticmethod
    def _get_obst_surf_ids(obst: "Obstruction") -> list[str]:
        """Get all SURF_IDs used by an obstruction."""
        ids = []
        if obst.surf_id:
            ids.append(obst.surf_id)
        if obst.surf_id_top:
            ids.append(obst.surf_id_top)
        if obst.surf_id_bottom:
            ids.append(obst.surf_id_bottom)
        if obst.surf_id_sides:
            ids.append(obst.surf_id_sides)
        return ids
