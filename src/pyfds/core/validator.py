"""
Validation module for FDS input files.

This module provides validation functionality to ensure FDS input files
are syntactically and semantically correct before execution.
"""

import re
from pathlib import Path
from typing import TYPE_CHECKING, Any, Protocol

if TYPE_CHECKING:
    from pyfds.core.namelists import Mesh, Time
    from pyfds.core.simulation import Simulation


class ValidationError(Exception):
    """Exception raised for validation errors."""

    pass


class ValidationWarning:
    """Represents a validation warning."""

    def __init__(self, message: str, severity: str = "warning"):
        """
        Initialize a validation warning.

        Parameters
        ----------
        message : str
            Warning message
        severity : str
            Severity level ('info', 'warning', 'error')
        """
        self.message = message
        self.severity = severity

    def __str__(self) -> str:
        return f"[{self.severity.upper()}] {self.message}"


class Validator:
    """
    Validator for FDS simulation configurations.

    Performs comprehensive validation of FDS input including:
    - Parameter type and range checking
    - Cross-reference validation
    - Physical reasonableness checks
    - Mesh quality assessment
    """

    def __init__(self) -> None:
        """Initialize the validator."""
        self.warnings: list[ValidationWarning] = []
        self.errors: list[str] = []

    def validate_simulation(self, simulation: "Simulation") -> bool:
        """
        Validate a complete simulation.

        Parameters
        ----------
        simulation : Simulation
            Simulation object to validate

        Returns
        -------
        bool
            True if validation passes (no errors)

        Raises
        ------
        ValidationError
            If critical errors are found
        """
        self.warnings.clear()
        self.errors.clear()

        # Required components
        self._check_required_components(simulation)

        # Validate individual components
        if simulation.time_params:
            self._validate_time(simulation.time_params)

        # Validate meshes
        for mesh in simulation.geometry.meshes:
            self._validate_mesh(mesh)

        # Cross-reference validation
        self._validate_surface_references(simulation)
        self._validate_material_references(simulation)
        self._validate_reaction_yield_conservation(simulation)
        self._check_duplicate_ids(simulation)

        # Mesh quality checks
        self._check_mesh_quality(simulation)

        # If errors exist, raise exception
        if self.errors:
            error_msg = "\n".join(self.errors)
            raise ValidationError(f"Validation failed:\n{error_msg}")

        return True

    def _check_required_components(self, simulation: "Simulation") -> None:
        """Check for required simulation components."""
        if not simulation.geometry.meshes:
            self.errors.append("At least one MESH is required")

        if not simulation.time_params:
            self.errors.append("TIME namelist is required")

    def _validate_time(self, time_params: "Time") -> None:
        """Validate TIME namelist parameters."""
        if time_params.t_end <= 0:
            self.errors.append("T_END must be positive")

        if time_params.t_begin is not None and time_params.t_begin >= time_params.t_end:
            self.errors.append("T_BEGIN must be less than T_END")

        if time_params.dt is not None and time_params.dt > time_params.t_end / 10:
            self.warnings.append(
                ValidationWarning(
                    f"DT ({time_params.dt}) seems large relative to T_END "
                    f"({time_params.t_end}). Consider smaller time step.",
                    severity="warning",
                )
            )

    def _validate_mesh(self, mesh: "Mesh") -> None:
        """Validate MESH namelist parameters."""
        # Check grid dimensions
        total_cells = mesh.ijk.total_cells

        if total_cells > 10_000_000:
            self.warnings.append(
                ValidationWarning(
                    f"Mesh has {total_cells:,} cells which may be computationally "
                    "expensive. Consider reducing resolution.",
                    severity="warning",
                )
            )

        if total_cells < 1000:
            self.warnings.append(
                ValidationWarning(
                    f"Mesh has only {total_cells} cells which may be too coarse "
                    "for accurate results.",
                    severity="info",
                )
            )

        # Check cell sizes
        dx, dy, dz = mesh.get_cell_size()

        if min(dx, dy, dz) < 0.001:
            self.warnings.append(
                ValidationWarning(
                    f"Very small cell size detected ({min(dx, dy, dz):.4f} m). "
                    "This may cause stability issues.",
                    severity="warning",
                )
            )

    def _validate_surface_references(self, simulation: "Simulation") -> None:
        """Validate that all surface references exist."""
        # Collect all surface IDs (including built-in surfaces)
        surface_ids = {s.id for s in simulation.material_mgr.surfaces}
        builtin_surfaces = {"INERT", "OPEN", "MIRROR", "PERIODIC"}
        all_valid_ids = surface_ids | builtin_surfaces

        # Check obstruction surface references
        for i, obst in enumerate(simulation.geometry.obstructions):
            for surf_id in [
                obst.surf_id,
                obst.surf_id_top,
                obst.surf_id_bottom,
                obst.surf_id_sides,
            ]:
                if surf_id and surf_id not in all_valid_ids:
                    self.errors.append(f"Obstruction {i} references undefined surface '{surf_id}'")

    def _check_duplicate_ids(self, simulation: "Simulation") -> None:
        """Check for duplicate IDs in devices, meshes, etc."""
        device_ids = [d.id for d in simulation.instrumentation.devices]
        # Report duplicate device IDs if any exist
        if len(device_ids) != len(set(device_ids)):
            self.errors.append("Duplicate device IDs found in instrumentation")

    def _check_mesh_quality(self, simulation: "Simulation") -> None:
        """Check mesh quality and provide recommendations."""
        for i, mesh in enumerate(simulation.geometry.meshes):
            dx, dy, dz = mesh.get_cell_size()

            # Check aspect ratio
            max_ratio = max(dx / dy, dy / dx, dx / dz, dz / dx, dy / dz, dz / dy)

            if max_ratio > 2.0:
                self.warnings.append(
                    ValidationWarning(
                        f"Mesh {i} has non-cubic cells (aspect ratio {max_ratio:.2f}). "
                        "FDS works best with cubic cells.",
                        severity="warning",
                    )
                )

            # Check if D*/dx ratio is reasonable for fire scenarios
            # Typical fire: D* = (Q/rho_inf/cp/T_inf/sqrt(g))^(2/5)
            # For 100 kW fire, D* ≈ 0.3 m
            # Recommend dx < D*/10
            characteristic_fire_size = 0.3  # meters
            recommended_dx = characteristic_fire_size / 10

            if max(dx, dy, dz) > recommended_dx * 2:
                self.warnings.append(
                    ValidationWarning(
                        f"Mesh {i} cell size ({max(dx, dy, dz):.3f} m) may be too "
                        f"coarse for typical fire simulations. "
                        f"Consider dx < {recommended_dx:.3f} m.",
                        severity="info",
                    )
                )

    def _validate_material_references(self, simulation: "Simulation") -> None:
        """Validate all material ID references."""
        material_ids = {m.id for m in simulation.material_mgr.materials}
        # TODO: Add species manager to track defined species
        # For now, skip species validation as species are not centrally managed
        species_ids: set[str] = set()  # Empty set until species manager is implemented

        for matl in simulation.material_mgr.materials:
            # Check SPEC_ID references
            if matl.spec_id:
                spec_ids = self._flatten_to_list(matl.spec_id)
                for spec_id in spec_ids:
                    if spec_id and spec_id not in species_ids:
                        # Skip validation for now - species not centrally managed
                        pass
                        # self.errors.append(f"Material '{matl.id}': SPEC_ID '{spec_id}' not defined")

            # Check residue MATL_ID references
            if matl.matl_id_products:
                matl_ids = self._flatten_to_list(matl.matl_id_products)
                for matl_id in matl_ids:
                    if matl_id and matl_id not in material_ids:
                        self.errors.append(
                            f"Material '{matl.id}': Residue MATL_ID '{matl_id}' not defined"
                        )

    def _validate_reaction_yield_conservation(self, simulation: "Simulation") -> None:
        """Validate that reaction yields are physically reasonable."""
        for matl in simulation.material_mgr.materials:
            if matl.n_reactions and matl.n_reactions > 0:
                for j in range(matl.n_reactions):
                    total_yield = 0.0
                    # Sum species yields
                    if matl.nu_spec:
                        total_yield += sum(self._get_reaction_yields(matl.nu_spec, j))
                    # Sum material yields
                    if matl.nu_matl:
                        total_yield += sum(self._get_reaction_yields(matl.nu_matl, j))

                    if total_yield > 1.01:
                        self.errors.append(
                            f"Material '{matl.id}' reaction {j + 1}: "
                            f"Total yield {total_yield:.2f} > 1.0"
                        )
                    elif total_yield < 0.99:
                        self.warnings.append(
                            ValidationWarning(
                                f"Material '{matl.id}' reaction {j + 1}: "
                                f"Total yield {total_yield:.2f} < 1.0 (mass loss)",
                                severity="warning",
                            )
                        )

    def _flatten_to_list(self, nested_list: Any) -> list:
        """Flatten nested lists to a single list."""
        if not isinstance(nested_list, list):
            return [nested_list] if nested_list else []

        flat = []
        for item in nested_list:
            if isinstance(item, list):
                flat.extend(self._flatten_to_list(item))
            else:
                flat.append(item)
        return flat

    def _get_reaction_yields(self, yield_data: Any, reaction_idx: int) -> list[float]:
        """Extract yields for a specific reaction from 2D or 1D data."""
        if not yield_data:
            return []

        if isinstance(yield_data, list) and reaction_idx < len(yield_data):
            reaction_yields = yield_data[reaction_idx]
            if isinstance(reaction_yields, list):
                return [float(y) for y in reaction_yields if y is not None]
            if isinstance(reaction_yields, (int, float)):
                return [float(reaction_yields)]
        return []

    def get_warnings(self) -> list[ValidationWarning]:
        """Get all validation warnings."""
        return self.warnings

    def get_errors(self) -> list[str]:
        """Get all validation errors."""
        return self.errors


def validate_fds_file(filepath: Path) -> bool:
    """
    Validate an existing FDS input file.

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


class ValidationStrategy(Protocol):
    """
    Strategy interface for simulation validation.

    This protocol defines the contract for all validation strategies.
    """

    def validate(self, simulation: "Simulation") -> list[str]:
        """
        Validate the simulation and return warnings.

        Parameters
        ----------
        simulation : Simulation
            The simulation to validate

        Returns
        -------
        list[str]
            List of validation warnings (empty if no issues)
        """
        ...


class BasicValidationStrategy:
    """
    Basic validation strategy.

    Performs minimal validation checks focusing on required components
    and delegating to manager validators.
    """

    def validate(self, simulation: "Simulation") -> list[str]:
        """
        Perform basic validation of the simulation.

        Parameters
        ----------
        simulation : Simulation
            The simulation to validate

        Returns
        -------
        list[str]
            List of validation warnings
        """
        warnings = []

        # Validate required components
        if not simulation.time_params:
            warnings.append("No time parameters defined - TIME namelist required")

        # Delegate to manager validators
        warnings.extend(simulation.geometry.validate())
        warnings.extend(simulation.material_mgr.validate())
        warnings.extend(simulation.physics.validate())
        warnings.extend(simulation.instrumentation.validate())
        warnings.extend(simulation.controls.validate())
        warnings.extend(simulation.ramps.validate())

        # Cross-manager validation: Check surface ID references
        referenced_surf_ids = set()
        for obst in simulation.geometry.obstructions:
            if obst.surf_id:
                referenced_surf_ids.add(obst.surf_id)
            if obst.surf_id_top:
                referenced_surf_ids.add(obst.surf_id_top)
            if obst.surf_id_bottom:
                referenced_surf_ids.add(obst.surf_id_bottom)
            if obst.surf_id_sides:
                referenced_surf_ids.add(obst.surf_id_sides)

        for vent in simulation.geometry.vents:
            if vent.surf_id:
                referenced_surf_ids.add(vent.surf_id)

        warnings.extend(simulation.material_mgr.validate_surface_references(referenced_surf_ids))

        return warnings


class ComprehensiveValidationStrategy:
    """
    Comprehensive validation strategy.

    Uses the full Validator class for thorough validation including
    parameter ranges, mesh quality, and semantic checks.
    """

    def validate(self, simulation: "Simulation") -> list[str]:
        """
        Perform comprehensive validation of the simulation.

        Parameters
        ----------
        simulation : Simulation
            The simulation to validate

        Returns
        -------
        list[str]
            List of validation warnings and errors
        """
        validator = Validator()
        try:
            validator.validate_simulation(simulation)
            # Convert ValidationWarning objects to strings
            return [str(w) for w in validator.warnings]
        except ValidationError as e:
            # Return errors as warnings for compatibility with existing API
            return [str(e)]
