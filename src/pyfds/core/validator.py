"""
Validation module for FDS input files.

This module provides validation functionality to ensure FDS input files
are syntactically and semantically correct before execution.
"""

import re
from pathlib import Path
from typing import TYPE_CHECKING, List

if TYPE_CHECKING:
    from pyfds.core.namelist import Mesh, Time
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
        self.warnings: List[ValidationWarning] = []
        self.errors: List[str] = []

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

        for mesh in simulation.meshes:
            self._validate_mesh(mesh)

        # Cross-reference validation
        self._validate_surface_references(simulation)
        self._validate_device_ids(simulation)

        # Mesh quality checks
        self._check_mesh_quality(simulation)

        # If errors exist, raise exception
        if self.errors:
            error_msg = "\n".join(self.errors)
            raise ValidationError(f"Validation failed:\n{error_msg}")

        return True

    def _check_required_components(self, simulation: "Simulation") -> None:
        """Check for required simulation components."""
        if not simulation.meshes:
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
        total_cells = mesh.ijk[0] * mesh.ijk[1] * mesh.ijk[2]

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
        surface_ids = {s.id for s in simulation.surfaces}
        builtin_surfaces = {"INERT", "OPEN", "MIRROR", "PERIODIC"}
        all_valid_ids = surface_ids | builtin_surfaces

        # Check obstruction surface references
        for i, obst in enumerate(simulation.obstructions):
            for surf_id in [
                obst.surf_id,
                obst.surf_id_top,
                obst.surf_id_bottom,
                obst.surf_id_sides,
            ]:
                if surf_id and surf_id not in all_valid_ids:
                    self.errors.append(f"Obstruction {i} references undefined surface '{surf_id}'")

    def _validate_device_ids(self, simulation: "Simulation") -> None:
        """Validate device IDs are unique."""
        device_ids = [d.id for d in simulation.devices]
        duplicates = {id for id in device_ids if device_ids.count(id) > 1}

        if duplicates:
            self.errors.append(f"Duplicate device IDs found: {', '.join(duplicates)}")

    def _check_mesh_quality(self, simulation: "Simulation") -> None:
        """Check mesh quality and provide recommendations."""
        for i, mesh in enumerate(simulation.meshes):
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

    def get_warnings(self) -> List[ValidationWarning]:
        """Get all validation warnings."""
        return self.warnings

    def get_errors(self) -> List[str]:
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
