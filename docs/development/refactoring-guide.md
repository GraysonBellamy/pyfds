# PyFDS Refactoring Guide: Code Consolidation

This document outlines three targeted refactoring tasks to improve code organization and maintainability:

1. **Consolidate enums to `enums.py`**
2. **Remove phase/stage comments from code**
3. **Consolidate validators into `validation/` module**

---

## Table of Contents

- [1. Consolidate Enums to `enums.py`](#1-consolidate-enums-to-enumspy)
  - [1.1 Current State](#11-current-state)
  - [1.2 Target State](#12-target-state)
  - [1.3 Implementation Steps](#13-implementation-steps)
  - [1.4 Migration Checklist](#14-migration-checklist)
- [2. Remove Phase/Stage Comments](#2-remove-phasestage-comments)
  - [2.1 Comments to Remove](#21-comments-to-remove)
  - [2.2 Implementation Steps](#22-implementation-steps)
- [3. Consolidate Validators](#3-consolidate-validators)
  - [3.1 Current State](#31-current-state)
  - [3.2 Target State](#32-target-state)
  - [3.3 Implementation Steps](#33-implementation-steps)
  - [3.4 Migration Checklist](#34-migration-checklist)

---

## 1. Consolidate Enums to `enums.py`

### 1.1 Current State

Enums are scattered across multiple files:

| Enum Class | Current Location | Values |
|------------|-----------------|--------|
| `SolidGeometry` | `core/enums.py` | CARTESIAN, CYLINDRICAL, SPHERICAL, INNER_CYLINDRICAL |
| `BackingCondition` | `core/enums.py` | VOID, INSULATED, EXPOSED |
| `HeatTransferModel` | `core/enums.py` | LOGLAW, IMPINGING_JET |
| `SprayPattern` | `core/enums.py` | UNIFORM, GAUSSIAN |
| `Severity` | `core/validator.py` | ERROR, WARNING, INFO |
| `TurbulenceModel` | `core/namelists/misc.py` | DEARDORFF, DYNAMIC_SMAGORINSKY, VREMAN, WALE |
| `VentType` | `core/namelists/vent.py` | OPEN, HVAC, SURFACE, MIRROR, PERIODIC |
| `VentShape` | `core/namelists/vent.py` | RECTANGULAR, CIRCULAR, ANNULAR |
| `ControlFunction` | `core/namelists/ctrl.py` | ANY, ALL, ONLY, TIME_DELAY, CUSTOM, KILL, RESTART |

**Problems:**
- Inconsistent organization (some in `enums.py`, some inline)
- Enums in `enums.py` are not exported at package level
- Maintenance burden when looking for enum definitions
- Circular import risks when enums are in namelist files

### 1.2 Target State

All enums consolidated in `src/pyfds/core/enums.py`:

```python
"""Enumerations for FDS parameters.

This module contains all enumeration types used across PyFDS,
organized by their FDS namelist or functional area.
"""

from enum import Enum


# =============================================================================
# Validation Enums
# =============================================================================

class Severity(str, Enum):
    """Validation issue severity levels."""
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


# =============================================================================
# SURF (Surface) Enums
# =============================================================================

class SolidGeometry(str, Enum):
    """Solid phase geometry types for SURF."""
    CARTESIAN = "CARTESIAN"
    CYLINDRICAL = "CYLINDRICAL"
    SPHERICAL = "SPHERICAL"
    INNER_CYLINDRICAL = "INNER CYLINDRICAL"


class BackingCondition(str, Enum):
    """Backing conditions for SURF."""
    VOID = "VOID"
    INSULATED = "INSULATED"
    EXPOSED = "EXPOSED"


class HeatTransferModel(str, Enum):
    """Heat transfer models for SURF."""
    LOGLAW = "LOGLAW"
    IMPINGING_JET = "IMPINGING JET"


class SprayPattern(str, Enum):
    """Spray patterns for particle generation on SURF."""
    UNIFORM = "UNIFORM"
    GAUSSIAN = "GAUSSIAN"


# =============================================================================
# MISC (Miscellaneous) Enums
# =============================================================================

class TurbulenceModel(str, Enum):
    """Turbulence models for MISC namelist."""
    DEARDORFF = "DEARDORFF"
    DYNAMIC_SMAGORINSKY = "DYNAMIC SMAGORINSKY"
    VREMAN = "VREMAN"
    WALE = "WALE"


# =============================================================================
# VENT Enums
# =============================================================================

class VentType(str, Enum):
    """Types of vents in FDS."""
    OPEN = "OPEN"
    HVAC = "HVAC"
    SURFACE = "SURFACE"
    MIRROR = "MIRROR"
    PERIODIC = "PERIODIC"


class VentShape(str, Enum):
    """Vent geometry types."""
    RECTANGULAR = "RECTANGULAR"
    CIRCULAR = "CIRCULAR"
    ANNULAR = "ANNULAR"


# =============================================================================
# CTRL (Control) Enums
# =============================================================================

class ControlFunction(str, Enum):
    """Control function types for CTRL namelist."""
    ANY = "ANY"
    ALL = "ALL"
    ONLY = "ONLY"
    TIME_DELAY = "TIME_DELAY"
    CUSTOM = "CUSTOM"
    KILL = "KILL"
    RESTART = "RESTART"


__all__ = [
    # Validation
    "Severity",
    # SURF
    "SolidGeometry",
    "BackingCondition",
    "HeatTransferModel",
    "SprayPattern",
    # MISC
    "TurbulenceModel",
    # VENT
    "VentType",
    "VentShape",
    # CTRL
    "ControlFunction",
]
```

### 1.3 Implementation Steps

#### Step 1: Update `core/enums.py`

Add the missing enums to `src/pyfds/core/enums.py`:

```python
# Add Severity enum (from validator.py)
class Severity(str, Enum):
    """Validation issue severity levels."""
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


# Add TurbulenceModel enum (from misc.py)
class TurbulenceModel(str, Enum):
    """Turbulence models for MISC namelist."""
    DEARDORFF = "DEARDORFF"
    DYNAMIC_SMAGORINSKY = "DYNAMIC SMAGORINSKY"
    VREMAN = "VREMAN"
    WALE = "WALE"


# Add VentType enum (from vent.py)
class VentType(str, Enum):
    """Types of vents in FDS."""
    OPEN = "OPEN"
    HVAC = "HVAC"
    SURFACE = "SURFACE"
    MIRROR = "MIRROR"
    PERIODIC = "PERIODIC"


# Add VentShape enum (from vent.py)
class VentShape(str, Enum):
    """Vent geometry types."""
    RECTANGULAR = "RECTANGULAR"
    CIRCULAR = "CIRCULAR"
    ANNULAR = "ANNULAR"


# Add ControlFunction enum (from ctrl.py)
class ControlFunction(str, Enum):
    """Control function types for CTRL namelist."""
    ANY = "ANY"
    ALL = "ALL"
    ONLY = "ONLY"
    TIME_DELAY = "TIME_DELAY"
    CUSTOM = "CUSTOM"
    KILL = "KILL"
    RESTART = "RESTART"
```

#### Step 2: Update imports in source files

**File: `src/pyfds/core/validator.py`**

```python
# REMOVE this enum definition:
# class Severity(Enum):
#     """Validation issue severity."""
#     ERROR = "error"
#     WARNING = "warning"
#     INFO = "info"

# ADD import:
from pyfds.core.enums import Severity
```

**File: `src/pyfds/core/namelists/misc.py`**

```python
# REMOVE this enum definition:
# class TurbulenceModel(str, Enum):
#     ...

# CHANGE import to:
from pyfds.core.enums import TurbulenceModel
```

**File: `src/pyfds/core/namelists/vent.py`**

```python
# REMOVE these enum definitions:
# class VentType(str, Enum):
#     ...
# class VentShape(str, Enum):
#     ...

# ADD import:
from pyfds.core.enums import VentShape, VentType
```

**File: `src/pyfds/core/namelists/ctrl.py`**

```python
# REMOVE this enum definition:
# class ControlFunction(str, Enum):
#     ...

# ADD import:
from pyfds.core.enums import ControlFunction
```

#### Step 3: Update re-exports

**File: `src/pyfds/core/namelists/__init__.py`**

```python
# CHANGE: Remove local imports of enums that were defined in namelist files
# These now come from enums.py

# Keep re-exporting for backward compatibility:
from pyfds.core.enums import ControlFunction, TurbulenceModel, VentShape, VentType
```

**File: `src/pyfds/core/__init__.py`**

```python
# ADD to exports:
from .enums import (
    BackingCondition,
    ControlFunction,
    HeatTransferModel,
    Severity,
    SolidGeometry,
    SprayPattern,
    TurbulenceModel,
    VentShape,
    VentType,
)
```

#### Step 4: Update test imports

Update any test files that import enums directly from their old locations.

### 1.4 Migration Checklist

- [ ] Add `Severity` to `enums.py`
- [ ] Add `TurbulenceModel` to `enums.py`
- [ ] Add `VentType` to `enums.py`
- [ ] Add `VentShape` to `enums.py`
- [ ] Add `ControlFunction` to `enums.py`
- [ ] Update `__all__` in `enums.py`
- [ ] Remove `Severity` from `validator.py`, add import
- [ ] Remove `TurbulenceModel` from `misc.py`, add import
- [ ] Remove `VentType` and `VentShape` from `vent.py`, add import
- [ ] Remove `ControlFunction` from `ctrl.py`, add import
- [ ] Update `core/__init__.py` exports
- [ ] Update `core/namelists/__init__.py` exports
- [ ] Update test file imports
- [ ] Run full test suite
- [ ] Update any documentation references

---

## 2. Remove Phase/Stage Comments

### 2.1 Comments to Remove

The following development phase/stage marker comments should be removed:

#### File: `src/pyfds/builders/reaction.py` (5 comments)

| Line | Comment to Remove |
|------|-------------------|
| ~76 | `# Stage 1.3 parameters` |
| ~87 | `# Phase 3 parameters` |
| ~429 | `# Phase 3 methods` |
| ~688 | `# Stage 1.3 parameters` |
| ~710 | `# Phase 3 parameters` |

#### File: `src/pyfds/core/namelists/vent.py` (2 comments)

| Line | Comment to Remove |
|------|-------------------|
| ~168 | `# === PHASE 4: VENT ENHANCEMENTS ===` |
| ~249 | `# Phase 4 validations` |

#### File: `src/pyfds/core/namelists/prop.py` (5 comments)

| Line | Comment to Remove |
|------|-------------------|
| ~83 | `# Sprinkler Properties (Stage 2.3)` |
| ~94 | `# Nozzle/Spray Properties (Stage 2.3)` |
| ~100 | `# Smoke Detector Properties (Stage 2.3)` |
| ~105 | `# Heat Detector Properties (Stage 2.3)` |
| ~112 | `# Visualization (Stage 2.3)` |

#### File: `src/pyfds/core/namelists/reac.py` (12 comments)

| Line | Comment to Remove |
|------|-------------------|
| ~143 | `# Reaction identification (Phase 3)` |
| ~146 | `# Product yields (Phase 3)` |
| ~151 | `# Energy parameters (Phase 3)` |
| ~159 | `# Two-step chemistry (Phase 3)` |
| ~164 | `# Product fractions for incomplete combustion (Phase 3)` |
| ~175 | `# Validation options (Phase 3)` |
| ~186 | `# Oxygen limit (Phase 3)` |
| ~191 | `# Auto-ignition exclusion zones (Phase 3)` |
| ~205 | `# Extinction Parameters (Stage 1.3)` |
| ~213 | `# Suppression Parameters (Stage 1.3)` |
| ~217 | `# Heat of Combustion Mode (Stage 1.3)` |
| ~220 | `# Species Tracking (Stage 1.3)` |

**Total: 24 comments to remove**

### 2.2 Implementation Steps

#### Step 1: Clean up `builders/reaction.py`

Replace phase comments with descriptive section headers:

```python
# BEFORE:
# Stage 1.3 parameters
self._extinction_model: str | None = None

# AFTER:
# Extinction parameters
self._extinction_model: str | None = None
```

```python
# BEFORE:
# Phase 3 parameters
self._id: str | None = None

# AFTER:
# Reaction identification
self._id: str | None = None
```

```python
# BEFORE:
# Phase 3 methods
def reaction_id(self, id: str) -> "ReactionBuilder":

# AFTER:
# (Just remove the comment entirely - method names are self-documenting)
def reaction_id(self, id: str) -> "ReactionBuilder":
```

#### Step 2: Clean up `core/namelists/vent.py`

```python
# BEFORE:
# === PHASE 4: VENT ENHANCEMENTS ===
# Geometry parameters
db: str | None = FdsField(...)

# AFTER:
# Geometry parameters
db: str | None = FdsField(...)
```

```python
# BEFORE:
# Phase 4 validations
if self.ior is not None and abs(self.ior) not in [1, 2, 3]:

# AFTER:
# Orientation validation
if self.ior is not None and abs(self.ior) not in [1, 2, 3]:
```

#### Step 3: Clean up `core/namelists/prop.py`

Replace phase markers with descriptive headers:

```python
# BEFORE:
# Sprinkler Properties (Stage 2.3)

# AFTER:
# Sprinkler properties
```

```python
# BEFORE:
# Nozzle/Spray Properties (Stage 2.3)

# AFTER:
# Nozzle and spray properties
```

(Repeat for all 5 comments)

#### Step 4: Clean up `core/namelists/reac.py`

Replace or remove all 12 phase/stage comments:

```python
# BEFORE:
# Reaction identification (Phase 3)
id: str | None = FdsField(None, description="Reaction identifier")

# AFTER:
id: str | None = FdsField(None, description="Reaction identifier")
```

```python
# BEFORE:
# Extinction Parameters (Stage 1.3)
extinction_model: str | None = FdsField(...)

# AFTER:
# Extinction parameters
extinction_model: str | None = FdsField(...)
```

#### Important: Comments to KEEP

Do **NOT** remove these comments - they refer to FDS physics concepts, not development phases:

- "solid phase" (FDS solid-phase combustion)
- "gas phase" (gas-phase properties)
- Any technical FDS terminology containing "phase"

---

## 3. Consolidate Validators

### 3.1 Current State

Validators are scattered across multiple locations:

| File | Contents | Purpose |
|------|----------|---------|
| `core/validator.py` | `Severity`, `Issue`, `Validator`, `validate_fds_file()` | Simulation-level validation |
| `core/validators/cross_references.py` | `CrossReferenceValidator` | Cross-reference validation (unused!) |
| `utils/input_validators.py` | `validate_chid()`, `validate_path()`, etc. | Input sanitization |
| `execution/validation.py` | `ParallelValidator` | MPI/OpenMP validation |

**Problems:**
- `CrossReferenceValidator` exists but is **not integrated** with main `Validator`
- Duplicate flatten utilities between validators
- Inconsistent return types (`list[Issue]` vs `tuple[list, list]`)
- `Severity` enum defined in `validator.py` instead of `enums.py`
- Validation logic split across unrelated directories

### 3.2 Target State

New consolidated structure:

```
src/pyfds/validation/
├── __init__.py              # Public API: ValidationResult, validate_simulation()
├── base.py                  # Issue dataclass (Severity moved to enums.py)
├── input.py                 # Input sanitization (from utils/input_validators.py)
├── simulation.py            # Simulation-level validation (from core/validator.py)
├── cross_references.py      # Cross-reference validation (integrated)
├── execution.py             # Parallel validation (from execution/validation.py)
└── utils.py                 # Shared utilities (flatten_matl_id, etc.)
```

### 3.3 Implementation Steps

#### Step 1: Create `validation/` directory structure

```bash
mkdir -p src/pyfds/validation
touch src/pyfds/validation/__init__.py
touch src/pyfds/validation/base.py
touch src/pyfds/validation/input.py
touch src/pyfds/validation/simulation.py
touch src/pyfds/validation/cross_references.py
touch src/pyfds/validation/execution.py
touch src/pyfds/validation/utils.py
```

#### Step 2: Create `validation/base.py`

```python
"""Base classes for validation system."""

from dataclasses import dataclass

from pyfds.core.enums import Severity


@dataclass(frozen=True)
class Issue:
    """A validation issue.

    Attributes
    ----------
    severity : Severity
        Issue severity level (ERROR, WARNING, INFO)
    message : str
        Human-readable description of the issue
    namelist : str, optional
        FDS namelist name where issue was found
    field : str, optional
        Field name where issue was found
    """
    severity: Severity
    message: str
    namelist: str | None = None
    field: str | None = None

    def __str__(self) -> str:
        prefix = f"[{self.severity.value.upper()}]"
        location = ""
        if self.namelist:
            location = f" {self.namelist}"
            if self.field:
                location += f".{self.field}"
        return f"{prefix}{location}: {self.message}"


@dataclass
class ValidationResult:
    """Result of a validation operation.

    Attributes
    ----------
    issues : list[Issue]
        All validation issues found
    """
    issues: list[Issue]

    @property
    def errors(self) -> list[Issue]:
        """Get only ERROR-level issues."""
        return [i for i in self.issues if i.severity == Severity.ERROR]

    @property
    def warnings(self) -> list[Issue]:
        """Get only WARNING-level issues."""
        return [i for i in self.issues if i.severity == Severity.WARNING]

    @property
    def is_valid(self) -> bool:
        """True if no ERROR-level issues exist."""
        return len(self.errors) == 0

    def __bool__(self) -> bool:
        """True if no issues at all."""
        return len(self.issues) == 0


__all__ = ["Issue", "ValidationResult"]
```

#### Step 3: Create `validation/utils.py`

```python
"""Shared utilities for validation."""

from typing import Any


def flatten_to_list(nested: str | list | None) -> list[str]:
    """Flatten nested lists to a single list of strings.

    Handles MATL_ID, SPEC_ID, and similar FDS array parameters
    that can be strings, lists, or nested lists.

    Parameters
    ----------
    nested : str | list | None
        Value that may be a string, list, or nested list

    Returns
    -------
    list[str]
        Flattened list of string values

    Examples
    --------
    >>> flatten_to_list("WOOD")
    ['WOOD']
    >>> flatten_to_list(["WOOD", "CHAR"])
    ['WOOD', 'CHAR']
    >>> flatten_to_list([["WOOD", "CHAR"], ["ASite"]])
    ['WOOD', 'CHAR', 'ASH']
    """
    if nested is None:
        return []
    if isinstance(nested, str):
        return [nested]

    flat: list[str] = []
    for item in nested:
        if isinstance(item, list):
            flat.extend(flatten_to_list(item))
        elif item is not None:
            flat.append(item)
    return flat


def get_surface_ids_from_obstruction(obst: Any) -> list[str]:
    """Extract all SURF_ID references from an obstruction.

    Parameters
    ----------
    obst : Obstruction
        Obstruction namelist object

    Returns
    -------
    list[str]
        List of all referenced surface IDs
    """
    surf_ids = []
    for attr in ["surf_id", "surf_id_top", "surf_id_bottom",
                 "surf_id_sides", "surf_id_interior"]:
        value = getattr(obst, attr, None)
        if value:
            surf_ids.append(value)
    return surf_ids


__all__ = ["flatten_to_list", "get_surface_ids_from_obstruction"]
```

#### Step 4: Move `input_validators.py` to `validation/input.py`

```python
"""Input validation and sanitization utilities.

This module provides validation for user-provided inputs before they
are used to construct simulation objects.
"""

import re
from pathlib import Path

from pyfds.exceptions import ValidationError

# Constants
MAX_FILE_SIZE = 100 * 1024 * 1024  # 100 MB
CHID_MAX_LENGTH = 60
CHID_PATTERN = re.compile(r'^[A-Za-z0-9_\-\.]+$')


def validate_chid(chid: str) -> str:
    """Validate and normalize FDS CHID (case identifier).

    Parameters
    ----------
    chid : str
        Proposed case identifier

    Returns
    -------
    str
        Validated and stripped CHID

    Raises
    ------
    ValidationError
        If CHID is invalid
    """
    chid = chid.strip()

    if not chid:
        raise ValidationError("CHID cannot be empty", field="chid")

    if len(chid) > CHID_MAX_LENGTH:
        raise ValidationError(
            f"CHID exceeds maximum length of {CHID_MAX_LENGTH}",
            field="chid",
            value=chid
        )

    if not CHID_PATTERN.match(chid):
        raise ValidationError(
            "CHID contains invalid characters. Use only letters, "
            "numbers, underscores, hyphens, and periods.",
            field="chid",
            value=chid
        )

    # Check for path separators
    if "/" in chid or "\\" in chid:
        raise ValidationError(
            "CHID cannot contain path separators",
            field="chid",
            value=chid
        )

    return chid


def validate_path(path: str | Path, must_exist: bool = False) -> Path:
    """Validate a filesystem path.

    Parameters
    ----------
    path : str | Path
        Path to validate
    must_exist : bool
        If True, path must exist on filesystem

    Returns
    -------
    Path
        Resolved Path object

    Raises
    ------
    ValidationError
        If path is invalid
    """
    try:
        p = Path(path).resolve()
    except (OSError, ValueError) as e:
        raise ValidationError(f"Invalid path: {e}", field="path", value=str(path))

    if must_exist and not p.exists():
        raise ValidationError(f"Path does not exist: {p}", field="path", value=str(path))

    return p


def validate_file_size(path: Path, max_size: int = MAX_FILE_SIZE) -> None:
    """Validate that a file is not too large to process.

    Parameters
    ----------
    path : Path
        Path to file
    max_size : int
        Maximum allowed size in bytes

    Raises
    ------
    ValidationError
        If file exceeds max size
    """
    if path.exists():
        size = path.stat().st_size
        if size > max_size:
            raise ValidationError(
                f"File too large: {size:,} bytes (max: {max_size:,})",
                field="path",
                value=str(path)
            )


def validate_positive_number(value: float, name: str = "value") -> float:
    """Validate that a number is positive (> 0).

    Parameters
    ----------
    value : float
        Number to validate
    name : str
        Name for error messages

    Returns
    -------
    float
        The validated value

    Raises
    ------
    ValidationError
        If value is not positive
    """
    if value <= 0:
        raise ValidationError(f"{name} must be positive", field=name, value=value)
    return value


def validate_non_negative_number(value: float, name: str = "value") -> float:
    """Validate that a number is non-negative (>= 0).

    Parameters
    ----------
    value : float
        Number to validate
    name : str
        Name for error messages

    Returns
    -------
    float
        The validated value

    Raises
    ------
    ValidationError
        If value is negative
    """
    if value < 0:
        raise ValidationError(f"{name} cannot be negative", field=name, value=value)
    return value


def safe_read_text(path: Path, max_size: int = MAX_FILE_SIZE) -> str:
    """Safely read a text file with size validation.

    Parameters
    ----------
    path : Path
        Path to file
    max_size : int
        Maximum allowed size

    Returns
    -------
    str
        File contents
    """
    validate_file_size(path, max_size)
    return path.read_text(encoding="utf-8")


__all__ = [
    "CHID_MAX_LENGTH",
    "CHID_PATTERN",
    "MAX_FILE_SIZE",
    "safe_read_text",
    "validate_chid",
    "validate_file_size",
    "validate_non_negative_number",
    "validate_path",
    "validate_positive_number",
]
```

#### Step 5: Create `validation/simulation.py`

Move content from `core/validator.py`:

```python
"""Simulation-level validation.

This module validates complete simulation configurations before
writing or running.
"""

from typing import TYPE_CHECKING

from pyfds.core.enums import Severity
from pyfds.validation.base import Issue, ValidationResult
from pyfds.validation.utils import flatten_to_list

if TYPE_CHECKING:
    from pyfds.core.registry import SimulationRegistry
    from pyfds.core.simulation import Simulation


class SimulationValidator:
    """
    Validates complete simulation configurations.

    Validation is performed in order:
    1. Required components (HEAD, TIME, MESH)
    2. Cross-reference validation (SURF_ID, MATL_ID, etc.)
    3. Geometry quality checks
    4. Physical reasonableness checks
    """

    # Built-in surfaces that don't need to be defined
    BUILTIN_SURFACES = {"INERT", "OPEN", "MIRROR", "PERIODIC"}

    # Built-in species that don't need to be defined
    BUILTIN_SPECIES = {"AIR", "PRODUCTS", "SOOT", "WATER VAPOR"}

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
            for surf_id in [obst.surf_id, obst.surf_id_top,
                           obst.surf_id_bottom, obst.surf_id_sides]:
                if surf_id and surf_id not in valid_surfaces:
                    issues.append(Issue(
                        Severity.ERROR,
                        f"References undefined surface '{surf_id}'",
                        "OBST", "surf_id"
                    ))

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
                        issues.append(Issue(
                            Severity.WARNING,
                            f"References undefined species '{spec_id}'",
                            "MATL", "spec_id"
                        ))

        return issues

    def _check_materials(self) -> list[Issue]:
        """Validate materials and surface references."""
        issues = []

        material_ids = set(self._registry.materials.list_ids())
        for surf in self._registry.surfaces.list_items():
            if surf.matl_id:
                for matl_id in flatten_to_list(surf.matl_id):
                    if matl_id and matl_id not in material_ids:
                        issues.append(Issue(
                            Severity.ERROR,
                            f"Surface '{surf.id}' references undefined material '{matl_id}'",
                            "SURF", "matl_id"
                        ))

        return issues

    def _check_geometry(self) -> list[Issue]:
        """Check geometry configuration: meshes, obstructions, etc."""
        issues = []

        meshes = self._registry.meshes.list_items()
        if not meshes:
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
                    mesh_id = mesh.id or 'unnamed'
                    issues.append(Issue(
                        Severity.WARNING,
                        f"Non-cubic cells (aspect ratio {max_ratio:.2f}) in mesh '{mesh_id}'",
                        "MESH"
                    ))
            except Exception:
                continue

        return issues

    def _check_species(self) -> list[Issue]:
        """Check species configuration."""
        issues = []

        # Check for multiple background species
        background_count = sum(
            1 for s in self._registry.species.list_items() if s.background
        )
        if background_count > 1:
            issues.append(Issue(
                Severity.ERROR,
                f"Multiple background species defined ({background_count}). Only one allowed.",
                "SPEC", "background"
            ))

        return issues

    def _check_physical_bounds(self) -> list[Issue]:
        """Check physical reasonableness."""
        issues = []

        # Check time parameters
        if self._registry.time:
            time = self._registry.time
            if time.t_end <= 0:
                issues.append(Issue(
                    Severity.ERROR, "T_END must be positive", "TIME", "t_end"
                ))
            if time.t_begin is not None and time.t_begin >= time.t_end:
                issues.append(Issue(
                    Severity.ERROR, "T_BEGIN must be less than T_END", "TIME", "t_begin"
                ))

        # Check mesh sizes
        for mesh in self._registry.meshes.list_items():
            total_cells = mesh.ijk.total_cells
            if total_cells > 10_000_000:
                issues.append(Issue(
                    Severity.WARNING,
                    f"Mesh has {total_cells:,} cells which may be computationally expensive",
                    "MESH", "ijk"
                ))

        return issues


# Backward compatibility alias
Validator = SimulationValidator


__all__ = ["SimulationValidator", "Validator"]
```

#### Step 6: Create `validation/cross_references.py`

Integrate the existing cross-reference validator:

```python
"""Cross-reference validation between FDS namelists.

This module provides detailed validation of references between
different namelist types (SURF→MATL, MATL→SPEC, SURF→RAMP, etc.).
"""

from typing import TYPE_CHECKING

from pyfds.core.enums import Severity
from pyfds.validation.base import Issue
from pyfds.validation.utils import flatten_to_list, get_surface_ids_from_obstruction

if TYPE_CHECKING:
    from pyfds.core.registry import SimulationRegistry


class CrossReferenceValidator:
    """Validates cross-references between FDS namelists.

    Provides more detailed reference checking than the main
    SimulationValidator, including:
    - RAMP references from SURF and MATL
    - BURN_AWAY surface configurations
    - HT3D configurations
    """

    # SURF attributes that can reference RAMPs
    SURF_RAMP_ATTRIBUTES = [
        "ramp_q", "ramp_mf", "ramp_t", "ramp_tmp_back", "ramp_t_i",
        "ramp_ef", "ramp_heat_transfer_coefficient",
        "ramp_heat_transfer_coefficient_back", "ramp_tmp_gas_front",
        "ramp_ihs",
    ]

    # MATL attributes that can reference RAMPs
    MATL_RAMP_ATTRIBUTES = [
        "conductivity_ramp", "specific_heat_ramp",
    ]

    def __init__(self, registry: "SimulationRegistry") -> None:
        self._registry = registry
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

    def _check_surface_material_refs(self) -> list[Issue]:
        """Check SURF→MATL references."""
        issues = []

        for surf in self._registry.surfaces.list_items():
            if surf.matl_id:
                for matl_id in flatten_to_list(surf.matl_id):
                    if matl_id and matl_id not in self._material_ids:
                        issues.append(Issue(
                            Severity.ERROR,
                            f"Surface '{surf.id}' references undefined material '{matl_id}'",
                            "SURF", "matl_id"
                        ))

        return issues

    def _check_ramp_refs(self) -> list[Issue]:
        """Check RAMP references from SURF and MATL."""
        issues = []

        # Check SURF ramp references
        for surf in self._registry.surfaces.list_items():
            for attr in self.SURF_RAMP_ATTRIBUTES:
                ramp_id = getattr(surf, attr, None)
                if ramp_id and ramp_id not in self._ramp_ids:
                    issues.append(Issue(
                        Severity.ERROR,
                        f"Surface '{surf.id}' references undefined ramp '{ramp_id}'",
                        "SURF", attr
                    ))

        # Check MATL ramp references
        for matl in self._registry.materials.list_items():
            for attr in self.MATL_RAMP_ATTRIBUTES:
                ramp_id = getattr(matl, attr, None)
                if ramp_id and ramp_id not in self._ramp_ids:
                    issues.append(Issue(
                        Severity.ERROR,
                        f"Material '{matl.id}' references undefined ramp '{ramp_id}'",
                        "MATL", attr
                    ))

        return issues

    def _check_material_species_refs(self) -> list[Issue]:
        """Check MATL→SPEC references."""
        issues = []

        for matl in self._registry.materials.list_items():
            if matl.spec_id:
                for spec_id in flatten_to_list(matl.spec_id):
                    if spec_id and spec_id not in self._species_ids:
                        issues.append(Issue(
                            Severity.WARNING,
                            f"Material '{matl.id}' references undefined species '{spec_id}'",
                            "MATL", "spec_id"
                        ))

        return issues

    def _check_burn_away_config(self) -> list[Issue]:
        """Check BURN_AWAY surface and obstruction configuration."""
        issues = []

        # Find surfaces with BURN_AWAY=True
        burn_away_surfaces = {
            surf.id for surf in self._registry.surfaces.list_items()
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
                    issues.append(Issue(
                        Severity.WARNING,
                        f"Obstruction '{obst_id}' uses BURN_AWAY surface but has no BULK_DENSITY",
                        "OBST", "bulk_density"
                    ))

        return issues

    def _check_ht3d_config(self) -> list[Issue]:
        """Check HT3D (3D heat transfer) configuration."""
        issues = []

        # Find surfaces with HT3D=True
        ht3d_surfaces = {
            surf.id for surf in self._registry.surfaces.list_items()
            if getattr(surf, "ht3d", False)
        }

        # Check that obstructions using HT3D surfaces have required parameters
        for obst in self._registry.obstructions.list_items():
            if getattr(obst, "ht3d", False):
                # HT3D obstructions need MATL_ID
                if not obst.matl_id:
                    obst_id = obst.id or "unnamed"
                    issues.append(Issue(
                        Severity.ERROR,
                        f"Obstruction '{obst_id}' has HT3D=True but no MATL_ID",
                        "OBST", "matl_id"
                    ))

        return issues


__all__ = ["CrossReferenceValidator"]
```

#### Step 7: Move execution validation to `validation/execution.py`

```python
"""Execution configuration validation.

Validates MPI and OpenMP parallel execution settings.
"""

import os
from typing import TYPE_CHECKING

from pyfds.core.enums import Severity
from pyfds.validation.base import Issue

if TYPE_CHECKING:
    from pyfds.core.simulation import Simulation


class ExecutionValidator:
    """Validates parallel execution configuration.

    Checks MPI process count, OpenMP thread count, and
    provides recommendations for optimal performance.
    """

    def __init__(self) -> None:
        self._cpu_count = os.cpu_count() or 1

    def validate(
        self,
        simulation: "Simulation",
        n_mpi: int = 1,
        n_threads: int = 1,
    ) -> list[Issue]:
        """Validate parallel configuration.

        Parameters
        ----------
        simulation : Simulation
            Simulation to validate
        n_mpi : int
            Number of MPI processes
        n_threads : int
            Number of OpenMP threads per process

        Returns
        -------
        list[Issue]
            Validation issues
        """
        issues: list[Issue] = []
        issues.extend(self._check_mpi_vs_meshes(simulation, n_mpi))
        issues.extend(self._check_thread_count(n_threads))
        issues.extend(self._check_total_parallelism(n_mpi, n_threads))
        return issues

    def _check_mpi_vs_meshes(
        self, simulation: "Simulation", n_mpi: int
    ) -> list[Issue]:
        """Check MPI process count vs mesh count."""
        issues = []

        mesh_count = len(simulation.meshes)

        if n_mpi > mesh_count:
            issues.append(Issue(
                Severity.WARNING,
                f"More MPI processes ({n_mpi}) than meshes ({mesh_count}). "
                f"Extra processes will be idle.",
                "execution", "n_mpi"
            ))
        elif mesh_count > n_mpi and mesh_count % n_mpi != 0:
            issues.append(Issue(
                Severity.INFO,
                f"Mesh count ({mesh_count}) not evenly divisible by MPI processes ({n_mpi}). "
                f"Load may be unbalanced.",
                "execution", "n_mpi"
            ))

        return issues

    def _check_thread_count(self, n_threads: int) -> list[Issue]:
        """Check OpenMP thread count."""
        issues = []

        if n_threads > self._cpu_count:
            issues.append(Issue(
                Severity.WARNING,
                f"Thread count ({n_threads}) exceeds CPU count ({self._cpu_count}). "
                f"This may reduce performance due to oversubscription.",
                "execution", "n_threads"
            ))

        return issues

    def _check_total_parallelism(self, n_mpi: int, n_threads: int) -> list[Issue]:
        """Check total parallel processes."""
        issues = []

        total = n_mpi * n_threads
        if total > self._cpu_count:
            issues.append(Issue(
                Severity.WARNING,
                f"Total parallel units ({total} = {n_mpi} MPI × {n_threads} threads) "
                f"exceeds CPU count ({self._cpu_count}).",
                "execution"
            ))

        return issues

    def suggest_config(self, simulation: "Simulation") -> dict[str, int]:
        """Suggest optimal parallel configuration.

        Parameters
        ----------
        simulation : Simulation
            Simulation to analyze

        Returns
        -------
        dict
            Suggested n_mpi and n_threads values
        """
        mesh_count = len(simulation.meshes)

        if mesh_count == 0:
            return {"n_mpi": 1, "n_threads": min(4, self._cpu_count)}

        # Use one MPI process per mesh, up to CPU count
        n_mpi = min(mesh_count, self._cpu_count)

        # Use remaining CPUs for OpenMP threads
        n_threads = max(1, self._cpu_count // n_mpi)

        return {"n_mpi": n_mpi, "n_threads": n_threads}


# Backward compatibility
ParallelValidator = ExecutionValidator


__all__ = ["ExecutionValidator", "ParallelValidator"]
```

#### Step 8: Create `validation/__init__.py`

```python
"""Validation module for PyFDS.

This module provides comprehensive validation for FDS simulations,
including input sanitization, simulation-level validation, cross-reference
checking, and execution configuration validation.

Examples
--------
>>> from pyfds.validation import validate_simulation
>>> result = validate_simulation(sim)
>>> if not result.is_valid:
...     for error in result.errors:
...         print(error)
"""

from pyfds.validation.base import Issue, ValidationResult
from pyfds.validation.cross_references import CrossReferenceValidator
from pyfds.validation.execution import ExecutionValidator, ParallelValidator
from pyfds.validation.input import (
    CHID_MAX_LENGTH,
    MAX_FILE_SIZE,
    safe_read_text,
    validate_chid,
    validate_file_size,
    validate_non_negative_number,
    validate_path,
    validate_positive_number,
)
from pyfds.validation.simulation import SimulationValidator, Validator

# Re-export Severity from enums for convenience
from pyfds.core.enums import Severity


def validate_simulation(simulation) -> ValidationResult:
    """Validate a complete simulation configuration.

    This is the main entry point for simulation validation.
    Runs all validation checks including required components,
    cross-references, geometry quality, and physical bounds.

    Parameters
    ----------
    simulation : Simulation
        Simulation to validate

    Returns
    -------
    ValidationResult
        Validation result with all issues

    Examples
    --------
    >>> sim = Simulation(chid="test")
    >>> sim.add(Mesh(...), Time(...))
    >>> result = validate_simulation(sim)
    >>> if result.is_valid:
    ...     print("Simulation is valid!")
    """
    validator = SimulationValidator(simulation)
    return validator.validate()


__all__ = [
    # Main entry point
    "validate_simulation",
    # Base classes
    "Issue",
    "Severity",
    "ValidationResult",
    # Validators
    "CrossReferenceValidator",
    "ExecutionValidator",
    "ParallelValidator",
    "SimulationValidator",
    "Validator",
    # Input validation
    "CHID_MAX_LENGTH",
    "MAX_FILE_SIZE",
    "safe_read_text",
    "validate_chid",
    "validate_file_size",
    "validate_non_negative_number",
    "validate_path",
    "validate_positive_number",
]
```

#### Step 9: Update imports throughout codebase

**File: `src/pyfds/core/__init__.py`**

```python
# CHANGE imports from validator.py to validation module
from pyfds.validation import Issue, Severity, Validator
```

**File: `src/pyfds/core/simulation.py`**

```python
# CHANGE import
from pyfds.validation import Validator
```

**File: `src/pyfds/utils/__init__.py`**

```python
# CHANGE imports to use validation module
from pyfds.validation.input import (
    validate_chid,
    validate_path,
    validate_file_size,
    validate_positive_number,
    validate_non_negative_number,
    safe_read_text,
)
```

**File: `src/pyfds/execution/__init__.py`**

```python
# CHANGE import
from pyfds.validation import ParallelValidator
```

#### Step 10: Clean up old files

After migration is complete and tests pass:

```bash
# Remove old validator files
rm src/pyfds/core/validator.py
rm -rf src/pyfds/core/validators/
rm src/pyfds/utils/input_validators.py
rm src/pyfds/execution/validation.py
```

### 3.4 Migration Checklist

- [ ] Create `validation/` directory structure
- [ ] Create `validation/base.py` with `Issue` and `ValidationResult`
- [ ] Create `validation/utils.py` with shared utilities
- [ ] Create `validation/input.py` (move from `utils/input_validators.py`)
- [ ] Create `validation/simulation.py` (move from `core/validator.py`)
- [ ] Create `validation/cross_references.py` (integrate existing)
- [ ] Create `validation/execution.py` (move from `execution/validation.py`)
- [ ] Create `validation/__init__.py` with public API
- [ ] Move `Severity` enum to `core/enums.py`
- [ ] Update `core/__init__.py` imports
- [ ] Update `core/simulation.py` imports
- [ ] Update `utils/__init__.py` imports
- [ ] Update `execution/__init__.py` imports
- [ ] Update all test file imports
- [ ] Run full test suite
- [ ] Remove old files after tests pass
- [ ] Update documentation references

---

## Testing Strategy

After implementing these changes, run the full test suite:

```bash
# Run all tests
pytest tests/ -v

# Run with coverage
pytest tests/ --cov=src/pyfds --cov-report=html

# Check for import errors
python -c "from pyfds import *"
python -c "from pyfds.core.enums import *"
python -c "from pyfds.validation import *"
```

## Backward Compatibility

The refactoring maintains backward compatibility through:

1. **Re-exports**: Old import paths continue to work via re-exports
2. **Aliases**: `Validator` aliased to `SimulationValidator`, `ParallelValidator` to `ExecutionValidator`
3. **Deprecation warnings**: Consider adding deprecation warnings for old import paths

```python
# Example deprecation warning (optional)
import warnings

# In old location
def __getattr__(name):
    if name == "Severity":
        warnings.warn(
            "Importing Severity from pyfds.core.validator is deprecated. "
            "Use pyfds.core.enums.Severity instead.",
            DeprecationWarning,
            stacklevel=2
        )
        from pyfds.core.enums import Severity
        return Severity
    raise AttributeError(f"module has no attribute {name}")
```

---

## Summary

| Task | Files Changed | Complexity |
|------|---------------|------------|
| Consolidate enums | ~10 files | Medium |
| Remove phase comments | 4 files | Low |
| Consolidate validators | ~15 files | High |

**Recommended order:**
1. Remove phase/stage comments (quick win, low risk)
2. Consolidate enums (medium complexity)
3. Consolidate validators (highest impact, do last)
