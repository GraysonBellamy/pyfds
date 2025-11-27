# PyFDS Test Reorganization Implementation Plan

> **Version:** 1.0
> **Date:** November 27, 2025
> **Status:** Completed

## Executive Summary

This document outlines a comprehensive plan to reorganize the PyFDS test suite for improved maintainability, discoverability, and scalability. The primary goals are:

1. Eliminate cryptic stage/priority-based naming conventions
2. Split monolithic test files into focused, per-component files
3. Consolidate duplicate/overlapping tests
4. Align test structure with source code structure

---

## Table of Contents

1. [Current State Analysis](#1-current-state-analysis)
2. [Issues and Pain Points](#2-issues-and-pain-points)
3. [Proposed New Structure](#3-proposed-new-structure)
4. [Detailed File Mapping](#4-detailed-file-mapping)
5. [Implementation Phases](#5-implementation-phases)
6. [Risk Mitigation](#6-risk-mitigation)
7. [Rollback Plan](#7-rollback-plan)
8. [Success Criteria](#8-success-criteria)

---

## 1. Current State Analysis

### 1.1 Current Directory Structure

```
tests/
├── conftest.py                         # Shared pytest fixtures (144 lines)
├── integration/
│   ├── __init__.py
│   ├── test_complete_workflows.py      # 378 lines (renamed from test_workflow.py)
│   ├── test_execution_integration.py   # Integration tests
│   ├── test_fire_scenarios.py          # 441 lines (renamed from test_stage1_features.py)
│   └── test_suppression_scenarios.py   # 505 lines (renamed from test_stage2_particle_systems.py)
├── unit/
│   ├── __init__.py
│   ├── analysis/
│   │   ├── __init__.py
│   │   └── test_results.py              # Moved from unit/
│   ├── builders/
│   │   ├── __init__.py
│   │   ├── test_control_builder.py     # 125 lines
│   │   ├── test_material_builder.py    # 211 lines
│   │   ├── test_part_builder.py        # 168 lines
│   │   ├── test_prop_builder.py        # 242 lines
│   │   ├── test_ramp_builder.py        # 148 lines
│   │   ├── test_reaction_builder.py    # 185 lines
│   │   ├── test_surf_builder.py        # 464 lines
│   │   └── test_vent_builder.py        # 122 lines
│   ├── core/
│   │   ├── __init__.py
│   │   ├── managers/
│   │   │   ├── __init__.py
│   │   │   ├── test_control_manager.py # 124 lines
│   │   │   ├── test_geometry_manager.py # 73 lines
│   │   │   ├── test_instrumentation_manager.py # 120 lines
│   │   │   ├── test_material_manager.py # 48 lines
│   │   │   ├── test_output_manager.py # 226 lines
│   │   │   ├── test_physics_manager.py # 110 lines
│   │   │   └── test_ramp_manager.py # 80 lines
│   │   ├── namelists/
│   │   │   ├── __init__.py
│   │   │   ├── test_ctrl.py            # From test_namelist.py
│   │   │   ├── test_devc.py            # From test_namelist.py
│   │   │   ├── test_factory.py         # From test_namelist_factory.py
│   │   │   ├── test_head.py            # From test_namelist.py
│   │   │   ├── test_init.py            # From test_namelist.py
│   │   │   ├── test_matl.py            # From test_namelist.py
│   │   │   ├── test_mesh.py            # From test_namelist.py
│   │   │   ├── test_misc.py            # From test_namelist.py
│   │   │   ├── test_obst.py            # From test_namelist.py
│   │   │   ├── test_prop.py            # From test_namelist.py
│   │   │   ├── test_ramp.py            # From test_namelist.py
│   │   │   ├── test_reac.py            # From test_namelist.py
│   │   │   ├── test_spec.py            # From test_spec.py
│   │   │   ├── test_surf.py            # CONSOLIDATED (75 tests)
│   │   │   ├── test_time.py            # From test_namelist.py
│   │   │   └── test_vent.py            # From test_namelist.py
│   │   ├── test_simulation.py          # Core simulation tests
│   │   └── test_validator.py           # Merged validator tests (39 tests)
│   ├── execution/
│   │   ├── __init__.py
│   │   ├── test_exceptions.py          # Moved from unit/
│   │   └── test_process.py             # Renamed from test_execution.py
│   ├── io/
│   │   ├── __init__.py
│   │   └── test_csv_parser.py          # Moved from unit/
│   └── utils/
│       ├── __init__.py
│       └── test_logging.py             # Moved from unit/
```

### 1.2 Source Code Structure (for reference)

```
src/pyfds/
├── __init__.py
├── core/
│   ├── __init__.py
│   ├── geometry.py
│   ├── simulation.py
│   ├── validator.py
│   ├── managers/
│   │   ├── __init__.py
│   │   ├── base.py
│   │   ├── control.py
│   │   ├── geometry.py
│   │   ├── instrumentation.py
│   │   ├── material.py
│   │   ├── output.py
│   │   ├── physics.py
│   │   └── ramp.py
│   └── namelists/
│       ├── __init__.py
│       ├── base.py
│       ├── ctrl.py
│       ├── devc.py
│       ├── factory.py
│       ├── head.py
│       ├── init.py
│       ├── matl.py
│       ├── mesh.py
│       ├── misc.py
│       ├── obst.py
│       ├── part.py
│       ├── prop.py
│       ├── ramp.py
│       ├── reac.py
│       ├── spec.py
│       ├── surf.py
│       ├── time.py
│       └── vent.py
├── builders/
│   ├── __init__.py
│   ├── base.py
│   ├── control.py
│   ├── devc.py
│   ├── material.py
│   ├── mesh.py
│   ├── part.py
│   ├── prop.py
│   ├── ramp.py
│   ├── reaction.py
│   ├── surf.py
│   └── vent.py
├── execution/
│   ├── __init__.py
│   ├── exceptions.py
│   ├── monitor.py
│   ├── process.py
│   └── runner.py
├── io/
│   └── parsers/
│       └── csv_parser.py
├── analysis/
│   ├── __init__.py
│   └── results.py
└── utils/
    ├── __init__.py
    ├── logging.py
    └── validation.py
```

---

## 2. Issues and Pain Points

### 2.1 Critical Issues

| Issue | Files Affected | Impact |
|-------|----------------|--------|
| **Monolithic test file** | `test_namelist.py` (679 lines) | Hard to navigate, tests 14+ classes in one file |
| **Cryptic naming** | `test_surf_enhancements.py`, `test_surf_priority1.py` | Names tied to development phases, not functionality |
| **Duplicate SURF tests** | 3 files testing SURF | Overlapping coverage, maintenance burden |
| **Stage-based naming** | `test_fire_scenarios.py`, `test_suppression_scenarios.py` | Clear naming convention |
| **Misplaced builders** | `test_builders/` at top level | Should be under `unit/` |

### 2.2 `test_namelist.py` Content Breakdown

| Class | Lines | Description |
|-------|-------|-------------|
| `TestHead` | ~35 | HEAD namelist validation |
| `TestTime` | ~35 | TIME namelist validation |
| `TestMesh` | ~50 | MESH namelist with cell size calculations |
| `TestSurface` | ~40 | Basic SURF creation (duplicated elsewhere) |
| `TestObstruction` | ~35 | OBST namelist validation |
| `TestDevice` | ~40 | DEVC namelist (XYZ/XB) |
| `TestRamp` | ~50 | RAMP with interpolation |
| `TestMaterial` | ~45 | MATL namelist validation |
| `TestReaction` | ~35 | REAC namelist with yields |
| `TestProp` | ~25 | PROP namelist |
| `TestCtrl` | ~30 | CTRL namelist control logic |
| `TestInit` | ~25 | INIT namelist |
| `TestVent` | ~120 | VENT namelist (complex, many variants) |
| `TestMisc` | ~100 | MISC namelist (many parameters) |

### 2.3 SURF Test Duplication Analysis

| File | Focus | Test Classes |
|------|-------|--------------|
| `test_namelist.py::TestSurface` | Basic creation, validation | 5 tests |
| `test_surf_enhancements.py` | Heat transfer, pyrolysis, radiation, ramps | 50+ tests |
| `test_surf_priority1.py` | Fire spread, species, SPyro, TGA | 40+ tests |
| `test_builders/test_surf_builder.py` | SurfBuilder API | 40+ tests |

**Overlap:** FDS output tests appear in all three files.

---

## 3. Proposed New Structure

### 3.1 Target Directory Structure

```
tests/
├── conftest.py                              # Shared fixtures (keep)
├── fixtures/
│   └── sample_fds_files/                    # Sample FDS files for parsing
│       ├── minimal.fds
│       └── complete.fds
│
├── unit/
│   ├── __init__.py
│   │
│   ├── core/
│   │   ├── __init__.py
│   │   │
│   │   ├── namelists/                       # One file per namelist class
│   │   │   ├── __init__.py
│   │   │   ├── test_head.py                 # From test_namelist.py
│   │   │   ├── test_time.py                 # From test_namelist.py
│   │   │   ├── test_mesh.py                 # From test_namelist.py
│   │   │   ├── test_surf.py                 # CONSOLIDATED (3 files → 1)
│   │   │   ├── test_obst.py                 # From test_namelist.py
│   │   │   ├── test_devc.py                 # From test_namelist.py
│   │   │   ├── test_ramp.py                 # From test_namelist.py
│   │   │   ├── test_matl.py                 # From test_namelist.py
│   │   │   ├── test_reac.py                 # From test_namelist.py
│   │   │   ├── test_prop.py                 # From test_namelist.py
│   │   │   ├── test_ctrl.py                 # From test_namelist.py
│   │   │   ├── test_init.py                 # From test_namelist.py
│   │   │   ├── test_vent.py                 # From test_namelist.py
│   │   │   ├── test_misc.py                 # From test_namelist.py
│   │   │   ├── test_spec.py                 # From unit/test_spec.py
│   │   │   ├── test_part.py                 # NEW (if Part tests exist)
│   │   │   └── test_factory.py              # From test_namelist_factory.py
│   │   │
│   │   ├── managers/                        # Keep as-is (well organized)
│   │   │   ├── __init__.py
│   │   │   ├── test_control_manager.py
│   │   │   ├── test_geometry_manager.py
│   │   │   ├── test_instrumentation_manager.py
│   │   │   ├── test_material_manager.py
│   │   │   ├── test_output_manager.py
│   │   │   ├── test_physics_manager.py
│   │   │   └── test_ramp_manager.py
│   │   │
│   │   ├── test_simulation.py               # Keep
│   │   ├── test_geometry.py                 # NEW if needed
│   │   └── test_validator.py                # Merge validator files
│   │
│   ├── builders/                            # Move from tests/test_builders/
│   │   ├── __init__.py
│   │   ├── test_control_builder.py
│   │   ├── test_devc_builder.py             # NEW (extract from integration)
│   │   ├── test_material_builder.py
│   │   ├── test_mesh_builder.py             # NEW (extract from integration)
│   │   ├── test_part_builder.py
│   │   ├── test_prop_builder.py
│   │   ├── test_ramp_builder.py
│   │   ├── test_reaction_builder.py
│   │   ├── test_surf_builder.py
│   │   └── test_vent_builder.py
│   │
│   ├── io/
│   │   ├── __init__.py
│   │   └── test_csv_parser.py               # Move from unit/
│   │
│   ├── execution/
│   │   ├── __init__.py
│   │   ├── test_exceptions.py               # Move from unit/
│   │   └── test_process.py                  # Rename from test_execution.py
│   │
│   ├── analysis/
│   │   ├── __init__.py
│   │   └── test_results.py                  # Move from unit/
│   │
│   └── utils/
│       ├── __init__.py
│       ├── test_logging.py                  # Move from unit/
│       └── test_validation.py               # Move from unit/
│
└── integration/
    ├── __init__.py
    ├── test_execution_integration.py        # Keep
    ├── test_complete_workflows.py           # Rename from test_workflow.py
    ├── test_fire_scenarios.py               # Rename from test_stage1_features.py
    └── test_suppression_scenarios.py        # Rename from test_stage2_particle_systems.py
```

### 3.2 Naming Conventions

| Pattern | Example | When to Use |
|---------|---------|-------------|
| `test_<namelist>.py` | `test_surf.py` | Namelist tests |
| `test_<builder>_builder.py` | `test_surf_builder.py` | Builder tests |
| `test_<manager>_manager.py` | `test_geometry_manager.py` | Manager tests |
| `test_<scenario>_scenarios.py` | `test_fire_scenarios.py` | Integration tests |

---

## 4. Detailed File Mapping

### 4.1 Files to Split

#### `test_namelist.py` → 14 files

| Source Class | Target File | Approximate Lines |
|--------------|-------------|-------------------|
| `TestHead` | `unit/core/namelists/test_head.py` | 35 |
| `TestTime` | `unit/core/namelists/test_time.py` | 35 |
| `TestMesh` | `unit/core/namelists/test_mesh.py` | 50 |
| `TestSurface` | `unit/core/namelists/test_surf.py` | (merged) |
| `TestObstruction` | `unit/core/namelists/test_obst.py` | 35 |
| `TestDevice` | `unit/core/namelists/test_devc.py` | 40 |
| `TestRamp` | `unit/core/namelists/test_ramp.py` | 50 |
| `TestMaterial` | `unit/core/namelists/test_matl.py` | 45 |
| `TestReaction` | `unit/core/namelists/test_reac.py` | 35 |
| `TestProp` | `unit/core/namelists/test_prop.py` | 25 |
| `TestCtrl` | `unit/core/namelists/test_ctrl.py` | 30 |
| `TestInit` | `unit/core/namelists/test_init.py` | 25 |
| `TestVent` | `unit/core/namelists/test_vent.py` | 120 |
| `TestMisc` | `unit/core/namelists/test_misc.py` | 100 |

### 4.2 Files to Merge

#### SURF Tests → `unit/core/namelists/test_surf.py`

| Source File | Classes to Include |
|-------------|-------------------|
| `test_namelist.py` | `TestSurface` (basic creation) |
| `test_surf_enhancements.py` | All classes (heat transfer, pyrolysis, radiation, etc.) |
| `test_surf_priority1.py` | All classes (fire spread, species, SPyro, etc.) |

**New Consolidated Structure:**
```python
"""Unit tests for SURF namelist."""

class TestSurfBasicCreation:
    """Basic Surface creation and validation."""

class TestSurfHeatRelease:
    """HRRPUA and heat release parameters."""

class TestSurfMassFlux:
    """MLRPUA and mass flux parameters."""

class TestSurfPyrolysisControl:
    """Pyrolysis control parameters (ignition, burn_away, backing)."""

class TestSurfRadiationProperties:
    """Radiation properties (emissivity, absorptivity)."""

class TestSurfTimeDependentProperties:
    """Time-dependent properties (ramps, tau)."""

class TestSurfMutuallyExclusiveHeatSources:
    """Validation of mutually exclusive heat sources."""

class TestSurfFireSpread:
    """Fire spread parameters (spread_rate, xyz)."""

class TestSurfSpeciesControl:
    """Species control parameters (spec_id, mass_fraction)."""

class TestSurfThermallyThickBurning:
    """Thermally-thick burning parameters."""

class TestSurfSPyroModel:
    """SPyro (Scaling Pyrolysis) model parameters."""

class TestSurfLayerDivide:
    """Solid phase gas transport parameters."""

class TestSurfTGAAnalysis:
    """TGA analysis parameters."""

class TestSurfLiquidEvaporation:
    """Liquid evaporation parameters."""

class TestSurfParticleGeneration:
    """Particle generation parameters (from SurfBuilder tests)."""

class TestSurfFDSOutput:
    """FDS output generation tests."""

class TestSurfBackwardCompatibility:
    """Backward compatibility tests."""
```

#### Validator Files → `unit/core/test_validator.py`

| Source File | Content |
|-------------|---------|
| `test_validation.py` | Utility validation functions |
| `test_validator_file.py` | FDS file validation |

### 4.3 Files to Move

| Source | Destination | Reason |
|--------|-------------|--------|
| `test_builders/` | `unit/builders/` | Builders are unit tests |
| `test_csv_parser.py` | `unit/io/test_csv_parser.py` | Matches source structure |
| `test_exceptions.py` | `unit/execution/test_exceptions.py` | Matches source structure |
| `test_execution.py` | `unit/execution/test_process.py` | Rename + move |
| `test_results.py` | `unit/analysis/test_results.py` | Matches source structure |
| `test_logging.py` | `unit/utils/test_logging.py` | Matches source structure |
| `test_validation.py` | `unit/utils/test_validation.py` | Matches source structure |
| `test_spec.py` | `unit/core/namelists/test_spec.py` | With other namelists |

### 4.4 Files to Rename

| Old Name | New Name | Reason |
|----------|----------|--------|
| `test_stage1_features.py` | `test_fire_scenarios.py` | ✅ Completed |
| `test_stage2_particle_systems.py` | `test_suppression_scenarios.py` | ✅ Completed |
| `test_workflow.py` | `test_complete_workflows.py` | ✅ Completed |
| `test_namelist_factory.py` | `test_factory.py` | Shorter, in namelists folder |

### 4.5 Files to Delete (after migration)

- `tests/unit/test_namelist.py` (split into 14 files)
- `tests/unit/test_surf_enhancements.py` (merged into test_surf.py)
- `tests/unit/test_surf_priority1.py` (merged into test_surf.py)
- `tests/unit/test_validator_file.py` (merged into test_validator.py)
- `tests/test_builders/` (moved to unit/builders/)

---

## 5. Implementation Phases

### Phase 1: Create Directory Structure (Low Risk)

**Duration:** 5 minutes
**Risk Level:** 🟢 Low
**Status:** ✅ Completed

**Actions:**
1. Create new directories:
   ```bash
   mkdir -p tests/unit/core/namelists
   mkdir -p tests/unit/builders
   mkdir -p tests/unit/io
   mkdir -p tests/unit/execution
   mkdir -p tests/unit/analysis
   mkdir -p tests/unit/utils
   mkdir -p tests/fixtures/sample_fds_files
   ```
   ✅ Completed
2. Create `__init__.py` files in each new directory ✅ Completed

**Verification:**
```bash
pytest tests/ --collect-only  # Should still work
```

---

### Phase 2: Split `test_namelist.py` (Medium Risk)

**Duration:** 30 minutes
**Risk Level:** 🟡 Medium
**Status:** ✅ Completed

**Actions:**
1. Create individual test files for each namelist class ✅ Completed
2. Add proper imports to each file ✅ Completed
3. Run tests to verify each file works ✅ Completed
4. Delete original `test_namelist.py` ✅ Completed

**File Template:**
```python
"""Unit tests for {NAMELIST} namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import {NamelistClass}
# Additional imports as needed


class Test{NamelistClass}:
    """Tests for {NamelistClass} namelist."""

    # Copy tests from original file
```

**Verification:**
```bash
pytest tests/unit/core/namelists/ -v
pytest tests/ --collect-only | grep -c "test session"  # Count should match
```

---

### Phase 3: Consolidate SURF Tests (Medium Risk)

**Duration:** 45 minutes
**Risk Level:** 🟡 Medium
**Status:** ✅ Completed

**Actions:**
1. Create new `tests/unit/core/namelists/test_surf.py` ✅ Completed
2. Copy and organize classes from:
   - `test_namelist.py::TestSurface` ✅ Completed
   - `test_surf_enhancements.py` (all classes) ✅ Completed
   - `test_surf_priority1.py` (all classes) ✅ Completed
3. Remove duplicate tests (keep most comprehensive version) ✅ Completed
4. Organize into logical class groupings ✅ Completed
5. Delete original files ✅ Completed

**Verification:**
```bash
pytest tests/unit/core/namelists/test_surf.py -v
pytest tests/unit/ -k surf -v  # All SURF-related tests
```

---

### Phase 4: Move Builder Tests (Low Risk)

**Duration:** 10 minutes
**Risk Level:** 🟢 Low
**Status:** ✅ Completed

**Actions:**
1. Move `tests/test_builders/` to `tests/unit/builders/` ✅ Completed
2. Update any relative imports ✅ Completed

**Commands:**
```bash
mv tests/test_builders/* tests/unit/builders/
rmdir tests/test_builders
```

**Verification:**
```bash
pytest tests/unit/builders/ -v
```

---

### Phase 5: Move and Organize Remaining Unit Tests (Low Risk)

**Duration:** 15 minutes
**Risk Level:** 🟢 Low
**Status:** ✅ Completed

**Actions:**
1. Move IO tests: `test_csv_parser.py` → `unit/io/` ✅ Completed
2. Move execution tests: `test_exceptions.py`, `test_execution.py` → `unit/execution/` ✅ Completed
   - `test_execution.py` renamed to `test_process.py`
3. Move analysis tests: `test_results.py` → `unit/analysis/` ✅ Completed
4. Move utils tests: `test_logging.py`, `test_validation.py` → `unit/utils/` ✅ Completed
5. Move `test_spec.py` → `unit/core/namelists/` ✅ Completed
6. Move `test_simulation.py` → `unit/core/` ✅ Completed
7. Move managers: `managers/` → `unit/core/managers/` ✅ Completed
8. Additional moves:
   - `test_namelist_factory.py` → `unit/core/namelists/test_factory.py` ✅ Completed
9. Create missing `__init__.py` files in managers directory ✅ Completed

**Verification:**
```bash
pytest tests/unit/ -v --collect-only
```

---

### Phase 6: Merge Validator Files (Low Risk)

**Duration:** 10 minutes
**Risk Level:** 🟢 Low
**Status:** ✅ Completed

**Actions:**
1. Create `tests/unit/core/test_validator.py` ✅ Completed
2. Merge content from:
   - `test_validation.py` (utility functions) ✅ Completed
   - `test_validator_file.py` (file validation) ✅ Completed
3. Delete original files ✅ Completed

**Verification:**
```bash
pytest tests/unit/core/test_validator.py -v
```

---

### Phase 7: Rename Integration Tests (Low Risk)

**Duration:** 5 minutes
**Risk Level:** 🟢 Low

**Actions:**
```bash
mv tests/integration/test_stage1_features.py tests/integration/test_fire_scenarios.py  # ✅ Completed
mv tests/integration/test_stage2_particle_systems.py tests/integration/test_suppression_scenarios.py  # ✅ Completed
mv tests/integration/test_workflow.py tests/integration/test_complete_workflows.py  # ✅ Completed
```

**Verification:**
```bash
pytest tests/integration/ -v --collect-only
```

---

### Phase 8: Final Cleanup and Verification (Low Risk)

**Duration:** 15 minutes
**Risk Level:** 🟢 Low

**Actions:**
1. ✅ Delete empty directories
2. ✅ Run full test suite (609 tests passed)
3. ✅ Check code coverage hasn't decreased (85% maintained)
4. ✅ Update any documentation references

**Final Verification:**
```bash
pytest tests/ -v  # ✅ 609 tests passed
pytest tests/ --cov=pyfds --cov-report=term-missing  # ✅ 85% coverage maintained
```

---

## 6. Risk Mitigation

### 6.1 Pre-Implementation Checklist

- [ ] Create backup branch: `git checkout -b backup/pre-test-reorg`
- [ ] Run full test suite and record results
- [ ] Record current test count: `pytest --collect-only | tail -1`
- [ ] Record current coverage: `pytest --cov=pyfds`

### 6.2 Per-Phase Verification

After each phase:
1. Run `pytest tests/ --collect-only` to verify test discovery
2. Run `pytest tests/ -x` to verify tests pass
3. Compare test count with baseline

### 6.3 Import Considerations

When moving files, update imports carefully:
- Relative imports may break
- `conftest.py` fixtures should still be discoverable
- Check for any hardcoded paths in tests

---

## 7. Rollback Plan

### 7.1 Git-Based Rollback

If issues are discovered after implementation:

```bash
# Return to backup branch
git checkout backup/pre-test-reorg

# Or reset main to before changes
git reset --hard HEAD~{number_of_commits}
```

### 7.2 Phase-Specific Rollback

Each phase can be rolled back independently:
- Keep original files until phase is verified
- Use `git stash` for work-in-progress
- Commit after each successful phase

---

## 8. Success Criteria

### 8.1 Quantitative Metrics

| Metric | Before | After | Target |
|--------|--------|-------|--------|
| Test count | (record) | (verify) | Same or higher |
| Test pass rate | 100% | 100% | 100% |
| Coverage | (record) | (verify) | Same or higher |
| Largest test file | 679 lines | <200 lines | ✓ |
| SURF test files | 3 | 1 | ✓ |

### 8.2 Qualitative Goals

- [ ] No cryptic stage/priority naming
- [ ] Test structure mirrors source structure
- [ ] Each test file tests one component
- [ ] Easy to find tests for any source file
- [ ] Clear pattern for adding new tests

---

## Appendix A: Import Templates

### A.1 Namelist Test Template

```python
"""Unit tests for {NAME} namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import {ClassName}


class Test{ClassName}:
    """Tests for {ClassName} namelist."""

    def test_basic_creation(self):
        """Test basic {ClassName} creation."""
        pass

    def test_to_fds(self):
        """Test FDS output format."""
        pass

    def test_validation(self):
        """Test validation rules."""
        pass
```

### A.2 Builder Test Template

```python
"""Unit tests for {Name}Builder."""

import pytest

from pyfds.builders import {Name}Builder
from pyfds.core.namelists import {NamelistClass}


class Test{Name}Builder:
    """Test {Name}Builder functionality."""

    def test_simple_build(self):
        """Test building a simple {name}."""
        pass

    def test_builder_cannot_be_reused(self):
        """Test that builder cannot be used twice."""
        pass
```

---

## Appendix B: Full File List

### B.1 Files to Create (New)

1. `tests/unit/core/__init__.py`
2. `tests/unit/core/namelists/__init__.py`
3. `tests/unit/core/namelists/test_head.py`
4. `tests/unit/core/namelists/test_time.py`
5. `tests/unit/core/namelists/test_mesh.py`
6. `tests/unit/core/namelists/test_surf.py` (consolidated)
7. `tests/unit/core/namelists/test_obst.py`
8. `tests/unit/core/namelists/test_devc.py`
9. `tests/unit/core/namelists/test_ramp.py`
10. `tests/unit/core/namelists/test_matl.py`
11. `tests/unit/core/namelists/test_reac.py`
12. `tests/unit/core/namelists/test_prop.py`
13. `tests/unit/core/namelists/test_ctrl.py`
14. `tests/unit/core/namelists/test_init.py`
15. `tests/unit/core/namelists/test_vent.py`
16. `tests/unit/core/namelists/test_misc.py`
17. `tests/unit/core/namelists/test_factory.py`
18. `tests/unit/core/test_validator.py` (merged)
19. `tests/unit/builders/__init__.py`
20. `tests/unit/io/__init__.py`
21. `tests/unit/execution/__init__.py`
22. `tests/unit/analysis/__init__.py`
23. `tests/unit/utils/__init__.py`

### B.2 Files to Delete (After Migration)

1. `tests/unit/test_namelist.py`
2. `tests/unit/test_surf_enhancements.py`
3. `tests/unit/test_surf_priority1.py`
4. `tests/unit/test_validator_file.py`
5. `tests/unit/test_namelist_factory.py`
6. `tests/test_builders/` (entire directory)

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-11-27 | AI Assistant | Initial draft |
