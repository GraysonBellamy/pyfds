# Fire and Pyrolysis Implementation Plan

## Overview

This document outlines a comprehensive phased implementation plan to achieve full coverage of FDS User Guide Chapter 9 "Fire and Pyrolysis" features in pyfds. The plan is organized into four phases based on priority, complexity, and dependencies.

**Current Status Summary:**
| Namelist | Current Coverage | Target |
|----------|-----------------|--------|
| MATL | ~95% | 100% |
| SURF (Fire/Pyrolysis) | ~95% | 100% |
| OBST | ~55% | 100% |
| REAC | ~95% | 100% |

---

## Phase 1: Critical Gaps for Production Use
**Timeline: 2-3 weeks**
**Priority: HIGH**

These features are essential for typical fire engineering simulations and address the most impactful gaps.

### 1.1 OBST Burn-Away Enhancement

**Goal:** Enable proper modeling of combustible obstructions that can burn away.

#### Parameters to Add to `Obstruction` class:

| Parameter | Type | Description | Default |
|-----------|------|-------------|---------|
| `bulk_density` | `float \| None` | Combustible mass per unit volume [kg/m³] | `None` |
| `surf_id_interior` | `str \| None` | Surface properties for newly exposed surfaces | `None` |
| `burn_away` | `bool` | Allow obstruction to burn away | `False` |

#### Implementation Tasks:

```python
# File: src/pyfds/core/namelists/obst.py

class Obstruction(NamelistBase):
    # ... existing fields ...

    # Add these new fields:
    bulk_density: float | None = Field(
        None, gt=0,
        description="Combustible mass per unit volume [kg/m³]"
    )
    surf_id_interior: str | None = Field(
        None,
        description="Surface ID for newly exposed surfaces during burn-away"
    )
    burn_away: bool = Field(
        False,
        description="Allow obstruction to burn away"
    )
```

#### Validation Rules:
- [ ] If `burn_away=True`, warn if no `SURF` with `BURN_AWAY=T` is referenced
- [ ] If `bulk_density` is specified, `burn_away` should be `True`
- [ ] `surf_id_interior` should reference a valid `SURF` ID

#### Tests to Add:
- [ ] `test_obst_burn_away_basic`
- [ ] `test_obst_bulk_density`
- [ ] `test_obst_surf_id_interior`
- [ ] `test_obst_burn_away_validation`

---

### 1.2 Multi-Layer Material Specification

**Goal:** Support composite materials with multiple layers containing multiple components.

#### Parameters to Add/Modify in `Surface` class:

| Parameter | Type | Description |
|-----------|------|-------------|
| `matl_id` | `list[list[str]] \| list[str] \| str` | Material IDs per layer per component |
| `matl_mass_fraction` | `list[list[float]] \| None` | Mass fractions per layer per component |
| `thickness` | `list[float] \| float` | Thickness per layer [m] |

#### Implementation Tasks:

```python
# File: src/pyfds/core/namelists/surf.py

class Surface(NamelistBase):
    # Modify existing matl_id to support 2D structure
    matl_id: list[list[str]] | list[str] | str | None = Field(
        None,
        description="Material ID(s): single, list per layer, or 2D array"
    )
    matl_mass_fraction: list[list[float]] | None = Field(
        None,
        description="Mass fractions for multi-component layers"
    )
    thickness: list[float] | float | None = Field(
        None, gt=0,
        description="Layer thickness(es) [m]"
    )
```

#### FDS Output Format:
```fortran
! Single layer, single material
&SURF ID='SIMPLE', MATL_ID='CONCRETE', THICKNESS=0.2 /

! Multi-layer, single material per layer
&SURF ID='WALL', MATL_ID='GYPSUM','INSULATION','GYPSUM',
      THICKNESS=0.013,0.1,0.013 /

! Multi-layer with mixed components
&SURF ID='WET_WALL',
      MATL_ID(1,1:2)='BRICK','WATER',
      MATL_MASS_FRACTION(1,1:2)=0.95,0.05,
      MATL_ID(2,1)='INSULATOR',
      THICKNESS=0.1,0.05 /
```

#### Validation Rules:
- [ ] Number of `thickness` values must match number of layers
- [ ] `matl_mass_fraction` values per layer must sum to 1.0
- [ ] All referenced `MATL_ID` must exist

#### Tests to Add:
- [ ] `test_surf_single_layer_single_material`
- [ ] `test_surf_multi_layer_single_material`
- [ ] `test_surf_multi_layer_multi_component`
- [ ] `test_surf_matl_mass_fraction_validation`

---

### 1.3 Multi-Reaction Product Specification (2D Arrays)

**Goal:** Support full FDS syntax for reaction products with multiple species/materials per reaction.

#### Current Limitation:
The current implementation uses 1D arrays for `NU_SPEC` and `NU_MATL`. FDS supports 2D indexing: `SPEC_ID(i,j)`, `NU_SPEC(i,j)` where `i` = product index, `j` = reaction index.

#### Parameters to Modify in `Material` class:

```python
# File: src/pyfds/core/namelists/matl.py

class Material(NamelistBase):
    # Change from 1D to support 2D structure
    spec_id: list[list[str]] | list[str] | str | None = Field(
        None,
        description="Gas species ID(s) per reaction"
    )
    nu_spec: list[list[float]] | list[float] | None = Field(
        None,
        description="Species yields: NU_SPEC(species_idx, reaction_idx)"
    )
    matl_id_products: list[list[str]] | list[str] | None = Field(
        None,
        alias="matl_id",  # Handle naming conflict with reactant MATL_ID
        description="Residue material ID(s) per reaction"
    )
    nu_matl: list[list[float]] | list[float] | None = Field(
        None,
        description="Residue yields: NU_MATL(material_idx, reaction_idx)"
    )
    heat_of_combustion_array: list[list[float]] | list[float] | None = Field(
        None,
        description="Heat of combustion per species per reaction [kJ/kg]"
    )
```

#### FDS Output Format:
```fortran
&MATL ID='WOOD',
      N_REACTIONS=1,
      SPEC_ID(1:4,1)='OXYGEN','WATER VAPOR','CARBON DIOXIDE','PYROLYZATE',
      NU_SPEC(1:4,1)=0,0,0,0.82,
      MATL_ID(1,1)='CHAR',
      NU_MATL(1,1)=0.18,
      HEAT_OF_COMBUSTION(4,1)=14500 /
```

#### Validation Rules:
- [ ] Sum of yields per reaction should be ≤ 1.0
- [ ] If yields sum < 1.0, issue warning about mass disappearance
- [ ] All `SPEC_ID` values must reference valid species
- [ ] All residue `MATL_ID` values must reference valid materials

---

### 1.4 Cross-Reference Validation

**Goal:** Ensure all ID references are valid across namelists.

#### Implementation Tasks:

```python
# File: src/pyfds/core/validator.py

class Validator:
    def _validate_material_references(self, simulation: "Simulation") -> None:
        """Validate all material ID references."""
        material_ids = {m.id for m in simulation.material_mgr.materials}
        species_ids = {s.id for s in simulation.physics.species}

        for matl in simulation.material_mgr.materials:
            # Check SPEC_ID references
            if matl.spec_id:
                spec_ids = self._flatten_to_list(matl.spec_id)
                for spec_id in spec_ids:
                    if spec_id and spec_id not in species_ids:
                        self.errors.append(
                            f"Material '{matl.id}': SPEC_ID '{spec_id}' not defined"
                        )

            # Check residue MATL_ID references
            if matl.nu_matl:
                matl_ids = self._flatten_to_list(matl.nu_matl)
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
                            f"Material '{matl.id}' reaction {j+1}: "
                            f"Total yield {total_yield:.2f} > 1.0"
                        )
                    elif total_yield < 0.99:
                        self.warnings.append(ValidationWarning(
                            f"Material '{matl.id}' reaction {j+1}: "
                            f"Total yield {total_yield:.2f} < 1.0 (mass loss)",
                            severity="warning"
                        ))
```

---

## Phase 2: Advanced Pyrolysis Features ✅ COMPLETED
**Timeline: 2-3 weeks**
**Priority: MEDIUM-HIGH**
**Status: IMPLEMENTED** - All four components successfully added with comprehensive tests and validation.

### 2.1 Particle Products from Pyrolysis

**Goal:** Enable solid-phase reactions to produce Lagrangian particles.

#### Parameters to Add to `Material` class:

| Parameter | Type | Description |
|-----------|------|-------------|
| `part_id` | `list[list[str]] \| list[str] \| None` | Particle class IDs per reaction |
| `nu_part` | `list[list[float]] \| list[float] \| None` | Particle yields per reaction |

#### Implementation:

```python
# File: src/pyfds/core/namelists/matl.py

class Material(NamelistBase):
    # ... existing fields ...

    # Particle products
    part_id: list[list[str]] | list[str] | None = Field(
        None,
        description="Particle class ID(s) produced by reactions"
    )
    nu_part: list[list[float]] | list[float] | None = Field(
        None,
        description="Particle yields: NU_PART(particle_idx, reaction_idx)"
    )
```

#### Validation:
- [ ] All `PART_ID` values must reference valid particle classes
- [ ] Particle yields should be reasonable (typically small fractions)

---

### 2.2 Delamination Model

**Goal:** Support layer fall-off for composite materials (CLT, laminated panels).

#### Parameters to Add to `Surface` class:

| Parameter | Type | Description |
|-----------|------|-------------|
| `delamination_tmp` | `list[float] \| None` | Temperature threshold per layer [°C] |
| `delamination_density` | `list[float] \| None` | Density threshold per layer [kg/m³] |

#### Implementation:

```python
# File: src/pyfds/core/namelists/surf.py

class Surface(NamelistBase):
    # ... existing fields ...

    # Delamination parameters
    delamination_tmp: list[float] | None = Field(
        None,
        description="Temperature threshold for layer delamination [°C]"
    )
    delamination_density: list[float] | None = Field(
        None,
        description="Density threshold for layer delamination [kg/m³]"
    )
```

#### Validation:
- [ ] Array length must match number of layers
- [ ] Temperature values should be reasonable (typically 100-1000°C)
- [ ] Density values should be less than initial material density

#### Builder Enhancement:

```python
# File: src/pyfds/builders/surf.py

class SurfBuilder:
    def with_delamination(
        self,
        layer: int,
        temperature: float | None = None,
        density: float | None = None
    ) -> "SurfBuilder":
        """
        Add delamination criterion for a layer.

        Parameters
        ----------
        layer : int
            Layer index (1-based)
        temperature : float, optional
            Temperature threshold [°C]
        density : float, optional
            Density threshold [kg/m³]
        """
        if temperature is not None:
            if "delamination_tmp" not in self._params:
                self._params["delamination_tmp"] = []
            # Ensure list is long enough
            while len(self._params["delamination_tmp"]) < layer:
                self._params["delamination_tmp"].append(None)
            self._params["delamination_tmp"][layer-1] = temperature

        if density is not None:
            if "delamination_density" not in self._params:
                self._params["delamination_density"] = []
            while len(self._params["delamination_density"]) < layer:
                self._params["delamination_density"].append(None)
            self._params["delamination_density"][layer-1] = density

        return self
```

---

### 2.3 Cone Calorimeter Simulation Support

**Goal:** Complete support for bench-scale test simulation.

#### Parameters to Add to `Surface` class:

| Parameter | Type | Description | Default |
|-----------|------|-------------|---------|
| `tmp_gas_front` | `float \| None` | Gas temperature for convection [°C] | `None` |
| `heat_transfer_coefficient` | `float \| None` | Fixed HTC [W/(m²·K)] | `None` |
| `ramp_ef` | `str \| None` | RAMP ID for external flux | `None` |
| `tau_ef` | `float \| None` | Tau for external flux [s] | `None` |

#### Implementation:

```python
# File: src/pyfds/core/namelists/surf.py

class Surface(NamelistBase):
    # ... existing fields ...

    # Cone calorimeter / bench-scale parameters
    tmp_gas_front: float | None = Field(
        None,
        description="Gas temperature for convective heat transfer [°C]"
    )
    heat_transfer_coefficient: float | None = Field(
        None, gt=0,
        description="Fixed heat transfer coefficient [W/(m²·K)]"
    )
    ramp_ef: str | None = Field(
        None,
        description="RAMP ID for external flux time history"
    )
    tau_ef: float | None = Field(
        None, gt=0,
        description="Time constant for external flux ramp-up [s]"
    )
```

#### Builder Helper:

```python
# File: src/pyfds/builders/surf.py

class SurfBuilder:
    def for_cone_calorimeter(
        self,
        heat_flux: float,
        gas_temp: float = 20.0,
        htc: float = 10.0
    ) -> "SurfBuilder":
        """
        Configure surface for cone calorimeter simulation.

        Parameters
        ----------
        heat_flux : float
            External radiant heat flux [kW/m²]
        gas_temp : float
            Ambient gas temperature [°C]
        htc : float
            Heat transfer coefficient [W/(m²·K)]
        """
        self._params["external_flux"] = heat_flux
        self._params["tmp_gas_front"] = gas_temp
        self._params["heat_transfer_coefficient"] = htc
        return self
```

---

### 2.4 Enhanced Material Validation

**Goal:** Improve validation ranges and add physical reasonableness checks.

#### Updated Validation Ranges:

```python
# File: src/pyfds/core/namelists/matl.py

@model_validator(mode="after")
def validate_material(self) -> "Material":
    """Validate material properties."""

    # Updated density range (allow low-density foams)
    if not (0.1 <= self.density <= 25000.0):
        raise ValueError(
            f"Material '{self.id}': DENSITY = {self.density} "
            f"is outside valid range [0.1, 25000.0] kg/m³"
        )

    # Updated conductivity range
    if self.conductivity is not None:
        if not (0.001 <= self.conductivity <= 2000.0):
            raise ValueError(
                f"Material '{self.id}': CONDUCTIVITY = {self.conductivity} "
                f"is outside valid range [0.001, 2000.0] W/(m·K)"
            )

    # Updated specific heat range
    if self.specific_heat is not None:
        if not (0.05 <= self.specific_heat <= 50.0):
            raise ValueError(
                f"Material '{self.id}': SPECIFIC_HEAT = {self.specific_heat} "
                f"is outside valid range [0.05, 50.0] kJ/(kg·K)"
            )

    # Activation energy physical reasonableness
    if self.e is not None:
        for i, e_val in enumerate(self.e):
            if e_val < 10000 or e_val > 500000:
                # Warning, not error - user may have specific values
                pass  # Log warning

    # Pre-exponential factor reasonableness
    if self.a is not None:
        for i, a_val in enumerate(self.a):
            if a_val < 1e3 or a_val > 1e20:
                # Warning for unusual values
                pass  # Log warning

    return self
```

---

## Phase 3: Combustion Chemistry Enhancement
**Timeline: 2-3 weeks**
**Priority: MEDIUM**
**Status: IMPLEMENTED** - All three components successfully implemented with comprehensive tests and validation.

### 3.1 REAC Parameter Expansion

**Goal:** Add missing combustion parameters for advanced chemistry modeling.

#### Parameters to Add to `Reaction` class:

| Parameter | Type | Description | Default |
|-----------|------|-------------|---------|
| `id` | `str \| None` | Reaction identifier | `None` |
| `hcn_yield` | `float` | HCN yield [kg/kg fuel] | `0.0` |
| `epumo2` | `float \| None` | Energy per unit mass O2 [kJ/kg] | `None` |
| `n_simple_chemistry_reactions` | `int` | Number of simple chemistry steps | `1` |
| `hoc_complete` | `float \| None` | Complete heat of combustion [kJ/kg] | `None` |
| `fuel_c_to_co_fraction` | `float` | Carbon to CO fraction | `0.0` |
| `fuel_n_to_hcn_fraction` | `float` | Nitrogen to HCN fraction | `0.0` |
| `fuel_h_to_h2_fraction` | `float` | Hydrogen to H2 fraction | `0.0` |
| `check_atom_balance` | `bool` | Check atom balance | `True` |
| `lower_oxygen_limit` | `float` | Lower oxygen index | `0.0` |

#### Implementation:

```python
# File: src/pyfds/core/namelists/reac.py

class Reaction(NamelistBase):
    # ... existing fields ...

    # Reaction identification
    id: str | None = Field(None, description="Reaction identifier")

    # Product yields
    hcn_yield: float = Field(0.0, ge=0, le=1, description="HCN yield [kg/kg fuel]")

    # Energy parameters
    epumo2: float | None = Field(
        None, gt=0,
        description="Energy per unit mass of O2 consumed [kJ/kg]"
    )
    hoc_complete: float | None = Field(
        None, gt=0,
        description="Complete heat of combustion [kJ/kg]"
    )

    # Two-step chemistry
    n_simple_chemistry_reactions: int = Field(
        1, ge=1, le=2,
        description="Number of simple chemistry reactions (1 or 2)"
    )

    # Product fractions for incomplete combustion
    fuel_c_to_co_fraction: float = Field(
        0.0, ge=0, le=1,
        description="Fraction of fuel carbon converted to CO"
    )
    fuel_n_to_hcn_fraction: float = Field(
        0.0, ge=0, le=1,
        description="Fraction of fuel nitrogen converted to HCN"
    )
    fuel_h_to_h2_fraction: float = Field(
        0.0, ge=0, le=1,
        description="Fraction of fuel hydrogen converted to H2"
    )

    # Validation options
    check_atom_balance: bool = Field(
        True,
        description="Check atom balance in reaction"
    )
    reac_atom_error: float = Field(
        1e-4, gt=0,
        description="Atom balance error tolerance"
    )
    reac_mass_error: float = Field(
        1e-4, gt=0,
        description="Mass balance error tolerance"
    )

    # Oxygen limit
    lower_oxygen_limit: float = Field(
        0.0, ge=0, le=1,
        description="Lower oxygen index for extinction"
    )
```

---

### 3.2 Auto-Ignition Exclusion Zones

**Goal:** Support spatial control of auto-ignition.

#### Parameters to Add:

| Parameter | Type | Description |
|-----------|------|-------------|
| `ait_exclusion_zone` | `tuple[float, ...]` | XB bounds for exclusion zone |
| `ait_exclusion_zone_temperature` | `float` | Temperature above which ignition is allowed |
| `ait_exclusion_zone_devc_id` | `str` | Device to control exclusion |
| `ait_exclusion_zone_ctrl_id` | `str` | Control logic for exclusion |

---

### 3.3 Reaction Builder Enhancement

```python
# File: src/pyfds/builders/reaction.py

class ReactionBuilder(Builder[Reaction]):
    """Builder for combustion reactions."""

    def __init__(self, fuel: str | None = None):
        super().__init__()
        self._fuel = fuel
        self._params: dict = {}

    def from_fuel(self, fuel_name: str) -> "ReactionBuilder":
        """Create reaction from predefined fuel database."""
        from .libraries.fuels import get_fuel_info
        info = get_fuel_info(fuel_name)
        self._fuel = fuel_name
        self._params.update({
            "c": info.get("c"),
            "h": info.get("h"),
            "o": info.get("o", 0),
            "n": info.get("n", 0),
            "heat_of_combustion": info.get("hoc"),
            "soot_yield": info.get("soot_yield", 0.01),
            "co_yield": info.get("co_yield", 0.0),
        })
        return self

    def with_two_step_chemistry(
        self,
        co_fraction: float = 0.1,
        hcn_fraction: float = 0.0
    ) -> "ReactionBuilder":
        """Enable two-step chemistry model."""
        self._params["n_simple_chemistry_reactions"] = 2
        self._params["fuel_c_to_co_fraction"] = co_fraction
        if hcn_fraction > 0:
            self._params["fuel_n_to_hcn_fraction"] = hcn_fraction
        return self

    def with_extinction(
        self,
        model: str = "EXTINCTION_2",
        critical_temp: float = 1427.0,
        lower_o2: float = 0.0
    ) -> "ReactionBuilder":
        """Configure extinction model."""
        self._params["extinction_model"] = model
        self._params["critical_flame_temperature"] = critical_temp
        if lower_o2 > 0:
            self._params["lower_oxygen_limit"] = lower_o2
        return self

    def build(self) -> Reaction:
        """Build the Reaction object."""
        self._check_built()

        if self._fuel:
            self._params["fuel"] = self._fuel

        reaction = Reaction(**self._params)
        self._mark_built()
        return reaction
```

---

## Phase 4: Testing, Documentation & Polish
**Timeline: 1-2 weeks**
**Priority: HIGH (after features)**

### 4.1 Comprehensive Test Suite

#### Unit Tests:

```python
# File: tests/unit/core/namelists/test_matl_advanced.py

class TestMaterialMultiReaction:
    """Tests for multi-reaction pyrolysis."""

    def test_multi_reaction_2d_arrays(self):
        """Test 2D array syntax for products."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            n_reactions=2,
            a=[1e10, 5e8],
            e=[100000, 120000],
            heat_of_reaction=[1800, 500],
            spec_id=[["VAPOR"], ["CO2", "H2O"]],
            nu_spec=[[0.82], [0.1, 0.08]],
            matl_id_products=[["CHAR"], []],
            nu_matl=[[0.18], []]
        )
        fds = mat.to_fds()
        assert "SPEC_ID(1,1)='VAPOR'" in fds
        assert "NU_SPEC(1,1)=0.82" in fds

    def test_yield_sum_validation(self):
        """Test that yields are validated."""
        with pytest.raises(ValidationError, match="yield"):
            Material(
                id="BAD",
                density=500.0,
                conductivity=0.13,
                specific_heat=2.5,
                n_reactions=1,
                nu_spec=[[0.6]],
                nu_matl=[[0.6]]  # Sum = 1.2 > 1.0
            )


class TestMaterialLiquidFuel:
    """Tests for liquid fuel evaporation model."""

    def test_liquid_fuel_basic(self):
        """Test basic liquid fuel definition."""
        ethanol = Material(
            id="ETHANOL_LIQUID",
            density=794,
            conductivity=0.17,
            specific_heat=2.44,
            boiling_temperature=78.5,
            spec_id="ETHANOL",
            heat_of_reaction=837,
            absorption_coefficient=1140
        )
        assert ethanol.boiling_temperature == 78.5
        fds = ethanol.to_fds()
        assert "BOILING_TEMPERATURE=78.5" in fds


class TestMaterialDelamination:
    """Tests for delamination parameters."""

    def test_delamination_temperature(self):
        """Test temperature-based delamination."""
        surf = Surface(
            id="CLT_PANEL",
            matl_id=["WOOD", "GLUE", "WOOD"],
            thickness=[0.02, 0.001, 0.02],
            delamination_tmp=[None, 200.0, None]
        )
        fds = surf.to_fds()
        assert "DELAMINATION_TMP(2)=200" in fds
```

#### Integration Tests:

```python
# File: tests/integration/test_pyrolysis_scenarios.py

class TestPyrolysisScenarios:
    """Integration tests for pyrolysis modeling."""

    def test_charring_wood_simulation(self):
        """Test complete charring wood setup."""
        sim = Simulation(chid="char_wood", title="Charring Wood")

        # Define char residue
        char = MaterialBuilder("CHAR") \
            .density(150) \
            .thermal_conductivity(0.1) \
            .specific_heat(1.0) \
            .build()

        # Define wood with charring reaction
        wood = MaterialBuilder("WOOD") \
            .density(500) \
            .thermal_conductivity(0.13) \
            .specific_heat(2.5) \
            .add_pyrolysis_reaction(
                a=1e10, e=100000,
                heat_of_reaction=1800,
                product_species="WOOD_GAS",
                residue_material="CHAR",
                species_yield=0.75,
                residue_yield=0.25
            ) \
            .build()

        sim.add_material(char)
        sim.add_material(wood)

        # Validate cross-references
        warnings = sim.validate()
        assert not any("CHAR" in w for w in warnings)

    def test_liquid_pool_fire(self):
        """Test liquid pool fire setup."""
        sim = Simulation(chid="pool_fire", title="Methanol Pool")

        methanol = MaterialBuilder("METHANOL_LIQUID") \
            .density(792) \
            .thermal_conductivity(0.2) \
            .specific_heat(2.51) \
            .as_liquid(
                boiling_temp=64.7,
                heat_of_vaporization=1100,
                absorption_coeff=140
            ) \
            .build()

        # ... rest of simulation setup
```

---

### 4.2 Documentation Updates

#### User Guide Sections to Add:

1. **Pyrolysis Modeling Guide** (`docs/guide/pyrolysis.md`)
   - Simple pyrolysis (single reaction)
   - Multi-reaction kinetics
   - Charring vs non-charring materials
   - Kinetic parameter estimation

2. **Liquid Fuel Guide** (`docs/guide/liquid_fuels.md`)
   - Pool fire modeling
   - Liquid mixtures
   - Evaporation physics

3. **Burn-Away Guide** (`docs/guide/burn_away.md`)
   - Obstruction burn-away
   - BULK_DENSITY usage
   - Interior surface exposure

4. **Advanced Chemistry Guide** (`docs/guide/combustion_chemistry.md`)
   - Two-step chemistry
   - Product yields
   - Extinction models

#### API Reference Updates:

- [ ] Update MATL reference with all new parameters
- [ ] Update SURF reference with multi-layer syntax
- [ ] Update OBST reference with burn-away parameters
- [ ] Update REAC reference with chemistry parameters

---

### 4.3 Example Files

```python
# File: examples/pyrolysis/01_simple_pyrolysis.py
"""
Simple Pyrolysis Example

Demonstrates a single-reaction pyrolysis material.
"""

# File: examples/pyrolysis/02_charring_material.py
"""
Charring Material Example

Demonstrates wood pyrolysis with char formation.
"""

# File: examples/pyrolysis/03_multi_layer_composite.py
"""
Multi-Layer Composite Example

Demonstrates a wall with multiple material layers.
"""

# File: examples/pyrolysis/04_liquid_pool_fire.py
"""
Liquid Pool Fire Example

Demonstrates liquid fuel evaporation and combustion.
"""

# File: examples/pyrolysis/05_cone_calorimeter.py
"""
Cone Calorimeter Simulation

Demonstrates bench-scale test setup using SOLID_PHASE_ONLY.
"""

# File: examples/pyrolysis/06_burn_away_furniture.py
"""
Burn-Away Furniture Example

Demonstrates combustible furniture with BULK_DENSITY.
"""
```

---

## Implementation Checklist

### Phase 1 (Critical)
- [ ] OBST: `bulk_density` parameter
- [ ] OBST: `surf_id_interior` parameter
- [ ] OBST: `burn_away` flag
- [ ] SURF: Multi-layer `matl_id` (2D array)
- [ ] SURF: `matl_mass_fraction` parameter
- [ ] MATL: 2D array `spec_id` / `nu_spec`
- [ ] MATL: 2D array `matl_id` / `nu_matl` for products
- [ ] Cross-reference validation for MATL_ID
- [ ] Cross-reference validation for SPEC_ID
- [ ] Yield sum validation

### Phase 2 (Advanced)
- [ ] MATL: `part_id` / `nu_part` for particles
- [ ] SURF: `delamination_tmp`
- [ ] SURF: `delamination_density`
- [ ] SURF: `tmp_gas_front`
- [ ] SURF: `heat_transfer_coefficient`
- [ ] SURF: `ramp_ef` / `tau_ef`
- [ ] SurfBuilder: `with_delamination()` method
- [ ] SurfBuilder: `for_cone_calorimeter()` method
- [ ] Updated validation ranges

### Phase 3 (Chemistry)
- [ ] REAC: `id` parameter
- [ ] REAC: `hcn_yield`
- [ ] REAC: `epumo2`
- [ ] REAC: `n_simple_chemistry_reactions`
- [ ] REAC: `hoc_complete`
- [ ] REAC: Carbon/nitrogen/hydrogen fractions
- [ ] REAC: `check_atom_balance`
- [ ] REAC: `lower_oxygen_limit`
- [ ] REAC: AIT exclusion zone parameters
- [ ] ReactionBuilder class

### Phase 4 (Polish)
- [ ] Unit tests for all new parameters
- [ ] Integration tests for scenarios
- [ ] Pyrolysis modeling guide
- [ ] Liquid fuel guide
- [ ] Burn-away guide
- [ ] Chemistry guide
- [ ] Example files
- [ ] API reference updates

---

## Dependencies & Prerequisites

### External Dependencies
- No new external dependencies required

### Internal Dependencies
- Phase 2 depends on Phase 1 completion (multi-layer support)
- Phase 3 can proceed in parallel with Phase 2
- Phase 4 requires all features from Phases 1-3

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| 2D array FDS syntax complexity | Medium | High | Extensive testing with FDS validation |
| Breaking changes to existing API | Low | High | Maintain backward compatibility |
| Validation logic complexity | Medium | Medium | Modular validation functions |
| Test coverage gaps | Low | Medium | Systematic test generation |

---

## Success Criteria

1. **100% parameter coverage** for Chapter 9 features
2. **All tests passing** with >90% code coverage
3. **Example files** for each major feature
4. **Documentation** complete and accurate
5. **FDS validation** - generated files run successfully in FDS
6. **Backward compatibility** - existing code continues to work
