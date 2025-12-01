# Flowfields Examples

This directory contains examples demonstrating velocity and mass flow boundary conditions,
inspired by FDS Verification Guide Chapter 6 (Flowfields).

## Examples

### 1. gas_filling.py
Demonstrates gas filling a closed room with a leak source.
- **Concepts**: MASS_FLUX_TOTAL for species injection, SPEC_ID on SURF
- **Key Features**:
  - Species definition (HYDROGEN)
  - Mass flux boundary condition with RAMP_MF
  - STRATIFICATION=.FALSE. for uniform mixing
  - Mass fraction and total mass tracking

### 2. helium_plume.py
Models a buoyant helium plume rising through ambient air.
- **Concepts**: Buoyancy-driven flow, light gas dispersion
- **Key Features**:
  - Species with molecular weight (MW=4.0 for helium)
  - ISOTHERMAL=.TRUE. for isothermal conditions
  - Open boundaries for natural entrainment
  - Vertical velocity and concentration profiles

### 3. symmetry_test.py
Tests flow solver symmetry with opposing velocity sources.
- **Concepts**: Symmetry verification, zero-gravity flow
- **Key Features**:
  - GVEC=(0,0,0) to disable gravity
  - NOISE=.FALSE. to eliminate initial perturbations
  - Symmetric geometry with opposing inlets
  - Velocity comparison at symmetric locations

### 4. velocity_bc.py
Demonstrates various velocity boundary condition types.
- **Concepts**: VEL parameter, RAMP_V for time-varying velocity
- **Key Features**:
  - Constant velocity inlet (VEL=-1.0)
  - Ramped velocity with smooth acceleration
  - Pulsed velocity with periodic on/off
  - Velocity measurements downstream of inlets

## Key PyFDS Features Demonstrated

```python
# Velocity boundary condition
inlet = Surf(id="INLET", vel=-1.0)  # Negative = into domain

# Time-varying velocity
ramp = Ramp(id="VEL_RAMP", points=[(0, 0), (5, 1), (25, 1), (30, 0)])
surf = Surf(id="INLET", vel=-2.0, ramp_v="VEL_RAMP")

# Mass flux with species
species = Species(id="HYDROGEN")
surf = Surf(id="LEAK", mass_flux_total=0.01, spec_id="HYDROGEN")

# Zero-gravity environment
misc = Misc(gvec=(0, 0, 0), noise=False, stratification=False)
```

## FDS Reference Cases

These examples are inspired by:
- **gas_filling_isothermal**: Species mass conservation tests
- **helium_2d_isothermal**: Buoyant plume verification
- **symmetry_test**: Solver symmetry verification
- **velocity_bc_test**: Boundary condition accuracy tests

## Running the Examples

```bash
# Run individual examples
python examples/flowfields/gas_filling.py
python examples/flowfields/helium_plume.py
python examples/flowfields/symmetry_test.py
python examples/flowfields/velocity_bc.py
```

## Notes

- Negative velocity (VEL=-1.0) means flow INTO the domain
- MASS_FLUX_TOTAL is in kg/(m²·s)
- ISOTHERMAL mode uses constant temperature throughout
- STRATIFICATION=.FALSE. prevents hydrostatic pressure adjustment
