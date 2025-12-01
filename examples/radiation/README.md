# Radiation Examples

This directory contains examples demonstrating radiation heat transfer modeling
in FDS, inspired by FDS Verification Guide Chapter 8 (Radiation).

## Examples

### 1. adiabatic_surface.py
Demonstrates the adiabatic surface technique for measuring thermal radiation.
- **Concepts**: SURF adiabatic parameter, radiation gauge sensors
- **Key Features**:
  - Adiabatic surfaces that balance convective and radiative flux
  - WALL TEMPERATURE measurement on adiabatic surfaces
  - Comparison of adiabatic vs regular surface response

### 2. emissivity_effects.py
Shows how surface emissivity affects radiative heat transfer.
- **Concepts**: MATL emissivity, radiative exchange
- **Key Features**:
  - High emissivity (0.95) vs low emissivity (0.1) surfaces
  - Hot plate radiation source with TMP_FRONT
  - Differential heating rates based on emissivity

### 3. heat_flux_gauges.py
Demonstrates different types of heat flux measurements.
- **Concepts**: Radiative, convective, and gauge heat flux
- **Key Features**:
  - GAUGE HEAT FLUX - total heat flux (radiative + convective)
  - RADIATIVE HEAT FLUX - radiative component only
  - CONVECTIVE HEAT FLUX - convective component only
  - INCIDENT HEAT FLUX - incident radiation without re-radiation
  - Array of gauges at multiple distances/heights

### 4. radiative_fraction.py
Demonstrates how combustion radiative fraction affects heat flux.
- **Concepts**: REAC radiative_fraction, soot effects
- **Key Features**:
  - RADIATIVE_FRACTION parameter for fuel radiation
  - SOOT_YIELD effect on radiation
  - Heat flux variation with distance from fire

## Key PyFDS Features Demonstrated

```python
# Adiabatic surface
surf = Surface(id="ADIABATIC", adiabatic=True)

# Surface emissivity
surf = Surface(id="HOT", tmp_front=500.0, emissivity=0.95)

# Material emissivity
matl = Material(id="REFLECTIVE", emissivity=0.1, ...)

# Radiative fraction in combustion
reac = Reaction(fuel="PROPANE", radiative_fraction=0.35, soot_yield=0.05)

# Heat flux gauges
devc = Device(id="HF", quantity="GAUGE HEAT FLUX", xyz=..., ior=-1)
devc = Device(id="RAD", quantity="RADIATIVE HEAT FLUX", xyz=..., ior=-1)
```

## FDS Reference Cases

These examples are inspired by:
- **adiabatic_surface_2d**: Adiabatic surface temperature verification
- **emissivity_test**: Emissivity effects on radiation exchange
- **heat_flux_***: Various heat flux measurement tests
- **radiative_fraction_***: Combustion radiation tests

## Running the Examples

```bash
# Run individual examples
python examples/radiation/adiabatic_surface.py
python examples/radiation/emissivity_effects.py
python examples/radiation/heat_flux_gauges.py
python examples/radiation/radiative_fraction.py
```

## Notes

- Emissivity range is 0-1 (0=perfectly reflective, 1=blackbody)
- ADIABATIC surfaces have zero net heat flux (q_rad = q_conv)
- IOR parameter on devices indicates face orientation
- Typical radiative fractions: methane ~0.15-0.20, propane ~0.25-0.30
