# Pressure Examples

This directory contains examples demonstrating pressure boundary conditions
and pressure-driven flow modeling in FDS.

## Examples

### 1. dynamic_pressure.py
Demonstrates dynamic pressure on OPEN boundaries for wind effects.
- **Concepts**: VENT DYNAMIC_PRESSURE, PRESSURE_RAMP
- **Key Features**:
  - Time-varying pressure with RAMP
  - Wind-driven ventilation simulation
  - Pressure differential across domain

### 2. room_pressurization.py
Models room pressurization from supply air.
- **Concepts**: Mechanical ventilation, pressure relief
- **Key Features**:
  - Supply velocity inlet with SURF VEL
  - Pressure build-up in enclosed space
  - Relief flow through opening

### 3. vent_pressure.py
Demonstrates pressure-driven flow through an opening.
- **Concepts**: Pressure differential between rooms
- **Key Features**:
  - Two connected rooms with different pressures
  - Flow through doorway/opening
  - Pressure and velocity measurement

### 4. pressure_bc.py
Shows ambient pressure effects (high altitude simulation).
- **Concepts**: MISC P_INF parameter
- **Key Features**:
  - Reduced ambient pressure (P_INF=80000)
  - Effect on density and mass flow
  - High-altitude conditions

## Key PyFDS Features Demonstrated

```python
# Dynamic pressure on OPEN boundary
vent = Vent(mb="XMIN", surf_id="OPEN", dynamic_pressure=50.0)

# Time-varying pressure with ramp
ramp = Ramp(id="WIND", points=[(0, 0), (5, 1), (25, 1), (30, 0)])
vent = Vent(mb="XMIN", surf_id="OPEN", dynamic_pressure=50.0, pressure_ramp="WIND")

# Ambient pressure setting
misc = Misc(p_inf=80000.0, tmpa=15.0)

# Pressure measurement
devc = Device(id="P", quantity="PRESSURE", xyz=Point3D(1, 1, 1))

# Volume flow measurement
devc = Device(id="FLOW", quantity="VOLUME FLOW", xb=Bounds3D.of(0, 0, 0, 2, 0, 2))
```

## FDS Reference Cases

These examples are inspired by:
- **pressure_boundary_***: Pressure boundary verification
- **pressure_rise_***: Room pressurization tests
- **vent_pressure_***: Pressure-driven vent tests

## Running the Examples

```bash
# Run individual examples
python examples/pressure/dynamic_pressure.py
python examples/pressure/room_pressurization.py
python examples/pressure/vent_pressure.py
python examples/pressure/pressure_bc.py
```

## Notes

- DYNAMIC_PRESSURE is in Pascals (Pa) above ambient
- Negative VEL on SURF means flow INTO the domain
- P_INF default is 101325 Pa (sea level)
- VOLUME FLOW is in m³/s, MASS FLOW is in kg/s
