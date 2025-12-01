# HVAC System Examples

This directory contains examples demonstrating HVAC (Heating, Ventilation,
and Air Conditioning) system modeling in FDS.

## Examples

### 1. `duct_flow.py`
**FDS Reference:** `HVAC/HVAC_flow_loss.fds`

Demonstrates duct flow distribution with loss coefficients:
- **HVAC TYPE_ID='DUCT'** with AREA, LENGTH, LOSS
- **HVAC TYPE_ID='NODE'** connecting vents to ducts
- Volume flow distribution through a tee junction

Key Features:
- Fixed volume flow specification (VOLUME_FLOW)
- Tee junction with internal node
- Loss coefficient-based flow distribution
- Duct velocity measurements (QUANTITY='DUCT VELOCITY')

### 2. `fan_system.py`
**FDS Reference:** `HVAC/fan_test.fds`

Demonstrates fan-driven flow between sealed compartments:
- **HVAC TYPE_ID='FAN'** with MAX_FLOW and MAX_PRESSURE
- Fan characteristic curve specification
- Parallel ducts with different flow resistances

Key Features:
- Fan performance curve (linear between MAX_FLOW and MAX_PRESSURE)
- Fan direction reversal (REVERSE parameter)
- Passive duct with loss coefficient
- Pressure and volume flow measurements

### 3. `hvac_filter.py`
**FDS Reference:** `HVAC/HVAC_filter.fds`

Demonstrates particle filtration in HVAC systems:
- **HVAC TYPE_ID='FILTER'** with EFFICIENCY and LOADING_MULTIPLIER
- Species tracking through HVAC components
- Filter loading accumulation

Key Features:
- Particulate species removal
- Clean filter loss coefficient (CLEAN_LOSS)
- Filter loading tracking (QUANTITY='FILTER LOADING')
- Node pressure and temperature measurements

### 4. `leakage.py`
**FDS Reference:** `HVAC/leak_test.fds`

Demonstrates pressure-driven leakage between compartments:
- **SURF LEAK_PATH** for zone-to-zone leakage
- Fan ramp-up time constant (TAU_FAN)
- Pressure differential measurements

Key Features:
- Zone connectivity via leak path surfaces
- Fan spin-up dynamics
- Pressure-driven flow distribution
- Combined fan and leakage flow

## API Classes Used

| Namelist | PyFDS Class | Purpose |
|----------|-------------|---------|
| HVAC | `Hvac` | Duct, node, fan, filter, leak components |
| VENT | `Vent` | Connections between HVAC and domain |
| SURF | `Surface` | HVAC surface type, leak path |
| DEVC | `Device` | Flow and pressure measurements |
| SPEC | `Species` | Filtered species definitions |
| ZONE | `Zone` | Pressure zone definitions |
| MESH | `Mesh` | Computational domain |
| TIME | `Time` | Simulation time parameters |
| OBST | `Obstruction` | Solid walls and partitions |

## Key HVAC Concepts

### HVAC Component Types
- **DUCT**: Flow path with area, length, and loss coefficients
- **NODE**: Junction point connecting ducts to vents or other ducts
- **FAN**: Mechanical ventilation with characteristic curve
- **FILTER**: Particle removal with efficiency and loading
- **AIRCOIL**: Heat exchange element
- **LEAK**: Zone-to-zone leakage path

### Flow Specification Methods
- `VOLUME_FLOW`: Fixed volume flow rate [m³/s]
- `MASS_FLOW`: Fixed mass flow rate [kg/s]
- `MAX_FLOW` + `MAX_PRESSURE`: Fan curve (linear interpolation)
- `LOSS`: Pressure loss coefficient [forward, reverse]

### HVAC Device Quantities
- `DUCT VELOCITY`: Flow velocity in duct [m/s]
- `DUCT VOLUME FLOW`: Volume flow rate [m³/s]
- `DUCT MASS FLOW`: Mass flow rate [kg/s]
- `NODE PRESSURE`: Pressure at node [Pa]
- `NODE TEMPERATURE`: Temperature at node [°C]
- `FILTER LOADING`: Accumulated filter mass [kg]
- `FILTER LOSS`: Current filter loss coefficient

## Running Examples

```bash
# Run individual example
python hvac/duct_flow.py
python hvac/fan_system.py
python hvac/hvac_filter.py
python hvac/leakage.py

# Output files are written to examples/fds/hvac/
```

## FDS User Guide References

- Chapter 15: HVAC Systems
- Technical Reference Guide: HVAC Chapter
