# Output Configuration Examples

This directory contains examples demonstrating FDS output configuration
using device-based measurements and mesh/obstruction output options.

## Important Note

The SLCF (slice files), BNDF (boundary files), and DUMP namelists are not
yet fully implemented in PyFDS. These examples focus on what's currently
available through DEVC and related output parameters.

## Examples

### device_output.py
Comprehensive device-based output configuration:
- Point measurements (XYZ) for temperature, velocity, heat flux
- Volume measurements (XB) for HRR, layer heights
- Sensor properties (PROP) for thermocouples, smoke detectors
- Multiple quantities at same location

### mesh_output.py
Mesh-level output control:
- `BNDF_MESH`: Enable/disable boundary files per mesh
- `BNDF_OBST`: Control boundary output for specific obstructions
- `BNDF_DEFAULT` in MISC: Global default for boundary files

### statistics_output.py
Statistical output options:
- Spatial statistics: `VOLUME MEAN`, `MAX`, `MIN`
- Mass flow measurements: `MASS FLOW +`, `MASS FLOW -`
- Layer analysis: `LAYER HEIGHT`, `UPPER TEMPERATURE`
- `TIME_AVERAGED` for smooth output

### surface_output.py
Surface and wall measurements:
- Wall temperatures: `WALL TEMPERATURE`, `BACK WALL TEMPERATURE`
- Heat fluxes: `INCIDENT`, `NET`, `CONVECTIVE`, `RADIATIVE`
- `IOR` for measurement orientation
- Multi-layer surfaces with different backing options

## Key Concepts

### Device Output Types

**Point Measurements (XYZ)**
```python
Device(
    id="TEMP_CENTER",
    xyz=Point3D(1.0, 1.0, 1.0),
    quantity="TEMPERATURE",
)
```

**Volume Measurements (XB)**
```python
Device(
    id="HRR_TOTAL",
    xb=Bounds3D.of(0.0, 2.0, 0.0, 2.0, 0.0, 2.0),
    quantity="HRR",
)
```

**Surface Measurements (with IOR)**
```python
Device(
    id="WALL_TEMP",
    xyz=Point3D(0.1, 1.0, 1.0),
    quantity="WALL TEMPERATURE",
    ior=-1,  # -X face orientation
)
```

### Sensor Properties
```python
# Thermocouple with response characteristics
Property(
    id="TC_1MM",
    quantity="THERMOCOUPLE",
    bead_diameter=0.001,  # 1mm bead
    bead_emissivity=0.9,
)

Device(
    id="TC_CEILING",
    xyz=Point3D(1.0, 1.0, 2.9),
    prop_id="TC_1MM",
)
```

### Statistical Quantities
```python
# Volume mean temperature
Device(
    id="AVG_TEMP",
    xb=Bounds3D.of(...),
    quantity="TEMPERATURE",
    statistics="VOLUME MEAN",
)

# Maximum in region
Device(
    id="MAX_TEMP",
    xb=Bounds3D.of(...),
    quantity="TEMPERATURE",
    statistics="MAX",
)
```

### Mesh-Level Control
```python
# Disable boundary output for background mesh
Mesh(
    id="BACKGROUND",
    ijk=Grid3D.of(20, 20, 20),
    xb=Bounds3D.of(...),
    bndf_mesh=False,
)

# Disable for specific obstruction
Obstruction(
    id="FLOOR",
    xb=Bounds3D.of(...),
    bndf_obst=False,  # Exclude from BNDF
)
```

## Common Quantity Types

### Gas Phase
- `TEMPERATURE` - Gas temperature (°C)
- `VELOCITY` - Speed magnitude (m/s)
- `U-VELOCITY`, `V-VELOCITY`, `W-VELOCITY` - Components
- `PRESSURE` - Background-subtracted pressure (Pa)
- `DENSITY` - Gas density (kg/m³)
- `VISIBILITY` - Visibility distance (m)
- `HRR` - Heat release rate (kW)

### Surface/Wall
- `WALL TEMPERATURE` - Front surface temperature (°C)
- `BACK WALL TEMPERATURE` - Back surface temperature
- `INCIDENT HEAT FLUX` - Incident radiative flux (kW/m²)
- `NET HEAT FLUX` - Absorbed flux
- `CONVECTIVE HEAT FLUX` - Convective component
- `RADIATIVE HEAT FLUX` - Radiative component
- `GAUGE HEAT FLUX` - Virtual radiometer reading

### Integrated
- `LAYER HEIGHT` - Two-zone layer interface (m)
- `UPPER TEMPERATURE` - Upper layer temperature (°C)
- `LOWER TEMPERATURE` - Lower layer temperature (°C)
- `PATH OBSCURATION` - Line-of-sight obscuration (%)
- `MASS FLOW +/-` - Mass flux through plane (kg/s)

## FDS References
- [Controls/device_test.fds](https://github.com/firemodels/fds/blob/master/Verification/Controls/device_test.fds)
- [Heat_Transfer/heat_conduction_a.fds](https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/heat_conduction_a.fds)
