# Sprinklers and Spray Examples

This directory contains examples demonstrating Lagrangian particle features in FDS,
including water sprays, sprinkler activation, and particle drag models.

## Examples

### 1. `sprinkler_activation.py`
**FDS Reference:** `Sprinklers_and_Sprays/activate_sprinklers.fds`

Demonstrates sprinkler device activation using:
- **PROP** namelist with RTI (Response Time Index) and activation temperature
- **DEVC** with sprinkler properties and setpoint
- **PART** for water droplet particles

Key Features:
- Sprinkler link temperature response modeling
- RTI-based thermal response
- Activation temperature setpoint
- Spray pattern parameters (SPRAY_ANGLE, FLOW_RATE)

### 2. `water_spray.py`
**FDS Reference:** `Sprinklers_and_Sprays/water_evaporation_*.fds`

Demonstrates water droplet evaporation using:
- **PART** with SPEC_ID='WATER VAPOR' for evaporating droplets
- **INIT** for particle initialization with MASS_PER_VOLUME
- **MISC** for environmental conditions (TMPA, HUMIDITY)

Key Features:
- Static water droplets in heated environment
- Monodisperse particle size distribution
- Water vapor species tracking
- Zero-gravity environment for evaporation studies

### 3. `bucket_test.py`
**FDS Reference:** `Sprinklers_and_Sprays/bucket_test_1.fds`

Demonstrates sprinkler spray pattern testing using:
- **PROP** with SPRAY_ANGLE range and PARTICLE_VELOCITY
- **RAMP** for flow rate time profile
- **DEVC** for immediate activation (QUANTITY='TIME')

Key Features:
- Spray angle distribution (30° to 80° from vertical)
- Time-ramped flow rate
- Particle velocity specification
- Open boundary conditions for spray collection

### 4. `particle_drag.py`
**FDS Reference:** `Sprinklers_and_Sprays/sphere_drag_1.fds`

Demonstrates particle drag verification using:
- **PART** with DRAG_COEFFICIENT for user-specified drag
- **SURF** with GEOMETRY='SPHERICAL' for solid particles
- **INIT** for cell-centered particle initialization

Key Features:
- Multiple drag coefficient values (5, 10, 20, 50)
- Static particles in flow field
- Pressure drop measurement
- Solid spherical particles with surface properties

## API Classes Used

| Namelist | PyFDS Class | Purpose |
|----------|-------------|---------|
| PART | `Particle` | Particle class properties |
| PROP | `Property` | Sprinkler/detector properties |
| DEVC | `Device` | Measurement devices and actuators |
| INIT | `Initialization` | Initial conditions and particle insertion |
| SPEC | `Species` | Species definitions (WATER VAPOR) |
| SURF | `Surface` | Surface properties for solid particles |
| MATL | `Material` | Material properties |
| RAMP | `Ramp` | Time-dependent parameters |
| MISC | `Misc` | Environmental conditions |
| MESH | `Mesh` | Computational domain |
| TIME | `Time` | Simulation time parameters |
| VENT | `Vent` | Boundary conditions |

## Key Concepts

### Sprinkler Activation
- **RTI (Response Time Index)**: Thermal response parameter [m^0.5·s^0.5]
- **Activation Temperature**: Sprinkler fuses at this temperature [°C]
- **C Factor**: Conduction parameter for link heating

### Particle Drag Laws
- **SPHERE**: Default drag law for spherical droplets
- **CYLINDER**: Drag law for cylindrical particles
- **SCREEN**: Special drag law for screen/mesh particles
- **USER_DRAG**: User-specified constant drag coefficient

### Spray Parameters
- **SPRAY_ANGLE**: Cone angle range from vertical [degrees]
- **FLOW_RATE**: Water flow rate [L/min]
- **PARTICLE_VELOCITY**: Initial droplet velocity [m/s]

## Running Examples

```bash
# Run individual example
python sprinklers/sprinkler_activation.py
python sprinklers/water_spray.py
python sprinklers/bucket_test.py
python sprinklers/particle_drag.py

# Output files are written to examples/fds/sprinklers/
```

## FDS User Guide References

- Chapter 18: Lagrangian Particles
- Chapter 19: Sprinklers
- Technical Reference Guide: Particle Chapter
