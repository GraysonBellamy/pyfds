# Control Logic (CTRL)

Implement automatic control systems and device-based automation in FDS simulations.

## Overview

**Controls** (CTRL namelist) enable dynamic simulation behavior based on device measurements or time. Common applications:

- Sprinkler activation based on temperature
- HVAC shutdown when smoke detected
- Door opening/closing sequences
- Suppression system activation
- Complex automated responses

```python
# Simple control: Activate when temperature > 68°C
sim.control(
    id='SPRINKLER_CTRL',
    input_id='TEMP_SPRINKLER',
    setpoint=68.0,
    initial_state=False
)
```

## Control Basics

### Device-Based Controls

Activate when a device exceeds a threshold:

```python
# Temperature device
sim.device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=(3, 2, 2.4)
)

# Control activates when temp > 74°C
sim.control(
    id='SPRINKLER_ACTIVATION',
    input_id='TEMP_CEILING',
    setpoint=74.0,
    initial_state=False
)
```

### Time-Based Controls

Activate at specific times:

```python
# Timer control
sim.control(
    id='TIMER_300',
    delay=300.0  # Activates at t=300s
)
```

### Control States

Controls have boolean states (True/False):

- `initial_state=False`: Starts inactive, activates when condition met
- `initial_state=True`: Starts active, deactivates when condition met

## Control Functions

### Threshold Detection

```python
# Activate when device EXCEEDS setpoint
sim.control(
    id='TEMP_HIGH',
    input_id='TEMP_DEVICE',
    setpoint=80.0,
    latch=True  # Stay activated once triggered
)

# Activate when device BELOW setpoint
sim.control(
    id='TEMP_LOW',
    input_id='TEMP_DEVICE',
    setpoint=20.0,
    latch=False  # Can deactivate
)
```

### Latching vs Non-Latching

```python
# Latching: Once activated, stays active
sim.control(
    id='ALARM',
    input_id='SMOKE_DET',
    setpoint=0.1,
    latch=True  # Once smoke detected, alarm stays on
)

# Non-latching: Can activate/deactivate
sim.control(
    id='FAN',
    input_id='TEMP',
    setpoint=30.0,
    latch=False  # Fan turns on/off as temp fluctuates
)
```

### Delays

```python
# Activation delay (must exceed setpoint for 60s)
sim.control(
    id='DELAYED_ACTIVATION',
    input_id='TEMP',
    setpoint=70.0,
    delay=60.0  # Wait 60s before activating
)

# Response time (realistic device lag)
sim.control(
    id='SPRINKLER',
    input_id='TEMP_LINK',
    setpoint=68.0,
    delay=5.0  # 5s thermal lag
)
```

## Logical Operations

### AND Logic

All inputs must be True:

```python
# Activate only if BOTH conditions met
sim.control(
    id='AND_CONTROL',
    input_id=['SMOKE_HIGH', 'TEMP_HIGH'],
    function_type='AND'
)
```

### OR Logic

Any input True activates:

```python
# Activate if ANY condition met
sim.control(
    id='OR_CONTROL',
    input_id=['SMOKE_DET_1', 'SMOKE_DET_2', 'SMOKE_DET_3'],
    function_type='OR'
)
```

### NOT Logic

Invert control state:

```python
# Active when input is False
sim.control(
    id='NOT_CONTROL',
    input_id='NORMAL_OPERATION',
    function_type='NOT'
)
```

### XOR Logic

Exclusive or (exactly one True):

```python
# Activate if exactly one input True
sim.control(
    id='XOR_CONTROL',
    input_id=['ZONE_1_ALARM', 'ZONE_2_ALARM'],
    function_type='XOR'
)
```

## Common Applications

### Sprinkler Activation

```python
from pyfds import Simulation

sim = Simulation(chid='sprinkler_system')
sim.time(t_end=600.0)
sim.mesh(ijk=(60, 50, 30), xb=(0, 6, 0, 5, 0, 3))

# Fire
sim.surface(id='FIRE', hrrpua=1200.0)
sim.obstruction(xb=(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

# Sprinkler head temperature (at ceiling)
sim.device(
    id='SPRINKLER_LINK',
    quantity='TEMPERATURE',
    xyz=(3, 2.5, 2.95)
)

# Control: Activate at 68°C (RTI included in device)
sim.control(
    id='SPRINKLER_CTRL',
    input_id='SPRINKLER_LINK',
    setpoint=68.0,
    latch=True  # Once activated, stays on
)

# Sprinkler spray surface (controlled)
sim.surface(
    id='SPRINKLER_SPRAY',
    mass_flux=0.05,  # Water spray (kg/m²/s)
    ctrl_id='SPRINKLER_CTRL'
)

# Sprinkler vent (activates when control True)
sim.vent(
    xb=(2.8, 3.2, 2.3, 2.7, 2.95, 2.95),
    surf_id='SPRINKLER_SPRAY'
)

sim.write('sprinkler_system.fds')
```

### HVAC Smoke Shutdown

```python
sim = Simulation(chid='hvac_shutdown')
sim.time(t_end=900.0)
sim.mesh(ijk=(80, 60, 30), xb=(0, 8, 0, 6, 0, 3))

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(3.5, 4.5, 2.5, 3.5, 0, 0.1), surf_id='FIRE')

# Smoke detector at ceiling
sim.device(
    id='SMOKE_DET',
    quantity='OPTICAL DENSITY',
    xyz=(4, 3, 2.9)
)

# Control: Shutdown HVAC when smoke detected (OD > 0.05)
sim.control(
    id='HVAC_SHUTDOWN',
    input_id='SMOKE_DET',
    setpoint=0.05,
    latch=True,
    initial_state=True  # HVAC starts ON
)

# HVAC vents controlled by smoke detector
# Control inverted: HVAC ON when control is True (no smoke)
sim.vent(
    xb=(1, 1.5, 1, 1.5, 3, 3),
    surf_id='HVAC',
    volume_flow=0.6,
    ctrl_id='HVAC_SHUTDOWN'
)

sim.vent(
    xb=(6.5, 7, 4.5, 5, 3, 3),
    surf_id='HVAC',
    volume_flow=-0.5,
    ctrl_id='HVAC_SHUTDOWN'
)

sim.write('hvac_shutdown.fds')
```

### Fire Suppression System

```python
sim = Simulation(chid='suppression_system')
sim.time(t_end=600.0)
sim.mesh(ijk=(100, 80, 40), xb=(0, 10, 0, 8, 0, 4))

# Fire
sim.surface(id='FIRE', hrrpua=1500.0)
sim.obstruction(xb=(4.5, 5.5, 3.5, 4.5, 0, 0.1), surf_id='FIRE')

# Multiple detectors (OR logic)
detectors = []
for i, (x, y) in enumerate([(3, 3), (5, 3), (7, 3), (5, 5)]):
    det_id = f'DET_{i+1}'
    sim.device(
        id=det_id,
        quantity='TEMPERATURE',
        xyz=(x, y, 3.9)
    )

    # Individual detector controls
    sim.control(
        id=f'CTRL_{i+1}',
        input_id=det_id,
        setpoint=74.0
    )
    detectors.append(f'CTRL_{i+1}')

# Master control: ANY detector activates system
sim.control(
    id='SUPPRESSION_ACTIVATE',
    input_id=detectors,
    function_type='OR',
    latch=True
)

# Suppression nozzles
nozzles = [(2, 4), (5, 2), (5, 6), (8, 4)]
for i, (x, y) in enumerate(nozzles):
    sim.surface(
        id=f'NOZZLE_{i+1}',
        mass_flux=0.08,
        ctrl_id='SUPPRESSION_ACTIVATE'
    )

    sim.vent(
        xb=(x-0.2, x+0.2, y-0.2, y+0.2, 3.95, 3.95),
        surf_id=f'NOZZLE_{i+1}'
    )

sim.write('suppression_system.fds')
```

### Door Opening Sequence

```python
sim = Simulation(chid='door_sequence')
sim.time(t_end=600.0)
sim.mesh(ijk=(100, 60, 30), xb=(0, 10, 0, 6, 0, 3))

# Fire in room 1
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 2.5, 3.5, 0, 0.1), surf_id='FIRE')

# Timer controls for door opening sequence
# Door 1 opens at t=120s
sim.control(id='DOOR1_OPEN', delay=120.0)

# Door 2 opens at t=240s
sim.control(id='DOOR2_OPEN', delay=240.0)

# Door 3 opens at t=360s
sim.control(id='DOOR3_OPEN', delay=360.0)

# Doors as removable obstructions
# Door 1: Between rooms 1 and 2
sim.obstruction(
    xb=(4.9, 5.1, 2.5, 3.5, 0, 2.1),
    surf_id='INERT',
    ctrl_id='DOOR1_OPEN',
    removable=True  # Removed when control activates
)

# Door 2: Between rooms 2 and 3
sim.obstruction(
    xb=(7.4, 7.6, 2.5, 3.5, 0, 2.1),
    surf_id='INERT',
    ctrl_id='DOOR2_OPEN',
    removable=True
)

# Door 3: Exit to outside
sim.obstruction(
    xb=(9.9, 10, 2.5, 3.5, 0, 2.1),
    surf_id='INERT',
    ctrl_id='DOOR3_OPEN',
    removable=True
)

sim.write('door_sequence.fds')
```

### Temperature-Controlled Vent

```python
sim = Simulation(chid='temp_vent')
sim.time(t_end=600.0)
sim.mesh(ijk=(60, 50, 30), xb=(0, 6, 0, 5, 0, 3))

# Fire
sim.surface(id='FIRE', hrrpua=1200.0)
sim.obstruction(xb=(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

# Temperature sensor at ceiling
sim.device(
    id='TEMP_CEILING',
    quantity='TEMPERATURE',
    xyz=(3, 2.5, 2.9)
)

# Control: Open vent when temp > 100°C
sim.control(
    id='VENT_OPEN',
    input_id='TEMP_CEILING',
    setpoint=100.0,
    latch=True
)

# Ceiling vent (initially closed obstruction)
sim.obstruction(
    xb=(2.5, 3.5, 2, 3, 2.95, 3),
    surf_id='INERT',
    ctrl_id='VENT_OPEN',
    removable=True  # Opens (removed) when hot
)

sim.write('temp_vent.fds')
```

## Advanced Controls

### Multi-Stage Response

```python
# Stage 1: Alert at 60°C
sim.control(id='ALERT', input_id='TEMP', setpoint=60.0)

# Stage 2: Evacuate at 80°C
sim.control(id='EVACUATE', input_id='TEMP', setpoint=80.0)

# Stage 3: Suppress at 100°C
sim.control(id='SUPPRESS', input_id='TEMP', setpoint=100.0, latch=True)
```

### Zone-Based System

```python
# Multiple zones with independent controls
zones = ['ZONE_A', 'ZONE_B', 'ZONE_C']

for zone in zones:
    # Detector in each zone
    sim.device(id=f'DET_{zone}', quantity='TEMPERATURE', xyz=(...))

    # Control for each zone
    sim.control(
        id=f'CTRL_{zone}',
        input_id=f'DET_{zone}',
        setpoint=74.0,
        latch=True
    )

    # Suppression in each zone
    sim.vent(
        xb=(...),
        surf_id='SUPPRESSION',
        ctrl_id=f'CTRL_{zone}'
    )
```

### Cascading Activation

```python
# Detector 1 triggers first stage
sim.control(id='STAGE_1', input_id='DET_1', setpoint=70.0)

# Stage 1 + Detector 2 triggers stage 2
sim.control(
    id='STAGE_2',
    input_id=['STAGE_1', 'DET_2_CTRL'],
    function_type='AND'
)

# Both stages trigger final response
sim.control(
    id='FINAL',
    input_id=['STAGE_1', 'STAGE_2'],
    function_type='AND',
    latch=True
)
```

## Control with RAMPs

Combine controls with time-varying properties:

```python
# Fire starts at t=0, grows normally
sim.ramp(id='FIRE_GROWTH', t=[0, 180], f=[0, 1])
sim.surface(id='FIRE', hrrpua=2000.0, ramp_q='FIRE_GROWTH')

# Control activates suppression at t=200s
sim.control(id='SUPPRESS', delay=200.0)

# Suppression ramp (immediate full flow)
sim.ramp(id='SUPPRESS_RAMP', t=[0, 1], f=[0, 1])

# Suppression surface with control and ramp
sim.surface(
    id='SUPPRESSION',
    mass_flux=0.1,
    ctrl_id='SUPPRESS',
    ramp_mf='SUPPRESS_RAMP'
)
```

## Best Practices

### 1. Clear Control Logic

```python
# Good: Descriptive IDs and comments
# Sprinkler activates when ceiling temperature > 68°C
sim.control(
    id='SPRINKLER_ACTIVATION',
    input_id='LINK_TEMPERATURE',
    setpoint=68.0,
    latch=True
)

# Poor: Unclear purpose
sim.control(id='C1', input_id='D1', setpoint=68.0)
```

### 2. Realistic Response Times

```python
# Include thermal lag for fusible links
sim.control(
    id='SPRINKLER',
    input_id='LINK_TEMP',
    setpoint=68.0,
    delay=5.0,  # RTI effect approximation
    latch=True
)
```

### 3. Test Control Logic

```python
# Add device to monitor control state
sim.device(
    id='CTRL_STATE',
    quantity='CONTROL VALUE',
    ctrl_id='SPRINKLER_ACTIVATION'
)
```

### 4. Use Latching Appropriately

```python
# Latch for one-time events (alarms, suppression)
sim.control(id='ALARM', input_id='SMOKE', setpoint=0.1, latch=True)

# Don't latch for cyclical controls (thermostats, pressure relief)
sim.control(id='THERMOSTAT', input_id='TEMP', setpoint=25.0, latch=False)
```

## Common Issues

??? question "Control not activating"
    **Cause**: Setpoint never reached or wrong initial state

    **Solution**: Check device values and initial state
    ```python
    # Monitor both device and control
    sim.device(id='TEMP', quantity='TEMPERATURE', xyz=(3, 2, 2))
    sim.device(id='CTRL_STATE', quantity='CONTROL VALUE', ctrl_id='MY_CTRL')
    ```

??? question "Control activates too early/late"
    **Cause**: Incorrect setpoint or missing delay

    **Solution**: Adjust setpoint or add delay
    ```python
    sim.control(
        id='CTRL',
        input_id='TEMP',
        setpoint=74.0,  # Check this value
        delay=10.0      # Add realistic delay
    )
    ```

??? question "Sprinkler doesn't spray"
    **Cause**: Control not linked to surface

    **Solution**: Link control to surface via ctrl_id
    ```python
    sim.surface(id='SPRAY', mass_flux=0.05, ctrl_id='SPRINKLER_CTRL')
    ```

## Control Function Summary

| Function | Description | Example Use |
|----------|-------------|-------------|
| **Threshold** | Device > setpoint | Temperature detection |
| **Timer** | Activate at time | Scheduled events |
| **AND** | All inputs True | Multi-sensor confirmation |
| **OR** | Any input True | Multiple detector zones |
| **NOT** | Invert input | Normally-open logic |
| **XOR** | Exactly one True | Exclusive zones |
| **Latch** | Stay activated | One-time suppression |
| **Delay** | Wait before activate | Thermal lag, debounce |

## Next Steps

- [Devices](devices.md) - Creating device inputs for controls
- [RAMP](ramps.md) - Time-varying properties with controls
- [Examples](../examples/advanced.md) - Advanced control systems
- [Fire Sources](fire-sources.md) - Controlling fire behavior

---

[Initial Conditions →](initial-conditions.md){ .md-button .md-button--primary }
