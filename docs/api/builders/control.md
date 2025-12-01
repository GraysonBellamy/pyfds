# ControlBuilder

::: pyfds.builders.control.ControlBuilder
    options:
      show_source: true
      heading_level: 2

## Overview

`ControlBuilder` creates control logic (`&CTRL` namelists) for device interactions and system automation.

## Key Features

- **Logic Gates**: ANY (OR), ALL (AND), ONLY (passthrough)
- **Time Delays**: Delayed activation
- **State Management**: Initial state, latch behavior
- **Special Functions**: KILL, RESTART, CUSTOM

## Quick Examples

### ANY Logic (OR)

```python
from pyfds.builders import ControlBuilder

# Activates if ANY detector triggers
alarm = (
    ControlBuilder('SMOKE_ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2', 'SMOKE_DET_3'])
    .build()
)
```

### ALL Logic (AND)

```python
# Activates only if ALL detectors trigger
multi_zone = (
    ControlBuilder('MULTI_ZONE')
    .all(['ZONE_1_DET', 'ZONE_2_DET', 'ZONE_3_DET'])
    .build()
)
```

### ONLY Logic (Passthrough)

```python
# Simple passthrough of single device
passthrough = (
    ControlBuilder('PASSTHROUGH')
    .only('HEAT_DET_1')
    .build()
)
```

### Time Delay

```python
# Delayed sprinkler activation (10 second delay)
delayed_sprinkler = (
    ControlBuilder('DELAYED_SPRINKLER')
    .time_delay('HEAT_DETECTOR', delay=10.0)
    .build()
)
```

### With Modifiers

```python
# Complex control with modifiers
ctrl = (
    ControlBuilder('COMPLEX_CTRL')
    .any(['DET_1', 'DET_2', 'DET_3'])
    .with_delay(3.0)              # 3 second delay
    .with_initial_state(False)    # Start deactivated
    .with_latch(True)             # Stay on once activated
    .build()
)
```

## Logic Functions

### ANY (OR Gate)

Activates when **any** input device activates:

```python
# Fire alarm if any smoke detector activates
alarm = ControlBuilder('ALARM').any(['SD_1', 'SD_2', 'SD_3']).build()

# Door opens if any occupant detector triggers
door = ControlBuilder('AUTO_DOOR').any(['OCCUPANT_1', 'OCCUPANT_2']).build()
```

### ALL (AND Gate)

Activates only when **all** input devices are active:

```python
# Requires all zones to be clear
safe = ControlBuilder('ALL_CLEAR').all(['ZONE_1_OK', 'ZONE_2_OK', 'ZONE_3_OK']).build()

# Multi-factor activation
secure = ControlBuilder('SECURE').all(['CARD_READER', 'PIN_OK', 'BIOMETRIC']).build()
```

### ONLY (Passthrough)

Simple one-to-one mapping:

```python
# Direct passthrough
ctrl = ControlBuilder('DIRECT').only('HEAT_DET').build()
```

## Modifiers

### Time Delay

Add delay to activation:

```python
# 5 second delay
ctrl = (
    ControlBuilder('DELAYED')
    .any(['DET_1', 'DET_2'])
    .with_delay(5.0)
    .build()
)

# Time delay function (alternative)
ctrl = (
    ControlBuilder('DELAYED')
    .time_delay('HEAT_DET', delay=10.0)
    .build()
)
```

### Initial State

Set the initial state (default is False):

```python
# Start in activated state
ctrl = (
    ControlBuilder('INITIALLY_ON')
    .any(['DET_1', 'DET_2'])
    .with_initial_state(True)
    .build()
)

# Start deactivated (default)
ctrl = (
    ControlBuilder('INITIALLY_OFF')
    .any(['DET_1', 'DET_2'])
    .with_initial_state(False)
    .build()
)
```

### Latch

Once activated, stay activated (ignore deactivation):

```python
# Latching alarm (stays on)
alarm = (
    ControlBuilder('LATCH_ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2'])
    .with_latch(True)
    .build()
)

# Non-latching (default - can turn off)
ctrl = (
    ControlBuilder('TOGGLE')
    .any(['DET_1', 'DET_2'])
    .with_latch(False)
    .build()
)
```

## Special Functions

### KILL

Stop the simulation when activated:

```python
# Stop simulation at t=600s
kill = ControlBuilder('KILL').kill(on_t=600).build()

# Kill on high temperature
kill = ControlBuilder('KILL_ON_TEMP').kill(on_device='TEMP_SENSOR').build()
```

### RESTART

Restart simulation from t=0:

```python
# Restart at t=300s
restart = ControlBuilder('RESTART').restart(on_t=300).build()

# Restart on device trigger
restart = ControlBuilder('RESTART_CTRL').restart(on_device='TRIGGER').build()
```

### CUSTOM

Use custom ramp function:

```python
# Custom control based on ramp
custom = (
    ControlBuilder('CUSTOM_CTRL')
    .custom(ramp_id='MY_RAMP')
    .build()
)
```

## Usage in Simulations

### Smoke Alarm System

```python
from pyfds import Simulation
from pyfds.builders import ControlBuilder, PropBuilder
from pyfds.core.geometry import Point3D

sim = Simulation('alarm_system')

# Add smoke detectors
for i in range(1, 4):
    sim.add(Device(
        id=f'SMOKE_DET_{i}',
        prop_id='SMOKE_DETECTOR',
        xyz=Point3D.of(i*2, 5, 2.5)
    )

# Add detector properties
smoke_prop = PropBuilder.smoke_detector(id='SMOKE_DETECTOR')
sim.add_prop(smoke_prop)

# ANY logic alarm (activates if any detector triggers)
alarm = (
    ControlBuilder('BUILDING_ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2', 'SMOKE_DET_3'])
    .with_latch(True)  # Stay on once activated
    .build()
)
sim.add_ctrl(alarm)
```

### Delayed Sprinkler Activation

```python
# Heat detector
sim.add(Device(id='HEAT_DET', prop_id='HEAT_DETECTOR', xyz=Point3D.of(5, 5, 2.5)))

heat_prop = PropBuilder.heat_detector(id='HEAT_DETECTOR', activation_temp=74)
sim.add_prop(heat_prop)

# Delayed sprinkler activation (10s delay)
sprinkler_ctrl = (
    ControlBuilder('DELAYED_SPRINKLER')
    .time_delay('HEAT_DET', delay=10.0)
    .with_latch(True)
    .build()
)
sim.add_ctrl(sprinkler_ctrl)

# Sprinkler device controlled by ctrl
sim.add(Device(
    id='SPRINKLER',
    prop_id='SPRINKLER_QR',
    xyz=Point3D.of(5, 5, 3),
    ctrl_id='DELAYED_SPRINKLER'
)
```

### Multi-Zone System

```python
# Detectors in each zone
for zone in range(1, 4):
    sim.add(Device(
        id=f'ZONE_{zone}_DET',
        prop_id='SMOKE_DETECTOR',
        xyz=Point3D.of(zone*3, 5, 2.5)
    )

# ALL logic - requires all zones
all_zones = (
    ControlBuilder('ALL_ZONES')
    .all(['ZONE_1_DET', 'ZONE_2_DET', 'ZONE_3_DET'])
    .build()
)
sim.add_ctrl(all_zones)

# ANY logic - any zone triggers alarm
any_zone = (
    ControlBuilder('ANY_ZONE')
    .any(['ZONE_1_DET', 'ZONE_2_DET', 'ZONE_3_DET'])
    .with_latch(True)
    .build()
)
sim.add_ctrl(any_zone)
```

### HVAC Control

```python
# Temperature-based HVAC control
sim.add(Device(id='TEMP_SENSOR', quantity='TEMPERATURE', xyz=Point3D.of(5, 5, 2)))

# Turn on HVAC when temp > threshold
hvac_on = (
    ControlBuilder('HVAC_ON')
    .only('TEMP_SENSOR')
    .build()
)
sim.add_ctrl(hvac_on)

# HVAC vent controlled by temperature
supply = VentBuilder.hvac_supply(
    xb=Bounds3D.of(5, 6, 5, 6, 3, 3),
    volume_flow=0.5,
    id='SUPPLY'
)
supply.ctrl_id = 'HVAC_ON'
sim.add_vent(supply)
```

### Safety Shutdown

```python
# Kill simulation on dangerous conditions
kill_high_temp = (
    ControlBuilder('KILL_HIGH_TEMP')
    .kill(on_device='MAX_TEMP_SENSOR')
    .build()
)
sim.add_ctrl(kill_high_temp)

# Kill at specific time
kill_timeout = (
    ControlBuilder('KILL_TIMEOUT')
    .kill(on_t=600)
    .build()
)
sim.add_ctrl(kill_timeout)
```

## Control Logic Truth Tables

### ANY (OR)

| Input 1 | Input 2 | Output |
|---------|---------|--------|
| False | False | False |
| False | True | True |
| True | False | True |
| True | True | True |

### ALL (AND)

| Input 1 | Input 2 | Output |
|---------|---------|--------|
| False | False | False |
| False | True | False |
| True | False | False |
| True | True | True |

## Best Practices

### Use Descriptive IDs

```python
# Good
alarm = ControlBuilder('SMOKE_ALARM_ZONE_1').any(['SD_1', 'SD_2']).build()

# Avoid
alarm = ControlBuilder('CTRL_1').any(['SD_1', 'SD_2']).build()
```

### Latch Critical Alarms

```python
# Good: Latch fire alarms
alarm = (
    ControlBuilder('FIRE_ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2'])
    .with_latch(True)  # Stay on
    .build()
)

# Avoid: Non-latching for safety-critical
alarm = ControlBuilder('FIRE_ALARM').any(['SD_1', 'SD_2']).build()  # Can turn off
```

### Add Appropriate Delays

```python
# Good: Delay to prevent false alarms
ctrl = (
    ControlBuilder('DELAYED_ALARM')
    .any(['SMOKE_DET_1', 'SMOKE_DET_2'])
    .with_delay(3.0)  # 3s to confirm
    .build()
)

# Avoid: No delay for fluctuating signals
ctrl = ControlBuilder('INSTANT').any(['NOISY_SENSOR']).build()
```

### Use ALL for Safety Interlocks

```python
# Good: Require multiple conditions for safety
safe_to_start = (
    ControlBuilder('SAFE_START')
    .all(['DOOR_CLOSED', 'POWER_OK', 'COOLANT_OK'])
    .build()
)
```

## Validation

The builder validates:

- **Function specified**: Must call one of: `.any()`, `.all()`, `.only()`, `.time_delay()`, `.kill()`, `.restart()`, `.custom()`
- **Valid device IDs**: Device IDs should exist (warning only)
- **No conflicting options**: Can't combine incompatible options

## See Also

- [User Guide - Control Logic](../../guide/controls.md)
- [User Guide - Builders](../../guide/builders.md)
- [PropBuilder](prop.md) - For device properties
- [User Guide - Devices & Measurement](../../guide/devices.md)
