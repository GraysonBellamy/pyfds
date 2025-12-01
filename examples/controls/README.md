# Controls Examples

This directory contains examples demonstrating FDS control logic using
CTRL and DEVC namelists for automated simulation behavior.

## Examples

### device_activation.py
Demonstrates device setpoint triggering using DEVC parameters:
- `SETPOINT`: Value at which device activates
- `TRIP_DIRECTION`: Direction of crossing (+1 above, -1 below, 0 both)
- `DEVC_ID` on OBST: Links obstruction visibility to device state

### control_logic.py
Demonstrates boolean control functions via CTRL namelist:
- `TIME_DELAY`: Triggers after specified delay from input
- `ALL`: Triggers when all inputs are true (AND logic)
- `ANY`: Triggers when any input is true (OR logic)
- `INITIAL_STATE`: Sets starting state of control

### vent_activation.py
Demonstrates automated vent control:
- `CTRL_ID` on VENT: Links vent to control logic
- `DEVC_ID` on VENT: Links vent directly to device
- Tracer particles for flow visualization

### hrr_freeze.py
Demonstrates freezing HRR at a setpoint:
- `RAMP` with `DEVC_ID`: Links ramp to a time-tracking device
- `NO_UPDATE_DEVC_ID`: Stops device updates when trigger activates
- Useful for maintaining steady-state conditions after reaching a threshold

## Key Concepts

### Device Setpoints
```python
Device(
    id="TEMP_SENSOR",
    xyz=Point3D(1.0, 1.0, 1.5),
    quantity="TEMPERATURE",
    setpoint=100.0,      # Trigger at 100°C
    trip_direction=1,    # Trigger when rising above
)
```

### Control Logic
```python
# Time delay control
Control(
    id="DELAY_CTRL",
    function_type=ControlFunction.TIME_DELAY,
    input_id="TIMER_3S",
    delay=2.0,
)

# ALL logic (AND)
Control(
    id="ALL_CTRL",
    function_type=ControlFunction.ALL,
    input_id=["TIMER_8S", "TEMP_50C"],
)
```

### Controlled Objects
```python
# Obstruction removed when device triggers
Obstruction(
    id="BLOCK",
    xb=Bounds3D.of(...),
    devc_id="TEMP_SENSOR",  # Direct device control
)

# Vent activated by control logic
Vent(
    xb=Bounds3D.of(...),
    surf_id="FIRE",
    ctrl_id="DELAY_CTRL",  # Control logic
)
```

## FDS References
- [Controls/device_test.fds](https://github.com/firemodels/fds/blob/master/Verification/Controls/device_test.fds)
- [Controls/control_test.fds](https://github.com/firemodels/fds/blob/master/Verification/Controls/control_test.fds)
- [Controls/activate_vents.fds](https://github.com/firemodels/fds/blob/master/Verification/Controls/activate_vents.fds)
- [Controls/hrr_freeze.fds](https://github.com/firemodels/fds/blob/master/Verification/Controls/hrr_freeze.fds)
