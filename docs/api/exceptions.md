# Exceptions

::: pyfds.exceptions

## Overview

PyFDS defines a hierarchy of exceptions for different error scenarios. All PyFDS exceptions inherit from `PyFDSError`.

## Exception Hierarchy

```
PyFDSError (base)
├── ValidationError
│   ├── DuplicateIdError
│   └── UnknownIdError
├── ConfigurationError
├── ExecutionError
│   ├── FDSNotFoundError
│   └── FDSTimeoutError
└── ParseError
```

## Importing Exceptions

```python
from pyfds.exceptions import (
    PyFDSError,
    ValidationError,
    DuplicateIdError,
    UnknownIdError,
    ConfigurationError,
    ExecutionError,
    FDSNotFoundError,
    FDSTimeoutError,
    ParseError,
)
```

## Base Exception

### PyFDSError

Base exception for all PyFDS errors. Catch this to handle any PyFDS-specific exception.

```python
from pyfds import Simulation
from pyfds.exceptions import PyFDSError

try:
    sim = Simulation(chid="test")
    # ... simulation setup ...
    sim.run()
except PyFDSError as e:
    print(f"PyFDS error: {e}")
```

## Validation Exceptions

### ValidationError

Raised when input validation fails.

```python
from pyfds import Simulation, Mesh
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.exceptions import ValidationError

sim = Simulation(chid="test")

try:
    # Invalid bounds (x1 < x0)
    mesh = Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(5, 0, 0, 5, 0, 3))
except ValidationError as e:
    print(f"Validation error: {e}")
    print(f"Field: {e.field}")
    print(f"Value: {e.value}")
```

**Attributes:**

- `field` (str | None): The field that failed validation
- `value` (Any): The invalid value

### DuplicateIdError

Raised when attempting to add an object with a duplicate ID.

```python
from pyfds import Simulation, Surface
from pyfds.exceptions import DuplicateIdError

sim = Simulation(chid="test")
sim.add(Surface(id="FIRE", hrrpua=1000))

try:
    sim.add(Surface(id="FIRE", hrrpua=2000))  # Duplicate ID
except DuplicateIdError as e:
    print(f"Duplicate ID error: {e}")
```

**Inherits from:** `ValidationError`

### UnknownIdError

Raised when referencing an undefined ID.

```python
from pyfds import Simulation, Obstruction
from pyfds.core.geometry import Bounds3D
from pyfds.exceptions import UnknownIdError

sim = Simulation(chid="test")

try:
    # Reference to undefined surface
    sim.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), surf_id="UNDEFINED"))
    sim.write("test.fds")  # Validation happens here
except UnknownIdError as e:
    print(f"Unknown ID: {e}")
```

**Inherits from:** `ValidationError`

### ConfigurationError

Raised when the simulation configuration is invalid.

```python
from pyfds.exceptions import ConfigurationError

try:
    # Invalid configuration scenario
    ...
except ConfigurationError as e:
    print(f"Configuration error: {e}")
```

## Execution Exceptions

### ExecutionError

Raised when FDS execution fails.

```python
from pyfds import run_fds
from pyfds.exceptions import ExecutionError

try:
    results = run_fds("simulation.fds")
except ExecutionError as e:
    print(f"Execution failed: {e}")
    print(f"Exit code: {e.exit_code}")
    print(f"Stderr: {e.stderr}")
```

**Attributes:**

- `exit_code` (int | None): FDS process exit code
- `stdout` (str | None): Standard output from FDS
- `stderr` (str | None): Standard error from FDS
- `fds_file` (str | None): Path to the FDS input file

### FDSNotFoundError

Raised when the FDS executable cannot be found.

```python
from pyfds import run_fds
from pyfds.exceptions import FDSNotFoundError

try:
    results = run_fds("simulation.fds")
except FDSNotFoundError as e:
    print(f"FDS not found: {e}")
    print("Please install FDS or set FDS_PATH environment variable")
```

**Inherits from:** `ExecutionError`

### FDSTimeoutError

Raised when FDS execution exceeds the timeout limit.

```python
from pyfds import run_fds
from pyfds.config import RunConfig
from pyfds.exceptions import FDSTimeoutError

try:
    results = run_fds("simulation.fds", config=RunConfig(timeout=60))
except FDSTimeoutError as e:
    print(f"Execution timed out: {e}")
```

**Inherits from:** `ExecutionError`

## Parsing Exceptions

### ParseError

Raised when parsing an FDS file fails.

```python
from pyfds import parse_fds
from pyfds.exceptions import ParseError

try:
    sim = parse_fds("invalid.fds")
except ParseError as e:
    print(f"Parse error: {e}")
```

## Error Handling Patterns

### Specific Exception Handling

```python
from pyfds import Simulation, run_fds
from pyfds.exceptions import (
    ValidationError,
    FDSNotFoundError,
    ExecutionError,
)

sim = Simulation(chid="test")
# ... build simulation ...

try:
    sim.write("test.fds")
    results = run_fds("test.fds")
except ValidationError as e:
    print(f"Invalid configuration: {e}")
    # Fix configuration
except FDSNotFoundError:
    print("FDS not installed")
    # Guide user to installation
except ExecutionError as e:
    print(f"Simulation failed: {e}")
    # Check output files, adjust parameters
```

### Catch-All Handling

```python
from pyfds import Simulation
from pyfds.exceptions import PyFDSError

try:
    sim = Simulation(chid="test")
    # ... simulation workflow ...
except PyFDSError as e:
    print(f"PyFDS error: {e}")
    # Handle any PyFDS-specific error
except Exception as e:
    print(f"Unexpected error: {e}")
    # Handle other errors
```

### Re-raising with Context

```python
from pyfds import run_fds
from pyfds.exceptions import ExecutionError

def run_batch_simulations(sim_files):
    """Run multiple simulations with error context."""
    results = []
    for fds_file in sim_files:
        try:
            results.append(run_fds(fds_file))
        except ExecutionError as e:
            raise ExecutionError(
                f"Failed in batch at {fds_file}: {e}",
                exit_code=e.exit_code,
                fds_file=fds_file
            ) from e
    return results
```

## See Also

- [Validation](utils/validation.md) - Input validation
- [Troubleshooting](../reference/troubleshooting.md) - Common errors and solutions
- [Execution Runner](execution/runner.md) - Running simulations
- [Job Management](execution/job.md) - Managing running jobs
