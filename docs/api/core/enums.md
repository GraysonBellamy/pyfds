# Enumerations

::: pyfds.core.enums

## Overview

PyFDS provides enumeration types for FDS parameters to ensure type safety and valid values. All enums inherit from `str` and `Enum`, making them compatible with string comparisons while providing IDE autocomplete.

## Importing Enums

```python
from pyfds.core.enums import (
    ControlFunction,
    TurbulenceModel,
    VentShape,
    VentType,
    BackingCondition,
    HeatTransferModel,
    SolidGeometry,
    BuiltinSpecies,
    BuiltinSurface,
)
```

## Validation Enums

### Severity

Validation issue severity levels.

```python
from pyfds.validation import Severity

# Use in validation
if issue.severity == Severity.ERROR:
    print("Critical error")
elif issue.severity == Severity.WARNING:
    print("Warning")
elif issue.severity == Severity.INFO:
    print("Information")
```

**Values:**

- `ERROR`: Critical issues that prevent FDS from running
- `WARNING`: Issues that may cause problems
- `INFO`: Informational messages

## Surface Enums

### SolidGeometry

Solid phase geometry types for SURF namelist.

```python
from pyfds import Surface
from pyfds.core.enums import SolidGeometry

surface = Surface(
    id="WALL",
    geometry=SolidGeometry.CARTESIAN,
    thickness=0.2
)
```

**Values:**

- `CARTESIAN`: Rectangular coordinates
- `CYLINDRICAL`: Cylindrical coordinates
- `SPHERICAL`: Spherical coordinates
- `INNER_CYLINDRICAL`: Inner cylindrical surface

### BackingCondition

Backing conditions for surfaces.

```python
from pyfds import Surface
from pyfds.core.enums import BackingCondition

surface = Surface(
    id="WALL",
    backing=BackingCondition.INSULATED,
    thickness=0.2
)
```

**Values:**

- `VOID`: No backing (default)
- `INSULATED`: Insulated backing
- `EXPOSED`: Exposed to ambient

### HeatTransferModel

Heat transfer models for surfaces.

```python
from pyfds.core.enums import HeatTransferModel

surface = Surface(
    id="WALL",
    heat_transfer_model=HeatTransferModel.LOGLAW
)
```

**Values:**

- `LOGLAW`: Log law wall model
- `IMPINGING_JET`: Impinging jet model

### SprayPattern

Spray patterns for particle generation.

```python
from pyfds.core.enums import SprayPattern

prop = Property(
    id="SPRINKLER",
    spray_pattern=SprayPattern.GAUSSIAN
)
```

**Values:**

- `UNIFORM`: Uniform distribution
- `GAUSSIAN`: Gaussian distribution

## Global Settings Enums

### TurbulenceModel

LES turbulence models for MISC namelist.

```python
from pyfds import Misc
from pyfds.core.enums import TurbulenceModel

misc = Misc(
    turbulence_model=TurbulenceModel.DEARDORFF
)
```

**Values:**

- `DEARDORFF`: Deardorff model (default)
- `DYNAMIC_SMAGORINSKY`: Dynamic Smagorinsky model
- `VREMAN`: Vreman model
- `WALE`: WALE model

### SimulationMode

Simulation mode for MISC namelist.

```python
from pyfds.core.enums import SimulationMode

misc = Misc(
    simulation_mode=SimulationMode.LES
)
```

**Values:**

- `VLES`: Very Large Eddy Simulation
- `LES`: Large Eddy Simulation (default)
- `DNS`: Direct Numerical Simulation
- `SVLES`: Simplified VLES

### LESFilterType

LES filter type for subgrid quantities.

```python
from pyfds.core.enums import LESFilterType

misc = Misc(
    les_filter_type=LESFilterType.MEAN
)
```

**Values:**

- `MEAN`: Mean filter
- `MAX`: Maximum filter

## Ventilation Enums

### VentType

Types of vents in FDS.

```python
from pyfds import Vent
from pyfds.core.enums import VentType

vent = Vent(
    surf_id="OPEN",
    vent_type=VentType.OPEN,
    xb=(0, 0, 0, 2, 0, 2)
)
```

**Values:**

- `OPEN`: Open boundary
- `HVAC`: HVAC vent
- `SURFACE`: Surface patch
- `MIRROR`: Mirror boundary
- `PERIODIC`: Periodic boundary

### VentShape

Vent geometry shapes.

```python
from pyfds.core.enums import VentShape

vent = Vent(
    surf_id="INLET",
    shape=VentShape.CIRCULAR,
    radius=0.5
)
```

**Values:**

- `RECTANGULAR`: Rectangular vent (default)
- `CIRCULAR`: Circular vent

## Control Enums

### ControlFunction

Control function types for CTRL namelist.

```python
from pyfds import Control
from pyfds.core.enums import ControlFunction

ctrl = Control(
    id="ALARM",
    function_type=ControlFunction.ANY,
    input_id=["DET_1", "DET_2"]
)
```

**Values:**

- `ANY`: True if any input is true
- `ALL`: True if all inputs are true
- `ONLY`: True if exactly one input is true
- `TIME_DELAY`: Time-delayed activation
- `CUSTOM`: Custom function
- `KILL`: Kill simulation
- `RESTART`: Restart simulation

## Geometry Enums

### ObstShape

Geometric shape types for obstructions.

```python
from pyfds.core.enums import ObstShape

obst = Obstruction(
    shape=ObstShape.SPHERE,
    xb=(5, 5, 5, 5, 1, 1),
    radius=0.5
)
```

**Values:**

- `SPHERE`: Spherical shape
- `CYLINDER`: Cylindrical shape
- `CONE`: Conical shape
- `BOX`: Box shape (default)

### TextureMapping

Texture mapping types for GEOM.

```python
from pyfds.core.enums import TextureMapping

geom = Geometry(
    id="SURFACE",
    texture_mapping=TextureMapping.SPHERICAL
)
```

**Values:**

- `RECTANGULAR`: Rectangular mapping
- `SPHERICAL`: Spherical mapping

### CoordinateSystem

Coordinate system types.

```python
from pyfds.core.enums import CoordinateSystem

# Used internally for coordinate transformations
```

**Values:**

- `RECTANGULAR`: Cartesian coordinates
- `SPHERICAL`: Spherical coordinates
- `CYLINDRICAL`: Cylindrical coordinates

## Combustion Enums

### ExtinctionModel

Combustion extinction models.

```python
from pyfds import Combustion
from pyfds.core.enums import ExtinctionModel

comb = Combustion(
    extinction_model=ExtinctionModel.EXTINCTION_2
)
```

**Values:**

- `EXTINCTION_1`: Extinction model 1
- `EXTINCTION_2`: Extinction model 2

## Device Enums

### StatisticsType

Device statistics types.

```python
from pyfds import Device
from pyfds.core.enums import StatisticsType

device = Device(
    id="MAX_TEMP",
    quantity="TEMPERATURE",
    statistics=StatisticsType.MAX,
    xyz=(5, 5, 2)
)
```

**Values:**

- `MIN`: Minimum value
- `MAX`: Maximum value
- `MEAN`: Mean value
- `RMS`: Root mean square
- `VARIANCE`: Variance
- `RANGE`: Range (max - min)
- `TIME_MIN`: Time of minimum
- `TIME_MAX`: Time of maximum
- `COV`: Covariance
- `CORRCOEF`: Correlation coefficient

## Ramp Enums

### RampInterpolation

RAMP interpolation types.

```python
from pyfds import Ramp
from pyfds.core.enums import RampInterpolation

ramp = Ramp(
    id="FIRE_GROWTH",
    t=[0, 60, 120],
    f=[0, 0.5, 1.0],
    interpolation=RampInterpolation.LINEAR
)
```

**Values:**

- `LINEAR`: Linear interpolation (default)
- `STEP`: Step function (constant between points)

## Particle Enums

### DragLaw

Drag law for particles.

```python
from pyfds import Particle
from pyfds.core.enums import DragLaw

part = Particle(
    id="DROPLET",
    drag_law=DragLaw.SPHERE
)
```

**Values:**

- `SPHERE`: Spherical drag
- `CYLINDER`: Cylindrical drag
- `SCREEN`: Screen drag

## Mesh Enums

### MeshBoundary

Mesh boundary locations.

```python
from pyfds.core.enums import MeshBoundary

# Used for mesh boundary conditions
boundary = MeshBoundary.XMIN  # Western boundary
```

**Values:**

- `XMIN`: Western boundary (negative x)
- `XMAX`: Eastern boundary (positive x)
- `YMIN`: Southern boundary (negative y)
- `YMAX`: Northern boundary (positive y)
- `ZMIN`: Bottom boundary (negative z)
- `ZMAX`: Top boundary (positive z)

## Built-in FDS Objects

### BuiltinSurface

Built-in FDS surfaces that don't require explicit definition.

```python
from pyfds import Obstruction
from pyfds.core.enums import BuiltinSurface
from pyfds.core.geometry import Bounds3D

# Can reference without creating SURF namelist
obst = Obstruction(
    xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
    surf_id=BuiltinSurface.INERT
)

# Check if surface is built-in
if surface_id in BuiltinSurface.values():
    print("No need to define this surface")
```

**Values:**

- `INERT`: Inert surface (default wall)
- `OPEN`: Open boundary
- `MIRROR`: Mirror boundary
- `PERIODIC`: Periodic boundary
- `HVAC`: HVAC surface
- `MASSLESS_TRACER`: Massless tracer particles
- `DROPLET`: Water droplets
- `VEGETATION`: Vegetation surface
- `EVACUATION`: Evacuation surface

### BuiltinSpecies

Built-in FDS species that don't require explicit definition.

```python
from pyfds import Reaction
from pyfds.core.enums import BuiltinSpecies

# Can reference without creating SPEC namelist
reaction = Reaction(
    fuel="PROPANE",
    spec_id_n2=BuiltinSpecies.NITROGEN
)

# Check if species is built-in
if species_id in BuiltinSpecies.values():
    print("No need to define this species")
```

**Values:**

- `AIR`: Air (mixture)
- `PRODUCTS`: Combustion products
- `SOOT`: Soot particles
- `WATER_VAPOR`: Water vapor
- `CARBON_DIOXIDE`: CO₂
- `CARBON_MONOXIDE`: CO
- `NITROGEN`: N₂
- `OXYGEN`: O₂

## Type Safety

Enums provide type safety and IDE autocomplete:

```python
from pyfds import Control
from pyfds.core.enums import ControlFunction

# Type-safe: IDE suggests valid values
ctrl = Control(
    id="ALARM",
    function_type=ControlFunction.ANY,  # IDE autocompletes: ANY, ALL, ONLY, ...
    input_id=["DET_1", "DET_2"]
)

# String values also work (backward compatible)
ctrl = Control(
    id="ALARM",
    function_type="ANY",
    input_id=["DET_1", "DET_2"]
)
```

## See Also

- [Namelists](../namelists/index.md) - Namelist classes using these enums
- [Validation](../utils/validation.md) - Using Severity enum
- [User Guide](../../guide/index.md) - Practical usage examples
