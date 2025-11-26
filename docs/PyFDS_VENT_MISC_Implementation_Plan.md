# PyFDS Implementation Plan: VENT and MISC Namelists

## Executive Summary

This document provides a comprehensive implementation plan for the `&VENT` and `&MISC` namelist groups in the PyFDS library. VENT handles boundary conditions, openings, and surface patches, while MISC contains miscellaneous simulation parameters that affect global behavior.

---

## Table of Contents

1. [VENT Namelist Implementation](#1-vent-namelist-implementation)
   - [Overview](#11-overview)
   - [Class Design](#12-class-design)
   - [Vent Types](#13-vent-types)
   - [Validation Rules](#14-validation-rules)
   - [Integration Points](#15-integration-points)
2. [MISC Namelist Implementation](#2-misc-namelist-implementation)
   - [Overview](#21-overview)
   - [Class Design](#22-class-design)
   - [Parameter Categories](#23-parameter-categories)
   - [Validation Rules](#24-validation-rules)
3. [Implementation Architecture](#3-implementation-architecture)
4. [Testing Strategy](#4-testing-strategy)
5. [Code Examples](#5-code-examples)
6. [Implementation Checklist](#6-implementation-checklist)

---

## 1. VENT Namelist Implementation

### 1.1 Overview

The `&VENT` namelist in FDS defines:
- Openings to the ambient (SURF_ID='OPEN')
- Supply and exhaust vents (SURF_ID='HVAC')
- Surface patches with different properties on obstructions
- Circular and non-rectangular vents
- Pressure boundaries

**Key Characteristics:**
- VENTs must align with mesh boundaries or obstruction faces
- VENTs override underlying obstruction surface properties
- Can be time-activated via DEVC_ID or CTRL_ID
- Support geometric transformations (rotation, spread)

### 1.2 Class Design

```python
# File: pyfds/namelists/vent.py

from typing import Optional, Tuple, List, Union, Dict
from dataclasses import dataclass, field
from enum import Enum
import numpy as np

class VentType(Enum):
    """Types of vents in FDS."""
    OPEN = 'OPEN'           # Opening to ambient
    HVAC = 'HVAC'          # HVAC connection
    SURFACE = 'SURFACE'     # Surface patch
    MIRROR = 'MIRROR'       # Mirror boundary
    PERIODIC = 'PERIODIC'   # Periodic boundary

class VentShape(Enum):
    """Vent geometry types."""
    RECTANGULAR = 'RECTANGULAR'
    CIRCULAR = 'CIRCULAR'
    ELLIPTICAL = 'ELLIPTICAL'
    ANNULAR = 'ANNULAR'

@dataclass
class Vent:
    """
    Represents FDS &VENT namelist for boundary conditions and openings.

    Attributes:
        xb: Bounding box coordinates (x1, x2, y1, y2, z1, z2)
        surf_id: Surface properties ID
        vent_type: Type of vent (determined from surf_id)

        # Geometric modifiers
        xyz: Center point for circular vents
        radius: Radius for circular vents
        radius_inner: Inner radius for annular vents
        spread_rate: Fire spread rate [m/s]

        # HVAC parameters
        volume_flow: Volume flow rate [m³/s]
        mass_flow: Mass flow rate [kg/s]
        vel: Velocity [m/s]

        # Control
        devc_id: Device ID for activation
        ctrl_id: Control ID for activation
        delay: Activation delay [s]
        t_activate: Activation time [s]

        # Mesh boundary
        mb: Mesh boundary location ('XMIN', 'XMAX', etc.)
        mesh_id: Specific mesh ID

        # Advanced
        dynamic_pressure: Use dynamic pressure BC
        tmp_exterior: Exterior temperature [°C]
        rho_exterior: Exterior density [kg/m³]

        # Color/visualization
        color: RGB color tuple or name
        transparency: Transparency value [0-1]
        outline: Show outline in Smokeview
    """
    # Required parameters
    xb: Optional[Tuple[float, float, float, float, float, float]] = None
    surf_id: str = 'INERT'

    # Geometric modifiers
    xyz: Optional[Tuple[float, float, float]] = None
    radius: Optional[float] = None
    radius_inner: Optional[float] = None
    spread_rate: Optional[float] = None

    # HVAC parameters
    volume_flow: Optional[float] = None
    mass_flow: Optional[float] = None
    vel: Optional[float] = None

    # Control
    devc_id: Optional[str] = None
    ctrl_id: Optional[str] = None
    delay: float = 0.0
    t_activate: Optional[float] = None

    # Mesh boundary
    mb: Optional[str] = None
    mesh_id: Optional[str] = None

    # Advanced
    dynamic_pressure: bool = False
    tmp_exterior: Optional[float] = None
    rho_exterior: Optional[float] = None

    # Visualization
    color: Optional[Union[str, Tuple[int, int, int]]] = None
    transparency: float = 1.0
    outline: bool = True

    # Derived properties
    vent_type: Optional[VentType] = None
    shape: VentShape = VentShape.RECTANGULAR
    area: Optional[float] = None

    def __post_init__(self):
        """Initialize and validate vent."""
        self._determine_type()
        self._determine_shape()
        self._calculate_area()
        self._validate()

    def _determine_type(self):
        """Determine vent type from surf_id."""
        if self.surf_id == 'OPEN':
            self.vent_type = VentType.OPEN
        elif self.surf_id == 'HVAC':
            self.vent_type = VentType.HVAC
        elif self.surf_id == 'MIRROR':
            self.vent_type = VentType.MIRROR
        elif self.surf_id == 'PERIODIC':
            self.vent_type = VentType.PERIODIC
        else:
            self.vent_type = VentType.SURFACE

    def _determine_shape(self):
        """Determine vent shape from parameters."""
        if self.xyz and self.radius:
            if self.radius_inner:
                self.shape = VentShape.ANNULAR
            else:
                self.shape = VentShape.CIRCULAR
        else:
            self.shape = VentShape.RECTANGULAR

    def _calculate_area(self):
        """Calculate vent area based on geometry."""
        if self.shape == VentShape.RECTANGULAR and self.xb:
            dx = self.xb[1] - self.xb[0]
            dy = self.xb[3] - self.xb[2]
            dz = self.xb[5] - self.xb[4]

            # Area is the non-zero face
            if abs(dx) < 1e-6:
                self.area = dy * dz
            elif abs(dy) < 1e-6:
                self.area = dx * dz
            elif abs(dz) < 1e-6:
                self.area = dx * dy

        elif self.shape == VentShape.CIRCULAR and self.radius:
            self.area = np.pi * self.radius**2

        elif self.shape == VentShape.ANNULAR and self.radius and self.radius_inner:
            self.area = np.pi * (self.radius**2 - self.radius_inner**2)

    def _validate(self):
        """Validate vent parameters."""
        # Must have either XB or MB
        if not self.xb and not self.mb:
            raise ValueError("Vent must have either XB or MB specified")

        # XB validation
        if self.xb:
            # Check that exactly one dimension is zero (defines a plane)
            dims = [
                self.xb[1] - self.xb[0],
                self.xb[3] - self.xb[2],
                self.xb[5] - self.xb[4]
            ]
            zero_dims = sum(1 for d in dims if abs(d) < 1e-6)

            if zero_dims != 1:
                raise ValueError(
                    f"Vent must be a plane (exactly one dimension zero), "
                    f"got dimensions: {dims}"
                )

        # Circular vent validation
        if self.shape in [VentShape.CIRCULAR, VentShape.ANNULAR]:
            if not self.xyz or not self.radius:
                raise ValueError(
                    f"{self.shape.value} vent requires XYZ and RADIUS"
                )

            if self.shape == VentShape.ANNULAR and not self.radius_inner:
                raise ValueError("Annular vent requires RADIUS_INNER")

            if self.radius_inner and self.radius_inner >= self.radius:
                raise ValueError(
                    f"RADIUS_INNER ({self.radius_inner}) must be less than "
                    f"RADIUS ({self.radius})"
                )

        # HVAC validation
        if self.vent_type == VentType.HVAC:
            flow_params = [self.volume_flow, self.mass_flow, self.vel]
            if sum(p is not None for p in flow_params) > 1:
                raise ValueError(
                    "HVAC vent can only specify one of: "
                    "VOLUME_FLOW, MASS_FLOW, or VEL"
                )

        # MB validation
        if self.mb:
            valid_mb = ['XMIN', 'XMAX', 'YMIN', 'YMAX', 'ZMIN', 'ZMAX']
            if self.mb not in valid_mb:
                raise ValueError(
                    f"MB must be one of {valid_mb}, got '{self.mb}'"
                )

    def to_fds(self) -> str:
        """Generate FDS namelist string."""
        lines = ["&VENT"]

        # Geometry
        if self.xb:
            xb_str = ','.join(f'{x:.3f}' for x in self.xb)
            lines.append(f"      XB={xb_str}")

        if self.mb:
            lines.append(f"      MB='{self.mb}'")
            if self.mesh_id:
                lines.append(f"      MESH_ID='{self.mesh_id}'")

        # Surface
        lines.append(f"      SURF_ID='{self.surf_id}'")

        # Circular geometry
        if self.xyz:
            xyz_str = ','.join(f'{x:.3f}' for x in self.xyz)
            lines.append(f"      XYZ={xyz_str}")

        if self.radius is not None:
            lines.append(f"      RADIUS={self.radius:.3f}")

        if self.radius_inner is not None:
            lines.append(f"      RADIUS_INNER={self.radius_inner:.3f}")

        # HVAC parameters
        if self.volume_flow is not None:
            lines.append(f"      VOLUME_FLOW={self.volume_flow:.3f}")

        if self.mass_flow is not None:
            lines.append(f"      MASS_FLOW={self.mass_flow:.3f}")

        if self.vel is not None:
            lines.append(f"      VEL={self.vel:.3f}")

        # Control
        if self.devc_id:
            lines.append(f"      DEVC_ID='{self.devc_id}'")

        if self.ctrl_id:
            lines.append(f"      CTRL_ID='{self.ctrl_id}'")

        if self.delay > 0:
            lines.append(f"      DELAY={self.delay:.1f}")

        # Advanced
        if self.dynamic_pressure:
            lines.append(f"      DYNAMIC_PRESSURE=T")

        if self.tmp_exterior is not None:
            lines.append(f"      TMP_EXTERIOR={self.tmp_exterior:.1f}")

        # Visualization
        if self.color:
            if isinstance(self.color, str):
                lines.append(f"      COLOR='{self.color}'")
            else:
                rgb_str = ','.join(str(c) for c in self.color)
                lines.append(f"      RGB={rgb_str}")

        if not self.outline:
            lines.append(f"      OUTLINE=F")

        lines[-1] += " /"
        return '\n'.join(lines)


class VentBuilder:
    """Builder for creating vents with common patterns."""

    def __init__(self):
        self.vent = Vent()

    def at_position(self, xb: Tuple[float, float, float, float, float, float]) -> 'VentBuilder':
        """Set vent position via bounding box."""
        self.vent.xb = xb
        return self

    def on_mesh_boundary(self, mb: str, mesh_id: Optional[str] = None) -> 'VentBuilder':
        """Place vent on mesh boundary."""
        self.vent.mb = mb
        self.vent.mesh_id = mesh_id
        return self

    def with_surface(self, surf_id: str) -> 'VentBuilder':
        """Set surface properties."""
        self.vent.surf_id = surf_id
        return self

    def circular(self, center: Tuple[float, float, float],
                radius: float, inner_radius: Optional[float] = None) -> 'VentBuilder':
        """Create circular or annular vent."""
        self.vent.xyz = center
        self.vent.radius = radius
        self.vent.radius_inner = inner_radius
        return self

    def hvac(self, volume_flow: Optional[float] = None,
            mass_flow: Optional[float] = None,
            velocity: Optional[float] = None) -> 'VentBuilder':
        """Configure as HVAC vent."""
        self.vent.surf_id = 'HVAC'
        self.vent.volume_flow = volume_flow
        self.vent.mass_flow = mass_flow
        self.vent.vel = velocity
        return self

    def with_control(self, devc_id: Optional[str] = None,
                    ctrl_id: Optional[str] = None,
                    delay: float = 0.0) -> 'VentBuilder':
        """Add control activation."""
        self.vent.devc_id = devc_id
        self.vent.ctrl_id = ctrl_id
        self.vent.delay = delay
        return self

    def build(self) -> Vent:
        """Build and validate vent."""
        return self.vent
```

### 1.3 Vent Types

#### 1.3.1 Open Vents
```python
# Opening to ambient
open_vent = Vent(
    xb=(10, 10, 2, 4, 0, 3),  # Door opening
    surf_id='OPEN'
)
```

#### 1.3.2 HVAC Vents
```python
# Supply vent
supply = VentBuilder() \
    .at_position((5, 6, 5, 6, 3, 3)) \
    .hvac(volume_flow=0.5) \
    .build()

# Exhaust vent
exhaust = VentBuilder() \
    .at_position((8, 9, 8, 9, 3, 3)) \
    .hvac(volume_flow=-0.3) \
    .build()
```

#### 1.3.3 Circular Vents
```python
# Circular burner
burner = VentBuilder() \
    .at_position((-1, 1, -1, 1, 0, 0)) \
    .circular(center=(0, 0, 0), radius=0.5) \
    .with_surface('FIRE') \
    .build()
```

#### 1.3.4 Surface Patches
```python
# Hot patch on obstruction
patch = Vent(
    xb=(2, 3, 2, 2, 1, 2),  # On face of obstruction
    surf_id='HOT_SURFACE'
)
```

### 1.4 Validation Rules

1. **Geometric Validation**
   - XB must define a plane (one dimension = 0)
   - Circular vents need XB large enough to contain circle
   - Must align with mesh boundaries (checked at runtime)

2. **Surface Reference**
   - surf_id must exist in simulation
   - Special handling for 'OPEN', 'HVAC', 'MIRROR', 'PERIODIC'

3. **Control Validation**
   - devc_id must reference existing device
   - ctrl_id must reference existing control

4. **HVAC Constraints**
   - Only one flow specification allowed
   - Volume flow in m³/s
   - Positive = inflow, Negative = outflow

### 1.5 Integration Points

```python
class Simulation:
    def add_vent(self, vent: Vent) -> Vent:
        """Add vent with validation."""
        # Check surface reference
        if vent.surf_id not in ['OPEN', 'HVAC', 'MIRROR', 'PERIODIC']:
            if vent.surf_id not in self.surfaces:
                raise ValueError(f"Surface '{vent.surf_id}' not found")

        # Check control references
        if vent.devc_id and vent.devc_id not in self.devices:
            raise ValueError(f"Device '{vent.devc_id}' not found")

        if vent.ctrl_id and vent.ctrl_id not in self.controls:
            raise ValueError(f"Control '{vent.ctrl_id}' not found")

        # Check alignment with obstructions (if applicable)
        if vent.vent_type == VentType.SURFACE:
            # Verify vent is on obstruction face
            pass  # Implementation depends on obstruction tracking

        self.vents.append(vent)
        return vent
```

---

## 2. MISC Namelist Implementation

### 2.1 Overview

The `&MISC` namelist contains miscellaneous parameters that affect global simulation behavior:
- Numerical parameters (CFL, turbulence models)
- Physical parameters (ambient conditions, gravity)
- Solver options (solid phase only, isothermal)
- Output controls (default behaviors)
- Advanced settings (restart, coupling)

**Key Characteristics:**
- Only one MISC line per simulation
- Parameters affect entire domain
- Many have significant performance impacts
- Some enable/disable major features

### 2.2 Class Design

```python
# File: pyfds/namelists/misc.py

from typing import Optional, Tuple, List
from dataclasses import dataclass, field
from enum import Enum

class TurbulenceModel(Enum):
    """LES turbulence models."""
    DEARDORFF = 'DEARDORFF'
    DYNAMIC_SMAGORINSKY = 'DYNAMIC SMAGORINSKY'
    VREMAN = 'VREMAN'
    WALE = 'WALE'
    RNG = 'RNG'

class FluxLimiter(Enum):
    """Flux limiter schemes."""
    GODUNOV = 1
    SUPERBEE = 2
    MINMOD = 3
    CHARM = 4

@dataclass
class Misc:
    """
    Represents FDS &MISC namelist for miscellaneous parameters.

    This class contains global simulation parameters that don't fit
    into other namelist groups but significantly affect simulation
    behavior.

    Attributes:
        # Ambient conditions
        tmpa: Ambient temperature [°C]
        p_inf: Background pressure [Pa]
        humidity: Relative humidity [%]

        # Gravity
        gvec: Gravity vector [m/s²]

        # Turbulence
        turbulence_model: LES model selection
        c_deardorff: Deardorff model constant
        c_smagorinsky: Smagorinsky constant
        c_vreman: Vreman model constant
        c_wale: WALE model constant

        # Numerics
        cfl_max: Maximum CFL number
        cfl_min: Minimum CFL number
        flux_limiter: Flux limiter scheme

        # Solver options
        solid_phase_only: Only solve solid heat transfer
        isothermal: Isothermal flow calculation
        stratification: Include stratification

        # Special modes
        level_set_mode: Wildfire spread mode
        particle_cfl: Use particle CFL

        # Output control
        bndf_default: Default boundary file output

        # Performance
        max_chemistry_iterations: Chemistry solver iterations

        # Restart
        restart: Enable restart capability
        restart_chid: CHID for restart file
    """

    # Ambient conditions
    tmpa: float = 20.0  # °C
    p_inf: float = 101325.0  # Pa
    humidity: float = 40.0  # %

    # Gravity
    gvec: Tuple[float, float, float] = (0.0, 0.0, -9.81)

    # Turbulence model
    turbulence_model: TurbulenceModel = TurbulenceModel.DEARDORFF
    c_deardorff: float = 0.1
    c_smagorinsky: float = 0.2
    c_vreman: float = 0.07
    c_wale: float = 0.6
    les_filter_width: Optional[float] = None

    # Numerical parameters
    cfl_max: float = 1.0
    cfl_min: float = 0.8
    cfl_velocity_norm: int = 2
    flux_limiter: FluxLimiter = FluxLimiter.SUPERBEE

    # Solver options
    solid_phase_only: bool = False
    isothermal: bool = False
    stratification: bool = True
    radiation: bool = True
    suppression: bool = True

    # Combustion
    max_chemistry_iterations: int = 3
    initial_unmixed_fraction: float = 0.0
    extinction_model: str = 'EXTINCTION 1'

    # Special calculation modes
    level_set_mode: Optional[int] = None
    particle_cfl: bool = True
    hvac_mass_transport: bool = False

    # Output control
    bndf_default: bool = True
    verbose: bool = False

    # Domain
    allow_underside_particles: bool = False
    allow_surface_particles: bool = True

    # Performance tuning
    porous_floor: bool = True
    texture_origin: Tuple[float, float, float] = (0.0, 0.0, 0.01)

    # Restart
    restart: bool = False
    restart_chid: Optional[str] = None

    # Experimental features
    second_order_interpolation: bool = True
    noise: bool = False

    # Constants
    pr: float = 0.7  # Prandtl number
    sc: float = 0.7  # Schmidt number

    # Time control
    lock_time_step: bool = False
    restrict_time_step: bool = True

    # Validation
    check_ht: bool = False  # Check heat transfer
    check_vn: bool = True   # Check von Neumann

    def __post_init__(self):
        """Validate MISC parameters."""
        self._validate_ambient()
        self._validate_numerics()
        self._validate_turbulence()
        self._validate_modes()

    def _validate_ambient(self):
        """Validate ambient conditions."""
        if not -273.15 < self.tmpa < 2000:
            raise ValueError(
                f"TMPA ({self.tmpa}°C) outside reasonable range"
            )

        if not 0 < self.p_inf < 1e6:
            raise ValueError(
                f"P_INF ({self.p_inf} Pa) outside reasonable range"
            )

        if not 0 <= self.humidity <= 100:
            raise ValueError(
                f"HUMIDITY ({self.humidity}%) must be 0-100"
            )

    def _validate_numerics(self):
        """Validate numerical parameters."""
        if not 0.1 <= self.cfl_max <= 10:
            raise ValueError(
                f"CFL_MAX ({self.cfl_max}) outside reasonable range [0.1, 10]"
            )

        if not 0.1 <= self.cfl_min <= self.cfl_max:
            raise ValueError(
                f"CFL_MIN ({self.cfl_min}) must be less than CFL_MAX ({self.cfl_max})"
            )

        if self.cfl_velocity_norm not in [0, 1, 2]:
            raise ValueError(
                f"CFL_VELOCITY_NORM must be 0, 1, or 2, got {self.cfl_velocity_norm}"
            )

    def _validate_turbulence(self):
        """Validate turbulence parameters."""
        if self.c_deardorff < 0 or self.c_deardorff > 1:
            raise ValueError(
                f"C_DEARDORFF ({self.c_deardorff}) must be in [0, 1]"
            )

        if self.c_smagorinsky < 0 or self.c_smagorinsky > 1:
            raise ValueError(
                f"C_SMAGORINSKY ({self.c_smagorinsky}) must be in [0, 1]"
            )

    def _validate_modes(self):
        """Validate special calculation modes."""
        if self.solid_phase_only and self.isothermal:
            raise ValueError(
                "Cannot use both SOLID_PHASE_ONLY and ISOTHERMAL"
            )

        if self.level_set_mode is not None:
            if self.level_set_mode not in [0, 1, 2]:
                raise ValueError(
                    f"LEVEL_SET_MODE must be 0, 1, or 2, got {self.level_set_mode}"
                )

    def to_fds(self) -> str:
        """Generate FDS namelist string."""
        lines = ["&MISC"]

        # Ambient conditions
        if self.tmpa != 20.0:
            lines.append(f"      TMPA={self.tmpa:.1f}")

        if self.p_inf != 101325.0:
            lines.append(f"      P_INF={self.p_inf:.0f}")

        if self.humidity != 40.0:
            lines.append(f"      HUMIDITY={self.humidity:.1f}")

        # Gravity (if non-standard)
        if self.gvec != (0.0, 0.0, -9.81):
            gvec_str = ','.join(f'{g:.2f}' for g in self.gvec)
            lines.append(f"      GVEC={gvec_str}")

        # Turbulence model
        if self.turbulence_model != TurbulenceModel.DEARDORFF:
            lines.append(f"      TURBULENCE_MODEL='{self.turbulence_model.value}'")

        # Turbulence constants (if non-default)
        if self.c_deardorff != 0.1:
            lines.append(f"      C_DEARDORFF={self.c_deardorff:.2f}")

        if self.c_smagorinsky != 0.2:
            lines.append(f"      C_SMAGORINSKY={self.c_smagorinsky:.2f}")

        # CFL parameters
        if self.cfl_max != 1.0:
            lines.append(f"      CFL_MAX={self.cfl_max:.2f}")

        if self.cfl_min != 0.8:
            lines.append(f"      CFL_MIN={self.cfl_min:.2f}")

        # Special modes
        if self.solid_phase_only:
            lines.append(f"      SOLID_PHASE_ONLY=T")

        if self.isothermal:
            lines.append(f"      ISOTHERMAL=T")

        if not self.radiation:
            lines.append(f"      RADIATION=F")

        if not self.suppression:
            lines.append(f"      SUPPRESSION=F")

        # Level set mode for wildfire
        if self.level_set_mode is not None:
            lines.append(f"      LEVEL_SET_MODE={self.level_set_mode}")

        # Restart
        if self.restart:
            lines.append(f"      RESTART=T")
            if self.restart_chid:
                lines.append(f"      RESTART_CHID='{self.restart_chid}'")

        # Output control
        if not self.bndf_default:
            lines.append(f"      BNDF_DEFAULT=F")

        if self.verbose:
            lines.append(f"      VERBOSE=T")

        lines[-1] += " /"
        return '\n'.join(lines)


class MiscBuilder:
    """Builder for MISC parameters with common configurations."""

    def __init__(self):
        self.misc = Misc()

    def ambient_conditions(self, temperature: float = 20.0,
                          pressure: float = 101325.0,
                          humidity: float = 40.0) -> 'MiscBuilder':
        """Set ambient conditions."""
        self.misc.tmpa = temperature
        self.misc.p_inf = pressure
        self.misc.humidity = humidity
        return self

    def turbulence(self, model: TurbulenceModel,
                  **constants) -> 'MiscBuilder':
        """Configure turbulence model."""
        self.misc.turbulence_model = model

        if 'c_deardorff' in constants:
            self.misc.c_deardorff = constants['c_deardorff']
        if 'c_smagorinsky' in constants:
            self.misc.c_smagorinsky = constants['c_smagorinsky']
        if 'c_vreman' in constants:
            self.misc.c_vreman = constants['c_vreman']
        if 'c_wale' in constants:
            self.misc.c_wale = constants['c_wale']

        return self

    def cfl_control(self, cfl_min: float = 0.8,
                   cfl_max: float = 1.0) -> 'MiscBuilder':
        """Set CFL number control."""
        self.misc.cfl_min = cfl_min
        self.misc.cfl_max = cfl_max
        return self

    def solid_phase_only(self) -> 'MiscBuilder':
        """Enable solid phase only calculation."""
        self.misc.solid_phase_only = True
        self.misc.radiation = True  # Usually want radiation
        return self

    def isothermal_flow(self) -> 'MiscBuilder':
        """Enable isothermal flow calculation."""
        self.misc.isothermal = True
        self.misc.radiation = False
        return self

    def wildfire_mode(self, level_set_mode: int = 1) -> 'MiscBuilder':
        """Configure for wildfire simulation."""
        self.misc.level_set_mode = level_set_mode
        return self

    def restart_from(self, chid: str) -> 'MiscBuilder':
        """Enable restart from previous simulation."""
        self.misc.restart = True
        self.misc.restart_chid = chid
        return self

    def build(self) -> Misc:
        """Build and validate MISC parameters."""
        return self.misc
```

### 2.3 Parameter Categories

#### 2.3.1 Ambient Conditions
```python
misc = MiscBuilder() \
    .ambient_conditions(
        temperature=25.0,  # Summer conditions
        humidity=70.0,
        pressure=101325.0
    ) \
    .build()
```

#### 2.3.2 Turbulence Models
```python
# Dynamic Smagorinsky
misc = MiscBuilder() \
    .turbulence(
        TurbulenceModel.DYNAMIC_SMAGORINSKY,
        c_smagorinsky=0.18
    ) \
    .build()

# WALE model
misc = MiscBuilder() \
    .turbulence(TurbulenceModel.WALE, c_wale=0.5) \
    .build()
```

#### 2.3.3 Special Modes
```python
# Solid heat transfer only
misc_solid = MiscBuilder() \
    .solid_phase_only() \
    .build()

# Isothermal flow
misc_iso = MiscBuilder() \
    .isothermal_flow() \
    .build()

# Wildfire simulation
misc_wildfire = MiscBuilder() \
    .wildfire_mode(level_set_mode=1) \
    .ambient_conditions(temperature=35.0, humidity=15.0) \
    .build()
```

### 2.4 Validation Rules

1. **Parameter Ranges**
   - Temperature: -273.15°C to 2000°C
   - Pressure: > 0 Pa
   - Humidity: 0-100%
   - CFL: 0.1 to 10

2. **Mode Conflicts**
   - Cannot use SOLID_PHASE_ONLY with ISOTHERMAL
   - ISOTHERMAL disables RADIATION
   - LEVEL_SET_MODE requires specific surface setup

3. **Performance Impact**
   - Lower CFL_MAX = smaller timesteps, more stable
   - Higher CFL_MAX = larger timesteps, less stable
   - PARTICLE_CFL can significantly slow simulations

---

## 3. Implementation Architecture

### 3.1 Integration with Simulation Class

```python
class Simulation:
    def __init__(self, chid: str):
        self.chid = chid
        self.misc = Misc()  # Default MISC parameters
        self.vents: List[Vent] = []

    def set_misc(self, misc: Misc):
        """Set MISC parameters (only one per simulation)."""
        self.misc = misc

        # Apply global effects
        if misc.solid_phase_only:
            # Disable gas phase calculations
            self._configure_solid_only()

        if misc.isothermal:
            # Disable heat transfer
            self._configure_isothermal()

    def add_vent(self, vent: Vent) -> Vent:
        """Add a vent with validation."""
        # Validate against existing entities
        self._validate_vent(vent)

        # Special handling for HVAC vents
        if vent.vent_type == VentType.HVAC:
            self._register_hvac_vent(vent)

        self.vents.append(vent)
        return vent

    def write(self, filename: str):
        """Write FDS input file."""
        with open(filename, 'w') as f:
            # HEAD
            f.write(f"&HEAD CHID='{self.chid}' /\n\n")

            # MISC (always near the top)
            f.write("! === Miscellaneous Parameters ===\n")
            f.write(self.misc.to_fds())
            f.write("\n\n")

            # ... other namelists ...

            # VENTS (after OBST, SURF)
            if self.vents:
                f.write("! === Vents and Openings ===\n")
                for vent in self.vents:
                    f.write(vent.to_fds())
                    f.write("\n")
                f.write("\n")
```

### 3.2 Cross-Validation

```python
class VentValidator:
    """Validates vent references and geometry."""

    @staticmethod
    def validate_vent_surface(vent: Vent, surfaces: Dict[str, 'Surface']):
        """Validate vent surface reference."""
        special_surfaces = ['OPEN', 'HVAC', 'MIRROR', 'PERIODIC']

        if vent.surf_id not in special_surfaces:
            if vent.surf_id not in surfaces:
                raise ValueError(
                    f"Vent references non-existent surface '{vent.surf_id}'"
                )

    @staticmethod
    def validate_vent_alignment(vent: Vent, obstructions: List['Obstruction']):
        """Validate vent aligns with obstruction faces."""
        if vent.vent_type == VentType.SURFACE:
            # Check that vent is on an obstruction face
            aligned = False
            for obst in obstructions:
                if VentValidator._is_on_face(vent.xb, obst.xb):
                    aligned = True
                    break

            if not aligned:
                raise ValueError(
                    f"Surface vent at {vent.xb} does not align with any obstruction"
                )

    @staticmethod
    def _is_on_face(vent_xb, obst_xb):
        """Check if vent is on obstruction face."""
        # Implementation: check if vent plane coincides with obstruction face
        pass
```

---

## 4. Testing Strategy

### 4.1 Unit Tests

```python
# tests/test_vent.py
import pytest
from pyfds.namelists.vent import Vent, VentBuilder, VentType

class TestVent:
    def test_rectangular_vent(self):
        """Test basic rectangular vent."""
        vent = Vent(
            xb=(5, 5, 2, 4, 0, 3),
            surf_id='OPEN'
        )

        assert vent.vent_type == VentType.OPEN
        assert vent.shape == VentShape.RECTANGULAR
        assert vent.area == pytest.approx(6.0)  # 2m x 3m

    def test_circular_vent(self):
        """Test circular vent creation."""
        vent = VentBuilder() \
            .at_position((-1, 1, -1, 1, 0, 0)) \
            .circular(center=(0, 0, 0), radius=0.5) \
            .with_surface('BURNER') \
            .build()

        assert vent.shape == VentShape.CIRCULAR
        assert vent.area == pytest.approx(np.pi * 0.25)

    def test_hvac_vent(self):
        """Test HVAC vent configuration."""
        vent = VentBuilder() \
            .at_position((5, 6, 5, 6, 3, 3)) \
            .hvac(volume_flow=0.5) \
            .build()

        assert vent.vent_type == VentType.HVAC
        assert vent.volume_flow == 0.5

    def test_invalid_geometry(self):
        """Test invalid vent geometry detection."""
        with pytest.raises(ValueError, match="must be a plane"):
            Vent(xb=(0, 1, 0, 1, 0, 1))  # 3D volume, not plane

    def test_mesh_boundary_vent(self):
        """Test mesh boundary vent."""
        vent = VentBuilder() \
            .on_mesh_boundary('XMIN') \
            .with_surface('OPEN') \
            .build()

        assert vent.mb == 'XMIN'
        assert vent.xb is None


# tests/test_misc.py
import pytest
from pyfds.namelists.misc import Misc, MiscBuilder, TurbulenceModel

class TestMisc:
    def test_default_parameters(self):
        """Test default MISC parameters."""
        misc = Misc()

        assert misc.tmpa == 20.0
        assert misc.p_inf == 101325.0
        assert misc.humidity == 40.0
        assert misc.turbulence_model == TurbulenceModel.DEARDORFF

    def test_ambient_conditions(self):
        """Test ambient condition settings."""
        misc = MiscBuilder() \
            .ambient_conditions(
                temperature=30.0,
                humidity=80.0
            ) \
            .build()

        assert misc.tmpa == 30.0
        assert misc.humidity == 80.0

    def test_turbulence_models(self):
        """Test turbulence model configuration."""
        misc = MiscBuilder() \
            .turbulence(
                TurbulenceModel.WALE,
                c_wale=0.5
            ) \
            .build()

        assert misc.turbulence_model == TurbulenceModel.WALE
        assert misc.c_wale == 0.5

    def test_special_modes(self):
        """Test special calculation modes."""
        # Solid phase only
        misc_solid = MiscBuilder().solid_phase_only().build()
        assert misc_solid.solid_phase_only is True

        # Isothermal
        misc_iso = MiscBuilder().isothermal_flow().build()
        assert misc_iso.isothermal is True
        assert misc_iso.radiation is False

    def test_mode_conflicts(self):
        """Test conflicting mode detection."""
        with pytest.raises(ValueError, match="Cannot use both"):
            misc = Misc(solid_phase_only=True, isothermal=True)

    def test_cfl_validation(self):
        """Test CFL parameter validation."""
        with pytest.raises(ValueError, match="CFL_MIN.*must be less than"):
            Misc(cfl_min=2.0, cfl_max=1.0)
```

### 4.2 Integration Tests

```python
# tests/test_integration_vent_misc.py
import pytest
from pyfds import Simulation
from pyfds.namelists.vent import VentBuilder
from pyfds.namelists.misc import MiscBuilder

class TestVentMiscIntegration:
    def test_complete_hvac_system(self):
        """Test HVAC system with vents and controls."""
        sim = Simulation('hvac_test')

        # Set ambient conditions
        misc = MiscBuilder() \
            .ambient_conditions(temperature=22.0, humidity=50.0) \
            .build()
        sim.set_misc(misc)

        # Add supply vents
        for x in [2, 8]:
            for y in [2, 8]:
                supply = VentBuilder() \
                    .at_position((x, x+1, y, y+1, 3, 3)) \
                    .hvac(volume_flow=0.25) \
                    .build()
                sim.add_vent(supply)

        # Add return vents
        return_vent = VentBuilder() \
            .at_position((4.5, 5.5, 4.5, 5.5, 0, 0)) \
            .hvac(volume_flow=-1.0) \
            .build()
        sim.add_vent(return_vent)

        # Verify total flow balance
        total_flow = sum(v.volume_flow or 0 for v in sim.vents)
        assert abs(total_flow) < 0.01  # Should balance

    def test_wildfire_configuration(self):
        """Test wildfire simulation setup."""
        sim = Simulation('wildfire')

        # Configure for wildfire
        misc = MiscBuilder() \
            .wildfire_mode(level_set_mode=1) \
            .ambient_conditions(
                temperature=35.0,
                humidity=15.0
            ) \
            .turbulence(TurbulenceModel.VREMAN) \
            .build()
        sim.set_misc(misc)

        # Add terrain vents
        terrain = Vent(
            xb=(0, 100, 0, 100, 0, 0),
            surf_id='GRASS'
        )
        sim.add_vent(terrain)

        assert sim.misc.level_set_mode == 1

    def test_restart_configuration(self):
        """Test restart setup."""
        sim = Simulation('restart_test')

        misc = MiscBuilder() \
            .restart_from('previous_run') \
            .cfl_control(cfl_min=0.5, cfl_max=0.8) \
            .build()
        sim.set_misc(misc)

        assert sim.misc.restart is True
        assert sim.misc.restart_chid == 'previous_run'
```

---

## 5. Code Examples

### 5.1 Complete Room with Ventilation

```python
from pyfds import Simulation
from pyfds.namelists.vent import VentBuilder
from pyfds.namelists.misc import MiscBuilder

# Create simulation
sim = Simulation('ventilated_room')

# Configure environment
misc = MiscBuilder() \
    .ambient_conditions(temperature=20.0, humidity=50.0) \
    .turbulence(TurbulenceModel.DEARDORFF, c_deardorff=0.1) \
    .cfl_control(cfl_min=0.8, cfl_max=1.0) \
    .build()
sim.set_misc(misc)

# Add room geometry
sim.add_mesh(ijk=(50, 50, 30), xb=(0, 5, 0, 5, 0, 3))

# Add door (open vent)
door = VentBuilder() \
    .at_position((0, 0, 2, 3, 0, 2)) \
    .with_surface('OPEN') \
    .build()
sim.add_vent(door)

# Add window (controlled by device)
window = VentBuilder() \
    .at_position((5, 5, 1, 2, 1, 2)) \
    .with_surface('OPEN') \
    .with_control(devc_id='TEMP_SENSOR', delay=30.0) \
    .build()
sim.add_vent(window)

# Add ceiling supply vent
supply = VentBuilder() \
    .at_position((2, 3, 2, 3, 2.9, 2.9)) \
    .hvac(volume_flow=0.5) \
    .with_control(ctrl_id='HVAC_CTRL') \
    .build()
sim.add_vent(supply)

# Add floor return vent
return_vent = VentBuilder() \
    .at_position((4, 4.5, 4, 4.5, 0.1, 0.1)) \
    .hvac(volume_flow=-0.5) \
    .build()
sim.add_vent(return_vent)
```

### 5.2 Circular Burner with Spread

```python
# Circular fire source with spreading
burner_base = VentBuilder() \
    .at_position((-2, 2, -2, 2, 0, 0)) \
    .circular(center=(0, 0, 0), radius=0.3) \
    .with_surface('BURNER') \
    .build()

# Set spread rate for fire growth
burner_base.spread_rate = 0.01  # 1 cm/s spread

sim.add_vent(burner_base)
```

### 5.3 Pressure-Driven Flow

```python
# Configure for pressure-driven flow
misc = MiscBuilder() \
    .ambient_conditions(temperature=20.0, pressure=101325.0) \
    .build()
sim.set_misc(misc)

# High pressure inlet
inlet = Vent(
    xb=(0, 0, 2, 3, 1, 2),
    surf_id='OPEN',
    dynamic_pressure=True,
    tmp_exterior=20.0,
    rho_exterior=1.2
)
sim.add_vent(inlet)

# Low pressure outlet
outlet = VentBuilder() \
    .on_mesh_boundary('XMAX') \
    .with_surface('OPEN') \
    .build()
sim.add_vent(outlet)
```

---

## 6. Implementation Checklist

### 6.1 VENT Implementation Tasks

#### Core Implementation
- [ ] Create `pyfds/namelists/vent.py`
- [ ] Implement `Vent` dataclass with all parameters
- [ ] Implement `VentType` and `VentShape` enums
- [ ] Add geometric validation (plane check)
- [ ] Implement area calculation
- [ ] Create `VentBuilder` class
- [ ] Add `to_fds()` method

#### Vent Types
- [ ] Implement OPEN vent handling
- [ ] Implement HVAC vent with flow parameters
- [ ] Implement circular vent geometry
- [ ] Implement annular vent support
- [ ] Implement mesh boundary vents (MB)
- [ ] Add surface patch support

#### Validation
- [ ] Validate XB defines a plane
- [ ] Validate circular vent geometry
- [ ] Check surf_id references
- [ ] Validate control references (devc_id, ctrl_id)
- [ ] Implement obstruction alignment check

#### Integration
- [ ] Add vents list to Simulation class
- [ ] Implement `add_vent()` method
- [ ] Add cross-reference validation
- [ ] Update FDS file writer

#### Testing
- [ ] Write unit tests (15+ tests)
- [ ] Test all vent types
- [ ] Test geometric validation
- [ ] Test HVAC flow balance
- [ ] Integration tests with surfaces

### 6.2 MISC Implementation Tasks

#### Core Implementation
- [ ] Create `pyfds/namelists/misc.py`
- [ ] Implement `Misc` dataclass
- [ ] Add all parameter groups
- [ ] Implement parameter validation
- [ ] Create `MiscBuilder` class
- [ ] Add `to_fds()` method

#### Parameter Groups
- [ ] Ambient conditions (TMPA, P_INF, HUMIDITY)
- [ ] Gravity vector (GVEC)
- [ ] Turbulence models and constants
- [ ] CFL control parameters
- [ ] Solver options (SOLID_PHASE_ONLY, ISOTHERMAL)
- [ ] Special modes (LEVEL_SET_MODE)
- [ ] Output control parameters
- [ ] Restart configuration

#### Validation
- [ ] Validate parameter ranges
- [ ] Check mode conflicts
- [ ] Validate turbulence parameters
- [ ] CFL min/max relationship

#### Integration
- [ ] Add misc attribute to Simulation
- [ ] Implement `set_misc()` method
- [ ] Apply global effects from MISC
- [ ] Update FDS file writer

#### Testing
- [ ] Write unit tests (12+ tests)
- [ ] Test parameter validation
- [ ] Test mode conflicts
- [ ] Test builder patterns
- [ ] Integration tests

### 6.3 Documentation Tasks

- [ ] Document all VENT parameters
- [ ] Document all MISC parameters
- [ ] Create usage examples
- [ ] Write integration guide
- [ ] Add performance notes
- [ ] Create troubleshooting section

---

## Implementation Notes

### Critical Considerations

1. **VENT Placement**
   - VENTs must align with mesh or obstruction faces
   - Circular vents need XB large enough to contain circle
   - HVAC vents should balance flow

2. **MISC Global Effects**
   - MISC parameters affect entire simulation
   - Some parameters have major performance impacts
   - Only one MISC line per simulation

3. **Validation Order**
   - Validate individual parameters first
   - Then check cross-references
   - Finally verify geometric constraints

4. **FDS File Order**
   - MISC should appear near top of file
   - VENTs come after SURF and OBST
   - Maintain proper namelist sequence

### Performance Tips

1. **CFL Numbers**
   - Lower CFL_MAX for stability
   - Higher CFL_MIN for efficiency
   - Adjust based on mesh resolution

2. **Turbulence Models**
   - Deardorff: Default, good balance
   - Dynamic Smagorinsky: More accurate, slower
   - Vreman/WALE: Modern alternatives

3. **Special Modes**
   - SOLID_PHASE_ONLY: Much faster for heat transfer only
   - ISOTHERMAL: Fast for flow-only studies
   - PARTICLE_CFL: Slower but more stable with particles

---

## Success Criteria

✅ All VENT types implemented and tested
✅ All MISC parameters implemented
✅ Comprehensive validation working
✅ 30+ unit tests passing
✅ Integration with Simulation class complete
✅ FDS files generate correctly
✅ Documentation complete with examples

---

*This implementation plan provides everything needed to implement VENT and MISC namelists in PyFDS. Start with core classes, add validation, integrate with Simulation, then test thoroughly.*
