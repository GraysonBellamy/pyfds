# PyFDS Implementation Roadmap

## Executive Summary

This document provides a comprehensive plan to expand pyfds from its current 17% FDS feature coverage to full support for fire dynamics simulations. The roadmap is organized into 4 priority stages over multiple development cycles.

**Current State**: 110/647 parameters implemented (17%)
**Target State**: Full FDS 6+ compatibility with type-safe Python interface

---

## Stage 1: Critical Features for Basic Fire Simulations
**Goal**: Enable practical fire simulations with heat release, detection, and control
**Target Coverage**: ~35% (230 parameters)
**Estimated Effort**: 3-4 weeks

### 1.1 SURF Namelist Enhancements
**Priority**: CRITICAL - Foundation for heat transfer and fire sources

#### Implementation Plan

**File**: `src/pyfds/core/namelists/surf.py`

Add the following parameter groups:

```python
# Heat Transfer Parameters
hrrpua: Optional[float] = Field(None, description="Heat release rate per unit area (kW/m²)")
mlrpua: Optional[float] = Field(None, alias="MASS_FLUX", description="Mass flux per unit area (kg/s/m²)")
mass_flux_total: Optional[float] = Field(None, description="Total mass flux (kg/s)")

# Convective Heat Transfer
convective_heat_flux: Optional[float] = Field(None, description="Convective heat flux (kW/m²)")
net_heat_flux: Optional[float] = Field(None, description="Net heat flux (kW/m²)")
external_flux: Optional[float] = Field(None, description="External radiative flux (kW/m²)")

# Pyrolysis Control
heat_of_combustion: Optional[float] = Field(None, description="Heat of combustion (kJ/kg)")
ignition_temperature: Optional[float] = Field(None, description="Ignition temperature (°C)")
burn_away: bool = Field(False, description="Remove surface when material burns away")
backing: Literal["VOID", "INSULATED", "EXPOSED"] = Field("EXPOSED", description="Backing condition")

# Radiation Properties
emissivity: Optional[float] = Field(None, description="Surface emissivity")
absorptivity: Optional[float] = Field(None, description="Surface absorptivity")

# Time-dependent properties
ramp_q: Optional[str] = Field(None, description="RAMP_ID for heat flux")
ramp_mf: Optional[str] = Field(None, description="RAMP_ID for mass flux")
tau_q: Optional[float] = Field(None, description="Ramp time for heat flux (s)")
tau_mf: Optional[float] = Field(None, description="Ramp time for mass flux (s)")
```

**Validation Logic**:
```python
@model_validator(mode='after')
def validate_heat_source(self) -> 'Surf':
    """Validate mutually exclusive heat source specifications"""
    heat_sources = [
        self.hrrpua is not None,
        self.mlrpua is not None,
        self.mass_flux_total is not None,
        self.convective_heat_flux is not None,
        self.net_heat_flux is not None
    ]
    if sum(heat_sources) > 1:
        raise ValueError("Only one heat source specification allowed per SURF")
    return self
```

**Builder Updates** (`src/pyfds/builders/surf.py`):
```python
def with_heat_release(self, hrrpua: float, ramp_id: Optional[str] = None) -> 'SurfBuilder':
    """Set heat release rate per unit area"""
    self.params['hrrpua'] = hrrpua
    if ramp_id:
        self.params['ramp_q'] = ramp_id
    return self

def with_mass_flux(self, mlrpua: float, ramp_id: Optional[str] = None) -> 'SurfBuilder':
    """Set mass flux per unit area"""
    self.params['mlrpua'] = mlrpua
    if ramp_id:
        self.params['ramp_mf'] = ramp_id
    return self

def with_ignition(self, temperature: float, burn_away: bool = False) -> 'SurfBuilder':
    """Set ignition properties"""
    self.params['ignition_temperature'] = temperature
    self.params['burn_away'] = burn_away
    return self
```

**Tests** (`tests/test_builders/test_surf_builder.py`):
```python
def test_heat_release_specification():
    surf = SurfBuilder("FIRE").with_heat_release(500.0).build()
    assert surf.hrrpua == 500.0

def test_mass_flux_with_ramp():
    surf = SurfBuilder("BURNER").with_mass_flux(0.01, ramp_id="fuel_ramp").build()
    assert surf.mlrpua == 0.01
    assert surf.ramp_mf == "fuel_ramp"

def test_mutually_exclusive_heat_sources():
    with pytest.raises(ValidationError):
        Surf(id="BAD", hrrpua=500.0, mlrpua=0.01)
```

---

### 1.2 DEVC Namelist Enhancements
**Priority**: CRITICAL - Enable detection and control systems

#### Implementation Plan

**File**: `src/pyfds/core/namelists/devc.py`

Add control and statistics parameters:

```python
# Control Parameters
setpoint: Optional[float] = Field(None, description="Activation setpoint value")
initial_state: bool = Field(False, description="Initial state of controlled device")
latch: bool = Field(True, description="Latch device state after activation")
trip_direction: Literal[1, -1, 0] = Field(1, description="Direction to trip: 1=above, -1=below, 0=both")

# Time Control
delay: Optional[float] = Field(None, description="Activation delay time (s)")
time_history: bool = Field(False, description="Output time history")

# Statistics
statistics: Optional[str] = Field(None, description="Statistical operation: MIN, MAX, MEAN, etc.")
statistics_start: Optional[float] = Field(None, description="Start time for statistics (s)")
temporal_statistic: Optional[str] = Field(None, description="Temporal statistic type")
spatial_statistic: Optional[str] = Field(None, description="Spatial statistic type")

# Device Relationships
ctrl_id: Optional[str] = Field(None, description="Control function ID")
devc_id: Optional[str] = Field(None, description="Device ID for control logic")
input_id: List[str] = Field(default_factory=list, description="Input device IDs")

# Orientation
orientation: Optional[Tuple[float, float, float]] = Field(None, description="Device orientation vector")
rotation: Optional[float] = Field(None, description="Rotation angle (degrees)")

# Output Control
hide_coordinates: bool = Field(False, description="Hide coordinates in output")
no_update_devc_id: Optional[str] = Field(None, description="Device to prevent updates")
```

**Validation**:
```python
@model_validator(mode='after')
def validate_control_logic(self) -> 'Devc':
    """Validate control logic consistency"""
    if self.setpoint is not None:
        if self.quantity is None:
            raise ValueError("QUANTITY required when SETPOINT is specified")

    if self.statistics is not None:
        valid_stats = ['MIN', 'MAX', 'MEAN', 'RMS', 'VARIANCE', 'RANGE']
        if self.statistics.upper() not in valid_stats:
            raise ValueError(f"STATISTICS must be one of {valid_stats}")

    return self
```

**Builder Updates** (`src/pyfds/builders/devc.py`):
```python
def with_control(self, setpoint: float, trip_direction: int = 1,
                 latch: bool = True, delay: float = 0.0) -> 'DevcBuilder':
    """Configure device as a controller"""
    self.params.update({
        'setpoint': setpoint,
        'trip_direction': trip_direction,
        'latch': latch,
        'delay': delay if delay > 0 else None
    })
    return self

def with_statistics(self, stat_type: str, start_time: float = 0.0) -> 'DevcBuilder':
    """Add statistical analysis to device"""
    self.params['statistics'] = stat_type.upper()
    if start_time > 0:
        self.params['statistics_start'] = start_time
    return self

def with_orientation(self, axis: Tuple[float, float, float],
                     rotation: float = 0.0) -> 'DevcBuilder':
    """Set device orientation"""
    self.params['orientation'] = axis
    if rotation != 0:
        self.params['rotation'] = rotation
    return self
```

**Tests**:
```python
def test_sprinkler_control():
    sprinkler = (DevcBuilder("SPRINK")
                .with_quantity("SPRINKLER_LINK_TEMPERATURE")
                .with_control(setpoint=74.0, trip_direction=1, latch=True)
                .at_point(Point3D(5.0, 5.0, 3.0))
                .build())
    assert sprinkler.setpoint == 74.0
    assert sprinkler.latch is True

def test_statistical_device():
    temp_avg = (DevcBuilder("AVG_TEMP")
               .with_quantity("TEMPERATURE")
               .with_statistics("MEAN", start_time=10.0)
               .in_bounds(Bounds3D(0, 10, 0, 10, 0, 3))
               .build())
    assert temp_avg.statistics == "MEAN"
```

---

### 1.3 REAC Namelist Enhancements
**Priority**: HIGH - Better combustion modeling

#### Implementation Plan

**File**: `src/pyfds/core/namelists/reac.py`

Add extinction and suppression parameters:

```python
# Extinction
extinction_model: Optional[str] = Field(None, description="Extinction model: EXTINCTION_1, EXTINCTION_2")
critical_flame_temperature: Optional[float] = Field(None, description="Critical flame temperature for extinction (K)")
auto_ignition_temperature: Optional[float] = Field(None, description="Auto-ignition temperature (K)")

# Suppression
suppression: bool = Field(False, description="Enable suppression model")
k_suppression: Optional[float] = Field(None, description="Suppression rate constant")

# Heat of Combustion
heat_of_combustion: Optional[float] = Field(None, description="Heat of combustion (kJ/kg)")
ideal: bool = Field(True, description="Use ideal heat of combustion")

# Radiative Fraction
radiative_fraction: Optional[float] = Field(None, description="Fraction of HRR emitted as radiation")

# Species Tracking
spec_id_nu: List[str] = Field(default_factory=list, description="Species IDs for stoichiometry")
nu: List[float] = Field(default_factory=list, description="Stoichiometric coefficients")

# Advanced
fixed_mix_time: Optional[float] = Field(None, description="Fixed mixing time (s)")
tau_chem: Optional[float] = Field(None, description="Chemical time scale (s)")
tau_flame: Optional[float] = Field(None, description="Flame time scale (s)")
```

**Validation**:
```python
@model_validator(mode='after')
def validate_species_stoichiometry(self) -> 'Reac':
    """Validate species stoichiometry specification"""
    if len(self.spec_id_nu) != len(self.nu):
        raise ValueError("SPEC_ID_NU and NU must have same length")
    return self
```

---

### 1.4 MESH Namelist Enhancements
**Priority**: HIGH - Multi-mesh and parallel support

#### Implementation Plan

**File**: `src/pyfds/core/namelists/mesh.py`

Add MPI and multi-mesh parameters:

```python
# Parallel Processing
mpi_process: Optional[int] = Field(None, description="MPI process number for this mesh")
n_threads: Optional[int] = Field(None, description="Number of OpenMP threads")

# Mesh Refinement
mult_id: Optional[str] = Field(None, description="MULT ID for mesh replication")

# Mesh Relationships
mesh_id: Optional[str] = Field(None, description="Unique mesh identifier")

# Performance
maximum_internal_iterations: int = Field(10, description="Max pressure iterations")
check_vn: bool = Field(True, description="Check Von Neumann number")
restrict_time_step: bool = Field(True, description="Restrict time step for stability")
vn_max: float = Field(1.0, description="Maximum Von Neumann number")
cfl_max: float = Field(1.0, description="Maximum CFL number")
cfl_min: float = Field(0.8, description="Minimum CFL number")

# Cylindrical Coordinates
cylindrical: bool = Field(False, description="Use cylindrical coordinates")
```

**Builder Updates**:
```python
def with_mpi(self, process: int, n_threads: Optional[int] = None) -> 'MeshBuilder':
    """Assign mesh to MPI process"""
    self.params['mpi_process'] = process
    if n_threads:
        self.params['n_threads'] = n_threads
    return self

def with_stability_control(self, cfl_max: float = 1.0,
                          vn_max: float = 1.0) -> 'MeshBuilder':
    """Set stability parameters"""
    self.params['cfl_max'] = cfl_max
    self.params['vn_max'] = vn_max
    return self
```

---

### 1.5 Testing Strategy for Stage 1

**Integration Tests** (`tests/integration/test_stage1_features.py`):

```python
def test_fire_with_sprinkler_control():
    """Test complete fire scenario with sprinkler activation"""
    sim = Simulation("sprinkler_test")

    # Fire source with ramped HRR
    fire_surf = (SurfBuilder("FIRE")
                .with_heat_release(500.0, ramp_id="fire_ramp")
                .build())

    # Sprinkler control device
    sprinkler = (DevcBuilder("SPRINK")
                .with_quantity("SPRINKLER_LINK_TEMPERATURE")
                .with_control(setpoint=74.0, latch=True)
                .at_point(Point3D(5.0, 5.0, 3.0))
                .build())

    # Verify FDS output
    fds_text = sim.to_fds()
    assert "SETPOINT" in fds_text
    assert "HRRPUA" in fds_text

def test_multi_mesh_parallel():
    """Test multi-mesh setup with MPI"""
    meshes = [
        MeshBuilder(f"MESH{i}")
        .with_bounds(Bounds3D(i*10, (i+1)*10, 0, 10, 0, 3))
        .with_grid(Grid3D(50, 50, 15))
        .with_mpi(process=i)
        .build()
        for i in range(4)
    ]

    assert all(m.mpi_process is not None for m in meshes)
```

---

## Stage 2: Particle Systems and Advanced Heat Transfer
**Goal**: Enable particle tracking, sprays, and complex pyrolysis
**Target Coverage**: ~55% (360 parameters)
**Estimated Effort**: 4-5 weeks

### 2.1 PART Namelist (New)
**Priority**: HIGH - Lagrangian particle tracking

#### Implementation Plan

**File**: `src/pyfds/core/namelists/part.py` (NEW)

```python
from pydantic import BaseModel, Field
from typing import Optional, List, Literal

class Part(BaseModel):
    """FDS PART namelist - Lagrangian particle properties"""

    # Identification
    id: str = Field(..., description="Unique particle class ID")

    # Physical Properties
    diameter: Optional[float] = Field(None, description="Particle diameter (m)")
    density: Optional[float] = Field(None, description="Particle density (kg/m³)")
    mass: Optional[float] = Field(None, description="Particle mass (kg)")

    # Particle Type
    sampling_factor: int = Field(1, description="Statistical sampling factor")
    static: bool = Field(False, description="Particles don't move")
    massless: bool = Field(False, description="Massless tracer particles")

    # Droplet Properties
    liquid_droplet: bool = Field(False, description="Water droplet model")
    initial_temperature: Optional[float] = Field(None, description="Initial temperature (°C)")
    boiling_temperature: Optional[float] = Field(None, description="Boiling temperature (°C)")
    heat_of_vaporization: Optional[float] = Field(None, description="Heat of vaporization (kJ/kg)")

    # Aerosol Properties
    spec_id: Optional[str] = Field(None, description="Gas species produced by particle")
    vaporization_temperature: Optional[float] = Field(None, description="Vaporization temp (°C)")

    # Drag
    drag_law: Literal["SPHERE", "CYLINDER", "SCREEN"] = Field("SPHERE", description="Drag law")
    drag_coefficient: Optional[List[float]] = Field(None, description="Drag coefficients")

    # Visualization
    color: Optional[str] = Field(None, description="Particle color for Smokeview")
    rgb: Optional[List[int]] = Field(None, description="RGB color values [0-255]")

    # Breakup
    breakup: bool = Field(False, description="Enable droplet breakup")
    breakup_cns_min: float = Field(0.5, description="Min CNS for breakup")
    breakup_cns_max: float = Field(12.0, description="Max CNS for breakup")

    # Surface Interaction
    surf_id: Optional[str] = Field(None, description="Surface ID for particle interaction")
    prop_id: Optional[str] = Field(None, description="Property ID for mass/energy")

    # Age and Lifetime
    age: Optional[float] = Field(None, description="Initial particle age (s)")
    lifetime: Optional[float] = Field(None, description="Particle lifetime (s)")

    # Orientation
    orientation: Optional[List[float]] = Field(None, description="Particle orientation vector")

    class Config:
        populate_by_name = True
        str_strip_whitespace = True
```

**Builder** (`src/pyfds/builders/part.py`):

```python
class PartBuilder:
    """Builder for PART namelist"""

    def __init__(self, part_id: str):
        self.params = {'id': part_id}

    def as_water_droplet(self, diameter: float, temp: float = 20.0) -> 'PartBuilder':
        """Configure as water droplet"""
        self.params.update({
            'liquid_droplet': True,
            'diameter': diameter,
            'density': 1000.0,
            'initial_temperature': temp,
            'boiling_temperature': 100.0,
            'heat_of_vaporization': 2260.0
        })
        return self

    def as_aerosol(self, diameter: float, spec_id: str) -> 'PartBuilder':
        """Configure as aerosol particle"""
        self.params.update({
            'diameter': diameter,
            'spec_id': spec_id,
            'massless': False
        })
        return self

    def with_breakup(self, enabled: bool = True) -> 'PartBuilder':
        """Enable/disable droplet breakup"""
        self.params['breakup'] = enabled
        return self

    def with_color(self, color: str) -> 'PartBuilder':
        """Set particle color for visualization"""
        self.params['color'] = color
        return self

    def with_lifetime(self, lifetime: float) -> 'PartBuilder':
        """Set particle lifetime"""
        self.params['lifetime'] = lifetime
        return self

    def build(self) -> Part:
        return Part(**self.params)
```

---

### 2.2 SURF Particle Generation
**Priority**: HIGH - Link particles to surfaces

Add to `src/pyfds/core/namelists/surf.py`:

```python
# Particle Generation
part_id: Optional[str] = Field(None, description="Particle class ID to generate")
particle_mass_flux: Optional[float] = Field(None, description="Particle mass flux (kg/s/m²)")
nppc: int = Field(1, description="Number of particles per cell")

# Droplet Distribution
median_diameter: Optional[float] = Field(None, description="Median droplet diameter (m)")
gamma_d: Optional[float] = Field(None, description="Distribution shape parameter")
spray_pattern: Optional[str] = Field(None, description="Spray pattern: UNIFORM, GAUSSIAN")

# Particle Velocity
vel_part: Optional[float] = Field(None, description="Particle velocity magnitude (m/s)")
particle_velocity: Optional[Tuple[float, float, float]] = Field(None, description="Particle velocity vector")
```

---

### 2.3 PROP Namelist (New)
**Priority**: MEDIUM - Device/particle properties

**File**: `src/pyfds/core/namelists/prop.py` (NEW)

```python
class Prop(BaseModel):
    """FDS PROP namelist - Device and particle properties"""

    id: str = Field(..., description="Property ID")

    # Sprinkler Properties
    quantity: Optional[str] = Field(None, description="Measured quantity")
    activation_temperature: Optional[float] = Field(None, description="Activation temp (°C)")
    rti: Optional[float] = Field(None, description="Response Time Index (m½s½)")
    c_factor: Optional[float] = Field(None, description="Sprinkler C-factor")
    spray_angle: Optional[Tuple[float, float]] = Field(None, description="Spray cone angles")

    # Nozzle/Spray
    flow_rate: Optional[float] = Field(None, description="Flow rate (L/min or kg/s)")
    pressure: Optional[float] = Field(None, description="Operating pressure (Pa)")
    orifice_diameter: Optional[float] = Field(None, description="Orifice diameter (m)")

    # Smoke Detector
    smokeview_id: Optional[str] = Field(None, description="Smokeview object ID")
    alpha_e: Optional[float] = Field(None, description="Extinction coefficient")
    beta_e: Optional[float] = Field(None, description="Scattering coefficient")

    # Heat Detector
    bead_diameter: Optional[float] = Field(None, description="Detector bead diameter (m)")
    bead_density: Optional[float] = Field(None, description="Detector bead density (kg/m³)")
    bead_specific_heat: Optional[float] = Field(None, description="Bead specific heat (kJ/kg/K)")

    # Visualization
    offset: Optional[float] = Field(None, description="Display offset")
```

---

### 2.4 MATL Enhancements
**Priority**: MEDIUM - Better pyrolysis modeling

Add to `src/pyfds/core/namelists/matl.py`:

```python
# Pyrolysis Products
spec_id: Optional[str] = Field(None, description="Gas species ID for pyrolysis")
nu_spec: Optional[float] = Field(None, description="Species yield coefficient")
nu_matl: Optional[str] = Field(None, description="Solid residue material ID")
yield_fraction: Optional[float] = Field(None, description="Fraction of material yielded")

# Heat of Reaction
heat_of_reaction: Optional[float] = Field(None, description="Heat of reaction (kJ/kg)")
heat_of_combustion: Optional[float] = Field(None, description="Heat of combustion (kJ/kg)")

# Reference State
reference_temperature: float = Field(20.0, description="Reference temperature (°C)")
reference_rate: Optional[float] = Field(None, description="Reference reaction rate")
```

---

### 2.5 Testing for Stage 2

```python
def test_sprinkler_spray_system():
    """Test complete sprinkler with droplet generation"""

    # Water droplet class
    droplet = (PartBuilder("WATER_DROP")
              .as_water_droplet(diameter=0.001, temp=20.0)
              .with_breakup(True)
              .build())

    # Sprinkler properties
    sprinkler_prop = Prop(
        id="STANDARD_SPRINKLER",
        activation_temperature=74.0,
        rti=100.0,
        flow_rate=100.0,  # L/min
        spray_angle=(60.0, 80.0),
        part_id="WATER_DROP"
    )

    # Sprinkler surface
    spray_surf = (SurfBuilder("SPRAY")
                 .with_particle_generation("WATER_DROP", mass_flux=0.1)
                 .build())

    assert droplet.liquid_droplet is True
    assert sprinkler_prop.flow_rate == 100.0
```

---

## Stage 3: Vegetation, Complex Geometry, and HVAC
**Goal**: Enable wildfire modeling, complex geometry, and HVAC systems
**Target Coverage**: ~75% (490 parameters)
**Estimated Effort**: 5-6 weeks

### 3.1 Vegetation Parameters
**Priority**: MEDIUM - Wildfire modeling

Add to `src/pyfds/core/namelists/surf.py`:

```python
# Vegetation - Level Set Fire Spread
veg_lset_fuel_index: Optional[int] = Field(None, description="Fuel model index")
veg_lset_char: bool = Field(False, description="Use char oxidation")
veg_lset_m1: Optional[float] = Field(None, description="1-hr fuel moisture (%)")
veg_lset_m10: Optional[float] = Field(None, description="10-hr fuel moisture (%)")
veg_lset_m100: Optional[float] = Field(None, description="100-hr fuel moisture (%)")
veg_lset_mex: Optional[float] = Field(None, description="Live fuel moisture (%)")
veg_lset_flame_height: Optional[float] = Field(None, description="Flame height (m)")
veg_lset_surface_fire_head_ros_model: Optional[str] = Field(None, description="ROS model")
veg_lset_crown_fire_head_ros_model: Optional[str] = Field(None, description="Crown ROS model")

# Vegetation - Drag and Structure
veg_drag_constant: Optional[float] = Field(None, description="Drag constant")
veg_height: Optional[float] = Field(None, description="Vegetation height (m)")
veg_initial_temperature: Optional[float] = Field(None, description="Initial temp (°C)")
veg_sav: Optional[float] = Field(None, description="Surface-area-to-volume ratio (1/m)")
veg_load: Optional[float] = Field(None, description="Vegetation loading (kg/m²)")
veg_bulk_density: Optional[float] = Field(None, description="Bulk density (kg/m³)")
```

---

### 3.2 MULT Namelist (New)
**Priority**: HIGH - Geometry replication

**File**: `src/pyfds/core/namelists/mult.py` (NEW)

```python
class Mult(BaseModel):
    """FDS MULT namelist - Geometry replication"""

    id: str = Field(..., description="Multiplier ID")

    # Translation
    dx: float = Field(0.0, description="X-direction spacing (m)")
    dy: float = Field(0.0, description="Y-direction spacing (m)")
    dz: float = Field(0.0, description="Z-direction spacing (m)")

    # Replication counts
    i_lower: int = Field(0, description="Lower bound in X")
    i_upper: int = Field(0, description="Upper bound in X")
    j_lower: int = Field(0, description="Lower bound in Y")
    j_upper: int = Field(0, description="Upper bound in Y")
    k_lower: int = Field(0, description="Lower bound in Z")
    k_upper: int = Field(0, description="Upper bound in Z")

    # Advanced
    dx_array: Optional[List[float]] = Field(None, description="Non-uniform X spacing")
    dy_array: Optional[List[float]] = Field(None, description="Non-uniform Y spacing")
    dz_array: Optional[List[float]] = Field(None, description="Non-uniform Z spacing")

    n_lower: int = Field(0, description="Lower bound in N")
    n_upper: int = Field(0, description="Upper bound in N")
```

**Builder**:
```python
class MultBuilder:
    """Builder for MULT namelist"""

    def uniform_replication(self, mult_id: str,
                          n_x: int, n_y: int, n_z: int,
                          dx: float, dy: float, dz: float) -> Mult:
        """Create uniform replication pattern"""
        return Mult(
            id=mult_id,
            dx=dx, dy=dy, dz=dz,
            i_upper=n_x-1,
            j_upper=n_y-1,
            k_upper=n_z-1
        )
```

---

### 3.3 GEOM Namelist (New)
**Priority**: MEDIUM - Complex geometry

**File**: `src/pyfds/core/namelists/geom.py` (NEW)

```python
class Geom(BaseModel):
    """FDS GEOM namelist - Complex geometry"""

    id: str = Field(..., description="Geometry ID")

    # Geometry Source
    geometry_file: Optional[str] = Field(None, alias="GEOMETRY_FILE",
                                         description="Binary geometry file")
    is_terrain: bool = Field(False, description="Terrain surface")

    # Surface Assignment
    surf_id: Optional[str] = Field(None, description="Surface ID")
    surf_id6: Optional[List[str]] = Field(None, description="6 surface IDs for faces")

    # Position and Orientation
    xyz: Optional[Tuple[float, float, float]] = Field(None, description="Position")
    orientation: Optional[List[float]] = Field(None, description="Orientation angles")
    scale: Optional[Tuple[float, float, float]] = Field(None, description="Scaling factors")

    # Texture Mapping
    texture_mapping: Optional[str] = Field(None, description="Texture mapping file")
    texture_origin: Optional[Tuple[float, float, float]] = Field(None,
                                                                  description="Texture origin")

    # Volume
    volume_integral: bool = Field(False, description="Compute volume integral")

    # Replication
    mult_id: Optional[str] = Field(None, description="MULT ID for replication")
```

---

### 3.4 HVAC Namelist (New)
**Priority**: MEDIUM - HVAC systems

**File**: `src/pyfds/core/namelists/hvac.py` (NEW)

```python
class Hvac(BaseModel):
    """FDS HVAC namelist - HVAC system components"""

    id: str = Field(..., description="HVAC component ID")
    type_id: str = Field(..., description="Component type: NODE, DUCT, FAN, etc.")

    # Node Properties
    xyz: Optional[Tuple[float, float, float]] = Field(None, description="Node location")
    vent_id: Optional[str] = Field(None, description="VENT ID for boundary")
    ambient: bool = Field(False, description="Ambient node")

    # Duct Properties
    node_id: Optional[List[str]] = Field(None, description="Connected node IDs [from, to]")
    length: Optional[float] = Field(None, description="Duct length (m)")
    diameter: Optional[float] = Field(None, description="Duct diameter (m)")
    area: Optional[float] = Field(None, description="Duct cross-section area (m²)")
    roughness: float = Field(0.0, description="Surface roughness (m)")
    loss: Optional[List[float]] = Field(None, description="Loss coefficients [entry, exit]")

    # Fan Properties
    max_flow: Optional[float] = Field(None, description="Maximum flow rate (m³/s)")
    max_pressure: Optional[float] = Field(None, description="Maximum pressure (Pa)")
    volume_flow: Optional[float] = Field(None, description="Volume flow rate (m³/s)")

    # Filter Properties
    efficiency: Optional[List[float]] = Field(None, description="Filter efficiencies")
    loading: Optional[float] = Field(None, description="Filter loading")

    # Control
    ctrl_id: Optional[str] = Field(None, description="Control function ID")
    ramp_id: Optional[str] = Field(None, description="RAMP ID for time-dependent")

    # Leak Properties
    leak_enthalpy: bool = Field(True, description="Include enthalpy in leakage")
```

---

## Stage 4: Advanced Features and Complete Coverage
**Goal**: Finite-rate chemistry, advanced radiation, full FDS compatibility
**Target Coverage**: ~95% (615+ parameters)
**Estimated Effort**: 6-8 weeks

### 4.1 Finite-Rate Chemistry
**Priority**: LOW - Advanced combustion research

Add to `src/pyfds/core/namelists/reac.py`:

```python
# Finite-Rate Chemistry Mode
finite_rate: bool = Field(False, description="Use finite-rate chemistry")

# Arrhenius Parameters
a: Optional[float] = Field(None, description="Pre-exponential factor")
e: Optional[float] = Field(None, description="Activation energy (kJ/mol)")
n_t: Optional[float] = Field(None, description="Temperature exponent")

# Third Body
third_body: bool = Field(False, description="Include third body collision")
efficiency: Optional[Dict[str, float]] = Field(None, description="Third body efficiencies")

# Reverse Reaction
reverse: bool = Field(False, description="Reversible reaction")
a_reverse: Optional[float] = Field(None, description="Reverse pre-exponential")
e_reverse: Optional[float] = Field(None, description="Reverse activation energy")

# Priority
priority: int = Field(1, description="Reaction priority for ordering")
```

Create new `src/pyfds/core/namelists/spec.py`:

```python
class Spec(BaseModel):
    """FDS SPEC namelist - Gas species properties"""

    id: str = Field(..., description="Species ID")

    # Physical Properties
    molecular_weight: Optional[float] = Field(None, alias="MW", description="Molecular weight (g/mol)")
    specific_heat: Optional[float] = Field(None, description="Specific heat (kJ/kg/K)")
    reference_enthalpy: Optional[float] = Field(None, description="Formation enthalpy (kJ/kg)")
    reference_temperature: float = Field(25.0, description="Reference temperature (°C)")

    # Transport
    diffusivity: Optional[float] = Field(None, description="Mass diffusivity (m²/s)")
    viscosity: Optional[float] = Field(None, description="Dynamic viscosity (kg/m/s)")
    conductivity: Optional[float] = Field(None, description="Thermal conductivity (W/m/K)")

    # Radiation
    absorbing: bool = Field(False, description="Participates in radiation")
    radcal_id: Optional[str] = Field(None, description="RADCAL species ID")

    # Lumped Species
    lumped_component_only: bool = Field(False, description="Only for lumped reactions")
    mass_fraction: Optional[List[float]] = Field(None, description="Component mass fractions")
    spec_id: Optional[List[str]] = Field(None, description="Component species IDs")

    # Aerosol
    aerosol: bool = Field(False, description="Aerosol species")
    density: Optional[float] = Field(None, description="Particle density (kg/m³)")
    mean_diameter: Optional[float] = Field(None, description="Mean particle diameter (m)")
```

---

### 4.2 CTRL Namelist (New)
**Priority**: MEDIUM - Advanced control logic

**File**: `src/pyfds/core/namelists/ctrl.py` (NEW)

```python
class Ctrl(BaseModel):
    """FDS CTRL namelist - Control functions"""

    id: str = Field(..., description="Control function ID")

    # Input Devices
    input_id: Optional[List[str]] = Field(None, description="Input device IDs")

    # Function Type
    function_type: str = Field(..., description="ANY, ALL, ONLY, etc.")

    # Logic Parameters
    latch: bool = Field(True, description="Latch output state")
    initial_state: bool = Field(False, description="Initial state")

    # Delay
    delay: Optional[float] = Field(None, description="Activation delay (s)")
    on_bound: Literal["LOWER", "UPPER"] = Field("UPPER", description="Activation bound")

    # Proportional Control
    proportional: bool = Field(False, description="Proportional control")
    constant: Optional[float] = Field(None, description="Proportional constant")
    setpoint: Optional[float] = Field(None, description="Control setpoint")

    # Differential/Integral
    differential: bool = Field(False, description="Differential control")
    integral: bool = Field(False, description="Integral control")

    # Cycling
    cycle_time: Optional[float] = Field(None, description="Cycle time (s)")
    trip_time: Optional[float] = Field(None, description="Trip time (s)")

    # Ramp
    ramp_id: Optional[str] = Field(None, description="RAMP ID for control output")
```

---

### 4.3 INIT Namelist (New)
**Priority**: LOW - Initial conditions

**File**: `src/pyfds/core/namelists/init.py` (NEW)

```python
class Init(BaseModel):
    """FDS INIT namelist - Initial conditions"""

    # Region
    xb: Optional[Tuple[float, float, float, float, float, float]] = Field(
        None, description="Bounds [x1, x2, y1, y2, z1, z2]")

    # Shape
    shape: Literal["RECTANGULAR", "CYLINDRICAL", "SPHERICAL"] = Field(
        "RECTANGULAR", description="Region shape")
    radius: Optional[float] = Field(None, description="Radius for cylindrical/spherical")

    # Temperature
    temperature: Optional[float] = Field(None, description="Initial temperature (°C)")

    # Velocity
    u0: float = Field(0.0, description="X-velocity (m/s)")
    v0: float = Field(0.0, description="Y-velocity (m/s)")
    w0: float = Field(0.0, description="Z-velocity (m/s)")

    # Species
    mass_fraction: Optional[List[float]] = Field(None, description="Species mass fractions")
    spec_id: Optional[List[str]] = Field(None, description="Species IDs")

    # Pressure
    pressure: Optional[float] = Field(None, description="Initial pressure (Pa)")

    # Particles
    part_id: Optional[str] = Field(None, description="Particle class ID")
    n_particles: Optional[int] = Field(None, description="Number of particles")
    n_particles_per_cell: Optional[int] = Field(None, description="Particles per cell")

    # Turbulence
    turbulence_intensity: Optional[float] = Field(None, description="Turbulence intensity")
```

---

### 4.4 ZONE Namelist (New)
**Priority**: LOW - Pressure zones

**File**: `src/pyfds/core/namelists/zone.py` (NEW)

```python
class Zone(BaseModel):
    """FDS ZONE namelist - Pressure zones"""

    # Region
    xb: Tuple[float, float, float, float, float, float] = Field(
        ..., description="Zone bounds")

    # Leakage
    leak_area: Optional[List[float]] = Field(None, description="Leak areas per face (m²)")
    leak_reference_pressure: float = Field(0.0, description="Reference pressure (Pa)")
    leak_pressure_exponent: float = Field(0.5, description="Pressure exponent")

    # HVAC Connection
    hvac_id: Optional[str] = Field(None, description="Connected HVAC node ID")

    # Periodic
    periodic: bool = Field(False, description="Periodic boundary conditions")
```

---

### 4.5 Additional Output Namelists

**ISOF** (Isosurface) - `src/pyfds/core/namelists/isof.py`:
```python
class Isof(BaseModel):
    """FDS ISOF namelist - Isosurface output"""

    quantity: str = Field(..., description="Quantity for isosurface")
    value: Optional[List[float]] = Field(None, description="Isosurface values")
    spec_id: Optional[str] = Field(None, description="Species ID")
    velo_index: Optional[int] = Field(None, description="Velocity component")

    # Averaging
    statistics: Optional[str] = Field(None, description="Statistical operation")
```

**PROF** (Profile) - `src/pyfds/core/namelists/prof.py`:
```python
class Prof(BaseModel):
    """FDS PROF namelist - Profile output"""

    xyz: Tuple[float, float, float] = Field(..., description="Profile line start")
    quantity: str = Field(..., description="Quantity to profile")
    format_index: int = Field(1, description="Output format")

    # Line definition
    ior: Optional[int] = Field(None, description="Profile direction")

    # Temporal
    dt: Optional[float] = Field(None, description="Output interval (s)")
```

---

## Stage 5: Testing, Documentation, and Polish
**Goal**: Comprehensive testing, examples, and production-ready release
**Estimated Effort**: 3-4 weeks

### 5.1 Comprehensive Test Suite

**Structure**:
```
tests/
├── unit/
│   ├── test_namelists/
│   │   ├── test_surf.py
│   │   ├── test_devc.py
│   │   ├── test_part.py
│   │   └── ...
│   └── test_builders/
│       ├── test_surf_builder.py
│       ├── test_devc_builder.py
│       └── ...
├── integration/
│   ├── test_fire_scenarios.py
│   ├── test_sprinkler_systems.py
│   ├── test_hvac_systems.py
│   └── test_wildfire.py
├── validation/
│   ├── test_fds_compatibility.py
│   └── test_against_reference_cases.py
└── examples/
    └── test_examples.py
```

**Key Test Cases**:

```python
# Validation against FDS reference cases
def test_fds_validation_suite():
    """Run subset of FDS validation suite through pyfds"""
    validation_cases = [
        "plate_view_factor",
        "door_crack",
        "compartment_fire",
        "spray_burner"
    ]
    for case in validation_cases:
        sim = load_validation_case(case)
        fds_output = sim.to_fds()
        # Compare against reference FDS input
        assert_fds_equivalent(fds_output, f"references/{case}.fds")
```

---

### 5.2 Documentation

**API Documentation**:
- Auto-generate from docstrings using Sphinx/MkDocs
- Include all parameters with types and descriptions
- Cross-reference with FDS User Guide sections

**Tutorial Documentation**:
```
docs/
├── getting_started/
│   ├── installation.md
│   ├── first_simulation.md
│   └── basic_concepts.md
├── tutorials/
│   ├── 01_simple_fire.md
│   ├── 02_sprinkler_activation.md
│   ├── 03_multi_mesh.md
│   ├── 04_particle_systems.md
│   ├── 05_hvac_modeling.md
│   └── 06_wildfire.md
├── how_to_guides/
│   ├── heat_release.md
│   ├── control_systems.md
│   ├── complex_geometry.md
│   └── performance_tuning.md
├── reference/
│   ├── api/
│   └── fds_mapping.md
└── validation/
    └── test_cases.md
```

---

### 5.3 Example Library

Create comprehensive examples in `examples/`:

```python
# examples/01_simple_room_fire.py
"""Simple room fire with heat detectors"""

from pyfds import Simulation
from pyfds.builders import *
from pyfds.geometry import *

def create_room_fire():
    sim = Simulation("room_fire")
    sim.add(Head(chid="room_fire", title="Simple Room Fire"))
    sim.add(Time(t_end=300.0))

    # Room mesh
    mesh = (MeshBuilder("ROOM")
           .with_bounds(Bounds3D(0, 5, 0, 4, 0, 3))
           .with_grid(Grid3D(50, 40, 30))
           .build())

    # Fire source
    fire_surf = (SurfBuilder("FIRE")
                .with_heat_release(500.0)
                .build())

    fire_obst = (ObstBuilder("BURNER")
                .with_bounds(Bounds3D(2, 3, 1.5, 2.5, 0, 0.1))
                .with_surf("FIRE", face="TOP")
                .build())

    # Heat detector
    detector = (DevcBuilder("HD1")
               .with_quantity("TEMPERATURE")
               .with_control(setpoint=75.0)
               .at_point(Point3D(2.5, 2.0, 2.8))
               .build())

    sim.add_all([mesh, fire_surf, fire_obst, detector])
    return sim

if __name__ == "__main__":
    sim = create_room_fire()
    sim.write("room_fire.fds")
```

```python
# examples/02_sprinkler_activation.py
"""Sprinkler activation with water spray"""

# examples/03_multi_room_smoke.py
"""Multi-room smoke spread"""

# examples/04_wildfire_spread.py
"""Wildfire spread using level-set method"""

# examples/05_hvac_smoke_control.py
"""HVAC-based smoke control system"""
```

---

### 5.4 Migration Guide

Create `MIGRATION.md` for updating existing code:

```markdown
# Migration Guide

## From Stage 1 to Stage 2

### Particle Systems
If you were manually creating particle sources, update to use PART namelist:

Before:
```python
surf = Surf(id="SPRAY", ...)  # Limited options
```

After:
```python
droplet = PartBuilder("WATER").as_water_droplet(0.001).build()
surf = SurfBuilder("SPRAY").with_particle_generation("WATER", 0.1).build()
```

### Control Systems
If using basic DEVC, enhance with control logic:

Before:
```python
devc = Devc(id="TEMP", quantity="TEMPERATURE", xyz=(5,5,3))
```

After:
```python
devc = (DevcBuilder("TEMP")
       .with_quantity("TEMPERATURE")
       .with_control(setpoint=74.0, latch=True)
       .at_point(Point3D(5,5,3))
       .build())
```
```

---

### 5.5 Performance Optimization

**Validation Caching**:
```python
# src/pyfds/core/base.py
from functools import lru_cache

@lru_cache(maxsize=1000)
def _validate_namelist_cached(cls, data: FrozenDict):
    """Cached validation for repeated patterns"""
    return cls.model_validate(dict(data))
```

**Lazy Loading**:
```python
# Only import heavy dependencies when needed
def _import_visualization():
    try:
        import matplotlib.pyplot as plt
        return plt
    except ImportError:
        raise ImportError("Visualization requires matplotlib")
```

**Batch Operations**:
```python
# src/pyfds/simulation.py
def add_mesh_array(self, bounds_list: List[Bounds3D],
                   grid: Grid3D, mpi_assign: bool = True):
    """Add multiple meshes efficiently"""
    meshes = []
    for i, bounds in enumerate(bounds_list):
        mesh = (MeshBuilder(f"MESH{i}")
               .with_bounds(bounds)
               .with_grid(grid)
               .with_mpi(i if mpi_assign else None)
               .build())
        meshes.append(mesh)
    self.add_all(meshes)
```

---

## Implementation Guidelines

### Code Quality Standards

1. **Type Safety**: All parameters must have proper Pydantic Field definitions
2. **Validation**: Use Pydantic validators for complex validation logic
3. **Documentation**: Every parameter needs docstring with units
4. **Testing**: 90%+ code coverage requirement
5. **FDS Compatibility**: All output must be valid FDS input

### Development Workflow

1. **Feature Branch**: Create feature branch from main
2. **Implementation**: Follow TDD - write tests first
3. **Documentation**: Update docs alongside code
4. **Review**: PR review required before merge
5. **Integration**: Run full test suite including FDS validation

### Git Commit Structure

```
feat(surf): Add particle generation parameters

- Add PART_ID, PARTICLE_MASS_FLUX to SURF
- Implement particle velocity controls
- Add builder methods for spray configuration
- Include tests for droplet generation

Refs: FDS User Guide Section 12.3
```

### Version Numbering

- **Stage 1**: v0.3.0 (Critical features)
- **Stage 2**: v0.4.0 (Particle systems)
- **Stage 3**: v0.5.0 (Geometry & HVAC)
- **Stage 4**: v0.6.0 (Advanced features)
- **Stage 5**: v1.0.0 (Production release)

---

## Risk Mitigation

### Backward Compatibility

- Use Pydantic field aliases to support FDS parameter names
- Deprecation warnings for any breaking changes
- Migration scripts for major version bumps

### FDS Version Compatibility

- Test against multiple FDS versions (6.7, 6.8, 6.9)
- Version-specific parameter validation
- Clear documentation of FDS version requirements

### Performance Concerns

- Profile validation overhead for large simulations
- Implement optional "fast mode" with reduced validation
- Lazy evaluation where possible

---

## Success Metrics

### Stage 1
- [ ] 35% parameter coverage achieved
- [ ] Fire + sprinkler scenarios work end-to-end
- [ ] 10+ integration tests passing
- [ ] Documentation for all new features

### Stage 2
- [ ] 55% parameter coverage
- [ ] Particle tracking validated against FDS
- [ ] Spray system examples working
- [ ] Performance benchmarks established

### Stage 3
- [ ] 75% parameter coverage
- [ ] HVAC system modeling functional
- [ ] Complex geometry import working
- [ ] Wildfire example validated

### Stage 4
- [ ] 95% parameter coverage
- [ ] Finite-rate chemistry functional
- [ ] All major FDS features supported
- [ ] Complete API documentation

### Stage 5
- [ ] Production-ready v1.0.0 release
- [ ] 90%+ test coverage
- [ ] 20+ comprehensive examples
- [ ] Full FDS validation suite passing

---

## Appendix: Parameter Tracking

### Full Coverage Checklist

Track implementation progress for all 647 parameters across:

- ✅ HEAD (2/2) - 100%
- ⚠️ TIME (3/5) - 60%
- ⚠️ MISC (5/72) - 7%
- ⚠️ DUMP (3/8) - 38%
- ⚠️ MESH (2/7) - 29%
- ⚠️ TRNX/Y/Z (0/15) - 0%
- ⚠️ REAC (3/16) - 19%
- ⚠️ RADI (1/5) - 20%
- ⚠️ SPEC (0/30) - 0%
- ⚠️ MATL (10/27) - 37%
- ❌ SURF (9/153) - 6%
- ⚠️ OBST (3/15) - 20%
- ⚠️ HOLE (2/4) - 50%
- ✅ VENT (16/30) - 53%
- ❌ DEVC (4/73) - 5%
- ⚠️ SLCF (3/6) - 50%
- ⚠️ BNDF (2/3) - 67%
- ✅ RAMP (5/5) - 100%
- ⚠️ TABL (0/3) - 0%
- ❌ PART (0/47) - 0%
- ❌ PROP (0/30) - 0%
- ❌ CTRL (0/12) - 0%
- ❌ INIT (0/15) - 0%
- ❌ ZONE (0/8) - 0%
- ❌ HVAC (0/40) - 0%
- ❌ MULT (0/15) - 0%
- ❌ GEOM (0/20) - 0%
- ❌ ISOF (0/5) - 0%
- ❌ PROF (0/5) - 0%

See detailed parameter lists in the comprehensive audit report.

---

**Document Version**: 1.0
**Last Updated**: 2025-11-27
**Status**: Ready for Implementation
