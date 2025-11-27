# Glossary

Definitions of fire safety, FDS, and PyFDS terminology.

## A

### Absorption Coefficient
Parameter controlling radiation absorption by gases and soot. Higher values mean more radiation is absorbed. Typical range: 0.1-10 1/m.

### Activation Temperature
Temperature at which a device (detector or sprinkler) activates. Commonly 68°C for sprinklers, 57-74°C for heat detectors.

### Adiabatic
No heat transfer through a boundary. In FDS, `SURF_ID='INERT'` creates an adiabatic wall.

### ASET
Available Safe Egress Time. Time from fire ignition until conditions become untenable for evacuation.

### Aspect Ratio
Ratio between cell dimensions. Ideal is 1:1:1 (cubic cells). Generally avoid ratios > 4:1 for accuracy.

## B

### Backing
Surface backing condition. Options:
- `'INSULATED'` - No heat loss through back
- `'EXPOSED'` - Both sides exposed to gas
- `'VOID'` - Back surface at ambient

### Boundary Condition
Specification of behavior at domain boundaries. Common types: OPEN, MIRROR, INERT, custom SURF_ID.

## C

### CFD
Computational Fluid Dynamics. Numerical solution of fluid flow equations. FDS is a CFD model specialized for fire.

### CFL Condition
Courant-Friedrichs-Lewy stability criterion. Limits time step based on cell size and flow velocity: Δt ≤ Δx / (|u| + c).

### Characteristic Fire Diameter
D* - Dimensionless fire diameter used for mesh sizing. Calculated from heat release rate and ambient conditions.

### CHID
Case IDentifier. Unique name for simulation. Used as prefix for all output files.

### CO Yield
Carbon monoxide production per unit fuel mass burned. Typical range: 0.001-0.01 kg/kg for most fuels.

### Combustion
Chemical reaction between fuel and oxygen that releases heat. FDS uses fast chemistry (mixing-controlled).

### Convection
Heat transfer by fluid motion. Natural (buoyancy-driven) or forced (fan-driven).

### Control Function
Logical function that activates/deactivates based on device inputs. Used with CTRL namelist.

## D

### Deardorff Model
Default LES turbulence model in FDS. Relates sub-grid scale turbulence to resolved flow field.

### Device (DEVC)
Measurement point or detector. Records quantities like temperature, velocity, or species concentration.

### DNS
Direct Numerical Simulation. Resolves all turbulent scales - too expensive for practical fire problems. FDS uses LES instead.

## E

### Emissivity
Surface radiative emissivity (0-1). Typical values: concrete 0.9, steel 0.7-0.95, aluminum 0.05-0.15.

### Extinction Coefficient
Measure of smoke obscuration. Related to visibility: vis ≈ C/K where C ≈ 8 (light-reflecting signs) or 3 (light-emitting signs).

## F

### FDS
Fire Dynamics Simulator. NIST's CFD model for fire-driven fluid flow. Open source, extensively validated.

### Flashover
Rapid transition to fully-developed fire where all combustible surfaces ignite. FDS can simulate post-flashover but not predict onset.

### Fuel
Combustible substance. In FDS, characterized by chemical formula, heat of combustion, and yields.

## G

### Grid
Computational mesh. Defines spatial discretization of domain.

### Grid Convergence
Process of demonstrating solution independence from mesh resolution. Run coarse/medium/fine meshes and compare.

## H

### Heat Flux
Rate of heat transfer per unit area (kW/m²). Components:
- Convective: From hot gases
- Radiative: From flames and hot surfaces
- Total (gauge): Sum of both

### Heat of Combustion
Energy released per unit mass of fuel burned (kJ/kg). Typical values: wood ~17,000, plastics 20,000-40,000.

### Heat Release Rate (HRR)
Power output of fire (kW or MW). Key quantity for fire size characterization.

### HRRPUA
Heat Release Rate Per Unit Area (kW/m²). Specifies fire intensity on surfaces.

### HVAC
Heating, Ventilation, and Air Conditioning. Can be modeled in FDS with ducts and fans.

## I

### IJK
Cell count triplet (I, J, K) for mesh in x, y, z directions. Product I×J×K = total cells.

### Init (INIT)
Initial conditions. Set temperature, velocity, or species at start of simulation.

### IOR
Index of Orientation. Indicates surface normal direction: ±1 (x), ±2 (y), ±3 (z). Used with VENT and surface devices.

## K

### K-factor
Sprinkler discharge coefficient relating flow rate to pressure: Q = K√P. Units: LPM/bar^0.5 or GPM/psi^0.5.

## L

### LES
Large Eddy Simulation. Turbulence model that resolves large scales and models small scales. FDS default approach.

### Layer Height
Height of smoke layer interface. Important for smoke control and tenability.

### Latch
Control property. If true, maintains activation state permanently. Used for sprinklers and alarm devices.

## M

### Material (MATL)
Thermal properties of solids: conductivity, specific heat, density, emissivity.

### Mesh (MESH)
Rectangular computational grid defined by IJK (cell counts) and XB (bounds).

### Mixture Fraction
Conserved scalar representing fuel/air mixing. FDS uses single mixture fraction for combustion.

### MLRPUA
Mass Loss Rate Per Unit Area (kg/s/m²). Alternative to HRRPUA for specifying burning rate.

## N

### Namelist
FDS input format. Group of parameters enclosed in &NAME ... / delimiters. PyFDS methods create these.

### NIST
National Institute of Standards and Technology. Developer of FDS.

## O

### Obstruction (OBST)
Solid object in domain. Defined by XB bounds and optional SURF_ID for thermal properties.

### Open Boundary
Boundary at ambient pressure where flow can enter/exit freely. Created with `SURF_ID='OPEN'`.

## P

### Plume
Rising column of hot gases above fire. Characterized by temperature, velocity, and entrainment rate.

### Pressure Solver
Algorithm for computing pressure field that ensures mass conservation. FDS uses FFT (single mesh) or iterative methods (multiple meshes).

### Pyrolysis
Thermal decomposition of solid fuel into gaseous products. Can be modeled in detail or simplified with prescribed HRRPUA.

## Q

### Quantity
Measured physical variable. Examples: TEMPERATURE, VELOCITY, HEAT FLUX, VISIBILITY. Specified in DEVC.

## R

### Radiation
Heat transfer by electromagnetic waves. Dominant in fires. FDS solves radiation transport equation with ~100 angles.

### Radiative Fraction
Fraction of combustion energy emitted as radiation (0-1). Typical: 0.3-0.4 for most fuels.

### Ramp (RAMP)
Time-varying function. Used to specify fire growth curves, decay, or other time-dependent inputs.

### RSET
Required Safe Egress Time. Time needed for complete evacuation. Must be less than ASET for safety.

### Reaction (REAC)
Combustion reaction. Defines fuel properties: formula, heat of combustion, soot yield, CO yield.

### RTI
Response Time Index. Thermal inertia of heat detector or sprinkler (m·s)^0.5. Typical: fast=30-50, standard=80-100, slow=150-250.

## S

### Simulation
PyFDS class for building FDS input files. Created with `Simulation(chid='name')`.

### Smokeview
FDS visualization program for viewing 3D results. Included with FDS download.

### Soot Yield
Soot production per unit fuel mass burned (kg/kg). Typical range: 0.001-0.1. Affects radiation and visibility.

### Species
Chemical components: fuel, oxygen, products, nitrogen. FDS tracks mass fractions.

### Sprinkler
Fire suppression device. Activates at temperature, sprays water to cool and suppress.

### Stratification
Vertical temperature/density gradients. Hot layer above, cool layer below. Gravity must be enabled.

### Surface (SURF)
Boundary condition specification. Can define fire source, thermal properties, velocity, or temperature.

## T

### Tenability
Conditions safe for human survival. Criteria include:
- Temperature: < 60°C at head height
- Visibility: > 10 m for large spaces, > 5 m for familiar areas
- CO: < 1,400 ppm for 30 min
- Heat flux: < 2.5 kW/m²

### Thermal Conductivity
Material property (W/m/K). Rate of heat conduction through solid. Insulator: < 0.1, metal: > 10.

### Time Step
Temporal discretization Δt. Auto-calculated by FDS from CFL condition. Can set initial value with DT.

### Turbulence
Chaotic fluid motion with eddies. FDS uses LES to model turbulent fire plumes.

## V

### Validation
Comparison of model predictions with experimental data. FDS has 100+ validation studies.

### Vent (VENT)
Boundary patch. Can be open, supply/exhaust, or any custom surface condition.

### Visibility
Distance at which signs are visible through smoke (m). Calculated from extinction coefficient.

### Viscosity
Fluid resistance to shear. Dynamic viscosity (Pa·s) or kinematic viscosity (m²/s).

### Volume Fraction
Species concentration as fraction of total volume (0-1). Alternative to mass fraction.

## W

### Wall Function
Model for heat transfer at solid boundaries. Accounts for convection, radiation, and conduction.

## X

### XB
Coordinate bounds: (x₀, x₁, y₀, y₁, z₀, z₁). Defines spatial extent of mesh, obstruction, vent, or device.

### XYZ
Point coordinates: (x, y, z). Used for point devices.

## Common Abbreviations

| Abbreviation | Meaning |
|--------------|---------|
| ASET | Available Safe Egress Time |
| BC | Boundary Condition |
| CFD | Computational Fluid Dynamics |
| CFL | Courant-Friedrichs-Lewy |
| CHID | Case IDentifier |
| CO | Carbon Monoxide |
| DNS | Direct Numerical Simulation |
| FDS | Fire Dynamics Simulator |
| HGL | Hot Gas Layer |
| HRR | Heat Release Rate |
| HRRPUA | Heat Release Rate Per Unit Area |
| HVAC | Heating, Ventilation, Air Conditioning |
| LES | Large Eddy Simulation |
| MLRPUA | Mass Loss Rate Per Unit Area |
| NIST | National Institute of Standards and Technology |
| RANS | Reynolds-Averaged Navier-Stokes |
| RSET | Required Safe Egress Time |
| RTI | Response Time Index |
| UL | Underwriters Laboratories |

## Units

### Common FDS Units

| Quantity | FDS Unit | SI Unit |
|----------|----------|---------|
| Length | m | m |
| Time | s | s |
| Temperature | °C | K (internally) |
| Pressure | Pa | Pa |
| Density | kg/m³ | kg/m³ |
| Velocity | m/s | m/s |
| Heat flux | kW/m² | W/m² |
| HRR | kW | W |
| Conductivity | W/m/K | W/m/K |
| Specific heat | kJ/kg/K | J/kg/K |
| Mass fraction | kg/kg | kg/kg |

### Unit Conversions

```python
# Temperature
celsius = kelvin - 273.15
fahrenheit = celsius * 9/5 + 32

# Heat flux
kW_per_m2 = BTU_per_ft2_s * 11.356

# HRR
kW = BTU_per_s * 1.055
MW = kW / 1000

# Pressure
bar = Pa / 100000
psi = Pa / 6894.76

# K-factor
LPM_per_bar05 = GPM_per_psi05 * 14.41
```

## PyFDS-Specific Terms

### Method Chaining
Returning `self` from methods to enable: `sim.time(...).mesh(...).surface(...)`.

### Validation
Checking simulation configuration for errors before writing. PyFDS validates automatically.

### FDS File
Text file with FDS namelists. Generated by `sim.write('file.fds')`.

### Results
Output from FDS simulation. Loaded with `FDSResults('chid')` for analysis.

## See Also

- [FDS Background](fds-background.md) - Detailed FDS theory
- [Namelist Reference](namelist-reference.md) - Complete parameter list
- [FAQ](faq.md) - Common questions
- [FDS User Guide](https://pages.nist.gov/fds-smv/) - Official documentation

---

[Back to Reference →](index.md){ .md-button .md-button--primary }
