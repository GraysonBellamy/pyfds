# FDS Background

Understanding Fire Dynamics Simulator (FDS) fundamentals for effective PyFDS usage.

## What is FDS?

Fire Dynamics Simulator (FDS) is a computational fluid dynamics (CFD) model developed by the National Institute of Standards and Technology (NIST) for simulating fire-driven fluid flow.

### Key Characteristics

**Specialized for Fire**

- Focus on smoke and heat transport from fires
- Low-speed, thermally-driven flows
- Large Eddy Simulation (LES) turbulence model
- Radiation transport solver

**Open Source**

- Free to download and use
- Active development by NIST
- Large international user community
- Extensive validation studies

**Well Validated**

- 100+ validation studies
- Compared against experimental data
- Published in peer-reviewed literature
- International collaborations

## Physical Models

### Governing Equations

FDS solves the Navier-Stokes equations for low-speed, thermally-driven flows:

**Conservation of Mass**

$$
\frac{\partial \rho}{\partial t} + \nabla \cdot \rho \mathbf{u} = 0
$$

**Conservation of Momentum**

$$
\frac{\partial}{\partial t}(\rho \mathbf{u}) + \nabla \cdot \rho \mathbf{u} \mathbf{u} + \nabla p = \rho \mathbf{g} + \mathbf{f} + \nabla \cdot \tau_{ij}
$$

**Conservation of Energy**

$$
\frac{\partial}{\partial t}(\rho h_s) + \nabla \cdot \rho h_s \mathbf{u} = \frac{Dp}{Dt} + \dot{q}''' - \dot{q}_b''' - \nabla \cdot \dot{q}''
$$

**Conservation of Species**

$$
\frac{\partial}{\partial t}(\rho Y_\alpha) + \nabla \cdot \rho Y_\alpha \mathbf{u} = \nabla \cdot \rho D_\alpha \nabla Y_\alpha + \dot{m}_\alpha'''
$$

### Turbulence Modeling

**Large Eddy Simulation (LES)**

FDS uses LES to model turbulent flow:

- Resolved scales: Computed directly on mesh
- Sub-grid scales: Modeled using turbulence models
- Deardorff model: Default turbulence closure

**Mesh Requirements**

For good LES results:

$$
D^* / \delta x \approx 4 \text{ to } 16
$$

Where:

- $D^*$ is the characteristic fire diameter
- $\delta x$ is the mesh cell size

### Combustion

**Mixture Fraction Model**

FDS uses a mixture fraction combustion model:

- Fuel and oxygen mix based on transport
- Combustion is infinitely fast (mixing-controlled)
- Heat release rate specified directly

**Simple Chemistry**

$$
\text{Fuel} + s \text{O}_2 \rightarrow \text{Products}
$$

Parameters:

- Heat of combustion
- Soot yield
- CO yield
- Radiative fraction

### Radiation

**Radiation Transport Equation (RTE)**

$$
\mathbf{s} \cdot \nabla I(\mathbf{x}, \mathbf{s}) = \kappa I_b(T) - \kappa I(\mathbf{x}, \mathbf{s})
$$

**Finite Volume Method**

- Divides solid angle into ~100 angles
- Solves RTE for each angle
- Accounts for absorption by soot and gases

**Gray Gas Assumption**

- Single absorption coefficient
- Function of soot concentration
- Simplifies multi-wavelength problem

## Numerical Methods

### Spatial Discretization

**Structured Rectilinear Grid**

- Cartesian coordinate system
- Rectangular cells (can be non-uniform)
- Simplifies obstacle and boundary representation

**Finite Difference Approximations**

- Second-order accurate in space
- Conservative formulation
- Preserves mass and energy

### Time Integration

**Predictor-Corrector Scheme**

1. **Predictor**: Explicit update
2. **Corrector**: Implicit pressure solve
3. Ensures divergence-free velocity field

**Time Step Control**

CFL condition limits time step:

$$
\Delta t \leq \frac{\delta x}{|\mathbf{u}| + c}
$$

Where $c$ is the speed of sound.

### Pressure Solver

**Poisson Equation**

$$
\nabla^2 H = -\frac{\partial}{\partial t}(\nabla \cdot \mathbf{u}) - \nabla \cdot \mathbf{F}
$$

Solved using:

- Fast Fourier Transform (FFT) for single mesh
- Iterative methods for multiple meshes

## Application Areas

### Building Fire Safety

**Evacuation Analysis**

- Smoke spread in buildings
- Tenability assessment
- Available safe egress time (ASET)

**Fire Protection Design**

- Sprinkler system effectiveness
- Smoke control systems
- Fire barrier performance

**Code Compliance**

- Performance-based design
- Alternative compliance methods
- Engineering analysis

### Wildland Fires

**Wildfire Spread**

- Wind-driven fire behavior
- Vegetation combustion
- Ember transport

**Wildland-Urban Interface**

- Structure ignition
- Fire break effectiveness
- Community protection

### Industrial Applications

**Process Safety**

- Flammable liquid fires
- Chemical facility hazards
- Explosion modeling

**Nuclear Safety**

- Cable fire scenarios
- Smoke transport in facilities
- Equipment damage assessment

### Special Applications

**Aircraft Fires**

- Cabin fire scenarios
- Cargo hold fires
- Fuel spill fires

**Tunnel Fires**

- Smoke control in tunnels
- Critical velocity for smoke
- Emergency egress

**Marine Fires**

- Ship compartment fires
- Offshore platform scenarios
- Ferry evacuation

## Mesh Design

### Characteristic Fire Diameter

The characteristic fire diameter is:

$$
D^* = \left(\frac{\dot{Q}}{\rho_\infty c_p T_\infty \sqrt{g}}\right)^{2/5}
$$

Where:

- $\dot{Q}$ is the heat release rate (kW)
- $\rho_\infty$ is ambient density (kg/m³)
- $c_p$ is specific heat (kJ/kg·K)
- $T_\infty$ is ambient temperature (K)
- $g$ is gravity (m/s²)

### Mesh Resolution Guidelines

| Application | $D^*/\delta x$ | Quality |
|-------------|----------------|---------|
| Coarse | 2-4 | Rough estimates |
| Moderate | 4-10 | Typical design |
| Fine | 10-16 | High accuracy |
| Very Fine | >16 | Research quality |

### Aspect Ratio

Cell aspect ratios should be reasonable:

- Ideal: 1:1:1 (cubic cells)
- Acceptable: Up to 2:1 aspect ratio
- Maximum: Generally avoid >4:1

## Boundary Conditions

### Wall Functions

**Heat Transfer at Walls**

$$
\dot{q}'' = h(T_w - T_g) + \epsilon \sigma (T_w^4 - T_\infty^4)
$$

Components:

- Convective heat transfer
- Radiative heat transfer
- 1D heat conduction into solid

**Material Properties**

Required for solid boundaries:

- Thermal conductivity
- Specific heat
- Density
- Emissivity

### Open Boundaries

**Pressure Boundary**

- Atmospheric pressure specified
- Flow in/out determined by simulation
- Common for room openings

**Velocity Boundary**

- Velocity specified
- Used for forced ventilation
- Supply/exhaust ducts

## Devices and Measurements

### Point Measurements

Measure quantities at specific locations:

- Temperature
- Velocity
- Species concentration
- Pressure

### Surface Measurements

Measure quantities on surfaces:

- Heat flux
- Wall temperature
- Radiative flux
- Convective flux

### Special Devices

**Sprinklers**

- RTI-based activation
- Spray pattern modeling
- Cooling effects

**Heat Detectors**

- RTI-based response
- Activation temperature
- Plume correlation

## Validation

### NIST Approach

FDS validation follows scientific method:

1. **Identify scenario**
2. **Select experiments**
3. **Setup simulation**
4. **Compare results**
5. **Document uncertainty**

### Validation Studies

Published comparisons include:

- Room fire experiments
- Plume correlations
- Detector activation
- Sprinkler suppression
- Tunnel fires
- Wildland fires

### Uncertainty

FDS predictions have typical uncertainties:

| Quantity | Uncertainty |
|----------|-------------|
| Gas temperature | ±15% |
| Heat flux | ±25% |
| Velocity | ±20% |
| Species | ±20-40% |

## Best Practices

### Model Setup

**Define Objectives**

- What questions need answers?
- What accuracy is required?
- What resources are available?

**Start Simple**

- Begin with coarse mesh
- Add complexity incrementally
- Validate at each step

**Document Assumptions**

- Material properties
- Fire characteristics
- Boundary conditions
- Mesh resolution

### Quality Assurance

**Grid Sensitivity**

Run multiple mesh resolutions:

- Coarse, medium, fine
- Compare key results
- Demonstrate convergence

**Verification**

Check simulation health:

- Mass conservation
- Energy balance
- CFL condition
- Pressure iterations

**Validation**

Compare against:

- Experimental data
- Analytical solutions
- Engineering correlations

### Reporting Results

**Include Context**

- Simulation objectives
- Key assumptions
- Limitations
- Uncertainties

**Show Sensitivity**

- Parameter variations
- Mesh convergence
- Scenario variations

**Visual Presentation**

- Clear plots
- Smokeview images
- Comparison charts
- Tables of results

## Limitations

### Model Limitations

**Mixture Fraction Combustion**

- Assumes fast chemistry
- Cannot predict ignition
- Limited for non-standard fuels

**Gray Gas Radiation**

- Single absorption coefficient
- May not capture spectral effects
- Simplified compared to reality

**LES Turbulence**

- Requires adequate mesh resolution
- Not RANS (time-averaged)
- Computationally expensive

### Application Limits

**Not Suitable For**

- Backdraft/flashover prediction
- Detailed ignition processes
- Slow pyrolysis
- Supersonic flows
- Strongly stratified flows with sharp gradients

**Use with Caution**

- Very large domains (>100 m)
- Very long times (>hours)
- Complex chemical kinetics
- Multi-phase flows

## Resources

### Official Documentation

- [FDS User Guide](https://pages.nist.gov/fds-smv/) - Complete documentation
- [FDS Technical Reference](https://pages.nist.gov/fds-smv/) - Theory and validation
- [FDS Validation Guide](https://pages.nist.gov/fds-smv/) - Validation studies

### Community

- [FDS Discussion Group](https://groups.google.com/g/fds-smv) - User forum
- [GitHub Issues](https://github.com/firemodels/fds) - Bug reports
- Training courses - NIST and international

### Publications

Key papers:

- McGrattan et al., "Fire Dynamics Simulator Technical Reference Guide"
- McGrattan et al., "Fire Dynamics Simulator User's Guide"
- Validation study compilations

## Using FDS with PyFDS

### PyFDS Advantages

**Programmatic Control**

```python
from pyfds import Simulation

# Build simulation in Python
sim = Simulation(chid='example')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# Write FDS file
sim.write('example.fds')
```

**Reproducibility**

- Version control with git
- Parameterized cases
- Automated workflows

**Integration**

- Pre-processing in Python
- Run FDS simulations
- Post-process results
- Generate reports

### Workflow

```mermaid
graph LR
    A[Define Parameters] --> B[Build Simulation]
    B --> C[Validate]
    C --> D[Run FDS]
    D --> E[Analyze Results]
    E --> F[Report]

    style A fill:#ff6b35
    style D fill:#004e89
    style F fill:#00a878
```

## Next Steps

- [User Guide](../guide/index.md) - Learn PyFDS syntax
- [Examples](../examples/index.md) - See complete simulations
- [Running Simulations](../execution/running.md) - Execute FDS
- [Analysis](../execution/analysis.md) - Process results

---

[Namelist Reference →](namelist-reference.md){ .md-button .md-button--primary }
