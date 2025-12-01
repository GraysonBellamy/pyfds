# PyFDS Examples

This directory contains example simulations demonstrating the capabilities of PyFDS.
Examples are organized by topic and aligned with the [FDS Verification Suite](https://github.com/firemodels/fds/tree/master/Verification).

## Directory Structure

```
examples/
├── _common/                 # Shared utilities
├── getting_started/         # Tutorial progression (5 examples)
├── fires/                   # Fire source examples (6 examples)
├── heat_transfer/           # Heat transfer verification (6 examples)
├── species/                 # Species & combustion (5 examples)
├── pyrolysis/               # Solid phase reactions (3 examples)
├── sprinklers/              # Suppression systems (4 examples)
├── hvac/                    # Ventilation systems (4 examples)
├── controls/                # Control logic (4 examples)
├── flowfields/              # Fluid dynamics (4 examples)
├── pressure/                # Pressure effects (4 examples)
├── radiation/               # Radiation modeling (4 examples)
├── complex_geometry/        # Advanced geometry (4 examples)
├── multi_mesh/              # Parallel computing (3 examples)
├── output/                  # Output configuration (4 examples)
├── fds/                     # Generated FDS output files
└── README.md                # This file
```

## Quick Start

Run any example directly:

```bash
# From the repository root
python examples/getting_started/01_minimal_simulation.py

# Or from the examples directory
cd examples
python getting_started/01_minimal_simulation.py
```

## Example Categories

### Phase 1: Core Examples

#### Getting Started (Tutorial Progression)

Start here to learn PyFDS basics. Examples build on each other:

| # | Example | Description | Key Concepts |
|---|---------|-------------|--------------|
| 1 | `01_minimal_simulation.py` | Absolute minimum simulation | HEAD, TIME, MESH |
| 2 | `02_simple_fire.py` | Basic fire with HRR | SURF, OBST, REAC |
| 3 | `03_adding_devices.py` | Temperature & heat flux sensors | DEVC |
| 4 | `04_materials_and_surfaces.py` | Custom materials | MATL, SURF |
| 5 | `05_time_varying_fire.py` | RAMP for HRR curves | RAMP |

#### Fires

Various fire source configurations:

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `simple_burner.py` | Constant HRR burner | Fires/simple_test.fds |
| `circular_burner.py` | Circular vent geometry | Fires/circular_burner.fds |
| `spray_burner.py` | Liquid spray fire | Fires/spray_burner.fds |
| `pool_fire.py` | Liquid pool evaporation | Pyrolysis/water_pool.fds |
| `burn_away.py` | Material consumption | Fires/box_burn_away1.fds |
| `heat_of_combustion.py` | Custom heat of combustion | Fires/HoC_Ideal.fds |

#### Heat Transfer

Heat transfer verification cases:

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `conduction_1d.py` | 1D heat conduction | Heat_Transfer/heat_conduction_a.fds |
| `conduction_kc.py` | Temperature-dependent k | Heat_Transfer/heat_conduction_kc.fds |
| `convective_cooling.py` | Convective boundary conditions | Heat_Transfer/convective_cooling.fds |
| `adiabatic_surface.py` | Adiabatic surface temperature | Radiation/adiabatic_surface_temperature.fds |
| `ht3d_sphere.py` | 3D heat transfer in solid | Heat_Transfer/ht3d_sphere_24.fds |
| `thermocouples.py` | Thermocouple response time | Heat_Transfer/thermocouple_time_constant.fds |

---

### Phase 2: Physics Categories

#### Species & Combustion

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `predefined_fuel.py` | Using PROPANE, METHANE, etc. | Species/methane_flame_simple.fds |
| `custom_reaction.py` | User-defined reaction | Species/propane_flame_2reac.fds |
| `lumped_species.py` | Lumped species for air vitiation | Species/methane_flame_lumped.fds |
| `extinction.py` | Flame extinction modeling | Extinction/extinction_1.fds |
| `soot_yield.py` | Soot production | Aerosols/propane_flame_deposition.fds |

#### Pyrolysis

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `simple_pyrolysis.py` | Basic solid decomposition | Pyrolysis/birch_tga_1step_2.fds |
| `multilayer_wall.py` | Layered construction | Pyrolysis/two_step_solid_reaction.fds |
| `charring_material.py` | Char formation | Pyrolysis/cell_burn_away.fds |

#### Sprinklers & Sprays

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `sprinkler_activation.py` | RTI-based activation | Sprinklers_and_Sprays/activate_sprinklers.fds |
| `water_spray.py` | Spray patterns and droplets | Sprinklers_and_Sprays/water_evaporation_1.fds |
| `bucket_test.py` | Mass collection verification | Sprinklers_and_Sprays/bucket_test_1.fds |
| `particle_drag.py` | Droplet drag coefficient | Sprinklers_and_Sprays/particle_drag.fds |

#### HVAC

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `simple_duct.py` | Basic duct flow | Flowfields/simple_duct.fds |
| `fan_curve.py` | Fan pressure-flow curve | HVAC/fan_test.fds |
| `hvac_network.py` | Multi-node HVAC system | HVAC/ashrae7_table.fds |
| `door_crack.py` | Leakage modeling | HVAC/door_crack.fds |

---

### Phase 3: Advanced Features

#### Controls

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `device_activation.py` | DEVC setpoint triggering | Controls/device_test.fds |
| `control_logic.py` | Boolean control functions | Controls/control_test.fds |
| `vent_activation.py` | Automated vent control | Controls/activate_vents.fds |
| `hrr_freeze.py` | Freezing HRR at setpoint | Controls/hrr_freeze.fds |

#### Flowfields

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `gas_filling.py` | Mass injection into domain | Flowfields/gas_filling.fds |
| `helium_plume.py` | Buoyant gas release | Flowfields/helium_2d_isothermal.fds |
| `symmetry_test.py` | Symmetry boundary conditions | Flowfields/symmetry_test.fds |
| `velocity_bc.py` | Specified velocity inlet | Flowfields/velocity_bc_test.fds |

#### Pressure Effects

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `dynamic_pressure.py` | Dynamic pressure boundaries | Pressure_Effects/pressure_boundary.fds |
| `room_pressurization.py` | Sealed room pressurization | Pressure_Effects/pressure_rise.fds |
| `vent_pressure.py` | Pressure-driven vents | Pressure_Effects/zone_break.fds |
| `pressure_bc.py` | Pressure boundary conditions | Custom verification |

#### Radiation

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `adiabatic_surface.py` | Adiabatic surface temperature | Radiation/adiabatic_surface_temperature.fds |
| `emissivity_effects.py` | Surface emissivity | Radiation/emissivity.fds |
| `heat_flux_gauges.py` | Radiative heat flux measurement | Radiation/radiation_shield.fds |
| `radiative_fraction.py` | Radiative fraction control | Fires/simple_test.fds |

#### Complex Geometry

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `sphere_geometry.py` | Spherical GEOM objects | Complex_Geometry/geom_simple.fds |
| `cylinder_geometry.py` | Cylindrical geometry | Miscellaneous/obst_cylinder.fds |
| `obst_array.py` | MULT for obstruction arrays | (Various) |
| `hole_geometry.py` | HOLE for cutouts | (Various) |

#### Multi-Mesh

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `multi_mesh_basic.py` | Basic MPI decomposition | Flowfields/symmetry_test_mpi.fds |
| `mult_obstruction.py` | MULT for obstruction arrays | (Various) |
| `mesh_alignment.py` | Mesh boundary alignment | Pressure_Solver/dancing_eddies.fds |

#### Output Configuration

| Example | Description | FDS Reference |
|---------|-------------|---------------|
| `device_output.py` | Device output configuration | Controls/device_test.fds |
| `mesh_output.py` | Mesh-based output | (Various) |
| `statistics_output.py` | Statistical output | (Various) |
| `surface_output.py` | Surface quantity output | (Various) |

---

## Not Yet Implemented

The following categories require namelists not yet available in PyFDS:

- **Atmospheric Effects** - Requires WIND, ZONE, ATMO namelists
- **WUI (Wildfire)** - Requires vegetation/terrain namelists

---

## Output

All examples write FDS input files to the `fds/` directory, organized by category:

```
fds/
├── getting_started/
│   ├── minimal_simulation.fds
│   └── ...
├── fires/
├── heat_transfer/
├── species/
├── pyrolysis/
├── sprinklers/
├── hvac/
├── controls/
├── flowfields/
├── pressure/
├── radiation/
├── complex_geometry/
├── multi_mesh/
└── output/
```

## FDS Reference Links

All examples reference verification cases from the official FDS repository:

- **Base URL**: https://github.com/firemodels/fds/tree/master/Verification
- **FDS User Guide**: https://pages.nist.gov/fds-smv/
- **Technical Reference**: https://pages.nist.gov/fds-smv/FDS_Technical_Reference_Guide.pdf

## Creating Your Own Examples

Use the `_common` utilities for consistent output:

```python
from examples._common import write_example

# ... create simulation ...

output_path = write_example(sim, "my_category")
```

## Requirements

- Python 3.11+
- PyFDS (installed in development mode)

```bash
pip install -e ".[dev]"
```

## Summary Statistics

| Phase | Categories | Examples |
|-------|------------|----------|
| Phase 1 (Core) | 3 | 17 |
| Phase 2 (Physics) | 4 | 16 |
| Phase 3 (Advanced) | 7 | 27 |
| **Total** | **14** | **60** |

## See Also

- [EXAMPLES_PLAN.md](EXAMPLES_PLAN.md) - Full restructuring plan with all planned examples
- [PyFDS Documentation](../docs/) - Complete API documentation
- [FDS Verification Guide](https://pages.nist.gov/fds-smv/FDS_Verification_Guide.pdf)
