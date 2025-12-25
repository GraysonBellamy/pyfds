# Chemical Species Examples

Examples demonstrating species tracking, custom reactions, and combustion chemistry in PyFDS.

## Overview

FDS supports several approaches to modeling combustion and species:

- **Simple chemistry** - Use predefined fuels with automatic stoichiometry
- **Lumped species** - Group multiple species to reduce computational cost
- **Custom reactions** - Define custom fuels and reaction parameters
- **Soot and CO yields** - Control combustion product generation

## Predefined Fuel Example

Use FDS predefined fuels for simple simulations:

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Surface, Obstruction, Reaction, Combustion, Ramp
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid="predefined_fuel", title="Methane Flame with Simple Chemistry")

# Time and domain
sim.add(Time(t_end=10.0, dt=0.01))
sim.add(Mesh(ijk=Grid3D.of(10, 10, 20), xb=Bounds3D.of(0.0, 1.0, 0.0, 1.0, 0.0, 2.0)))

# Combustion settings
sim.add(Combustion(suppression=False))

# Reaction using predefined fuel - FDS handles stoichiometry automatically
# CO_YIELD specifies carbon monoxide production rate
sim.add(Reaction(fuel="METHANE", co_yield=0.1))

# Fire ramp: full power for 5 seconds, then off
sim.add(Ramp(id="HRR", points=[(0.0, 1.0), (5.0, 1.0), (5.01, 0.0), (10.0, 0.0)]))

# Burner surface with 625 kW/m² (100 kW total for 0.4x0.4 m burner)
sim.add(Surface(id="BURNER", hrrpua=625.0, ramp_q="HRR"))

# Burner obstruction
sim.add(Obstruction(
    xb=Bounds3D.of(0.3, 0.7, 0.3, 0.7, 0.0, 0.1),
    surf_ids=('BURNER', 'INERT', 'INERT')
))

sim.write("predefined_fuel.fds")
```

### Predefined Fuels in FDS

| Fuel Name | Formula | Heat of Combustion (kJ/kg) |
|-----------|---------|---------------------------|
| METHANE | CH₄ | 50,016 |
| PROPANE | C₃H₈ | 46,334 |
| WOOD | CH₁.₇O₀.₇₄N₀.₀₀₂ | 16,400 |
| POLYURETHANE | CH₁.₁₇O₀.₃₃N₀.₀₇ | 25,300 |

## Lumped Species Example

Reduce computational cost by grouping species:

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Surface, Vent, Reaction, Species, Combustion, Ramp
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid="lumped_species", title="Methane Flame with Lumped Species")

# Time and domain
sim.add(Time(t_end=10.0))
sim.add(Mesh(ijk=Grid3D.of(10, 10, 20), xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 0.0, 2.0)))

# Combustion settings
sim.add(Combustion(suppression=False))

# Define primitive species as lumped components
# These won't be tracked individually, only as part of lumped species
sim.add(Species(id="NITROGEN", lumped_component_only=True))
sim.add(Species(id="OXYGEN", lumped_component_only=True))
sim.add(Species(id="WATER VAPOR", lumped_component_only=True))
sim.add(Species(id="CARBON DIOXIDE", lumped_component_only=True))

# Lumped AIR species (mass fractions: 76.7% N2, 23.3% O2)
sim.add(Species(
    id="AIR",
    spec_id=["NITROGEN", "OXYGEN"],
    mass_fraction=[0.767, 0.233],
    background=True  # This is the ambient gas
))

# Lumped PRODUCTS species (combustion products)
sim.add(Species(
    id="PRODUCTS",
    spec_id=["NITROGEN", "CARBON DIOXIDE", "WATER VAPOR"],
    mass_fraction=[0.726, 0.158, 0.116]
))

# Reaction: methane + air -> products
sim.add(Reaction(
    fuel="METHANE",
    spec_id_nu=["METHANE", "AIR", "PRODUCTS"],
    nu=[-1.0, -17.4, 18.4],  # mass-based stoichiometry
    heat_of_combustion=50000.0  # kJ/kg
))

# Fire ramp
sim.add(Ramp(id="FIRE_RAMP", points=[(0.0, 0.0), (2.0, 1.0), (10.0, 1.0)]))

# Burner surface
sim.add(Surface(id="BURNER", hrrpua=1000.0, ramp_q="FIRE_RAMP"))

# Burner vent at floor
sim.add(Vent(
    xb=Bounds3D.of(-0.05, 0.05, -0.05, 0.05, 0.0, 0.0),
    surf_id="BURNER"
))

# Open boundaries
sim.add(Vent(xb=Bounds3D.of(-0.5, -0.5, -0.5, 0.5, 0.0, 2.0), surf_id="OPEN"))
sim.add(Vent(xb=Bounds3D.of(0.5, 0.5, -0.5, 0.5, 0.0, 2.0), surf_id="OPEN"))
sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, -0.5, -0.5, 0.0, 2.0), surf_id="OPEN"))
sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, 0.5, 0.5, 0.0, 2.0), surf_id="OPEN"))
sim.add(Vent(xb=Bounds3D.of(-0.5, 0.5, -0.5, 0.5, 2.0, 2.0), surf_id="OPEN"))

sim.write("lumped_species.fds")
```

## Custom Reaction Example

Define custom fuels with specific properties:

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Surface, Obstruction, Reaction, Species
from pyfds.core.geometry import Bounds3D, Grid3D

sim = Simulation(chid="custom_fuel", title="Custom Fuel Reaction")

sim.add(Time(t_end=30.0))
sim.add(Mesh(ijk=Grid3D.of(20, 20, 30), xb=Bounds3D.of(0, 2, 0, 2, 0, 3)))

# Define a custom fuel species
sim.add(Species(
    id="MY_FUEL",
    formula="C6H10O5",  # Cellulose-like
    mw=162.14  # Molecular weight
))

# Custom reaction with specified yields
sim.add(Reaction(
    fuel="MY_FUEL",
    heat_of_combustion=17000.0,  # kJ/kg
    soot_yield=0.02,             # 2% soot
    co_yield=0.01                # 1% CO
))

# Fire surface
sim.add(Surface(id="FIRE", hrrpua=500.0, color="RED"))
sim.add(Obstruction(xb=Bounds3D.of(0.5, 1.5, 0.5, 1.5, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))

sim.write("custom_fuel.fds")
```

## Soot Yield Example

Control soot production for visibility calculations:

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Surface, Obstruction, Reaction, Device
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D

sim = Simulation(chid="soot_visibility", title="Soot Yield and Visibility")

sim.add(Time(t_end=300.0))
sim.add(Mesh(ijk=Grid3D.of(40, 40, 20), xb=Bounds3D.of(0, 4, 0, 4, 0, 2)))

# Reaction with high soot yield (typical of plastics)
sim.add(Reaction(
    fuel="POLYURETHANE",
    soot_yield=0.10,   # 10% soot yield
    co_yield=0.02      # 2% CO yield
))

# Fire source
sim.add(Surface(id="FIRE", hrrpua=300.0))
sim.add(Obstruction(xb=Bounds3D.of(1.5, 2.5, 1.5, 2.5, 0, 0.1), surf_ids=('FIRE', 'INERT', 'INERT')))

# Visibility measurement devices
for z in [0.5, 1.0, 1.5]:
    sim.add(Device(
        id=f"VIS_{int(z*10)}",
        quantity="VISIBILITY",
        xyz=Point3D.of(2.0, 2.0, z)
    ))

# Soot density measurement
sim.add(Device(
    id="SOOT_DENSITY",
    quantity="DENSITY",
    spec_id="SOOT",
    xyz=Point3D.of(2.0, 2.0, 1.8)
))

sim.write("soot_visibility.fds")
```

## Extinction Modeling

Model flame extinction under low-oxygen conditions:

```python
from pyfds import Simulation
from pyfds.core.namelists import Time, Mesh, Surface, Vent, Reaction, Combustion, Device
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D

sim = Simulation(chid="extinction", title="Flame Extinction Example")

sim.add(Time(t_end=60.0))
sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

# Enable suppression (extinction modeling)
sim.add(Combustion(
    suppression=True,
    extinction_model="EXTINCTION 2"  # Critical flame temperature model
))

# Propane reaction
sim.add(Reaction(
    fuel="PROPANE",
    soot_yield=0.01,
    co_yield=0.006,
    critical_flame_temperature=1427.0  # °C for extinction
))

# Fire surface
sim.add(Surface(id="FIRE", hrrpua=1000.0))

# Burner vent at floor
sim.add(Vent(
    xb=Bounds3D.of(0.5, 1.5, 0.5, 1.5, 0.0, 0.0),
    surf_id="FIRE"
))

# Enclosure walls (no openings - fire will self-extinguish)
for vent_xb in [
    Bounds3D.of(0, 0, 0, 2, 0, 2),
    Bounds3D.of(2, 2, 0, 2, 0, 2),
    Bounds3D.of(0, 2, 0, 0, 0, 2),
    Bounds3D.of(0, 2, 2, 2, 0, 2),
    Bounds3D.of(0, 2, 0, 2, 2, 2),
]:
    sim.add(Vent(xb=vent_xb, surf_id="INERT"))

# Monitor oxygen and HRR
sim.add(Device(id="O2", quantity="VOLUME FRACTION", spec_id="OXYGEN", xyz=Point3D.of(1, 1, 1)))
sim.add(Device(id="HRR", quantity="HRR", xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))

sim.write("extinction.fds")
```

## See Also

- [Combustion Guide](../guide/combustion.md) - Combustion modeling overview
- [Advanced Chemistry](../guide/combustion_chemistry.md) - Detailed reaction chemistry
- [Basic Examples](basic.md) - Getting started examples
- [Advanced Examples](advanced.md) - Complex simulation examples
