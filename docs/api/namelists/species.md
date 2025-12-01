# Species Namelists

Classes for defining chemical species and combustion parameters in FDS simulations.

## Species

The `Species` class represents the FDS `SPEC` namelist for defining gas species.

::: pyfds.core.namelists.Species
    options:
      show_source: true
      members:
        - __init__

## Combustion

The `Combustion` class represents the FDS `COMB` namelist for combustion model parameters.

::: pyfds.core.namelists.Combustion
    options:
      show_source: true
      members:
        - __init__

## Usage Examples

### Predefined Species

```python
from pyfds import Simulation, Species, Reaction

sim = Simulation(chid="species_example")

# Use a predefined fuel - FDS knows the properties
sim.add(Reaction(fuel="METHANE", co_yield=0.1))
```

### Custom Species

```python
from pyfds import Simulation, Species

sim = Simulation(chid="custom_species")

# Define a custom species with specific properties
sim.add(Species(
    id="MY_GAS",
    formula="C2H6O",  # Ethanol
    mw=46.07
))
```

### Lumped Species

```python
from pyfds import Simulation, Species

sim = Simulation(chid="lumped_example")

# Define component species
sim.add(Species(id="NITROGEN", lumped_component_only=True))
sim.add(Species(id="OXYGEN", lumped_component_only=True))

# Define lumped species from components
sim.add(Species(
    id="AIR",
    spec_id=["NITROGEN", "OXYGEN"],
    mass_fraction=[0.767, 0.233],
    background=True
))
```

## See Also

- [Species Examples](../../examples/species.md) - Working species examples
- [Combustion Guide](../../guide/combustion.md) - Combustion modeling
- [Reaction Class](complex.md) - Reaction definitions
