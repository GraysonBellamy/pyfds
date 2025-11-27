"""
Example demonstrating Phase 3 namelists: RAMP, MATL, REAC, PROP, CTRL, INIT

This example creates a simple fire simulation with:
- Temperature-dependent material properties
- Time-varying heat release rate
- Sprinkler activation control
"""

from pathlib import Path

from pyfds.core.geometry import Point3D
from pyfds.core.namelists import (
    ControlFunction,
    Ctrl,
    Init,
    Material,
    Prop,
    Ramp,
    Reaction,
)
from pyfds.core.simulation import Simulation

# Create simulation
sim = Simulation(chid="phase3_demo", title="Phase 3 Features Demo")

# Set time parameters
sim.time(t_end=300.0, dt=0.1)

# Define mesh
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

# ===== RAMP: Time-varying heat release rate =====
hrr_ramp = Ramp(id="FIRE_RAMP", points=[(0, 0), (30, 100), (60, 500), (120, 1000), (300, 1000)])
sim.add_ramp(hrr_ramp)

# ===== REAC: Define combustion reaction =====
reaction = Reaction(fuel="PROPANE", soot_yield=0.015, co_yield=0.01)
sim.add_reaction(reaction)

# ===== MATL: Material with temperature-dependent conductivity =====
# First, create a temperature ramp for conductivity
k_ramp = Ramp(id="STEEL_K", points=[(20, 54.0), (200, 48.0), (400, 40.0), (800, 27.0)])
sim.add_ramp(k_ramp)

# Now create the material
steel = Material(
    id="STEEL", density=7850.0, conductivity_ramp="STEEL_K", specific_heat=0.46, emissivity=0.85
)
sim.add_material(steel)

# Simple wood material
wood = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5, emissivity=0.9)
sim.add_material(wood)

# ===== SURF: Define surfaces =====
# Fire surface with HRR ramp
sim.surface(id="FIRE", hrrpua=1000.0, color="RED")

# Steel wall
sim.surface(id="STEEL_WALL", matl_id="STEEL", thickness=0.01, color="GRAY")

# ===== PROP: Sprinkler properties =====
sprinkler_prop = Prop(id="SPRINKLER", activation_temperature=68, rti=50, flow_rate=60, k_factor=80)
sim.add_prop(sprinkler_prop)

# ===== OBST: Create geometry =====
# Floor (fire source)
sim.obstruction(xb=(2, 3, 2, 3, 0, 0.01), surf_id="FIRE")

# Walls
sim.obstruction(xb=(0, 5, 0, 0.1, 0, 2.5), surf_id="STEEL_WALL")
sim.obstruction(xb=(0, 5, 4.9, 5, 0, 2.5), surf_id="STEEL_WALL")
sim.obstruction(xb=(0, 0.1, 0, 5, 0, 2.5), surf_id="STEEL_WALL")
sim.obstruction(xb=(4.9, 5, 0, 5, 0, 2.5), surf_id="STEEL_WALL")

# ===== DEVC: Temperature sensors =====
sim.device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=Point3D(2.5, 2.5, 2.0))
sim.device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=Point3D(2.5, 2.5, 2.45))

# ===== CTRL: Sprinkler control (activate above 60°C) =====
# Note: In a real simulation, you'd link this to a sprinkler device
ctrl = Ctrl(
    id="SPRINKLER_CTRL",
    function_type=ControlFunction.CUSTOM,
    input_id="TEMP_CEILING",
    initial_state=False,
)
sim.add_ctrl(ctrl)

# ===== INIT: Initial hot gas layer =====
init = Init(xb=(0, 5, 0, 5, 2.0, 2.5), temperature=100)
sim.add_init(init)

# Generate FDS file
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)
output_file = sim.write(output_dir / "phase3_demo.fds")
print(f"✓ Created FDS file: {output_file}")
print(f"✓ Simulation uses {len(sim.ramps.ramps)} ramps")
print(f"✓ Simulation uses {len(sim.material_mgr.materials)} materials")
print(f"✓ Simulation uses {len(sim.physics.reactions)} reaction(s)")
print(f"✓ Simulation uses {len(sim.instrumentation.props)} device properties")
print(f"✓ Simulation uses {len(sim.controls.ctrls)} controls")
print(f"✓ Simulation uses {len(sim.controls.inits)} initial condition(s)")
