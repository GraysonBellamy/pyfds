#!/usr/bin/env python3
"""
Sprinkler Activation Example
============================

Demonstrates a basic sprinkler with RTI-based thermal activation.
The sprinkler activates when link temperature reaches the setpoint,
then water droplets are released.

FDS Reference: Sprinklers_and_Sprays/bucket_test_1.fds
https://github.com/firemodels/fds/blob/master/Verification/Sprinklers_and_Sprays/bucket_test_1.fds

Key Namelists: PROP (RTI, ACTIVATION_TEMPERATURE, FLOW_RATE), PART, DEVC

Usage
-----
    python sprinkler_activation.py

Output
------
    fds/sprinklers/sprinkler_activation.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Mesh,
    Particle,
    Property,
    Ramp,
    Reaction,
    Species,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation with a thermally-activated sprinkler."""

    # Create simulation
    sim = Simulation(chid="sprinkler_activation", title="Sprinkler Activation Test")

    # Time
    sim.add(Time(t_end=60.0, dt=0.05))

    # Mesh
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(-2.0, 2.0, -2.0, 2.0, 0.0, 4.0)))

    # Simple reaction for fire
    sim.add(Reaction(fuel="PROPANE"))

    # Water vapor species for droplets
    sim.add(Species(id="WATER VAPOR"))

    # Water droplet particle class
    sim.add(
        Particle(
            id="WATER_DROPS",
            spec_id="WATER VAPOR",
            diameter=0.00075,  # 750 micron droplets
            sampling_factor=1,
        )
    )

    # Flow ramp: ramp up to full flow
    sim.add(Ramp(id="SPRAY", points=[(0.0, 0.0), (1.0, 1.0), (60.0, 1.0)]))

    # Sprinkler property with RTI activation
    sim.add(
        Property(
            id="K-11",
            quantity="SPRINKLER LINK TEMPERATURE",
            activation_temperature=68.0,  # 68°C activation (standard)
            rti=50.0,  # Response Time Index [m^0.5·s^0.5]
            c_factor=0.7,  # C-factor for convective heating
            part_id="WATER_DROPS",
            flow_rate=75.0,  # 75 L/min
            flow_ramp="SPRAY",
            particle_velocity=10.0,  # 10 m/s initial velocity
            spray_angle=(30.0, 80.0),  # Spray cone angles
            offset=0.10,  # Offset distance below ceiling
        )
    )

    # Sprinkler device at ceiling
    sim.add(
        Device(
            id="SPR_1",
            xyz=Point3D.of(0.0, 0.0, 3.9),
            prop_id="K-11",
            quantity="SPRINKLER LINK TEMPERATURE",
        )
    )

    # Fire source
    sim.add(Surface(id="BURNER", hrrpua=500.0, color="RED"))
    sim.add(
        Vent(
            xb=Bounds3D.of(-0.25, 0.25, -0.25, 0.25, 0.0, 0.0),
            surf_id="BURNER",
        )
    )

    # Open boundaries
    sim.add(Vent(mb="XMIN", surf_id="OPEN"))
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))
    sim.add(Vent(mb="YMIN", surf_id="OPEN"))
    sim.add(Vent(mb="YMAX", surf_id="OPEN"))
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "sprinklers")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
