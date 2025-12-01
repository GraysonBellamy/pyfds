#!/usr/bin/env python3
"""
Bucket Test Example
===================

Demonstrates a sprinkler spray pattern test where water droplets
are collected at ground level. This tests spray angle distribution.

FDS Reference: Sprinklers_and_Sprays/bucket_test_1.fds
https://github.com/firemodels/fds/blob/master/Verification/Sprinklers_and_Sprays/bucket_test_1.fds

Key Namelists: PROP (SPRAY_ANGLE, PARTICLE_VELOCITY), DEVC (SETPOINT)

Usage
-----
    python bucket_test.py

Output
------
    fds/sprinklers/bucket_test.fds
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
    Species,
    Time,
    Vent,
)


def main():
    """Create a sprinkler bucket test simulation."""

    # Create simulation
    sim = Simulation(chid="bucket_test", title="Sprinkler Bucket Test")

    # Time
    sim.add(Time(t_end=40.0, dt=0.05))

    # Mesh covering spray area
    sim.add(
        Mesh(
            ijk=Grid3D.of(25, 25, 25),
            xb=Bounds3D.of(-5.0, 5.0, -5.0, 5.0, 0.0, 5.0),
        )
    )

    # Water vapor species
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

    # Flow ramp: ramp up, steady, ramp down
    sim.add(
        Ramp(
            id="SPRAY",
            points=[(0.0, 0.0), (1.0, 1.0), (30.0, 1.0), (31.0, 0.0)],
        )
    )

    # Sprinkler property with specific spray pattern
    sim.add(
        Property(
            id="K-11",
            quantity="SPRINKLER LINK TEMPERATURE",
            offset=0.10,
            part_id="WATER_DROPS",
            flow_rate=180.0,  # 180 L/min
            flow_ramp="SPRAY",
            particle_velocity=10.0,  # 10 m/s initial velocity
            spray_angle=(30.0, 80.0),  # Spray cone: 30° to 80° from vertical
            smokeview_id="sprinkler_upright",
        )
    )

    # Sprinkler device - activates immediately (setpoint=0)
    sim.add(
        Device(
            id="SPR_1",
            xyz=Point3D.of(0.0, 0.0, 4.9),
            prop_id="K-11",
            quantity="TIME",
            setpoint=0.0,  # Activate at t=0
        )
    )

    # Open boundaries on sides
    sim.add(Vent(mb="XMIN", surf_id="OPEN"))
    sim.add(Vent(mb="XMAX", surf_id="OPEN"))
    sim.add(Vent(mb="YMIN", surf_id="OPEN"))
    sim.add(Vent(mb="YMAX", surf_id="OPEN"))

    # Write output
    output_path = write_example(sim, "sprinklers")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
