#!/usr/bin/env python3
"""
Adding Devices Example
======================

This example demonstrates how to add measurement devices to capture
simulation data. Devices (DEVC) are essential for extracting time-series
data from FDS simulations.

FDS Reference: Heat_Transfer/adiabatic_con_flux.fds
https://github.com/firemodels/fds/blob/master/Verification/Heat_Transfer/adiabatic_con_flux.fds

Key Namelists: DEVC

Usage
-----
    python 03_adding_devices.py

Output
------
    fds/getting_started/adding_devices.fds
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from examples._common import write_example
from pyfds import Simulation
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Device, Mesh, Obstruction, Reaction, Surface, Time


def main():
    """Create a simulation with measurement devices."""

    # Create simulation
    sim = Simulation(chid="adding_devices", title="Fire with Measurement Devices")

    # Time: 60 seconds
    sim.add(Time(t_end=60.0))

    # Mesh: Simple room
    sim.add(Mesh(ijk=Grid3D.of(30, 30, 20), xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 2.0)))

    # Reaction
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))

    # Fire surface
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="RED"))

    # Fire source
    sim.add(
        Obstruction(
            xb=Bounds3D.of(1.25, 1.75, 1.25, 1.75, 0.0, 0.1),
            surf_ids=("FIRE", "INERT", "INERT"),
        )
    )

    # =========================================================================
    # DEVICES - Various measurement types
    # =========================================================================

    # Point temperature measurement at ceiling
    sim.add(Device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=Point3D.of(1.5, 1.5, 1.95)))

    # Temperature measurements at different heights (thermocouple tree)
    heights = [0.5, 1.0, 1.5, 1.9]
    for h in heights:
        sim.add(
            Device(id=f"TEMP_{int(h * 100)}cm", quantity="TEMPERATURE", xyz=Point3D.of(1.5, 1.5, h))
        )

    # Heat flux gauge on floor
    sim.add(
        Device(
            id="HF_FLOOR",
            quantity="GAUGE HEAT FLUX",
            xyz=Point3D.of(2.5, 2.5, 0.0),
            ior=3,  # Facing up (+Z direction)
        )
    )

    # Convective heat flux on wall
    sim.add(
        Device(
            id="HF_WALL",
            quantity="CONVECTIVE HEAT FLUX",
            xyz=Point3D.of(0.0, 1.5, 1.0),
            ior=1,  # Facing +X direction
        )
    )

    # Volume mean temperature
    sim.add(
        Device(
            id="TEMP_AVG",
            quantity="TEMPERATURE",
            xb=Bounds3D.of(0.5, 2.5, 0.5, 2.5, 0.5, 1.9),
            spatial_statistic="VOLUME MEAN",
        )
    )

    # Write output
    output_path = write_example(sim, "getting_started")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
