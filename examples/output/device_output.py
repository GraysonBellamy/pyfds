#!/usr/bin/env python3
"""
Device Output Example
=====================

Demonstrates device-based output configuration including various
QUANTITY types, spatial configurations, and output options.

FDS Reference: Controls/device_test.fds
https://github.com/firemodels/fds/blob/master/Verification/Controls/device_test.fds

Key Namelists: DEVC (QUANTITY, XB, XYZ, STATISTICS), PROP

Usage
-----
    python device_output.py

Output
------
    fds/output/device_output.fds
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
    Property,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create a simulation demonstrating various device output configurations."""

    # Create simulation
    sim = Simulation(
        chid="device_output",
        title="Device Output Configuration Examples",
    )

    # Time parameters
    sim.add(Time(t_end=60.0))

    # Single mesh
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 30, 30),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
        )
    )

    # Simple fire for generating data to measure
    sim.add(Reaction(fuel="PROPANE", soot_yield=0.01))
    sim.add(Surface(id="FIRE", hrrpua=500.0, color="ORANGE"))

    sim.add(
        Vent(
            xb=Bounds3D.of(1.2, 1.8, 1.2, 1.8, 0.0, 0.0),
            surf_id="FIRE",
        )
    )

    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # ==========================================================================
    # Point Measurements (XYZ)
    # ==========================================================================

    # Basic point temperature measurement
    sim.add(
        Device(
            id="TEMP_CENTER",
            xyz=Point3D(1.5, 1.5, 1.5),
            quantity="TEMPERATURE",
        )
    )

    # Multiple temperatures at different heights
    for z in [0.5, 1.0, 1.5, 2.0, 2.5]:
        sim.add(
            Device(
                id=f"TEMP_Z{int(z * 10):02d}",
                xyz=Point3D(1.5, 1.5, z),
                quantity="TEMPERATURE",
            )
        )

    # Velocity measurement
    sim.add(
        Device(
            id="VEL_PLUME",
            xyz=Point3D(1.5, 1.5, 2.0),
            quantity="VELOCITY",
        )
    )

    # W-velocity (vertical component only)
    sim.add(
        Device(
            id="W_VEL_PLUME",
            xyz=Point3D(1.5, 1.5, 2.0),
            quantity="W-VELOCITY",
        )
    )

    # Heat flux at target
    sim.add(
        Device(
            id="HF_TARGET",
            xyz=Point3D(2.5, 1.5, 0.5),
            quantity="INCIDENT HEAT FLUX",
            ior=1,  # Facing -X direction (toward fire)
        )
    )

    # ==========================================================================
    # Volume Measurements (XB)
    # ==========================================================================

    # Total HRR in entire domain
    sim.add(
        Device(
            id="HRR_TOTAL",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
            quantity="HRR",
        )
    )

    # Layer height measurement using integration
    sim.add(
        Device(
            id="LAYER_HEIGHT",
            xb=Bounds3D.of(1.4, 1.6, 1.4, 1.6, 0.0, 3.0),
            quantity="LAYER HEIGHT",
        )
    )

    # Upper layer temperature
    sim.add(
        Device(
            id="UPPER_TEMP",
            xb=Bounds3D.of(1.4, 1.6, 1.4, 1.6, 0.0, 3.0),
            quantity="UPPER TEMPERATURE",
        )
    )

    # Average temperature in upper layer region
    sim.add(
        Device(
            id="AVG_TEMP_UPPER",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 2.0, 3.0),
            quantity="TEMPERATURE",
            statistics="VOLUME MEAN",
        )
    )

    # Maximum temperature anywhere in domain
    sim.add(
        Device(
            id="MAX_TEMP",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 3.0),
            quantity="TEMPERATURE",
            statistics="MAX",
        )
    )

    # ==========================================================================
    # Sensor Properties (PROP)
    # ==========================================================================

    # Define thermocouple with response time
    sim.add(
        Property(
            id="TC_SLOW",
            quantity="THERMOCOUPLE",
            bead_diameter=0.003,  # 3mm bead
            bead_emissivity=0.9,
        )
    )

    sim.add(
        Property(
            id="TC_FAST",
            quantity="THERMOCOUPLE",
            bead_diameter=0.001,  # 1mm bead (faster response)
            bead_emissivity=0.9,
        )
    )

    # Thermocouples with different response times
    sim.add(
        Device(
            id="TC_SLOW_CENTER",
            xyz=Point3D(1.5, 1.5, 2.5),
            prop_id="TC_SLOW",
        )
    )

    sim.add(
        Device(
            id="TC_FAST_CENTER",
            xyz=Point3D(1.5, 1.5, 2.5),
            prop_id="TC_FAST",
        )
    )

    # ==========================================================================
    # Smoke and Visibility
    # ==========================================================================

    # Smoke detector (chamber obscuration)
    sim.add(
        Property(
            id="SMOKE_DET",
            quantity="CHAMBER OBSCURATION",
            alpha_e=2.5,  # Extinction coefficient for alarm
        )
    )

    sim.add(
        Device(
            id="SD_CEILING",
            xyz=Point3D(1.5, 1.5, 2.9),
            prop_id="SMOKE_DET",
        )
    )

    # Visibility measurement
    sim.add(
        Device(
            id="VISIBILITY",
            xyz=Point3D(1.5, 1.5, 1.8),
            quantity="VISIBILITY",
        )
    )

    # Soot density
    sim.add(
        Device(
            id="SOOT_DENSITY",
            xyz=Point3D(1.5, 1.5, 2.5),
            quantity="DENSITY",
            spec_id="SOOT",
        )
    )

    # ==========================================================================
    # Time-based Output
    # ==========================================================================

    # Clock device for synchronization
    sim.add(
        Device(
            id="CLOCK",
            xyz=Point3D(0.1, 0.1, 0.1),
            quantity="TIME",
        )
    )

    # Write output
    output_path = write_example(sim, "output")
    print(f"Created: {output_path}")

    print("\nGenerated FDS file:")
    print("-" * 40)
    print(sim.to_fds())


if __name__ == "__main__":
    main()
