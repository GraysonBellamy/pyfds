#!/usr/bin/env python3
"""
Heat Flux Gauges Example
========================

Demonstrates different types of heat flux measurements
including radiative and convective components.

Based on FDS Verification: heat_flux tests.

Key Namelists: DEVC (GAUGE HEAT FLUX, RADIATIVE HEAT FLUX, CONVECTIVE HEAT FLUX)

Usage
-----
    python heat_flux_gauges.py

Output
------
    fds/radiation/heat_flux_gauges.fds
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
    Misc,
    Reaction,
    Surface,
    Time,
    Vent,
)


def main():
    """Create heat flux gauge example."""
    sim = Simulation(
        chid="heat_flux_gauges",
        title="Heat Flux Gauge Measurements",
    )

    # Time and misc
    sim.add(Time(t_end=60.0))
    sim.add(Misc(radiation=True))

    # Domain
    sim.add(
        Mesh(
            ijk=Grid3D.of(30, 30, 25),
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 2.5),
        )
    )

    # Combustion
    sim.add(Reaction(fuel="PROPANE"))

    # Fire source
    fire_surf = Surface(id="FIRE", hrrpua=500.0, color="ORANGE")
    sim.add(fire_surf)

    # Burner on floor center
    sim.add(Vent(xb=Bounds3D.of(1.2, 1.8, 1.2, 1.8, 0.0, 0.0), surf_id="FIRE"))

    # Open top
    sim.add(Vent(mb="ZMAX", surf_id="OPEN"))

    # --- Heat Flux Gauges ---

    # Array of gauges at different distances from fire
    distances = [0.5, 1.0, 1.5]
    heights = [0.5, 1.0, 1.5]

    for d in distances:
        for h in heights:
            # Gauge on +X side of fire
            gauge_id = f"HF_D{int(d * 10):02d}_H{int(h * 10):02d}"

            # Total (gauge) heat flux
            sim.add(
                Device(
                    id=f"{gauge_id}_GAUGE",
                    quantity="GAUGE HEAT FLUX",
                    xyz=Point3D(1.5 + d, 1.5, h),
                    ior=-1,  # Facing fire
                )
            )

    # Detailed component measurements at one location
    # Total gauge heat flux
    sim.add(
        Device(
            id="HF_TOTAL",
            quantity="GAUGE HEAT FLUX",
            xyz=Point3D(2.5, 1.5, 1.0),
            ior=-1,
        )
    )

    # Radiative heat flux only
    sim.add(
        Device(
            id="HF_RADIATIVE",
            quantity="RADIATIVE HEAT FLUX",
            xyz=Point3D(2.5, 1.5, 1.0),
            ior=-1,
        )
    )

    # Convective heat flux only
    sim.add(
        Device(
            id="HF_CONVECTIVE",
            quantity="CONVECTIVE HEAT FLUX",
            xyz=Point3D(2.5, 1.5, 1.0),
            ior=-1,
        )
    )

    # Incident radiative flux (without surface re-radiation)
    sim.add(
        Device(
            id="HF_INCIDENT",
            quantity="INCIDENT HEAT FLUX",
            xyz=Point3D(2.5, 1.5, 1.0),
            ior=-1,
        )
    )

    # Floor-level gauges
    sim.add(
        Device(
            id="FLOOR_HF_1",
            quantity="GAUGE HEAT FLUX",
            xyz=Point3D(0.5, 1.5, 0.0),
            ior=3,  # Facing up
        )
    )

    sim.add(
        Device(
            id="FLOOR_HF_2",
            quantity="GAUGE HEAT FLUX",
            xyz=Point3D(2.5, 1.5, 0.0),
            ior=3,
        )
    )

    # Ceiling-level gauges (for compartment fire)
    sim.add(
        Device(
            id="CEILING_HF_1",
            quantity="GAUGE HEAT FLUX",
            xyz=Point3D(1.5, 0.5, 2.4),
            ior=-3,  # Facing down
        )
    )

    sim.add(
        Device(
            id="CEILING_HF_2",
            quantity="GAUGE HEAT FLUX",
            xyz=Point3D(1.5, 2.5, 2.4),
            ior=-3,
        )
    )

    # Gas temperature for correlation
    sim.add(
        Device(
            id="GAS_T",
            quantity="TEMPERATURE",
            xyz=Point3D(2.5, 1.5, 1.0),
        )
    )

    # HRR
    sim.add(
        Device(
            id="HRR",
            quantity="HRR",
            xb=Bounds3D.of(0.0, 3.0, 0.0, 3.0, 0.0, 2.5),
        )
    )

    return sim


if __name__ == "__main__":
    sim = main()
    write_example(sim, "radiation")
