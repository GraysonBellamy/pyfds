#!/usr/bin/env python3
"""
SPyro Model Example - Cone Calorimeter Data Scaling

Demonstrates using cone calorimeter test data with the SPyro
(Scaling Pyrolysis) model to predict fire behavior at different
heat flux conditions.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders import MaterialBuilder, RampBuilder, SurfBuilder


def main():
    """Create SPyro model simulation from cone calorimeter data."""

    sim = Simulation(chid="spyro_example", title="SPyro Cone Calorimeter Scaling")

    # Material properties (from material characterization)
    plywood = (
        MaterialBuilder("PLYWOOD")
        .density(545)
        .thermal_conductivity(0.12)
        .specific_heat(1.2)
        .emissivity(0.9)
        .build()
    )

    # Cone calorimeter HRR data at 50 kW/m² (normalized to peak)
    # Time (s), HRR/HRR_peak
    hrr_data = (
        RampBuilder("PLYWOOD_HRR_50")
        .add_point(0, 0.0)
        .add_point(30, 0.1)
        .add_point(60, 0.5)
        .add_point(90, 1.0)
        .add_point(120, 0.8)
        .add_point(180, 0.4)
        .add_point(300, 0.1)
        .add_point(400, 0.0)
        .build()
    )

    # Surface with SPyro model
    plywood_surf = (
        SurfBuilder("PLYWOOD_SURF")
        .with_material("PLYWOOD", thickness=0.012)
        .with_spyro_model(
            reference_heat_flux=50.0,  # Test was at 50 kW/m²
            ramp_q="PLYWOOD_HRR_50",
            reference_thickness=0.012,
        )
        .with_ignition(temperature=350)
        .build()
    )

    sim.add_material(plywood)
    sim.add_ramp(hrr_data)
    sim.add_surface(plywood_surf)

    # ... rest of simulation setup

    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)
    sim.write(output_dir / "spyro_example.fds")

    print("SPyro example written successfully")


if __name__ == "__main__":
    main()
