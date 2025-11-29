#!/usr/bin/env python3
"""
SPyro Model Example - Cone Calorimeter Data Scaling

Demonstrates using cone calorimeter test data with the SPyro
(Scaling Pyrolysis) model to predict fire behavior at different
heat flux conditions.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders import MaterialBuilder, RampBuilder, ReactionBuilder, SurfBuilder
from pyfds.core.namelists import Surface


def main():
    """Create SPyro model simulation from cone calorimeter data."""

    sim = Simulation(chid="spyro_example", title="SPyro Cone Calorimeter Scaling")

    # Time parameters
    sim.time(t_end=600.0)  # 10 minutes simulation

    # Mesh - small domain for cone calorimeter
    sim.mesh(ijk=(20, 20, 20), xb=(0.0, 0.2, 0.0, 0.2, 0.0, 0.2))  # 20cm x 20cm x 20cm domain

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

    # Add HRRPUA required for SPyro model
    plywood_surf.hrrpua = 1.0

    # Combustion reaction for pyrolysis products
    reaction = (
        ReactionBuilder()
        .fuel("WOOD")  # Use WOOD as representative of pyrolysis products
        .yields(soot=0.015, co=0.005)
        .radiative_fraction(0.35)
        .complete_heat_of_combustion(18000.0)  # Heat of combustion for wood gas
        .build()
    )

    sim.add_reaction(reaction)

    sim.add_material(plywood)
    sim.add_ramp(hrr_data)
    sim.add_surface(plywood_surf)

    # Sample obstruction - 10cm x 10cm x 1.2cm plywood sample
    sim.obstruction(xb=(0.05, 0.15, 0.05, 0.15, 0.0, 0.012), surf_id="PLYWOOD_SURF")

    # Ventilation vents - open boundaries for cone calorimeter
    sim.vent(xb=(0.0, 0.2, 0.0, 0.2, 0.2, 0.2), surf_id="OPEN")  # Top vent
    sim.vent(xb=(0.0, 0.0, 0.0, 0.2, 0.0, 0.2), surf_id="OPEN")  # Left vent
    sim.vent(xb=(0.2, 0.2, 0.0, 0.2, 0.0, 0.2), surf_id="OPEN")  # Right vent
    sim.vent(xb=(0.0, 0.2, 0.0, 0.0, 0.0, 0.2), surf_id="OPEN")  # Front vent
    sim.vent(xb=(0.0, 0.2, 0.2, 0.2, 0.0, 0.2), surf_id="OPEN")  # Back vent

    # Heat flux boundary condition (cone heater)
    cone_heater = Surface(id="CONE_HEATER", external_flux=75.0)  # 75 kW/m² incident heat flux
    sim.add_surface(cone_heater)
    sim.vent(xb=(0.0, 0.2, 0.0, 0.2, 0.0, 0.0), surf_id="CONE_HEATER")  # Bottom boundary

    output_dir = Path(__file__).parent.parent / "fds"
    output_dir.mkdir(exist_ok=True)
    sim.write(output_dir / "spyro_example.fds")

    print("SPyro example written successfully")


if __name__ == "__main__":
    main()
