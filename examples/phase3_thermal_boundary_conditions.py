#!/usr/bin/env python3
"""
Phase 3 Thermal Boundary Conditions Example

This example demonstrates the new SURF thermal enhancements from FDS Chapters 6-7,
including temperature boundary conditions, heat transfer parameters, solid phase
geometry, 3D heat conduction, numerical parameters, internal heat sources, and
visualization parameters.
"""

from pathlib import Path

from pyfds import Simulation
from pyfds.builders import SurfBuilder


def create_thermal_surface_examples():
    """Create various thermal surface examples."""

    # Example 1: Temperature boundary conditions with ramps
    thermal_wall = (
        SurfBuilder("THERMAL_WALL")
        .with_color("GRAY")
        .with_initial_temperature(
            tmp_front_initial=293.15,  # 20°C initial front temperature
            tmp_inner=373.15,  # 100°C initial solid interior
            tmp_back=323.15,  # 50°C fixed back temperature
            tmp_gas_back=313.15,  # 40°C back gas temperature
        )
        .with_temperature_ramps(
            ramp_t="WALL_TEMP_RAMP",  # Time-varying surface temperature
            ramp_tmp_back="BACK_TEMP_RAMP",  # Time-varying back temperature
        )
        .build()
    )

    # Example 2: Adiabatic surface with custom heat transfer
    adiabatic_panel = (
        SurfBuilder("ADIABATIC_PANEL")
        .with_adiabatic()  # No heat transfer
        .with_color("BLACK")
        .build()
    )

    # Example 3: Heat transfer model with Nusselt correlation
    convective_surface = (
        SurfBuilder("CONVECTIVE_SURFACE")
        .with_heat_transfer_model(
            model="LOGLAW",  # Logarithmic law heat transfer
            length_scale=1.0,  # Characteristic length [m]
            ramp_htc="HTC_RAMP",  # Time-varying HTC
            blowing=False,  # No mass flux effect
        )
        .with_nusselt_correlation(
            c0=0.0,
            c1=0.037,
            c2=0.8,
            m=0.8,  # Custom Nu correlation
        )
        .with_color("BLUE")
        .build()
    )

    # Example 4: Impinging jet heat transfer
    jet_surface = (
        SurfBuilder("JET_SURFACE")
        .with_impinging_jet(sigma=0.1)  # Jet width [m]
        .with_color("RED")
        .build()
    )

    # Example 5: Solid phase geometry with 3D heat conduction
    cylindrical_wall = (
        SurfBuilder("CYLINDRICAL_WALL")
        .with_geometry(
            geometry="CYLINDRICAL",
            inner_radius=0.05,  # Hollow cylinder
            length=2.0,  # Cylinder length [m]
            radius=0.1,  # Outer radius [m]
            horizontal=False,  # Vertical orientation
        )
        .with_3d_heat_conduction(variable_thickness=False)
        .with_color("GREEN")
        .build()
    )

    # Example 6: Numerical parameters for solid phase heat transfer
    numerical_surface = (
        SurfBuilder("NUMERICAL_SURFACE")
        .with_numerical_params(
            stretch_factor=[1.1, 1.2, 1.3],  # Node spacing per layer
            cell_size_factor=[0.1, 0.15, 0.2],  # Cell size multipliers
            n_layer_cells_max=[20, 30, 40],  # Max cells per layer
            time_step_factor=10.0,  # Time step subdivision
            delta_tmp_max=10.0,  # Max temp change per step
            minimum_layer_thickness=[0.001, 0.002, 0.003],  # Min layer thickness
            remesh_ratio=0.15,  # Remeshing trigger
        )
        .with_color("YELLOW")
        .build()
    )

    # Example 7: Internal heat source with ramp
    heated_surface = (
        SurfBuilder("HEATED_SURFACE")
        .with_internal_heat_source(
            heat_source=[1000.0, 2000.0, 3000.0],  # Heat source per layer [kW/m³]
            ramp_id="HEAT_RAMP",  # Time-varying heat source
        )
        .with_color("ORANGE")
        .build()
    )

    # Example 8: Default surface with texture mapping
    default_surface = (
        SurfBuilder("DEFAULT_WALL")
        .as_default()  # Mark as default boundary condition
        .with_texture(texture_map="concrete_wall.png", width=1.0, height=1.0, transparency=1.0)
        .with_color("GRAY")
        .build()
    )

    return [
        thermal_wall,
        adiabatic_panel,
        convective_surface,
        jet_surface,
        cylindrical_wall,
        numerical_surface,
        heated_surface,
        default_surface,
    ]


def main():
    """Main function demonstrating thermal surface usage."""

    # Create simulation
    sim = Simulation(
        chid="thermal_boundary_conditions", title="Phase 3 Thermal Boundary Conditions"
    )

    # Add thermal surfaces
    thermal_surfaces = create_thermal_surface_examples()
    for surface in thermal_surfaces:
        sim.add_surface(surface)

    # Create a simple room with thermal boundaries
    sim.mesh(ijk=(20, 20, 10), xb=(0, 4, 0, 4, 0, 2))

    # Add obstructions with different thermal surfaces
    sim.obstruction(
        xb=(0, 4, 0, 0, 0, 2),  # Left wall
        surf_id="THERMAL_WALL",
    )
    sim.obstruction(
        xb=(4, 4, 0, 4, 0, 2),  # Right wall
        surf_id="ADIABATIC_PANEL",
    )
    sim.obstruction(
        xb=(0, 4, 4, 4, 0, 2),  # Back wall
        surf_id="CONVECTIVE_SURFACE",
    )
    sim.obstruction(
        xb=(0, 4, 0, 4, 0, 0),  # Floor
        surf_id="CYLINDRICAL_WALL",
    )
    sim.obstruction(
        xb=(0, 4, 0, 4, 2, 2),  # Ceiling
        surf_id="HEATED_SURFACE",
    )

    # Add a fire source
    sim.obstruction(
        xb=(1.8, 2.2, 1.8, 2.2, 0.1, 0.2),  # Small fire
        surf_id="FIRE",
    )

    # Add some devices for monitoring
    sim.device(id="CENTER_TEMP", xyz=(2, 2, 1), quantity="TEMPERATURE")

    sim.device(
        id="WALL_FLUX",
        xyz=(0.01, 2, 1),  # Near left wall
        quantity="WALL HEAT FLUX",
    )

    # Set simulation time
    sim.time(t_end=60.0)  # 1 minute simulation

    # Generate FDS input file
    fds_input = sim.to_fds()
    print("Generated FDS input with thermal boundary conditions:")
    print("=" * 60)
    print(fds_input)
    print("=" * 60)

    # Save to file
    Path("thermal_boundary_conditions.fds").write_text(fds_input)

    print("FDS input saved to: thermal_boundary_conditions.fds")
    print("\nThis example demonstrates:")
    print("- Temperature boundary conditions with ramps")
    print("- Adiabatic and convective heat transfer")
    print("- Solid phase geometry (cylindrical)")
    print("- 3D heat conduction capabilities")
    print("- Internal heat sources")
    print("- Texture mapping for visualization")
    print("- Default surface boundary conditions")


if __name__ == "__main__":
    main()
