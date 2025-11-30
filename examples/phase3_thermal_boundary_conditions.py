#!/usr/bin/env python3
"""
Phase 3 Thermal Boundary Conditions Example

This example demonstrates the new SURF thermal enhancements from FDS Chapters 6-7,
including temperature boundary conditions, heat transfer parameters, solid phase
geometry, 3D heat conduction, numerical parameters, internal heat sources, and
visualization parameters.
"""

from pathlib import Path

from pyfds import Simulation, Surface
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists.devc import Device
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.time import Time


def create_thermal_surface_examples():
    """Create various thermal surface examples."""

    # Example 1: Temperature boundary conditions with ramps
    thermal_wall = Surface(
        id="THERMAL_WALL",
        color="GRAY",
        tmp_front_initial=293.15,  # 20°C initial front temperature
        tmp_inner=373.15,  # 100°C initial solid interior
        tmp_back=323.15,  # 50°C fixed back temperature
        tmp_gas_back=313.15,  # 40°C back gas temperature
        ramp_t="WALL_TEMP_RAMP",  # Time-varying surface temperature
        ramp_tmp_back="BACK_TEMP_RAMP",  # Time-varying back temperature
    )

    # Example 2: Adiabatic surface with custom heat transfer
    adiabatic_panel = Surface(
        id="ADIABATIC_PANEL",
        adiabatic=True,  # No heat transfer
        color="BLACK",
    )

    # Example 3: Heat transfer model with Nusselt correlation
    convective_surface = Surface(
        id="CONVECTIVE_SURFACE",
        heat_transfer_model="LOGLAW",  # Logarithmic law heat transfer
        length_scale=1.0,  # Characteristic length [m]
        ramp_htc="HTC_RAMP",  # Time-varying HTC
        blowing=False,  # No mass flux effect
        c0=0.0,
        c1=0.037,
        c2=0.8,
        m=0.8,  # Custom Nu correlation
        color="BLUE",
    )

    # Example 4: Impinging jet heat transfer
    jet_surface = Surface(
        id="JET_SURFACE",
        sigma=0.1,  # Jet width [m]
        color="RED",
    )

    # Example 5: Solid phase geometry with 3D heat conduction
    cylindrical_wall = Surface(
        id="CYLINDRICAL_WALL",
        geometry="CYLINDRICAL",
        inner_radius=0.05,  # Hollow cylinder
        length=2.0,  # Cylinder length [m]
        radius=0.1,  # Outer radius [m]
        horizontal=False,  # Vertical orientation
        conduction_3d=True,
        variable_thickness=False,
        color="GREEN",
    )

    # Example 6: Numerical parameters for solid phase heat transfer
    numerical_surface = Surface(
        id="NUMERICAL_SURFACE",
        stretch_factor=[1.1, 1.2, 1.3],  # Node spacing per layer
        cell_size_factor=[0.1, 0.15, 0.2],  # Cell size multipliers
        n_layer_cells_max=[20, 30, 40],  # Max cells per layer
        time_step_factor=10.0,  # Time step subdivision
        delta_tmp_max=10.0,  # Max temp change per step
        minimum_layer_thickness=[0.001, 0.002, 0.003],  # Min layer thickness
        remesh_ratio=0.15,  # Remeshing trigger
        color="YELLOW",
    )

    # Example 7: Internal heat source with ramp
    heated_surface = Surface(
        id="HEATED_SURFACE",
        heat_source=[1000.0, 2000.0, 3000.0],  # Heat source per layer [kW/m³]
        ramp_id="HEAT_RAMP",  # Time-varying heat source
        color="ORANGE",
    )

    # Example 8: Default surface with texture mapping
    default_surface = Surface(
        id="DEFAULT_WALL",
        default=True,  # Mark as default boundary condition
        texture_map="concrete_wall.png",
        texture_width=1.0,
        texture_height=1.0,
        transparency=1.0,
        color="GRAY",
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
        sim.add(surface)

    # Create a simple room with thermal boundaries
    sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 4, 0, 4, 0, 2)))

    # Add obstructions with different thermal surfaces
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0, 4, 0, 0, 0, 2),  # Left wall
            surf_id="THERMAL_WALL",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(4, 4, 0, 4, 0, 2),  # Right wall
            surf_id="ADIABATIC_PANEL",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0, 4, 4, 4, 0, 2),  # Back wall
            surf_id="CONVECTIVE_SURFACE",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0, 4, 0, 4, 0, 0),  # Floor
            surf_id="CYLINDRICAL_WALL",
        )
    )
    sim.add(
        Obstruction(
            xb=Bounds3D.of(0, 4, 0, 4, 2, 2),  # Ceiling
            surf_id="HEATED_SURFACE",
        )
    )

    # Add a fire source
    sim.add(
        Obstruction(
            xb=Bounds3D.of(1.8, 2.2, 1.8, 2.2, 0.1, 0.2),  # Small fire
            surf_id="FIRE",
        )
    )

    # Add some devices for monitoring
    sim.add(
        Device(
            id="CENTER_TEMP",
            xyz=Point3D.of(2, 2, 1),
            quantity="TEMPERATURE",
        )
    )

    sim.add(
        Device(
            id="WALL_FLUX",
            xyz=Point3D.of(0.01, 2, 1),  # Near left wall
            quantity="WALL HEAT FLUX",
        )
    )

    # Set simulation time
    sim.add(Time(t_end=60.0))  # 1 minute simulation

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
