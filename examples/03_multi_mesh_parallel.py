"""
Multi-Mesh Parallel Fire Simulation Example
============================================

This example demonstrates parallel processing capabilities:
- Multiple computational meshes
- MPI process assignment
- OpenMP threading
- Fire spread across mesh boundaries
- Performance optimization with stability controls

This showcases the parallel computing features added in Stage 1.4 (MESH enhancements).
"""

from pathlib import Path

from pyfds.builders import DevcBuilder, MeshBuilder, ReactionBuilder, SurfBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Time
from pyfds.core.simulation import Simulation


def create_multi_mesh_fire():
    """
    Create a multi-mesh fire simulation for parallel processing.

    Layout: Long corridor subdivided into 4 meshes
    - Total size: 40m x 4m x 3m
    - Each mesh: 10m x 4m x 3m
    - 4 MPI processes (one per mesh)
    - 4 OpenMP threads per process
    """
    # Create simulation with metadata
    sim = Simulation(
        chid="multi_mesh_parallel", title="Multi-Mesh Parallel Fire - PyFDS Stage 1 Example"
    )
    sim.add(Time(t_end=300.0))

    # Create 4 meshes in a row
    n_meshes = 4
    mesh_length = 10.0  # meters
    mesh_width = 4.0
    mesh_height = 3.0

    for i in range(n_meshes):
        x_min = i * mesh_length
        x_max = (i + 1) * mesh_length

        mesh = (
            MeshBuilder()
            .with_id(f"CORRIDOR_{i + 1}")
            .with_bounds(Bounds3D(x_min, x_max, 0, mesh_width, 0, mesh_height))
            .with_grid(Grid3D(50, 20, 15))  # 50 cells per mesh = 200 total in x
            .with_mpi(process=i, n_threads=4)
            .with_stability_control(cfl_max=0.90, vn_max=0.90)
            .with_max_iterations(15)
            .build()
        )
        sim.add(mesh)

    # Fire source at the beginning of the corridor (Mesh 1)
    fire_surf = (
        SurfBuilder("CORRIDOR_FIRE")
        .with_heat_release(1000.0)
        .with_radiation(emissivity=0.9)
        .with_backing("EXPOSED")
        .build()
    )
    sim.add(fire_surf)

    # Reaction with extinction model
    reaction = (
        ReactionBuilder()
        .fuel("WOOD")
        .with_extinction("EXTINCTION_1", critical_temp=1100.0)
        .radiative_fraction(0.35)
        .build()
    )
    sim.add(reaction)

    # Temperature sensors along the corridor
    # Place sensors at mesh boundaries to monitor mesh communication
    sensor_x_positions = [5, 10, 15, 20, 25, 30, 35]  # At mesh centers and boundaries

    for x in sensor_x_positions:
        # Ceiling temperature
        ceiling_temp = (
            DevcBuilder(f"TEMP_CEILING_X{int(x):02d}")
            .with_quantity("TEMPERATURE")
            .at_point(Point3D(x, mesh_width / 2, mesh_height - 0.1))
            .with_time_history(True)
            .build()
        )
        sim.add(ceiling_temp)

        # Mid-height temperature
        mid_temp = (
            DevcBuilder(f"TEMP_MID_X{int(x):02d}")
            .with_quantity("TEMPERATURE")
            .at_point(Point3D(x, mesh_width / 2, mesh_height / 2))
            .with_time_history(True)
            .build()
        )
        sim.add(mid_temp)

    # Volume-averaged temperature for each mesh
    for i in range(n_meshes):
        x_min = i * mesh_length
        x_max = (i + 1) * mesh_length

        avg_temp = (
            DevcBuilder(f"AVG_TEMP_MESH{i + 1}")
            .with_quantity("TEMPERATURE")
            .with_statistics("MEAN", start_time=10.0)
            .in_bounds(Bounds3D(x_min, x_max, 0, mesh_width, 0, mesh_height))
            .build()
        )
        sim.add(avg_temp)

        max_temp = (
            DevcBuilder(f"MAX_TEMP_MESH{i + 1}")
            .with_quantity("TEMPERATURE")
            .with_statistics("MAX", start_time=10.0)
            .in_bounds(Bounds3D(x_min, x_max, 0, mesh_width, 0, mesh_height))
            .build()
        )
        sim.add(max_temp)

    # Velocity measurements to track flow between meshes
    for i in range(1, n_meshes):
        x_boundary = i * mesh_length

        # Velocity at mesh boundary
        vel_boundary = (
            DevcBuilder(f"VEL_BOUNDARY_{i}")
            .with_quantity("VELOCITY")
            .at_point(Point3D(x_boundary, mesh_width / 2, mesh_height / 2))
            .with_time_history(True)
            .build()
        )
        sim.add(vel_boundary)

    return sim


if __name__ == "__main__":
    simulation = create_multi_mesh_fire()

    # Create output directory
    output_dir = Path(__file__).parent / "fds"
    output_dir.mkdir(exist_ok=True)

    # Write FDS input file
    output_file = simulation.write(output_dir / "multi_mesh_parallel.fds")

    print("Multi-mesh parallel simulation created!")
    print(f"Output file: {output_file}")
    print("\nMesh configuration:")
    print("  - Total domain: 40m x 4m x 3m")
    print("  - Number of meshes: 4")
    print("  - Each mesh: 10m x 4m x 3m")
    print("  - Grid per mesh: 50 x 20 x 15 = 15,000 cells")
    print("  - Total cells: 60,000")
    print("\nParallel processing:")
    print("  - MPI processes: 4 (one per mesh)")
    print("  - OpenMP threads per process: 4")
    print("  - Total CPU cores utilized: 16")
    print("\nStability controls:")
    print("  - CFL max: 0.90")
    print("  - VN max: 0.90")
    print("  - Max pressure iterations: 15")
    print("\nMonitoring:")
    print("  - 14 point temperature sensors (ceiling + mid-height)")
    print("  - 8 volume-averaged devices (MEAN + MAX per mesh)")
    print("  - 3 velocity sensors at mesh boundaries")
    print("\nTo run with MPI:")
    print("  mpiexec -n 4 fds multi_mesh_parallel.fds")
