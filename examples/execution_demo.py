"""
Execution Demo - Phase 2 + Priority 1 Features

This example demonstrates the complete workflow of creating, executing,
and analyzing FDS simulations using PyFDS Phase 2 features plus Priority 1
enhancements:
- Parallel execution validation
- Platform-specific execution
- Graceful stopping with CHID.stop
"""

import time
from pathlib import Path

from pyfds import Simulation
from pyfds.core.geometry import Point3D
from pyfds.execution import ParallelValidator

# Create output directory
output_dir = Path(__file__).parent / "fds"
output_dir.mkdir(exist_ok=True)


def blocking_execution_demo():
    """
    Demo 1: Simple blocking execution.

    This is the simplest way to run a simulation - it blocks until completion
    and returns the results directly.
    """
    print("\n" + "=" * 60)
    print("DEMO 1: Blocking Execution")
    print("=" * 60)

    # Create a simple room fire simulation
    sim = Simulation(chid="blocking_demo", title="Blocking Execution Demo")

    # Set time parameters (short simulation for demo)
    sim.time(t_end=10.0)

    # Create a small mesh for fast execution
    sim.mesh(ijk=(10, 10, 10), xb=(0, 2, 0, 2, 0, 2))

    # Add a fire surface
    sim.surface(id="FIRE", hrrpua=500.0, color="RED")

    # Add a burner
    sim.obstruction(xb=(0.8, 1.2, 0.8, 1.2, 0, 0.1), surf_id="FIRE")

    # Add temperature devices
    sim.device(id="TEMP_1", quantity="TEMPERATURE", xyz=Point3D(1.0, 1.0, 0.5))
    sim.device(id="TEMP_2", quantity="TEMPERATURE", xyz=Point3D(1.0, 1.0, 1.0))
    sim.device(id="TEMP_3", quantity="TEMPERATURE", xyz=Point3D(1.0, 1.0, 1.5))

    print("\n📝 Simulation setup complete")
    print(f"   CHID: {sim.chid}")
    print(f"   Meshes: {len(sim.geometry.meshes)}")
    print(f"   Devices: {len(sim.instrumentation.devices)}")

    # Run simulation (blocks until complete)
    print("\n🚀 Starting simulation (this will block until complete)...")
    try:
        results = sim.run(
            n_threads=2,  # Use 2 OpenMP threads
            output_dir=output_dir,
            monitor=True,  # Enable progress monitoring
            validate=True,  # Validate before running
        )

        print("\n✅ Simulation completed successfully!")

        # Analyze results
        print("\n📊 Results Analysis:")
        summary = results.summary()
        print(f"   Peak HRR: {summary.get('peak_hrr', 0):.1f} kW")
        print(f"   Duration: {summary.get('duration', 0):.1f} s")
        print(f"   Number of devices: {summary.get('num_devices', 0)}")

        # Get device data
        temp_1 = results.get_device("TEMP_1")
        max_temp = temp_1["TEMP_1"].max()
        print(f"   Max temperature at TEMP_1: {max_temp:.1f} °C")

        # Plot HRR
        print("\n📈 Saving HRR plot...")
        results.plot_hrr(save_as=output_dir / "blocking_demo_hrr.png")
        print(f"   Saved to: {output_dir / 'blocking_demo_hrr.png'}")

    except Exception as e:
        print(f"\n❌ Simulation failed: {e}")
        print("\nNote: This demo requires FDS to be installed.")
        print("Install FDS from: https://pages.nist.gov/fds-smv/")


def non_blocking_execution_demo():
    """
    Demo 2: Non-blocking execution with progress monitoring.

    This demo shows how to run a simulation in the background while
    monitoring progress in real-time.
    """
    print("\n" + "=" * 60)
    print("DEMO 2: Non-Blocking Execution with Progress Monitoring")
    print("=" * 60)

    # Create simulation
    sim = Simulation(chid="nonblocking_demo", title="Non-Blocking Demo")
    sim.time(t_end=20.0)  # Longer simulation to see progress
    sim.mesh(ijk=(15, 15, 15), xb=(0, 3, 0, 3, 0, 3))
    sim.surface(id="FIRE", hrrpua=1000.0, color="ORANGE")
    sim.obstruction(xb=(1.2, 1.8, 1.2, 1.8, 0, 0.2), surf_id="FIRE")
    sim.device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=Point3D(1.5, 1.5, 1.5))

    print("\n🚀 Starting simulation in background...")

    try:
        # Start simulation without blocking
        job = sim.run(
            n_threads=4,
            output_dir=output_dir,
            monitor=True,
            wait=False,  # Don't wait for completion
        )

        print("✓ Simulation started!")
        print("\n📊 Monitoring progress:")
        print("-" * 60)

        # Monitor progress while simulation runs
        last_progress = -1
        while job.is_running():
            progress = job.progress
            if progress != last_progress and progress > 0:
                print(f"   Progress: {progress:5.1f}%", end="")
                if job.estimated_time_remaining:
                    print(f" | ETA: {job.estimated_time_remaining:6.1f}s", end="")
                print()
                last_progress = progress

            time.sleep(2)  # Check every 2 seconds

        print("-" * 60)

        # Get results
        print("\n⏳ Retrieving results...")
        results = job.get_results()

        print("✅ Simulation completed!")
        print("\n📊 Results:")
        summary = results.summary()
        print(f"   Peak HRR: {summary.get('peak_hrr', 0):.1f} kW")
        print(f"   Duration: {summary.get('duration', 0):.1f} s")

    except Exception as e:
        print(f"\n❌ Simulation failed: {e}")
        print("\nNote: This demo requires FDS to be installed.")


def runner_api_demo():
    """
    Demo 3: Using the FDSRunner API directly.

    This demo shows how to use the lower-level FDSRunner API for
    more control over execution.
    """
    print("\n" + "=" * 60)
    print("DEMO 3: Using FDSRunner API")
    print("=" * 60)

    from pyfds import FDSRunner

    # Create and write simulation file
    sim = Simulation(chid="runner_demo", title="FDSRunner API Demo")
    sim.time(t_end=5.0)
    sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

    fds_file = output_dir / "runner_demo.fds"
    sim.write(fds_file)

    print(f"\n📝 FDS file written to: {fds_file}")

    try:
        # Create runner
        runner = FDSRunner()
        print("\n🔧 FDS Runner initialized")
        print(f"   FDS version: {runner.fds_version}")

        # Run simulation
        print("\n🚀 Running simulation...")
        results = runner.run(
            fds_file=fds_file,
            n_threads=2,
            monitor=True,
            wait=True,
        )

        print("✅ Simulation completed!")
        print(f"\n📊 Results available at: {results.output_dir}")

    except Exception as e:
        print(f"\n❌ Error: {e}")


def validation_demo():
    """
    Demo 4: Validation before execution.

    This demo shows how validation works and how to handle validation
    errors before running simulations.
    """
    print("\n" + "=" * 60)
    print("DEMO 4: Validation Demo")
    print("=" * 60)

    # Create a simulation with potential issues
    sim = Simulation(chid="validation_demo", title="Validation Demo")
    sim.time(t_end=10.0)

    # Add a highly non-cubic mesh (will generate warning)
    sim.mesh(ijk=(100, 10, 10), xb=(0, 10, 0, 1, 0, 1))

    # Add obstruction with undefined surface (will generate warning)
    sim.obstruction(xb=(4, 6, 0, 1, 0, 0.5), surf_id="UNDEFINED_SURFACE")

    print("\n🔍 Running validation...")
    warnings = sim.validate()

    if warnings:
        print(f"\n⚠️  Found {len(warnings)} validation warning(s):")
        for i, warning in enumerate(warnings, 1):
            print(f"   {i}. {warning}")

        print("\n💡 Validation modes:")
        print("   1. Default: Print warnings but continue")
        print("   2. Strict: Raise exception on warnings")
        print("   3. No validation: Skip validation entirely")

        print("\n📝 Example: Default mode (warnings only)")
        print("   Would print warnings and continue...")
        # results = sim.run(validate=True, strict=False)

        print("\n📝 Example: Strict mode (raise on warnings)")
        print("   Would raise ValueError due to warnings")
        # results = sim.run(validate=True, strict=True)
    else:
        print("✅ No validation warnings!")


def parallel_validation_demo():
    """
    Demo 5: Parallel execution validation (Priority 1 Feature).

    Demonstrates automatic validation of parallel configuration and
    recommendations for optimal performance.
    """
    print("\n" + "=" * 60)
    print("DEMO 5: Parallel Execution Validation (Priority 1)")
    print("=" * 60)

    # Create validator
    validator = ParallelValidator()

    # Example 1: Single mesh simulation
    print("\n📊 Scenario 1: Single Mesh Simulation")
    sim1 = Simulation(chid="single_mesh")
    sim1.mesh(ijk=(50, 50, 50), xb=(0, 5, 0, 5, 0, 5))

    config1 = validator.recommend_configuration(sim1)
    print("  Mesh count: 1")
    print(f"  ✅ Recommended: {config1['n_mpi']} MPI, {config1['n_threads']} threads")
    print(f"  📝 Rationale: {config1['rationale']}")

    # Check what happens with bad configuration
    warnings = validator.validate_all(sim1, n_mpi=4, n_threads=2)
    if warnings:
        print("\n  ⚠️  Using n_mpi=4 with 1 mesh would trigger warnings:")
        for w in warnings:
            print(f"     - {w[:80]}...")

    # Example 2: Multi-mesh simulation
    print("\n📊 Scenario 2: Multi-Mesh Simulation (4 meshes)")
    sim2 = Simulation(chid="multi_mesh")
    for i in range(4):
        sim2.mesh(ijk=(25, 25, 25), xb=(i * 2.5, (i + 1) * 2.5, 0, 2.5, 0, 2.5))

    config2 = validator.recommend_configuration(sim2)
    print("  Mesh count: 4")
    print(f"  ✅ Recommended: {config2['n_mpi']} MPI, {config2['n_threads']} threads")
    print(f"  📝 Rationale: {config2['rationale']}")

    # Example 3: Validation catches issues
    print("\n📊 Scenario 3: Catching Configuration Issues")
    print("  Testing: 2 MPI processes with 4 meshes")
    warnings = validator.validate_mpi_mesh_count(sim2, n_mpi=2)
    if warnings:
        print(f"  ⚠️  Warning: {warnings[0][:100]}...")

    print("\n💡 Tip: PyFDS automatically validates parallel config when you run sim.run()")


def graceful_stop_demo():
    """
    Demo 6: Graceful stopping with CHID.stop (Priority 1 Feature).

    Demonstrates requesting FDS to stop gracefully by creating a CHID.stop file.
    """
    print("\n" + "=" * 60)
    print("DEMO 6: Graceful Stopping (Priority 1)")
    print("=" * 60)

    # Create a longer simulation that we can stop
    sim = Simulation(chid="graceful_stop_demo", title="Graceful Stop Demo")
    sim.time(t_end=60.0)  # 60 second simulation
    sim.mesh(ijk=(20, 20, 20), xb=(0, 2, 0, 2, 0, 2))
    sim.surface(id="FIRE", hrrpua=500.0)
    sim.obstruction(xb=(0.8, 1.2, 0.8, 1.2, 0, 0.1), surf_id="FIRE")

    print("\n🔥 Starting simulation in background...")
    print("   Will request graceful stop after a few seconds")

    # Run in non-blocking mode
    job = sim.run(
        n_threads=1,
        output_dir=output_dir,
        wait=False,
        monitor=True,
    )

    # Wait a bit for simulation to start
    print("\n⏱️  Letting simulation run for 5 seconds...")
    time.sleep(5)

    # Check progress
    if job.is_running():
        progress = job.progress
        print(f"   Current progress: {progress:.1f}%")

        # Request graceful stop
        print("\n🛑 Requesting graceful stop...")
        job.request_stop()
        print("   ✅ CHID.stop file created")
        print("   📝 FDS will stop after completing current timestep")

        # Wait for FDS to stop (with timeout)
        print("\n⏳ Waiting for FDS to stop gracefully...")
        max_wait = 30
        start = time.time()
        while job.is_running() and (time.time() - start) < max_wait:
            time.sleep(1)

        if not job.is_running():
            print("   ✅ Simulation stopped gracefully!")
            print(f"   📊 Final progress: {job.progress:.1f}%")
        else:
            print("   ⚠️  Simulation still running after timeout")
            job.kill()  # Force kill if needed
    else:
        print("   i  Simulation completed before stop was requested")

    print("\n💡 Tip: Use job.request_stop() instead of job.kill() for clean shutdown")


def main():
    """Run all demos."""
    print("\n" + "=" * 60)
    print("PyFDS Phase 2 - Execution Demos")
    print("=" * 60)
    print("\nThese demos showcase the execution and analysis features")
    print("introduced in Phase 2 of PyFDS development.")
    print("\nNote: FDS must be installed to run execution demos.")
    print("      Download from: https://pages.nist.gov/fds-smv/")

    # Run demos
    try:
        blocking_execution_demo()
    except Exception as e:
        print(f"Demo 1 skipped: {e}")

    try:
        non_blocking_execution_demo()
    except Exception as e:
        print(f"Demo 2 skipped: {e}")

    try:
        runner_api_demo()
    except Exception as e:
        print(f"Demo 3 skipped: {e}")

    validation_demo()  # This one doesn't need FDS

    # New Priority 1 demos
    parallel_validation_demo()

    try:
        graceful_stop_demo()
    except Exception as e:
        print(f"Graceful stop demo skipped: {e}")

    print("\n" + "=" * 60)
    print("Demos complete!")
    print("=" * 60)
    print(f"\nOutput files saved to: {output_dir.absolute()}")


if __name__ == "__main__":
    main()
