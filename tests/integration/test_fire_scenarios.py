"""Integration tests for Stage 1 features.

Tests real-world fire simulation scenarios using SURF, DEVC, REAC, and MESH enhancements.
"""

from pyfds.builders import DevcBuilder, MeshBuilder, ReactionBuilder, SurfBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import Device, Mesh, Reaction, Surface


class TestFireWithSprinklerControl:
    """Test fire scenario with sprinkler activation control."""

    def test_fire_surface_with_ramped_hrr(self):
        """Test fire surface with time-ramped heat release rate."""
        fire = (
            SurfBuilder("FIRE")
            .with_heat_release(1000.0, ramp_id="t2_growth")
            .with_radiation(emissivity=0.9)
            .with_color("ORANGE")
            .build()
        )

        assert fire.id == "FIRE"
        assert fire.hrrpua == 1000.0
        assert fire.ramp_q == "t2_growth"
        assert fire.emissivity == 0.9
        assert fire.color == "ORANGE"

        fds_output = fire.to_fds()
        assert "FIRE" in fds_output
        assert "1000" in fds_output or "1000.0" in fds_output
        assert "t2_growth" in fds_output
        assert "0.9" in fds_output

    def test_sprinkler_with_control_logic(self):
        """Test sprinkler device with temperature-based control."""
        sprinkler = (
            DevcBuilder("SPRINK1")
            .with_quantity("SPRINKLER_LINK_TEMPERATURE")
            .with_control(setpoint=74.0, trip_direction=1, latch=True, delay=2.0)
            .at_point(Point3D(5.0, 5.0, 3.0))
            .with_prop("STANDARD_SPRINKLER")
            .build()
        )

        assert sprinkler.id == "SPRINK1"
        assert sprinkler.quantity == "SPRINKLER_LINK_TEMPERATURE"
        assert sprinkler.setpoint == 74.0
        assert sprinkler.trip_direction == 1
        assert sprinkler.latch is True
        assert sprinkler.delay == 2.0
        assert sprinkler.xyz == Point3D(5.0, 5.0, 3.0)

        fds_output = sprinkler.to_fds()
        assert "SPRINK1" in fds_output
        assert "SPRINKLER_LINK_TEMPERATURE" in fds_output
        assert "74" in fds_output or "74.0" in fds_output
        assert "2" in fds_output or "2.0" in fds_output

    def test_complete_fire_sprinkler_scenario(self):
        """Test complete scenario: fire with sprinkler and temperature monitoring."""
        # Fire source
        fire_surf = (
            SurfBuilder("BURNER")
            .with_heat_release(500.0, ramp_id="fire_ramp")
            .with_ignition(temperature=250.0, burn_away=True)
            .with_radiation(emissivity=0.85)
            .build()
        )

        # Sprinkler detector
        sprinkler = (
            DevcBuilder("SPRINK")
            .with_quantity("SPRINKLER_LINK_TEMPERATURE")
            .with_control(setpoint=74.0, latch=True)
            .at_point(Point3D(5.0, 5.0, 2.8))
            .build()
        )

        # Temperature sensor
        temp_sensor = (
            DevcBuilder("TEMP_CEILING")
            .with_quantity("TEMPERATURE")
            .at_point(Point3D(5.0, 5.0, 2.9))
            .with_time_history(True)
            .build()
        )

        # Verify all components created successfully
        assert isinstance(fire_surf, Surface)
        assert isinstance(sprinkler, Device)
        assert isinstance(temp_sensor, Device)

        # Verify control logic
        assert sprinkler.setpoint == 74.0
        assert temp_sensor.time_history is True


class TestMultiMeshParallelSimulation:
    """Test multi-mesh parallel fire simulation."""

    def test_single_mesh_with_mpi(self):
        """Test single mesh with MPI assignment."""
        mesh = (
            MeshBuilder()
            .with_id("MESH1")
            .with_bounds(Bounds3D(0, 10, 0, 10, 0, 3))
            .with_grid(Grid3D(50, 50, 15))
            .with_mpi(process=0, n_threads=4)
            .build()
        )

        assert mesh.id == "MESH1"
        assert mesh.mpi_process == 0
        assert mesh.n_threads == 4
        assert mesh.xb == Bounds3D(0, 10, 0, 10, 0, 3)
        assert mesh.ijk == Grid3D(50, 50, 15)

    def test_multi_mesh_array(self):
        """Test array of meshes for parallel processing."""
        meshes = []
        for i in range(4):
            mesh = (
                MeshBuilder()
                .with_id(f"MESH{i}")
                .with_bounds(Bounds3D(i * 10, (i + 1) * 10, 0, 10, 0, 3))
                .with_grid(Grid3D(50, 50, 15))
                .with_mpi(process=i, n_threads=2)
                .build()
            )
            meshes.append(mesh)

        assert len(meshes) == 4
        assert all(m.mpi_process is not None for m in meshes)
        assert all(m.n_threads == 2 for m in meshes)
        assert meshes[0].xb == Bounds3D(0, 10, 0, 10, 0, 3)
        assert meshes[3].xb == Bounds3D(30, 40, 0, 10, 0, 3)

    def test_mesh_with_stability_control(self):
        """Test mesh with custom stability parameters."""
        mesh = (
            MeshBuilder()
            .with_bounds(Bounds3D(0, 10, 0, 10, 0, 3))
            .with_grid(Grid3D(100, 100, 30))
            .with_stability_control(cfl_max=0.9, cfl_min=0.7, vn_max=0.9)
            .with_max_iterations(20)
            .build()
        )

        assert mesh.cfl_max == 0.9
        assert mesh.cfl_min == 0.7
        assert mesh.vn_max == 0.9
        assert mesh.maximum_internal_iterations == 20

        fds_output = mesh.to_fds()
        assert "20" in fds_output  # max_iterations
        # Stability parameters are not output in MESH (they belong in MISC)


class TestStatisticalTemperatureMonitoring:
    """Test statistical averaging and monitoring."""

    def test_volume_average_temperature(self):
        """Test device with volume averaging statistics."""
        avg_temp = (
            DevcBuilder("AVG_TEMP")
            .with_quantity("TEMPERATURE")
            .with_statistics("MEAN", start_time=10.0)
            .in_bounds(Bounds3D(0, 10, 0, 10, 0, 3))
            .build()
        )

        assert avg_temp.id == "AVG_TEMP"
        assert avg_temp.quantity == "TEMPERATURE"
        assert avg_temp.statistics == "MEAN"
        assert avg_temp.statistics_start == 10.0
        assert avg_temp.xb == Bounds3D(0, 10, 0, 10, 0, 3)

        fds_output = avg_temp.to_fds()
        assert "MEAN" in fds_output
        assert "10" in fds_output or "10.0" in fds_output

    def test_multiple_statistical_devices(self):
        """Test multiple devices with different statistics."""
        devices = []

        # Mean temperature
        devices.append(
            DevcBuilder("MEAN_TEMP")
            .with_quantity("TEMPERATURE")
            .with_statistics("MEAN")
            .in_bounds(Bounds3D(0, 5, 0, 5, 0, 3))
            .build()
        )

        # Max temperature
        devices.append(
            DevcBuilder("MAX_TEMP")
            .with_quantity("TEMPERATURE")
            .with_statistics("MAX")
            .in_bounds(Bounds3D(0, 5, 0, 5, 0, 3))
            .build()
        )

        # Min temperature
        devices.append(
            DevcBuilder("MIN_TEMP")
            .with_quantity("TEMPERATURE")
            .with_statistics("MIN")
            .in_bounds(Bounds3D(0, 5, 0, 5, 0, 3))
            .build()
        )

        assert len(devices) == 3
        assert devices[0].statistics == "MEAN"
        assert devices[1].statistics == "MAX"
        assert devices[2].statistics == "MIN"

    def test_temporal_and_spatial_statistics(self):
        """Test device with both temporal and spatial statistics."""
        devc = (
            DevcBuilder("COMPLEX_STAT")
            .with_quantity("VELOCITY")
            .with_statistics("MEAN")
            .with_temporal_statistic("RMS")
            .with_spatial_statistic("MAX")
            .in_bounds(Bounds3D(0, 10, 0, 10, 0, 3))
            .build()
        )

        assert devc.statistics == "MEAN"
        assert devc.temporal_statistic == "RMS"
        assert devc.spatial_statistic == "MAX"


class TestExtinctionAndSuppressionModeling:
    """Test combustion extinction and suppression features."""

    def test_reaction_with_extinction(self):
        """Test reaction with extinction model."""
        reac = (
            ReactionBuilder()
            .fuel("PROPANE")
            .with_extinction("EXTINCTION_1", critical_temp=1200.0)
            .radiative_fraction(0.35)
            .build()
        )

        assert reac.fuel == "PROPANE"
        assert reac.extinction_model == "EXTINCTION_1"
        assert reac.critical_flame_temperature == 1200.0
        assert reac.radiative_fraction == 0.35

        fds_output = reac.to_fds()
        assert "PROPANE" in fds_output
        assert "EXTINCTION_1" in fds_output
        assert "1200" in fds_output or "1200.0" in fds_output

    def test_reaction_with_suppression(self):
        """Test reaction with suppression model."""
        reac = (
            ReactionBuilder()
            .fuel("WOOD")
            .with_suppression(k_suppression=0.5)
            .soot_yield(0.015)
            .build()
        )

        assert reac.fuel == "WOOD"
        assert reac.suppression is True
        assert reac.k_suppression == 0.5
        assert reac.soot_yield == 0.015

        fds_output = reac.to_fds()
        assert "WOOD" in fds_output
        assert "0.5" in fds_output

    def test_reaction_with_species_stoichiometry(self):
        """Test reaction with species stoichiometry."""
        reac = (
            ReactionBuilder()
            .fuel("METHANE")
            .with_species_stoichiometry(["CO2", "H2O"], [1.0, 2.0])
            .radiative_fraction(0.30)
            .build()
        )

        assert reac.spec_id_nu == ["CO2", "H2O"]
        assert reac.nu == [1.0, 2.0]

    def test_reaction_with_time_scales(self):
        """Test reaction with custom time scales."""
        reac = (
            ReactionBuilder().fuel("PROPANE").with_time_scales(tau_chem=0.1, tau_flame=0.5).build()
        )

        assert reac.tau_chem == 0.1
        assert reac.tau_flame == 0.5

    def test_reaction_with_non_ideal_hoc(self):
        """Test reaction with non-ideal heat of combustion."""
        reac = ReactionBuilder().fuel("ETHANOL").use_non_ideal_hoc().build()

        assert reac.ideal is False


class TestCylindricalMesh:
    """Test cylindrical coordinate mesh."""

    def test_cylindrical_mesh_creation(self):
        """Test creation of cylindrical coordinate mesh."""
        mesh = (
            MeshBuilder()
            .with_bounds(Bounds3D(0, 1, 0, 1, 0, 3))
            .with_grid(Grid3D(50, 50, 100))
            .as_cylindrical()
            .build()
        )

        assert mesh.cylindrical is True
        assert mesh.xb == Bounds3D(0, 1, 0, 1, 0, 3)

        fds_output = mesh.to_fds()
        assert "CYLINDRICAL" in fds_output or "cylindrical" in fds_output.lower()


class TestDeviceOrientation:
    """Test device orientation features."""

    def test_device_with_orientation(self):
        """Test device with custom orientation."""
        devc = (
            DevcBuilder("ORIENTED_SENSOR")
            .with_quantity("VELOCITY")
            .with_orientation((0.0, 0.0, 1.0), rotation=45.0)
            .at_point(Point3D(5.0, 5.0, 1.5))
            .build()
        )

        assert devc.orientation == (0.0, 0.0, 1.0)
        assert devc.rotation == 45.0


class TestComplexFireScenario:
    """Test complex integrated fire scenario."""

    def test_compartment_fire_with_full_instrumentation(self):
        """Test complete compartment fire with all Stage 1 features."""
        # Mesh
        mesh = (
            MeshBuilder()
            .with_id("ROOM")
            .with_bounds(Bounds3D(0, 6, 0, 4, 0, 3))
            .with_grid(Grid3D(60, 40, 30))
            .with_stability_control(cfl_max=0.95)
            .build()
        )

        # Fire surface with ignition
        fire = (
            SurfBuilder("FIRE")
            .with_heat_release(750.0, ramp_id="t2_medium")
            .with_ignition(temperature=300.0, burn_away=True)
            .with_radiation(emissivity=0.9, absorptivity=0.85)
            .with_backing("INSULATED")
            .build()
        )

        # Reaction with extinction
        reaction = (
            ReactionBuilder()
            .fuel("POLYURETHANE")
            .with_extinction("EXTINCTION_1", critical_temp=1100.0)
            .radiative_fraction(0.30)
            .yields(soot=0.10, co=0.04)
            .build()
        )

        # Sprinkler
        sprinkler = (
            DevcBuilder("SPRINK")
            .with_quantity("SPRINKLER_LINK_TEMPERATURE")
            .with_control(setpoint=74.0, trip_direction=1, latch=True)
            .at_point(Point3D(3.0, 2.0, 2.8))
            .build()
        )

        # Temperature monitoring
        temp_ceiling = (
            DevcBuilder("TEMP_CEILING_AVG")
            .with_quantity("TEMPERATURE")
            .with_statistics("MEAN", start_time=5.0)
            .in_bounds(Bounds3D(0, 6, 0, 4, 2.5, 3.0))
            .build()
        )

        # Verify all components
        assert isinstance(mesh, Mesh)
        assert isinstance(fire, Surface)
        assert isinstance(reaction, Reaction)
        assert isinstance(sprinkler, Device)
        assert isinstance(temp_ceiling, Device)

        # Verify key properties
        assert mesh.cfl_max == 0.95
        assert fire.ignition_temperature == 300.0
        assert fire.burn_away is True
        assert reaction.extinction_model == "EXTINCTION_1"
        assert sprinkler.setpoint == 74.0
        assert temp_ceiling.statistics == "MEAN"

        # Verify FDS output generation
        assert all(obj.to_fds() for obj in [mesh, fire, reaction, sprinkler, temp_ceiling])


class TestBackwardCompatibility:
    """Test that Stage 1 enhancements don't break existing functionality."""

    def test_simple_surf_still_works(self):
        """Test that simple SURF creation still works."""
        surf = Surface(id="SIMPLE", hrrpua=500.0, color="RED")
        assert surf.id == "SIMPLE"
        assert surf.hrrpua == 500.0

    def test_simple_devc_still_works(self):
        """Test that simple DEVC creation still works."""
        devc = Device(id="TEMP", quantity="TEMPERATURE", xyz=Point3D(5, 5, 2))
        assert devc.id == "TEMP"
        assert devc.quantity == "TEMPERATURE"

    def test_simple_mesh_still_works(self):
        """Test that simple MESH creation still works."""
        mesh = Mesh(ijk=Grid3D(50, 50, 25), xb=Bounds3D(0, 10, 0, 10, 0, 5))
        assert mesh.ijk == Grid3D(50, 50, 25)

    def test_simple_reaction_still_works(self):
        """Test that simple REAC creation still works."""
        reac = Reaction(fuel="PROPANE")
        assert reac.fuel == "PROPANE"
