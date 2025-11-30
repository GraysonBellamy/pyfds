"""
Integration tests for complete PyFDS workflows.
"""

from pyfds import Simulation
from pyfds.core import Validator
from pyfds.core.enums import TurbulenceModel
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Head,
    Mesh,
    Misc,
    Obstruction,
    Surface,
    Time,
    Vent,
)
from pyfds.core.registry import SimulationRegistry


class TestBasicWorkflow:
    """Test complete workflow from creation to file writing."""

    def test_simple_room_fire(self, tmp_path):
        """Test creating a simple room fire simulation."""
        # Create simulation
        sim = Simulation(chid="room_fire", title="Simple Room Fire Test")

        # Set time parameters
        sim.add(Time(t_end=600.0, dt=0.1))

        # Define computational domain
        sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))

        # Create fire surface
        sim.add(Surface(id="BURNER", hrrpua=1000.0, color="RED"))

        # Add fire source (burner)
        sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id="BURNER"))

        # Add measurement devices
        sim.add(Device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=Point3D.of(2.5, 2.5, 2.4)))
        sim.add(Device(id="TEMP_CORNER", quantity="TEMPERATURE", xyz=Point3D.of(0.5, 0.5, 2.4)))

        # Validate simulation
        warnings = sim.validate()
        # Should have no critical warnings for this simple setup
        assert isinstance(warnings, list)

        # Write to file
        output_file = tmp_path / "room_fire.fds"
        result_path = sim.write(output_file)

        # Verify file was created
        assert result_path.exists()
        assert result_path.name == "room_fire.fds"

        # Read and verify content
        content = result_path.read_text()

        # Check all sections are present
        assert "&HEAD" in content
        assert "CHID='room_fire'" in content
        assert "&TIME" in content
        assert "T_END=600.0" in content
        assert "&MESH" in content
        assert "IJK=50,50,25" in content
        assert "&SURF" in content
        assert "ID='BURNER'" in content
        assert "&OBST" in content
        assert "&DEVC" in content
        assert "TEMP_CEILING" in content
        assert "TEMP_CORNER" in content
        assert "&TAIL" in content

    def test_multi_mesh_simulation(self, tmp_path):
        """Test simulation with multiple meshes."""
        sim = Simulation(chid="multi_mesh")
        sim.add(Time(t_end=300.0))

        # Add two adjacent meshes
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 2, 0, 2, 0, 1), id="MESH01"))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(2, 4, 0, 2, 0, 1), id="MESH02"))

        # Write file
        output_file = tmp_path / "multi_mesh.fds"
        sim.write(output_file)

        content = output_file.read_text()

        # Verify both meshes are present
        assert "MESH01" in content
        assert "MESH02" in content
        assert content.count("&MESH") == 2

    def test_parametric_setup(self, tmp_path):
        """Test creating multiple simulations with different parameters."""
        # Test creating variations programmatically
        hrr_values = [500, 1000, 1500, 2000]
        simulation_files = []

        for hrr in hrr_values:
            sim = Simulation(chid=f"fire_{hrr}")
            sim.add(Time(t_end=300.0))
            sim.add(Mesh(ijk=Grid3D.of(30, 30, 15), xb=Bounds3D.of(0, 3, 0, 3, 0, 1.5)))
            sim.add(Surface(id="FIRE", hrrpua=float(hrr)))
            sim.add(Obstruction(xb=Bounds3D.of(1, 2, 1, 2, 0, 0.1), surf_id="FIRE"))
            sim.add(Device(id="TEMP", quantity="TEMPERATURE", xyz=Point3D.of(1.5, 1.5, 1.4)))

            output_file = tmp_path / f"fire_{hrr}.fds"
            sim.write(output_file)
            simulation_files.append(output_file)

        # Verify all files were created
        assert len(simulation_files) == 4
        for filepath in simulation_files:
            assert filepath.exists()

        # Verify HRR values in files
        for hrr, filepath in zip(hrr_values, simulation_files, strict=True):
            content = filepath.read_text()
            assert f"HRRPUA={float(hrr)}" in content


class TestValidation:
    """Test validation workflows."""

    def test_validation_with_validator_class(self):
        """Test using Validator class directly."""
        sim = Simulation(chid="test")
        sim.add(Time(t_end=100.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        validator = Validator(sim)
        is_valid = validator.validate()
        assert len(is_valid) == 0

    def test_validation_catches_errors(self):
        """Test validation catches critical errors."""
        registry = SimulationRegistry()

        validator = Validator(registry)

        issues = validator.validate()
        # Should have errors for missing HEAD, TIME, and MESH
        assert len(issues) >= 3
        assert any("HEAD" in str(issue) for issue in issues)
        assert any("TIME" in str(issue) for issue in issues)
        assert any("MESH" in str(issue) for issue in issues)

    def test_validation_surface_references(self):
        """Test validation of surface ID references."""
        registry = SimulationRegistry()
        registry.head = Head(chid="test")
        registry.time = Time(t_end=100.0)
        registry.meshes.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Add surface
        registry.surfaces.add(Surface(id="FIRE", hrrpua=1000.0))

        # Reference defined surface - should be OK
        registry.obstructions.add(Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 0, 0.1), surf_id="FIRE"))

        # Reference undefined surface - should warn
        registry.obstructions.add(
            Obstruction(xb=Bounds3D.of(0, 1, 0, 1, 1, 1.1), surf_id="UNDEFINED")
        )

        validator = Validator(registry)
        issues = validator.validate()

        # Should have error for undefined surface
        assert any("UNDEFINED" in str(issue) for issue in issues)


class TestComplexScenario:
    """Test more complex simulation scenarios."""

    def test_room_with_walls_and_door(self, tmp_path):
        """Test creating a room with walls and door opening."""
        sim = Simulation(chid="room_with_door", title="Room Fire with Door Opening")

        # Time settings
        sim.add(Time(t_end=600.0))

        # Mesh
        sim.add(Mesh(ijk=Grid3D.of(60, 60, 30), xb=Bounds3D.of(0, 6, 0, 6, 0, 3)))

        # Surfaces
        sim.add(Surface(id="WALL", color="GRAY", tmp_front=20.0))
        sim.add(Surface(id="FIRE", hrrpua=1000.0, color="RED"))

        # Room walls (simplified - just floor and ceiling for this test)
        sim.add(Obstruction(xb=Bounds3D.of(0, 6, 0, 6, 0, 0), surf_id="WALL"))  # Floor
        sim.add(Obstruction(xb=Bounds3D.of(0, 6, 0, 6, 3, 3), surf_id="WALL"))  # Ceiling

        # Fire source
        sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2.5, 3.5, 0, 0.2), surf_id="FIRE"))

        # Measurement devices in a line
        for i, z in enumerate([0.5, 1.0, 1.5, 2.0, 2.5], start=1):
            sim.add(Device(id=f"TEMP_{i}", quantity="TEMPERATURE", xyz=Point3D.of(3.0, 3.0, z)))

        # Write and verify
        output_file = tmp_path / "room_with_door.fds"
        sim.write(output_file)

        content = output_file.read_text()

        # Check all devices are present
        for i in range(1, 6):
            assert f"TEMP_{i}" in content

        # Check surfaces
        assert "ID='WALL'" in content
        assert "ID='FIRE'" in content

        # Validate
        warnings = sim.validate()
        # Should complete without critical errors
        assert isinstance(warnings, list)


class TestVentMiscIntegration:
    """Test VENT and MISC integration with simulations."""

    def test_room_with_door_and_ambient(self, tmp_path):
        """Test simulation with door opening and custom ambient conditions."""
        sim = Simulation("room_with_door")

        # Set ambient conditions
        sim.add(Misc(tmpa=25.0, humidity=60.0))

        # Add mesh
        sim.add(Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)))
        sim.add(Time(t_end=60.0))

        # Add surfaces
        sim.add(Surface(id="FIRE", hrrpua=1000.0, color="RED"))

        # Add burner
        sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id="FIRE"))

        # Add door opening
        sim.add(Vent(xb=Bounds3D.of(5, 5, 2, 3, 0, 2), surf_id="OPEN"))

        # Write and verify
        output_file = tmp_path / "room_with_door.fds"
        sim.write(output_file)
        content = output_file.read_text()

        # Verify MISC and VENT are present
        assert "&MISC" in content
        assert "&VENT" in content
        assert "TMPA=25" in content
        assert "SURF_ID='OPEN'" in content

    def test_hvac_system(self, tmp_path):
        """Test HVAC system with supply and exhaust vents.

        Note: HVAC flow parameters (VOLUME_FLOW, VEL, MASS_FLOW) are defined
        on SURF namelists, not VENT namelists, per FDS User Guide.
        """
        sim = Simulation("hvac_test")
        sim.add(Misc())  # Use defaults
        sim.add(Mesh(ijk=Grid3D.of(40, 40, 20), xb=Bounds3D.of(0, 10, 0, 10, 0, 3)))
        sim.add(Time(t_end=300.0))

        # Define HVAC surfaces with flow parameters
        sim.add(Surface(id="HVAC_SUPPLY", volume_flow=-0.5))  # Negative = supply
        sim.add(Surface(id="HVAC_EXHAUST", volume_flow=0.4))  # Positive = exhaust

        # Add HVAC vents that reference the surfaces
        sim.add(Vent(xb=Bounds3D.of(2, 2.5, 2, 2.5, 3, 3), surf_id="HVAC_SUPPLY"))
        sim.add(Vent(xb=Bounds3D.of(7.5, 8, 7.5, 8, 3, 3), surf_id="HVAC_EXHAUST"))

        output_file = tmp_path / "hvac_test.fds"
        sim.write(output_file)
        content = output_file.read_text()

        # Verify HVAC surfaces and vents
        assert content.count("&VENT") == 2
        assert content.count("&SURF") >= 2
        assert "SURF_ID='HVAC_SUPPLY'" in content
        assert "SURF_ID='HVAC_EXHAUST'" in content
        assert "VOLUME_FLOW=-0.5" in content
        assert "VOLUME_FLOW=0.4" in content

    def test_wildfire_simulation(self, tmp_path):
        """Test wildfire simulation with special settings."""
        sim = Simulation("wildfire")

        # Wildfire mode with custom turbulence
        sim.add(
            Misc(
                level_set_mode=1,
                tmpa=35.0,
                humidity=15.0,
                turbulence_model=TurbulenceModel.VREMAN,
            )
        )

        sim.add(Mesh(ijk=Grid3D.of(100, 100, 30), xb=Bounds3D.of(0, 100, 0, 100, 0, 30)))
        sim.add(Time(t_end=600.0))

        # Add boundary vents for wind
        sim.add(Vent(mb="XMIN", surf_id="OPEN"))
        sim.add(Vent(mb="XMAX", surf_id="OPEN"))

        output_file = tmp_path / "wildfire.fds"
        sim.write(output_file)
        content = output_file.read_text()

        # Verify wildfire settings
        assert "LEVEL_SET_MODE=1" in content
        assert "TMPA=35" in content
        assert "HUMIDITY=15" in content
        assert "TURBULENCE_MODEL='VREMAN'" in content
        assert "MB='XMIN'" in content
        assert "MB='XMAX'" in content

    def test_circular_vent(self, tmp_path):
        """Test circular vent in simulation."""
        sim = Simulation("circular_vent")
        sim.add(Mesh(ijk=Grid3D.of(30, 30, 30), xb=Bounds3D.of(-3, 3, -3, 3, 0, 6)))
        sim.add(Misc())
        sim.add(Time(t_end=120.0))

        # Add circular burner
        sim.add(Surface(id="BURNER", hrrpua=500.0))
        sim.add(
            Vent(
                xb=Bounds3D.of(-2, 2, -2, 2, 0, 0),
                surf_id="BURNER",
                xyz=Point3D.of(0, 0, 0),
                radius=1.0,
            )
        )

        # Add open boundaries
        sim.add(Vent(mb="XMIN", surf_id="OPEN"))
        sim.add(Vent(mb="XMAX", surf_id="OPEN"))

        output_file = tmp_path / "circular_vent.fds"
        sim.write(output_file)
        content = output_file.read_text()

        assert "XYZ=0" in content
        assert "RADIUS=1" in content
        assert content.count("&VENT") == 3

    def test_solid_phase_only_mode(self, tmp_path):
        """Test solid phase only heat transfer simulation."""
        sim = Simulation("heat_transfer")

        # Solid phase only - no fluid flow
        sim.add(Misc(solid_phase_only=True, radiation=False))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        sim.add(Time(t_end=1000.0))

        # Add material and surface
        sim.add(Surface(id="HOT", tmp_front=500.0))
        sim.add(Obstruction(xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0), surf_id="HOT"))

        output_file = tmp_path / "heat_transfer.fds"
        sim.write(output_file)
        content = output_file.read_text()

        assert "SOLID_PHASE_ONLY=.TRUE." in content
        assert "RADIATION=.FALSE." in content

    def test_namelist_order_with_vent_misc(self, tmp_path):
        """Test that namelists appear in correct order."""
        sim = Simulation("order_test")

        sim.add(Misc(tmpa=25.0))
        sim.add(Time(t_end=100.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        sim.add(Vent(mb="ZMAX", surf_id="OPEN"))
        sim.add(Surface(id="TEST", color="BLUE"))
        sim.add(Obstruction(xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0.2)))

        output_file = tmp_path / "order_test.fds"
        sim.write(output_file)
        content = output_file.read_text()

        # Check order
        head_pos = content.find("&HEAD")
        time_pos = content.find("&TIME")
        misc_pos = content.find("&MISC")
        mesh_pos = content.find("&MESH")
        surf_pos = content.find("&SURF")
        obst_pos = content.find("&OBST")
        vent_pos = content.find("&VENT")
        tail_pos = content.find("&TAIL")

        # Verify order
        assert head_pos < time_pos < misc_pos < mesh_pos < surf_pos < obst_pos < vent_pos < tail_pos
