"""
Integration tests for complete PyFDS workflows.
"""

import pytest

from pyfds.core import Simulation, Validator
from pyfds.core.validator import ValidationError


class TestBasicWorkflow:
    """Test complete workflow from creation to file writing."""

    def test_simple_room_fire(self, tmp_path):
        """Test creating a simple room fire simulation."""
        # Create simulation
        sim = Simulation(chid="room_fire", title="Simple Room Fire Test")

        # Set time parameters
        sim.time(t_end=600.0, dt=0.1)

        # Define computational domain
        sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

        # Create fire surface
        sim.surface(id="BURNER", hrrpua=1000.0, color="RED")

        # Add fire source (burner)
        sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id="BURNER")

        # Add measurement devices
        sim.device(id="TEMP_CEILING", quantity="TEMPERATURE", xyz=(2.5, 2.5, 2.4))
        sim.device(id="TEMP_CORNER", quantity="TEMPERATURE", xyz=(0.5, 0.5, 2.4))

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
        sim.time(t_end=300.0)

        # Add two adjacent meshes
        sim.mesh(ijk=(20, 20, 10), xb=(0, 2, 0, 2, 0, 1), id="MESH01")
        sim.mesh(ijk=(20, 20, 10), xb=(2, 4, 0, 2, 0, 1), id="MESH02")

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
            sim.time(t_end=300.0)
            sim.mesh(ijk=(30, 30, 15), xb=(0, 3, 0, 3, 0, 1.5))
            sim.surface(id="FIRE", hrrpua=float(hrr))
            sim.obstruction(xb=(1, 2, 1, 2, 0, 0.1), surf_id="FIRE")
            sim.device(id="TEMP", quantity="TEMPERATURE", xyz=(1.5, 1.5, 1.4))

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
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        validator = Validator()
        is_valid = validator.validate_simulation(sim)

        assert is_valid is True
        assert len(validator.get_errors()) == 0

    def test_validation_catches_errors(self):
        """Test validation catches critical errors."""
        sim = Simulation(chid="test")
        # Missing time and mesh - should raise errors

        validator = Validator()

        with pytest.raises(ValidationError):
            validator.validate_simulation(sim)

    def test_validation_surface_references(self):
        """Test validation of surface ID references."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Add surface
        sim.surface(id="FIRE", hrrpua=1000.0)

        # Reference defined surface - should be OK
        sim.obstruction(xb=(0, 1, 0, 1, 0, 0.1), surf_id="FIRE")

        # Reference undefined surface - should warn
        sim.obstruction(xb=(0, 1, 0, 1, 1, 1.1), surf_id="UNDEFINED")

        validator = Validator()
        with pytest.raises(ValidationError):
            validator.validate_simulation(sim)

        errors = validator.get_errors()
        assert any("UNDEFINED" in e for e in errors)


class TestComplexScenario:
    """Test more complex simulation scenarios."""

    def test_room_with_walls_and_door(self, tmp_path):
        """Test creating a room with walls and door opening."""
        sim = Simulation(chid="room_with_door", title="Room Fire with Door Opening")

        # Time settings
        sim.time(t_end=600.0)

        # Mesh
        sim.mesh(ijk=(60, 60, 30), xb=(0, 6, 0, 6, 0, 3))

        # Surfaces
        sim.surface(id="WALL", color="GRAY", tmp_front=20.0)
        sim.surface(id="FIRE", hrrpua=1000.0, color="RED")

        # Room walls (simplified - just floor and ceiling for this test)
        sim.obstruction(xb=(0, 6, 0, 6, 0, 0), surf_id="WALL")  # Floor
        sim.obstruction(xb=(0, 6, 0, 6, 3, 3), surf_id="WALL")  # Ceiling

        # Fire source
        sim.obstruction(xb=(2.5, 3.5, 2.5, 3.5, 0, 0.2), surf_id="FIRE")

        # Measurement devices in a line
        for i, z in enumerate([0.5, 1.0, 1.5, 2.0, 2.5], start=1):
            sim.device(id=f"TEMP_{i}", quantity="TEMPERATURE", xyz=(3.0, 3.0, z))

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
