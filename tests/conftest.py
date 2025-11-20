"""Shared pytest fixtures for PyFDS tests."""

import pytest

from pyfds import Simulation


@pytest.fixture
def basic_simulation():
    """
    Create a basic valid simulation for testing.

    Returns
    -------
    Simulation
        A minimal valid simulation with CHID, TIME, and MESH
    """
    sim = Simulation(chid="test", title="Test Simulation")
    sim.time(t_end=100.0)
    sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
    return sim


@pytest.fixture
def fire_simulation():
    """
    Create a simulation with fire for testing.

    Returns
    -------
    Simulation
        A simulation with fire source and measurements
    """
    sim = Simulation(chid="fire_test", title="Fire Test")
    sim.time(t_end=600.0)
    sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

    # Add fire surface
    sim.surface(id="FIRE", hrrpua=1000.0, color="RED")

    # Add burner obstruction
    sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id="FIRE")

    # Add temperature devices
    sim.device(id="TEMP_1", quantity="TEMPERATURE", xyz=(2.5, 2.5, 1.0))
    sim.device(id="TEMP_2", quantity="TEMPERATURE", xyz=(2.5, 2.5, 2.0))

    return sim


@pytest.fixture
def room_simulation():
    """
    Create a simulation of a room enclosure.

    Returns
    -------
    Simulation
        A simulation with walls, floor, and ceiling
    """
    sim = Simulation(chid="room", title="Room Enclosure")
    sim.time(t_end=300.0)
    sim.mesh(ijk=(30, 30, 25), xb=(0, 6, 0, 6, 0, 3))

    # Add wall surface
    sim.surface(id="WALL", color="GRAY")

    # Add walls
    sim.obstruction(xb=(0, 0, 0, 6, 0, 3), surf_id="WALL")  # Left wall
    sim.obstruction(xb=(6, 6, 0, 6, 0, 3), surf_id="WALL")  # Right wall
    sim.obstruction(xb=(0, 6, 0, 0, 0, 3), surf_id="WALL")  # Front wall
    sim.obstruction(xb=(0, 6, 6, 6, 0, 3), surf_id="WALL")  # Back wall
    sim.obstruction(xb=(0, 6, 0, 6, 0, 0), surf_id="WALL")  # Floor
    sim.obstruction(xb=(0, 6, 0, 6, 3, 3), surf_id="WALL")  # Ceiling

    return sim


@pytest.fixture
def temp_fds_file(tmp_path):
    """
    Create a temporary FDS file for testing.

    Parameters
    ----------
    tmp_path : Path
        Pytest tmp_path fixture

    Returns
    -------
    Path
        Path to temporary FDS file
    """
    fds_content = """&HEAD CHID='test', TITLE='Test Simulation' /
&TIME T_END=100.0 /
&MESH IJK=10,10,10, XB=0,1,0,1,0,1 /
&TAIL /
"""
    fds_file = tmp_path / "test.fds"
    fds_file.write_text(fds_content)
    return fds_file


@pytest.fixture
def temp_output_dir(tmp_path):
    """
    Create a temporary output directory for testing.

    Parameters
    ----------
    tmp_path : Path
        Pytest tmp_path fixture

    Returns
    -------
    Path
        Path to temporary output directory
    """
    output_dir = tmp_path / "output"
    output_dir.mkdir(exist_ok=True)
    return output_dir


@pytest.fixture
def multi_mesh_simulation():
    """
    Create a simulation with multiple meshes for testing.

    Returns
    -------
    Simulation
        A simulation with multiple meshes for parallel processing
    """
    sim = Simulation(chid="multi_mesh", title="Multi-Mesh Test")
    sim.time(t_end=100.0)

    # Add multiple meshes
    sim.mesh(id="MESH1", ijk=(20, 20, 20), xb=(0, 2, 0, 2, 0, 2), mpi_process=0)
    sim.mesh(id="MESH2", ijk=(20, 20, 20), xb=(2, 4, 0, 2, 0, 2), mpi_process=1)
    sim.mesh(id="MESH3", ijk=(20, 20, 20), xb=(0, 2, 2, 4, 0, 2), mpi_process=2)
    sim.mesh(id="MESH4", ijk=(20, 20, 20), xb=(2, 4, 2, 4, 0, 2), mpi_process=3)

    return sim
