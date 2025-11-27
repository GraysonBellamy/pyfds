"""
Unit tests for OutputManager.
"""

from pyfds.core.managers import (
    ControlManager,
    GeometryManager,
    InstrumentationManager,
    MaterialManager,
    OutputManager,
    PhysicsManager,
    RampManager,
)
from pyfds.core.namelists import Head, Time
from pyfds.core.simulation import Simulation


class TestOutputManager:
    """Tests for OutputManager class."""

    def test_initialization(self):
        """Test manager initialization with all required managers."""
        geom = GeometryManager()
        mat = MaterialManager()
        phys = PhysicsManager()
        inst = InstrumentationManager()
        ctrl = ControlManager()
        ramps = RampManager()
        head = Head(chid="test")
        time = Time(t_end=100.0)

        mgr = OutputManager(geom, mat, phys, inst, ctrl, ramps, head, time)

        # Verify manager references are stored correctly
        assert mgr.geometry is geom
        assert mgr.material_mgr is mat
        assert mgr.physics is phys
        assert mgr.instrumentation is inst
        assert mgr.controls is ctrl
        assert mgr.ramps is ramps
        # head and time_params are private attributes
        assert mgr._head is head
        assert mgr._time_params is time

    def test_to_fds_basic(self):
        """Test basic FDS file generation."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        fds_content = sim.to_fds()

        # Should contain essential namelists
        assert "&HEAD" in fds_content
        assert "CHID='test'" in fds_content
        assert "&TIME" in fds_content
        assert "T_END=100.0" in fds_content
        assert "&MESH" in fds_content
        assert "&TAIL" in fds_content

    def test_to_fds_with_surface(self):
        """Test FDS generation with surface."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.surface(id="FIRE", hrrpua=1000.0)

        fds_content = sim.to_fds()

        assert "&SURF" in fds_content
        assert "ID='FIRE'" in fds_content
        assert "HRRPUA=1000.0" in fds_content

    def test_to_fds_namelist_ordering(self):
        """Test that namelists are in correct order."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.reaction(fuel="PROPANE")
        sim.surface(id="FIRE", hrrpua=1000.0)
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(0.5, 0.5, 0.5))

        fds_content = sim.to_fds()

        # Find positions of key namelists
        head_pos = fds_content.find("&HEAD")
        time_pos = fds_content.find("&TIME")
        mesh_pos = fds_content.find("&MESH")
        reac_pos = fds_content.find("&REAC")
        surf_pos = fds_content.find("&SURF")
        # OBST may or may not be present in output for this test; not used below
        devc_pos = fds_content.find("&DEVC")
        tail_pos = fds_content.find("&TAIL")

        # Verify ordering (HEAD->TIME->MESH->REAC->SURF->DEVC->TAIL)
        assert head_pos < time_pos
        assert time_pos < mesh_pos
        assert mesh_pos < reac_pos
        assert reac_pos < surf_pos
        assert surf_pos < devc_pos
        assert devc_pos < tail_pos

    def test_to_fds_ramps_before_materials(self):
        """Test that RAMPs appear before MATLs in FDS output."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Add ramp
        sim.ramp(id="TEMP_RAMP", points=[(20, 1.0), (100, 1.5)])

        # Add material that references ramp
        sim.material(id="WOOD", density=500.0, conductivity=0.13, specific_heat_ramp="TEMP_RAMP")

        fds_content = sim.to_fds()

        # RAMPs must appear before MATLs
        ramp_pos = fds_content.find("&RAMP")
        matl_pos = fds_content.find("&MATL")

        assert ramp_pos != -1, "RAMP not found in output"
        assert matl_pos != -1, "MATL not found in output"
        assert ramp_pos < matl_pos, "RAMP must appear before MATL"

    def test_to_fds_with_all_managers(self):
        """Test FDS generation using all managers."""
        sim = Simulation(chid="comprehensive_test")
        sim.time(t_end=300.0)
        sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))

        # Geometry
        sim.obstruction(xb=(2, 3, 2, 3, 0, 0.1), surf_id="FIRE")

        # Materials
        sim.surface(id="FIRE", hrrpua=1000.0)
        sim.material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)

        # Ramps
        sim.ramp(id="FIRE_RAMP", points=[(0, 0), (300, 1000)])

        # Physics
        sim.reaction(fuel="PROPANE")
        sim.set_misc(tmpa=25.0)

        # Instrumentation
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(2.5, 2.5, 2.0))
        sim.prop(id="SPRINKLER", activation_temperature=68.0)

        # Controls
        sim.ctrl(id="ALARM", function_type="ANY", input_id=["TEMP1"])
        sim.init(xb=(0, 5, 0, 5, 0, 0.1), temperature=400.0)

        fds_content = sim.to_fds()

        # Verify all namelists are present
        assert "&HEAD" in fds_content
        assert "&TIME" in fds_content
        assert "&MESH" in fds_content
        assert "&OBST" in fds_content
        assert "&SURF" in fds_content
        assert "&MATL" in fds_content
        assert "&RAMP" in fds_content
        assert "&REAC" in fds_content
        assert "&MISC" in fds_content
        assert "&DEVC" in fds_content
        assert "&PROP" in fds_content
        assert "&CTRL" in fds_content
        assert "&INIT" in fds_content
        assert "&TAIL" in fds_content

    def test_to_fds_empty_simulation(self):
        """Test FDS generation with minimal simulation."""
        sim = Simulation(chid="minimal")

        fds_content = sim.to_fds()

        # Should still have HEAD and TAIL
        assert "&HEAD" in fds_content
        assert "CHID='minimal'" in fds_content
        assert "&TAIL" in fds_content

    def test_to_fds_with_vents(self):
        """Test FDS generation with vents."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.vent(xb=(0, 0, 0, 1, 0, 1), surf_id="OPEN")

        fds_content = sim.to_fds()

        assert "&VENT" in fds_content
        assert "SURF_ID='OPEN'" in fds_content

    def test_to_fds_multiple_meshes(self):
        """Test FDS generation with multiple meshes."""
        sim = Simulation(chid="test")
        sim.time(t_end=100.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.mesh(ijk=(20, 20, 10), xb=(1, 3, 0, 2, 0, 1))

        fds_content = sim.to_fds()

        # Should have two MESH namelists
        mesh_count = fds_content.count("&MESH")
        assert mesh_count == 2

    def test_validate_returns_empty_list(self):
        """Test that OutputManager validate returns empty list."""
        sim = Simulation(chid="test")

        # OutputManager delegates validation to other managers
        # Its own validate should return empty
        output_mgr = OutputManager(
            sim.geometry,
            sim.material_mgr,
            sim.physics,
            sim.instrumentation,
            sim.controls,
            sim.ramps,
            sim.head,
            sim.time_params,
        )

        warnings = output_mgr.validate()
        assert warnings == []
