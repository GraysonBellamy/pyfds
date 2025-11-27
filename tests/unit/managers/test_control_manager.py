"""
Unit tests for ControlManager.
"""

from pyfds.core.managers.control import ControlManager
from pyfds.core.namelists import Ctrl, Init
from pyfds.core.namelists.ctrl import ControlFunction
from pyfds.core.simulation import Simulation


class TestControlManager:
    """Tests for ControlManager class."""

    def test_initialization(self):
        """Test manager initialization."""
        mgr = ControlManager()
        assert mgr.ctrls == []
        assert mgr.inits == []

    def test_add_ctrl_directly(self):
        """Test adding control directly to manager."""
        mgr = ControlManager()
        ctrl = Ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD1", "SD2"])
        mgr.add_ctrl(ctrl)

        assert len(mgr.ctrls) == 1
        assert mgr.ctrls[0].id == "ALARM"

    def test_add_ctrl_via_simulation_builder(self):
        """Test adding control through simulation builder method."""
        sim = Simulation(chid="test")
        sim.ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD1", "SD2"])

        assert len(sim.controls.ctrls) == 1
        assert sim.controls.ctrls[0].function_type == ControlFunction.ANY

    def test_add_ctrl_via_simulation_add_method(self):
        """Test adding control through simulation add_ctrl method."""
        sim = Simulation(chid="test")
        ctrl = Ctrl(
            id="DELAY", function_type=ControlFunction.TIME_DELAY, input_id="SPRINKLER1", delay=5.0
        )
        sim.add_ctrl(ctrl)

        assert len(sim.controls.ctrls) == 1
        assert sim.controls.ctrls[0].delay == 5.0

    def test_add_multiple_ctrls(self):
        """Test adding multiple controls."""
        sim = Simulation(chid="test")
        sim.ctrl(id="CTRL1", function_type=ControlFunction.ANY, input_id=["A", "B"])
        sim.ctrl(id="CTRL2", function_type=ControlFunction.ALL, input_id=["C", "D"])

        assert len(sim.controls.ctrls) == 2

    def test_add_init_directly(self):
        """Test adding initial condition directly to manager."""
        mgr = ControlManager()
        init = Init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500.0)
        mgr.add_init(init)

        assert len(mgr.inits) == 1
        assert mgr.inits[0].temperature == 500.0

    def test_add_init_via_simulation_builder(self):
        """Test adding initial condition through simulation builder method."""
        sim = Simulation(chid="test")
        sim.init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500.0)

        assert len(sim.controls.inits) == 1
        assert sim.controls.inits[0].temperature == 500.0

    def test_add_init_via_simulation_add_method(self):
        """Test adding initial condition through simulation add_init method."""
        sim = Simulation(chid="test")
        init = Init(xyz=(5, 5, 1), temperature=300.0)
        sim.add_init(init)

        assert len(sim.controls.inits) == 1
        assert sim.controls.inits[0].xyz.x == 5
        assert sim.controls.inits[0].xyz.y == 5
        assert sim.controls.inits[0].xyz.z == 1

    def test_add_multiple_inits(self):
        """Test adding multiple initial conditions."""
        sim = Simulation(chid="test")
        sim.init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500.0)
        sim.init(xb=(10, 20, 0, 10, 0, 0.1), temperature=600.0)

        assert len(sim.controls.inits) == 2

    def test_add_init_with_species(self):
        """Test adding initial condition with species."""
        sim = Simulation(chid="test")
        sim.init(
            xb=(0, 10, 0, 10, 0, 0.1), temperature=500.0, spec_id=["PROPANE"], mass_fraction=[0.05]
        )

        assert len(sim.controls.inits) == 1
        assert sim.controls.inits[0].spec_id == ["PROPANE"]
        assert sim.controls.inits[0].mass_fraction == [0.05]

    def test_validate_no_warnings_when_empty(self):
        """Test validation passes with no controls or inits."""
        mgr = ControlManager()
        warnings = mgr.validate()
        assert len(warnings) == 0

    def test_validate_with_ctrls(self):
        """Test validation with controls."""
        sim = Simulation(chid="test")
        sim.ctrl(id="ALARM", function_type=ControlFunction.ANY, input_id=["SD1", "SD2"])

        warnings = sim.controls.validate()
        assert len(warnings) == 0

    def test_validate_with_inits(self):
        """Test validation with initial conditions."""
        sim = Simulation(chid="test")
        sim.init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500.0)

        warnings = sim.controls.validate()
        assert len(warnings) == 0
