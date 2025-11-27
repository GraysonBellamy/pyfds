"""
Unit tests for InstrumentationManager.
"""

from pyfds.core.managers.instrumentation import InstrumentationManager
from pyfds.core.namelists import Device, Prop
from pyfds.core.simulation import Simulation


class TestInstrumentationManager:
    """Tests for InstrumentationManager class."""

    def test_initialization(self):
        """Test manager initialization."""
        mgr = InstrumentationManager()
        assert mgr.devices == []
        assert mgr.props == []

    def test_add_device_directly(self):
        """Test adding device directly to manager."""
        mgr = InstrumentationManager()
        device = Device(id="TEMP1", quantity="TEMPERATURE", xyz=(1, 1, 1))
        mgr.add_device(device)

        assert len(mgr.devices) == 1
        assert mgr.devices[0].id == "TEMP1"

    def test_add_device_via_simulation_builder(self):
        """Test adding device through simulation builder method."""
        sim = Simulation(chid="test")
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(1.0, 1.0, 1.0))

        assert len(sim.instrumentation.devices) == 1
        assert sim.instrumentation.devices[0].quantity == "TEMPERATURE"

    def test_add_device_via_simulation_add_method(self):
        """Test adding device through simulation add_device method."""
        sim = Simulation(chid="test")
        device = Device(id="TEMP2", quantity="VELOCITY", xyz=(2, 2, 2))
        sim.add_device(device)

        assert len(sim.instrumentation.devices) == 1
        assert sim.instrumentation.devices[0].id == "TEMP2"

    def test_add_multiple_devices(self):
        """Test adding multiple devices."""
        sim = Simulation(chid="test")
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(1, 1, 1))
        sim.device(id="TEMP2", quantity="TEMPERATURE", xyz=(2, 2, 2))
        sim.device(id="TEMP3", quantity="TEMPERATURE", xyz=(3, 3, 3))

        assert len(sim.instrumentation.devices) == 3

    def test_add_prop_directly(self):
        """Test adding prop directly to manager."""
        mgr = InstrumentationManager()
        prop = Prop(id="SPRINKLER", activation_temperature=68.0)
        mgr.add_prop(prop)

        assert len(mgr.props) == 1
        assert mgr.props[0].id == "SPRINKLER"

    def test_add_prop_via_simulation_builder(self):
        """Test adding prop through simulation builder method."""
        sim = Simulation(chid="test")
        sim.prop(id="SPRINKLER", activation_temperature=68.0)

        assert len(sim.instrumentation.props) == 1
        assert sim.instrumentation.props[0].activation_temperature == 68.0

    def test_add_prop_via_simulation_add_method(self):
        """Test adding prop through simulation add_prop method."""
        sim = Simulation(chid="test")
        prop = Prop(id="SMOKE_DET", activation_obscuration=3.28)
        sim.add_prop(prop)

        assert len(sim.instrumentation.props) == 1
        assert sim.instrumentation.props[0].id == "SMOKE_DET"

    def test_add_multiple_props(self):
        """Test adding multiple props."""
        sim = Simulation(chid="test")
        sim.prop(id="SPRINKLER1", activation_temperature=68.0)
        sim.prop(id="SPRINKLER2", activation_temperature=74.0)

        assert len(sim.instrumentation.props) == 2

    def test_validate_no_warnings_when_empty(self):
        """Test validation passes with no devices or props."""
        mgr = InstrumentationManager()
        warnings = mgr.validate()
        assert len(warnings) == 0

    def test_validate_with_devices(self):
        """Test validation with devices."""
        sim = Simulation(chid="test")
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(1, 1, 1))
        sim.device(id="TEMP2", quantity="TEMPERATURE", xyz=(2, 2, 2))

        warnings = sim.instrumentation.validate()
        assert len(warnings) == 0

    def test_validate_with_props(self):
        """Test validation with props."""
        sim = Simulation(chid="test")
        sim.prop(id="SPRINKLER", activation_temperature=68.0)

        warnings = sim.instrumentation.validate()
        assert len(warnings) == 0

    def test_validate_duplicate_device_ids(self):
        """Test validation catches duplicate device IDs."""
        sim = Simulation(chid="test")
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(1, 1, 1))
        sim.device(id="TEMP1", quantity="TEMPERATURE", xyz=(2, 2, 2))  # Duplicate ID

        warnings = sim.instrumentation.validate()
        duplicate_warnings = [w for w in warnings if "duplicate" in w.lower()]
        assert len(duplicate_warnings) > 0
