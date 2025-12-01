"""Unit tests for HVAC namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Hvac


class TestHvacBasic:
    """Tests for basic HVAC functionality."""

    def test_basic_duct_creation(self):
        """Test basic duct creation."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
        )
        assert duct.id == "DUCT1"
        assert duct.type_id == "DUCT"
        assert duct.node_id == ("NODE1", "NODE2")
        assert duct.area == 0.04
        assert duct.length == 5.0

    def test_basic_node_creation(self):
        """Test basic node creation."""
        node = Hvac(
            id="NODE1",
            type_id="NODE",
            xyz=(5.0, 5.0, 2.0),
            vent_id="VENT1",
        )
        assert node.id == "NODE1"
        assert node.type_id == "NODE"
        assert node.xyz == (5.0, 5.0, 2.0)
        assert node.vent_id == "VENT1"

    def test_basic_fan_creation(self):
        """Test basic fan creation."""
        fan = Hvac(
            id="FAN1",
            type_id="FAN",
            max_flow=1.0,
            max_pressure=500.0,
        )
        assert fan.id == "FAN1"
        assert fan.type_id == "FAN"
        assert fan.max_flow == 1.0
        assert fan.max_pressure == 500.0


class TestHvacDuct:
    """Tests for HVAC duct parameters."""

    def test_duct_with_diameter(self):
        """Test duct with diameter specification."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            diameter=0.2,
            length=10.0,
            roughness=0.001,
        )
        assert duct.diameter == 0.2
        assert duct.roughness == 0.001

    def test_duct_with_perimeter(self):
        """Test duct with perimeter specification."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            perimeter=0.8,
            length=5.0,
        )
        assert duct.area == 0.04
        assert duct.perimeter == 0.8

    def test_duct_cross_section_types(self):
        """Test round and square cross-section flags."""
        round_duct = Hvac(
            id="ROUND",
            type_id="DUCT",
            node_id=("N1", "N2"),
            diameter=0.2,
            length=5.0,
            round=True,
        )
        assert round_duct.round is True

        square_duct = Hvac(
            id="SQUARE",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=5.0,
            square=True,
        )
        assert square_duct.square is True

    def test_duct_with_loss_coefficients(self):
        """Test duct with loss coefficients."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            loss=[0.5, 0.8],
        )
        assert duct.loss == [0.5, 0.8]

    def test_duct_with_volume_flow(self):
        """Test duct with fixed volume flow."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            volume_flow=0.5,
            tau_vf=2.0,
        )
        assert duct.volume_flow == 0.5
        assert duct.tau_vf == 2.0

    def test_duct_with_mass_flow(self):
        """Test duct with fixed mass flow."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            mass_flow=0.1,
        )
        assert duct.mass_flow == 0.1

    def test_duct_with_waypoints(self):
        """Test duct with waypoints."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            waypoints=[0.0, 0.0, 0.0, 2.5, 0.0, 1.0, 5.0, 0.0, 2.0],
        )
        assert len(duct.waypoints) == 9

    def test_duct_with_n_cells(self):
        """Test duct with number of cells for mass transport."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            n_cells=50,
        )
        assert duct.n_cells == 50

    def test_duct_reverse(self):
        """Test duct with reverse flow direction."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            reverse=True,
        )
        assert duct.reverse is True


class TestHvacNode:
    """Tests for HVAC node parameters."""

    def test_node_with_vent(self):
        """Test node with vent connection."""
        node = Hvac(
            id="NODE1",
            type_id="NODE",
            xyz=(5.0, 5.0, 2.0),
            vent_id="SUPPLY_VENT",
        )
        assert node.vent_id == "SUPPLY_VENT"

    def test_ambient_node(self):
        """Test ambient node."""
        node = Hvac(
            id="AMBIENT_NODE",
            type_id="NODE",
            xyz=(0.0, 0.0, 0.0),
            ambient=True,
        )
        assert node.ambient is True

    def test_node_with_ducts(self):
        """Test node with multiple duct connections."""
        node = Hvac(
            id="JUNCTION",
            type_id="NODE",
            xyz=(5.0, 5.0, 2.0),
            duct_id=["DUCT1", "DUCT2", "DUCT3"],
        )
        assert node.duct_id == ["DUCT1", "DUCT2", "DUCT3"]

    def test_node_with_filter(self):
        """Test node with filter."""
        node = Hvac(
            id="FILTER_NODE",
            type_id="NODE",
            xyz=(5.0, 5.0, 2.0),
            filter_id="HEPA_FILTER",
        )
        assert node.filter_id == "HEPA_FILTER"


class TestHvacFan:
    """Tests for HVAC fan parameters."""

    def test_fan_with_curve(self):
        """Test fan with performance curve."""
        fan = Hvac(
            id="FAN1",
            type_id="FAN",
            max_flow=2.0,
            max_pressure=1000.0,
            tau_fan=5.0,
        )
        assert fan.max_flow == 2.0
        assert fan.max_pressure == 1000.0
        assert fan.tau_fan == 5.0

    def test_fan_with_fixed_flow(self):
        """Test fan with fixed volume flow."""
        fan = Hvac(
            id="FAN1",
            type_id="FAN",
            volume_flow=1.5,
        )
        assert fan.volume_flow == 1.5


class TestHvacFilter:
    """Tests for HVAC filter parameters."""

    def test_filter_basic(self):
        """Test basic filter."""
        filter_hvac = Hvac(
            id="FILTER1",
            type_id="FILTER",
            clean_loss=2.0,
            efficiency=[0.99],
            spec_id=["SOOT"],
        )
        assert filter_hvac.clean_loss == 2.0
        assert filter_hvac.efficiency == [0.99]
        assert filter_hvac.spec_id == ["SOOT"]

    def test_filter_with_loading(self):
        """Test filter with loading parameters."""
        filter_hvac = Hvac(
            id="FILTER1",
            type_id="FILTER",
            clean_loss=2.0,
            efficiency=[0.99, 0.95],
            spec_id=["SOOT", "SMOKE"],
            loading=[0.0, 0.0],
            loading_multiplier=[100.0, 50.0],
        )
        assert filter_hvac.loading == [0.0, 0.0]
        assert filter_hvac.loading_multiplier == [100.0, 50.0]


class TestHvacAircoil:
    """Tests for HVAC air coil parameters."""

    def test_aircoil_basic(self):
        """Test basic air coil."""
        aircoil = Hvac(
            id="COIL1",
            type_id="AIRCOIL",
            coolant_mass_flow=0.5,
            coolant_specific_heat=4.18,
            coolant_temperature=7.0,
            efficiency=[0.8],
        )
        assert aircoil.coolant_mass_flow == 0.5
        assert aircoil.coolant_specific_heat == 4.18
        assert aircoil.coolant_temperature == 7.0
        assert aircoil.efficiency == [0.8]

    def test_aircoil_with_fixed_q(self):
        """Test air coil with fixed heat transfer."""
        aircoil = Hvac(
            id="COIL1",
            type_id="AIRCOIL",
            fixed_q=-50.0,  # Cooling
            tau_ac=2.0,
        )
        assert aircoil.fixed_q == -50.0
        assert aircoil.tau_ac == 2.0


class TestHvacDamper:
    """Tests for HVAC damper parameters."""

    def test_damper_basic(self):
        """Test basic damper."""
        damper = Hvac(
            id="DAMPER1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=0.1,
            damper=True,
            loss=[100.0, 100.0],
        )
        assert damper.damper is True
        assert damper.loss == [100.0, 100.0]

    def test_damper_with_ramp(self):
        """Test damper with variable loss ramp."""
        damper = Hvac(
            id="DAMPER1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=0.1,
            damper=True,
            ramp_loss="DAMPER_RAMP",
        )
        assert damper.ramp_loss == "DAMPER_RAMP"


class TestHvacLeak:
    """Tests for HVAC leak parameters."""

    def test_leak_basic(self):
        """Test basic leak."""
        leak = Hvac(
            id="LEAK1",
            type_id="LEAK",
            vent_id="LEAK_VENT1",
            vent2_id="LEAK_VENT2",
            discharge_coefficient=0.6,
        )
        assert leak.discharge_coefficient == 0.6

    def test_leak_with_pressure_params(self):
        """Test leak with pressure parameters."""
        leak = Hvac(
            id="LEAK1",
            type_id="LEAK",
            vent_id="LEAK_VENT1",
            vent2_id="LEAK_VENT2",
            leak_pressure_exponent=0.65,
            leak_reference_pressure=4.0,
            leak_enthalpy=True,
        )
        assert leak.leak_pressure_exponent == 0.65
        assert leak.leak_reference_pressure == 4.0
        assert leak.leak_enthalpy is True

    def test_leak_with_particle_transport(self):
        """Test leak with particle transport."""
        leak = Hvac(
            id="LEAK1",
            type_id="LEAK",
            vent_id="LEAK_VENT1",
            vent2_id="LEAK_VENT2",
            transport_particles=True,
        )
        assert leak.transport_particles is True


class TestHvacControl:
    """Tests for HVAC control parameters."""

    def test_control_with_ctrl_id(self):
        """Test HVAC with control ID."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=5.0,
            ctrl_id="FAN_CONTROL",
        )
        assert duct.ctrl_id == "FAN_CONTROL"

    def test_control_with_devc_id(self):
        """Test HVAC with device ID."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=5.0,
            devc_id="SMOKE_DETECTOR",
        )
        assert duct.devc_id == "SMOKE_DETECTOR"

    def test_control_with_ramp(self):
        """Test HVAC with ramp ID."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=5.0,
            ramp_id="FLOW_RAMP",
        )
        assert duct.ramp_id == "FLOW_RAMP"


class TestHvacOutput:
    """Tests for HVAC output parameters."""

    def test_output_quantities(self):
        """Test output quantity specification."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=5.0,
            quantity=["DUCT VELOCITY", "DUCT TEMPERATURE"],
        )
        assert duct.quantity == ["DUCT VELOCITY", "DUCT TEMPERATURE"]

    def test_output_with_species(self):
        """Test output with species specification."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=5.0,
            quantity=["DUCT MASS FRACTION"],
            quantity_spec_id=["CARBON DIOXIDE"],
            dry=[True],
        )
        assert duct.quantity_spec_id == ["CARBON DIOXIDE"]
        assert duct.dry == [True]


class TestHvacVisualization:
    """Tests for HVAC geometry visualization."""

    def test_geom_flag(self):
        """Test geometry visualization flag."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            diameter=0.2,
            length=5.0,
            geom=True,
        )
        assert duct.geom is True

    def test_geom2_flag(self):
        """Test alternative geometry visualization flag."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            diameter=0.2,
            length=5.0,
            geom2=True,
        )
        assert duct.geom2 is True


class TestHvacValidation:
    """Tests for HVAC validation."""

    def test_type_id_validation(self):
        """Test type_id validation."""
        # Each type has different requirements, test them individually
        # DUCT requires node_id
        duct = Hvac(id="DUCT1", type_id="DUCT", node_id=("N1", "N2"), length=1.0)
        assert duct.type_id == "DUCT"

        # NODE requires xyz or vent_id
        node = Hvac(id="NODE1", type_id="NODE", xyz=(0.0, 0.0, 0.0))
        assert node.type_id == "NODE"

        # FAN requires flow
        fan = Hvac(id="FAN1", type_id="FAN", volume_flow=1.0)
        assert fan.type_id == "FAN"

        # FILTER - efficiency is list of floats with separate spec_id list
        filter_hvac = Hvac(id="FILTER1", type_id="FILTER", efficiency=[0.9], spec_id=["SOOT"])
        assert filter_hvac.type_id == "FILTER"

        # AIRCOIL
        aircoil = Hvac(
            id="AIRCOIL1",
            type_id="AIRCOIL",
            coolant_specific_heat=4186.0,
            coolant_mass_flow=0.1,
            coolant_temperature=280.0,
        )
        assert aircoil.type_id == "AIRCOIL"

        # LEAK
        leak = Hvac(id="LEAK1", type_id="LEAK", area=0.01)
        assert leak.type_id == "LEAK"

        # Case insensitive
        hvac = Hvac(id="TEST", type_id="duct", node_id=("N1", "N2"), length=1.0)
        assert hvac.type_id == "DUCT"

        # Invalid type
        with pytest.raises(ValidationError):
            Hvac(id="TEST", type_id="INVALID")

    def test_duct_requires_node_id(self):
        """Test that DUCT type requires NODE_ID."""
        with pytest.raises(ValidationError, match="DUCT requires NODE_ID"):
            Hvac(id="DUCT1", type_id="DUCT", area=0.04, length=5.0)

    def test_fan_requires_flow(self):
        """Test that FAN type requires flow specification."""
        with pytest.raises(ValidationError, match="FAN requires MAX_FLOW or VOLUME_FLOW"):
            Hvac(id="FAN1", type_id="FAN", max_pressure=500.0)

    def test_node_id_length(self):
        """Test NODE_ID must have exactly 2 elements for DUCT."""
        # Single node is invalid for a duct
        with pytest.raises(ValidationError, match=r"NODE_ID|2"):
            Hvac(id="DUCT1", type_id="DUCT", node_id=("N1",), area=0.04, length=5.0)


class TestHvacFdsOutput:
    """Tests for FDS output generation."""

    def test_to_fds_duct(self):
        """Test FDS output for duct."""
        duct = Hvac(
            id="DUCT1",
            type_id="DUCT",
            node_id=("NODE1", "NODE2"),
            area=0.04,
            length=5.0,
            roughness=0.001,
        )
        fds_str = duct.to_fds()
        assert "&HVAC" in fds_str
        assert "ID='DUCT1'" in fds_str
        assert "TYPE_ID='DUCT'" in fds_str
        assert "NODE_ID='NODE1','NODE2'" in fds_str
        assert "AREA=0.04" in fds_str
        assert "LENGTH=5.0" in fds_str
        assert "ROUGHNESS=0.001" in fds_str
        assert "/" in fds_str

    def test_to_fds_node(self):
        """Test FDS output for node."""
        node = Hvac(
            id="NODE1",
            type_id="NODE",
            xyz=(5.0, 5.0, 2.0),
            vent_id="VENT1",
        )
        fds_str = node.to_fds()
        assert "ID='NODE1'" in fds_str
        assert "TYPE_ID='NODE'" in fds_str
        assert "XYZ=5.0,5.0,2.0" in fds_str
        assert "VENT_ID='VENT1'" in fds_str

    def test_to_fds_fan(self):
        """Test FDS output for fan."""
        fan = Hvac(
            id="FAN1",
            type_id="FAN",
            max_flow=1.0,
            max_pressure=500.0,
        )
        fds_str = fan.to_fds()
        assert "ID='FAN1'" in fds_str
        assert "TYPE_ID='FAN'" in fds_str
        assert "MAX_FLOW=1.0" in fds_str
        assert "MAX_PRESSURE=500.0" in fds_str

    def test_to_fds_damper(self):
        """Test FDS output for damper."""
        damper = Hvac(
            id="DAMPER1",
            type_id="DUCT",
            node_id=("N1", "N2"),
            area=0.04,
            length=0.1,
            damper=True,
        )
        fds_str = damper.to_fds()
        assert "DAMPER=.TRUE." in fds_str
