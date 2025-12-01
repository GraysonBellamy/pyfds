"""Tests for CommonProps factory class."""

from pyfds.builders.libraries import CommonProps


class TestCommonProps:
    """Test CommonProps functionality."""

    def test_sprinkler_basic(self):
        """Test basic sprinkler property."""
        prop = CommonProps.sprinkler(id="SPRINKLER", activation_temp=68, rti=50, flow_rate=60)

        assert prop.id == "SPRINKLER"
        assert prop.activation_temperature == 68
        assert prop.rti == 50
        assert prop.flow_rate == 60

    def test_sprinkler_with_k_factor(self):
        """Test sprinkler with K-factor."""
        prop = CommonProps.sprinkler(id="SPRINKLER_K", activation_temp=74, rti=100, k_factor=80)

        assert prop.k_factor == 80
        assert prop.flow_rate is None  # Should not have flow_rate

    def test_sprinkler_with_spray_angle(self):
        """Test sprinkler with spray angle."""
        prop = CommonProps.sprinkler(
            id="SPRINKLER",
            activation_temp=68,
            rti=50,
            flow_rate=60,
            spray_angle=(45, 90),
        )

        assert prop.spray_angle == (45, 90)

    def test_smoke_detector_default(self):
        """Test smoke detector with default obscuration."""
        prop = CommonProps.smoke_detector(id="SMOKE_DET")

        assert prop.id == "SMOKE_DET"
        assert prop.quantity == "CHAMBER_OBSCURATION"
        assert prop.activation_obscuration == 3.28  # UL standard

    def test_smoke_detector_custom_obscuration(self):
        """Test smoke detector with custom obscuration."""
        prop = CommonProps.smoke_detector(id="SMOKE_DET", activation_obscuration=5.0)

        assert prop.activation_obscuration == 5.0

    def test_heat_detector_default_rti(self):
        """Test heat detector with default RTI."""
        prop = CommonProps.heat_detector(id="HEAT_DET", activation_temp=74)

        assert prop.id == "HEAT_DET"
        assert prop.activation_temperature == 74
        assert prop.rti == 5.0  # Default fast-response

    def test_heat_detector_custom_rti(self):
        """Test heat detector with custom RTI."""
        prop = CommonProps.heat_detector(id="HEAT_DET", activation_temp=74, rti=50)

        assert prop.rti == 50

    def test_quick_response_sprinkler(self):
        """Test predefined quick-response sprinkler."""
        prop = CommonProps.quick_response_sprinkler()

        assert prop.id == "SPRINKLER_QR"
        assert prop.activation_temperature == 68
        assert prop.rti == 50
        assert prop.flow_rate == 60

    def test_quick_response_sprinkler_custom_id(self):
        """Test quick-response sprinkler with custom ID."""
        prop = CommonProps.quick_response_sprinkler(id="MY_SPRINKLER")

        assert prop.id == "MY_SPRINKLER"

    def test_standard_response_sprinkler(self):
        """Test predefined standard-response sprinkler."""
        prop = CommonProps.standard_response_sprinkler()

        assert prop.id == "SPRINKLER_SR"
        assert prop.activation_temperature == 74
        assert prop.rti == 100
        assert prop.k_factor == 80

    def test_standard_response_sprinkler_custom_id(self):
        """Test standard-response sprinkler with custom ID."""
        prop = CommonProps.standard_response_sprinkler(id="STANDARD")

        assert prop.id == "STANDARD"

    def test_fds_output_format_sprinkler(self):
        """Test FDS output format for sprinkler."""
        prop = CommonProps.sprinkler(id="TEST", activation_temp=68, rti=50, flow_rate=60)

        fds_output = prop.to_fds()
        assert "&PROP" in fds_output
        assert "ID='TEST'" in fds_output
        assert "ACTIVATION_TEMPERATURE=68" in fds_output
        assert "RTI=50" in fds_output

    def test_fds_output_format_smoke_detector(self):
        """Test FDS output format for smoke detector."""
        prop = CommonProps.smoke_detector(id="SMOKE")

        fds_output = prop.to_fds()
        assert "&PROP" in fds_output
        assert "QUANTITY='CHAMBER_OBSCURATION'" in fds_output


class TestCommonPropsEnhanced:
    """Test enhanced CommonProps functionality (Stage 2.3)."""

    def test_sprinkler_with_c_factor(self):
        """Test sprinkler with C-factor."""
        prop = CommonProps.sprinkler(id="SPRINKLER_C", activation_temp=68, rti=50, c_factor=3.5)
        assert prop.c_factor == 3.5

    def test_sprinkler_with_pressure(self):
        """Test sprinkler with operating pressure."""
        prop = CommonProps.sprinkler(id="SPRINKLER_P", activation_temp=68, rti=50, pressure=200000)
        assert prop.pressure == 200000

    def test_sprinkler_with_orifice_diameter(self):
        """Test sprinkler with orifice diameter."""
        prop = CommonProps.sprinkler(
            id="SPRINKLER_D", activation_temp=68, rti=50, orifice_diameter=0.012
        )
        assert prop.orifice_diameter == 0.012

    def test_smoke_detector_with_optical_properties(self):
        """Test smoke detector with extinction and scattering coefficients."""
        prop = CommonProps.smoke_detector(
            id="OPTICAL_SMOKE", activation_obscuration=3.28, alpha_e=0.5, beta_e=0.3
        )
        assert prop.alpha_e == 0.5
        assert prop.beta_e == 0.3

    def test_smoke_detector_with_smokeview_id(self):
        """Test smoke detector with Smokeview ID."""
        prop = CommonProps.smoke_detector(id="SMOKE_VIS", smokeview_id="SMOKE_VIZ_1")
        assert prop.smokeview_id == "SMOKE_VIZ_1"

    def test_heat_detector_with_bead_properties(self):
        """Test heat detector with bead properties."""
        prop = CommonProps.heat_detector(
            id="HEAT_BEAD",
            activation_temp=74,
            rti=10.0,
            bead_diameter=0.001,
            bead_density=8000,
            bead_specific_heat=0.5,
        )
        assert prop.bead_diameter == 0.001
        assert prop.bead_density == 8000
        assert prop.bead_specific_heat == 0.5

    def test_nozzle_with_flow_rate(self):
        """Test nozzle with flow rate."""
        prop = CommonProps.nozzle(id="SPRAY_NOZZLE", flow_rate=50, pressure=300000)
        assert prop.id == "SPRAY_NOZZLE"
        assert prop.flow_rate == 50
        assert prop.pressure == 300000

    def test_nozzle_with_k_factor(self):
        """Test nozzle with K-factor and orifice diameter."""
        prop = CommonProps.nozzle(id="K_NOZZLE", k_factor=80, orifice_diameter=0.01)
        assert prop.k_factor == 80
        assert prop.orifice_diameter == 0.01


class TestCommonPropsFDSOutput:
    """Test FDS output for enhanced PROP features."""

    def test_sprinkler_with_all_parameters_fds(self):
        """Test FDS output for sprinkler with all parameters."""
        prop = CommonProps.sprinkler(
            id="FULL_SPRINKLER",
            activation_temp=68,
            rti=50,
            flow_rate=60,
            k_factor=80,
            spray_angle=(45, 90),
            c_factor=3.5,
            pressure=200000,
            orifice_diameter=0.012,
        )
        fds_output = prop.to_fds()
        assert "&PROP" in fds_output
        assert "FULL_SPRINKLER" in fds_output
        assert "68" in fds_output
        assert "50" in fds_output
        assert "60" in fds_output
        assert "80" in fds_output
        assert "3.5" in fds_output
        assert "200000" in fds_output
        assert "0.012" in fds_output

    def test_smoke_detector_with_optical_fds(self):
        """Test FDS output for smoke detector with optical properties."""
        prop = CommonProps.smoke_detector(id="OPTICAL", alpha_e=0.5, beta_e=0.3, smokeview_id="VIZ")
        fds_output = prop.to_fds()
        assert "OPTICAL" in fds_output
        assert "0.5" in fds_output
        assert "0.3" in fds_output
        assert "VIZ" in fds_output

    def test_heat_detector_with_bead_fds(self):
        """Test FDS output for heat detector with bead properties."""
        prop = CommonProps.heat_detector(
            id="BEAD_DET",
            activation_temp=74,
            bead_diameter=0.001,
            bead_density=8000,
            bead_specific_heat=0.5,
        )
        fds_output = prop.to_fds()
        assert "BEAD_DET" in fds_output
        assert "0.001" in fds_output
        assert "8000" in fds_output
        assert "0.5" in fds_output

    def test_nozzle_fds_output(self):
        """Test FDS output for nozzle."""
        prop = CommonProps.nozzle(id="NOZZLE", flow_rate=50, pressure=300000)
        fds_output = prop.to_fds()
        assert "&PROP" in fds_output
        assert "NOZZLE" in fds_output
        assert "50" in fds_output
        assert "300000" in fds_output
