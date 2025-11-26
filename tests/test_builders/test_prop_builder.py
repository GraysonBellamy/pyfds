"""Tests for PropBuilder class."""

import pytest

from pyfds.builders import PropBuilder


class TestPropBuilder:
    """Test PropBuilder functionality."""

    def test_sprinkler_basic(self):
        """Test basic sprinkler property."""
        prop = PropBuilder.sprinkler(id="SPRINKLER", activation_temp=68, rti=50, flow_rate=60)

        assert prop.id == "SPRINKLER"
        assert prop.activation_temperature == 68
        assert prop.rti == 50
        assert prop.flow_rate == 60

    def test_sprinkler_with_k_factor(self):
        """Test sprinkler with K-factor."""
        prop = PropBuilder.sprinkler(id="SPRINKLER_K", activation_temp=74, rti=100, k_factor=80)

        assert prop.k_factor == 80
        assert prop.flow_rate is None  # Should not have flow_rate

    def test_sprinkler_with_spray_angle(self):
        """Test sprinkler with spray angle."""
        prop = PropBuilder.sprinkler(
            id="SPRINKLER",
            activation_temp=68,
            rti=50,
            flow_rate=60,
            spray_angle=(45, 90),
        )

        assert prop.spray_angle == (45, 90)

    def test_smoke_detector_default(self):
        """Test smoke detector with default obscuration."""
        prop = PropBuilder.smoke_detector(id="SMOKE_DET")

        assert prop.id == "SMOKE_DET"
        assert prop.quantity == "CHAMBER_OBSCURATION"
        assert prop.activation_obscuration == 3.28  # UL standard

    def test_smoke_detector_custom_obscuration(self):
        """Test smoke detector with custom obscuration."""
        prop = PropBuilder.smoke_detector(id="SMOKE_DET", activation_obscuration=5.0)

        assert prop.activation_obscuration == 5.0

    def test_heat_detector_default_rti(self):
        """Test heat detector with default RTI."""
        prop = PropBuilder.heat_detector(id="HEAT_DET", activation_temp=74)

        assert prop.id == "HEAT_DET"
        assert prop.activation_temperature == 74
        assert prop.rti == 5.0  # Default fast-response

    def test_heat_detector_custom_rti(self):
        """Test heat detector with custom RTI."""
        prop = PropBuilder.heat_detector(id="HEAT_DET", activation_temp=74, rti=50)

        assert prop.rti == 50

    def test_quick_response_sprinkler(self):
        """Test predefined quick-response sprinkler."""
        prop = PropBuilder.quick_response_sprinkler()

        assert prop.id == "SPRINKLER_QR"
        assert prop.activation_temperature == 68
        assert prop.rti == 50
        assert prop.flow_rate == 60

    def test_quick_response_sprinkler_custom_id(self):
        """Test quick-response sprinkler with custom ID."""
        prop = PropBuilder.quick_response_sprinkler(id="MY_SPRINKLER")

        assert prop.id == "MY_SPRINKLER"

    def test_standard_response_sprinkler(self):
        """Test predefined standard-response sprinkler."""
        prop = PropBuilder.standard_response_sprinkler()

        assert prop.id == "SPRINKLER_SR"
        assert prop.activation_temperature == 74
        assert prop.rti == 100
        assert prop.k_factor == 80

    def test_standard_response_sprinkler_custom_id(self):
        """Test standard-response sprinkler with custom ID."""
        prop = PropBuilder.standard_response_sprinkler(id="STANDARD")

        assert prop.id == "STANDARD"

    def test_fds_output_format_sprinkler(self):
        """Test FDS output format for sprinkler."""
        prop = PropBuilder.sprinkler(id="TEST", activation_temp=68, rti=50, flow_rate=60)

        fds_output = prop.to_fds()
        assert "&PROP" in fds_output
        assert "ID='TEST'" in fds_output
        assert "ACTIVATION_TEMPERATURE=68" in fds_output
        assert "RTI=50" in fds_output

    def test_fds_output_format_smoke_detector(self):
        """Test FDS output format for smoke detector."""
        prop = PropBuilder.smoke_detector(id="SMOKE")

        fds_output = prop.to_fds()
        assert "&PROP" in fds_output
        assert "QUANTITY='CHAMBER_OBSCURATION'" in fds_output

    def test_build_raises_error(self):
        """Test that build() method raises NotImplementedError."""
        builder = PropBuilder()
        with pytest.raises(NotImplementedError, match="factory methods"):
            builder.build()
