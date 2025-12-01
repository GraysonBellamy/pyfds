"""Unit tests for PROP namelist."""

from pyfds.core.namelists import Property


class TestProp:
    """Tests for Property namelist."""

    def test_sprinkler_prop(self):
        """Test sprinkler property."""
        prop = Property(id="SPRINKLER", activation_temperature=68, rti=50)
        assert prop.id == "SPRINKLER"
        assert prop.activation_temperature == 68

    def test_prop_to_fds(self):
        """Test FDS output format."""
        prop = Property(id="SPRINKLER", activation_temperature=68)
        fds_str = prop.to_fds()
        assert "&PROP ID='SPRINKLER'" in fds_str
        assert "ACTIVATION_TEMPERATURE=68" in fds_str
