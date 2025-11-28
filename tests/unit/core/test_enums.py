"""Unit tests for enumerations."""

from pyfds.core.enums import BackingCondition, HeatTransferModel, SolidGeometry, SprayPattern


class TestSolidGeometry:
    """Tests for SolidGeometry enum."""

    def test_enum_values(self):
        """Test that enum values match FDS parameter names."""
        assert SolidGeometry.CARTESIAN == "CARTESIAN"
        assert SolidGeometry.CYLINDRICAL == "CYLINDRICAL"
        assert SolidGeometry.SPHERICAL == "SPHERICAL"
        assert SolidGeometry.INNER_CYLINDRICAL == "INNER CYLINDRICAL"

    def test_enum_members(self):
        """Test all enum members are present."""
        expected = {"CARTESIAN", "CYLINDRICAL", "SPHERICAL", "INNER CYLINDRICAL"}
        actual = {member.value for member in SolidGeometry}
        assert actual == expected


class TestBackingCondition:
    """Tests for BackingCondition enum."""

    def test_enum_values(self):
        """Test that enum values match FDS parameter names."""
        assert BackingCondition.VOID == "VOID"
        assert BackingCondition.INSULATED == "INSULATED"
        assert BackingCondition.EXPOSED == "EXPOSED"

    def test_enum_members(self):
        """Test all enum members are present."""
        expected = {"VOID", "INSULATED", "EXPOSED"}
        actual = {member.value for member in BackingCondition}
        assert actual == expected


class TestHeatTransferModel:
    """Tests for HeatTransferModel enum."""

    def test_enum_values(self):
        """Test that enum values match FDS parameter names."""
        assert HeatTransferModel.LOGLAW == "LOGLAW"
        assert HeatTransferModel.IMPINGING_JET == "IMPINGING JET"

    def test_enum_members(self):
        """Test all enum members are present."""
        expected = {"LOGLAW", "IMPINGING JET"}
        actual = {member.value for member in HeatTransferModel}
        assert actual == expected


class TestSprayPattern:
    """Tests for SprayPattern enum."""

    def test_enum_values(self):
        """Test that enum values match FDS parameter names."""
        assert SprayPattern.UNIFORM == "UNIFORM"
        assert SprayPattern.GAUSSIAN == "GAUSSIAN"

    def test_enum_members(self):
        """Test all enum members are present."""
        expected = {"UNIFORM", "GAUSSIAN"}
        actual = {member.value for member in SprayPattern}
        assert actual == expected
