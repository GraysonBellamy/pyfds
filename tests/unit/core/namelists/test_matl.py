"""Unit tests for MATL namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Material


class TestMaterial:
    """Tests for Material namelist."""

    def test_basic_creation(self):
        """Test basic material creation."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        assert mat.id == "WOOD"
        assert mat.density == 500.0

    def test_material_with_ramp(self):
        """Test material with temperature-dependent properties."""
        mat = Material(id="STEEL", density=7850.0, conductivity_ramp="STEEL_K", specific_heat=0.46)
        assert mat.conductivity_ramp == "STEEL_K"

    def test_material_validation_no_conductivity(self):
        """Test that conductivity is required."""
        with pytest.raises(ValidationError, match="CONDUCTIVITY"):
            Material(id="BAD", density=500.0, specific_heat=2.5)

    def test_material_validation_density_range(self):
        """Test density range validation."""
        with pytest.raises(ValidationError, match="DENSITY"):
            Material(id="BAD", density=0.5, conductivity=0.13, specific_heat=2.5)

    def test_material_to_fds(self):
        """Test FDS output format."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        fds_str = mat.to_fds()
        assert "&MATL ID='WOOD'" in fds_str
        assert "DENSITY=500" in fds_str
