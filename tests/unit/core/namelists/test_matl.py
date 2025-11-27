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
            Material(id="BAD", density=0.05, conductivity=0.13, specific_heat=2.5)

    def test_material_to_fds(self):
        """Test FDS output format."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        fds_str = mat.to_fds()
        assert "&MATL ID='WOOD'" in fds_str
        assert "DENSITY=500" in fds_str

    def test_particle_products_single_reaction(self):
        """Test particle products for single reaction."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            part_id="SOOT",
            nu_part=0.01,
        )
        assert mat.part_id == "SOOT"
        assert mat.nu_part == 0.01
        fds_str = mat.to_fds()
        assert "PART_ID='SOOT'" in fds_str
        assert "NU_PART=0.01" in fds_str

    def test_particle_products_multi_reaction(self):
        """Test particle products for multi-reaction material."""
        mat = Material(
            id="PLASTIC",
            density=1000.0,
            conductivity=0.2,
            specific_heat=2.0,
            n_reactions=2,
            part_id=["SOOT", "SMOKE"],
            nu_part=[0.01, 0.005],
        )
        fds_str = mat.to_fds()
        assert "PART_ID(1)='SOOT'" in fds_str
        assert "PART_ID(2)='SMOKE'" in fds_str
        assert "NU_PART(1)=0.01" in fds_str
        assert "NU_PART(2)=0.005" in fds_str


class TestMaterialMultiReaction:
    """Tests for multi-reaction pyrolysis."""

    def test_multi_reaction_2d_arrays(self):
        """Test 2D array syntax for products."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            n_reactions=2,
            a=[1e10, 5e8],
            e=[100000, 120000],
            heat_of_reaction=[1800, 500],
            spec_id=[["VAPOR"], ["CO2", "H2O"]],
            nu_spec=[[0.82], [0.1, 0.08]],
            matl_id=[["CHAR"], []],
            nu_matl=[[0.18], []],
        )
        fds = mat.to_fds()
        assert "SPEC_ID(1:1,1)='VAPOR'" in fds
        assert "NU_SPEC(1:1,1)=0.82" in fds
        assert "MATL_ID(1:1,1)='CHAR'" in fds
        assert "NU_MATL(1:1,1)=0.18" in fds
        assert "SPEC_ID(1:2,2)='CO2','H2O'" in fds
        assert "NU_SPEC(1:2,2)=0.1,0.08" in fds

    def test_yield_sum_validation(self):
        """Test that yields are validated."""
        with pytest.raises(ValidationError, match="yield"):
            Material(
                id="BAD",
                density=500.0,
                conductivity=0.13,
                specific_heat=2.5,
                n_reactions=1,
                nu_spec=[[0.6]],
                nu_matl=[[0.6]],  # Sum = 1.2 > 1.0
            )

    def test_single_reaction_with_multiple_species(self):
        """Test single reaction with multiple gaseous products."""
        mat = Material(
            id="PLASTIC",
            density=1000.0,
            conductivity=0.2,
            specific_heat=2.0,
            n_reactions=1,
            spec_id=["CO", "CO2", "H2O"],
            nu_spec=[0.1, 0.3, 0.4],
            matl_id="CHAR",
            nu_matl=0.2,
        )
        fds = mat.to_fds()
        assert "SPEC_ID='CO','CO2','H2O'" in fds
        assert "NU_SPEC=0.1,0.3,0.4" in fds
        assert "MATL_ID='CHAR'" in fds
        assert "NU_MATL=0.2" in fds


class TestMaterialLiquidFuel:
    """Tests for liquid fuel evaporation model."""

    def test_liquid_fuel_basic(self):
        """Test basic liquid fuel definition."""
        ethanol = Material(
            id="ETHANOL_LIQUID",
            density=794,
            conductivity=0.17,
            specific_heat=2.44,
            boiling_temperature=78.5,
            spec_id="ETHANOL",
            heat_of_reaction=837,
            absorption_coefficient=1140,
        )
        assert ethanol.boiling_temperature == 78.5
        fds = ethanol.to_fds()
        assert "BOILING_TEMPERATURE=78.5" in fds

    def test_liquid_fuel_with_heat_of_vaporization(self):
        """Test liquid fuel with heat of vaporization."""
        methanol = Material(
            id="METHANOL_LIQUID",
            density=792,
            conductivity=0.2,
            specific_heat=2.51,
            boiling_temperature=64.7,
            heat_of_vaporization=1100,
            spec_id="METHANOL",
            absorption_coefficient=140,
        )
        assert methanol.heat_of_vaporization == 1100
        fds = methanol.to_fds()
        assert "HEAT_OF_VAPORIZATION=1100" in fds


class TestMaterialDelamination:
    """Tests for delamination parameters."""

    def test_delamination_temperature(self):
        """Test temperature-based delamination."""
        mat = Material(
            id="CLT_PANEL",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            delamination_tmp=200.0,
        )
        assert mat.delamination_tmp == 200.0
        fds = mat.to_fds()
        assert "DELAMINATION_TMP=200" in fds

    def test_delamination_density(self):
        """Test density-based delamination."""
        mat = Material(
            id="COMPOSITE",
            density=800.0,
            conductivity=0.15,
            specific_heat=1.8,
            delamination_density=300.0,
        )
        assert mat.delamination_density == 300.0
        fds = mat.to_fds()
        assert "DELAMINATION_DENSITY=300" in fds
