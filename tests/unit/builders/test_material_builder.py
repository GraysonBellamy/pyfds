"""Tests for MaterialBuilder class."""

import pytest

from pyfds.builders import MaterialBuilder
from pyfds.builders.libraries import CommonMaterials


class TestMaterialBuilder:
    """Test MaterialBuilder functionality."""

    def test_simple_material(self):
        """Test creating simple material with constant properties."""
        mat = (
            MaterialBuilder("WOOD")
            .density(500)
            .thermal_conductivity(0.13)
            .specific_heat(2.5)
            .build()
        )

        assert mat.id == "WOOD"
        assert mat.density == 500
        assert mat.conductivity == 0.13
        assert mat.specific_heat == 2.5
        assert mat.emissivity == 0.9  # Default

    def test_temperature_dependent_properties(self):
        """Test material with temperature-dependent properties."""
        mat = (
            MaterialBuilder("STEEL")
            .density(7850)
            .thermal_conductivity_ramp("STEEL_K")
            .specific_heat(0.46)
            .emissivity(0.7)
            .build()
        )

        assert mat.conductivity_ramp == "STEEL_K"
        assert mat.conductivity is None  # Should be None when using ramp
        assert mat.emissivity == 0.7

    def test_custom_emissivity(self):
        """Test setting custom emissivity."""
        mat = (
            MaterialBuilder("ALUMINUM")
            .density(2700)
            .thermal_conductivity(237)
            .specific_heat(0.90)
            .emissivity(0.2)
            .build()
        )

        assert mat.emissivity == 0.2

    def test_pyrolysis_single_reaction(self):
        """Test material with single pyrolysis reaction."""
        from pyfds.core.models import PyrolysisProduct, PyrolysisReaction

        reaction = PyrolysisReaction(
            a=1e10,
            e=80000,
            heat_of_reaction=1000,
            products=[PyrolysisProduct(spec_id="FUEL_VAPOR", nu_spec=1.0)],
        )

        mat = (
            MaterialBuilder("FOAM")
            .density(40)
            .thermal_conductivity(0.04)
            .specific_heat(1.5)
            .add_reaction(reaction)
            .build()
        )

        assert mat.reactions == [reaction]
        assert mat.n_reactions == 1

    def test_pyrolysis_multi_reaction(self):
        """Test material with multiple pyrolysis reactions."""
        from pyfds.core.models import PyrolysisProduct, PyrolysisReaction

        reaction1 = PyrolysisReaction(
            a=1e10,
            e=80000,
            heat_of_reaction=1000,
            products=[PyrolysisProduct(spec_id="VAPOR_1", nu_spec=1.0)],
        )
        reaction2 = PyrolysisReaction(
            a=5e8,
            e=120000,
            heat_of_reaction=1500,
            products=[PyrolysisProduct(matl_id="CHAR", nu_matl=1.0)],
        )

        mat = (
            MaterialBuilder("COMPLEX")
            .density(500)
            .thermal_conductivity(0.15)
            .specific_heat(2.0)
            .add_reaction(reaction1)
            .add_reaction(reaction2)
            .build()
        )

        assert mat.reactions == [reaction1, reaction2]
        assert mat.n_reactions == 2

    def test_structured_pyrolysis_reaction(self):
        """Test material with structured PyrolysisReaction."""
        from pyfds.core.models import PyrolysisProduct, PyrolysisReaction

        reaction = PyrolysisReaction(
            a=1e10,
            e=150000,
            heat_of_reaction=500,
            products=[
                PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.8),
                PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
            ],
        )

        mat = (
            MaterialBuilder("WOOD")
            .density(500)
            .thermal_conductivity(0.13)
            .specific_heat(2.5)
            .add_reaction(reaction)
            .build()
        )

        assert mat.reactions == [reaction]
        assert mat.n_reactions == 1

    def test_missing_density_error(self):
        """Test error when density not specified."""
        with pytest.raises(ValueError, match="density is required"):
            MaterialBuilder("TEST").thermal_conductivity(1.0).specific_heat(1.0).build()

    def test_builder_reuse_error(self):
        """Test that builder cannot be reused."""
        builder = MaterialBuilder("TEST").density(1000).thermal_conductivity(1.0).specific_heat(1.0)
        builder.build()

        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()

    def test_predefined_concrete(self):
        """Test predefined concrete material from CommonMaterials."""
        concrete = CommonMaterials.concrete()

        assert concrete.id == "CONCRETE"
        assert concrete.density == 2400
        assert concrete.conductivity == 1.6
        assert concrete.specific_heat == 0.88

    def test_predefined_gypsum(self):
        """Test predefined gypsum material from CommonMaterials."""
        gypsum = CommonMaterials.gypsum()

        assert gypsum.id == "GYPSUM"
        assert gypsum.density == 930
        assert gypsum.conductivity == 0.48

    def test_predefined_steel(self):
        """Test predefined steel material from CommonMaterials."""
        steel = CommonMaterials.steel()

        assert steel.id == "STEEL"
        assert steel.density == 7850
        assert steel.conductivity == 45.8
        assert steel.emissivity == 0.7

    def test_predefined_aluminum(self):
        """Test predefined aluminum material from CommonMaterials."""
        aluminum = CommonMaterials.aluminum()

        assert aluminum.id == "ALUMINUM"
        assert aluminum.density == 2700
        assert aluminum.conductivity == 237

    def test_predefined_brick(self):
        """Test predefined brick material from CommonMaterials."""
        brick = CommonMaterials.brick()

        assert brick.id == "BRICK"
        assert brick.density == 1920

    def test_predefined_wood(self):
        """Test predefined wood material from CommonMaterials."""
        wood = CommonMaterials.wood()

        assert wood.id == "WOOD"
        assert wood.density == 500
        assert wood.conductivity == 0.13

    def test_fds_output_format(self):
        """Test FDS output format."""
        mat = (
            MaterialBuilder("TEST")
            .density(1000)
            .thermal_conductivity(1.0)
            .specific_heat(2.0)
            .build()
        )

        fds_output = mat.to_fds()
        assert "&MATL" in fds_output
        assert "ID='TEST'" in fds_output
        assert "DENSITY=1000" in fds_output
        assert "CONDUCTIVITY=1" in fds_output or "CONDUCTIVITY=1.0" in fds_output

    def test_both_conductivity_and_ramp_override(self):
        """Test that setting ramp overrides constant value."""
        mat = (
            MaterialBuilder("TEST")
            .density(1000)
            .thermal_conductivity(1.0)  # Set constant first
            .thermal_conductivity_ramp("RAMP_K")  # Override with ramp
            .specific_heat(1.0)
            .build()
        )

        assert mat.conductivity is None
        assert mat.conductivity_ramp == "RAMP_K"

    def test_custom_absorption_coefficient(self):
        """Test setting custom absorption coefficient."""
        mat = (
            MaterialBuilder("TEST")
            .density(1000)
            .thermal_conductivity(1.0)
            .specific_heat(1.0)
            .absorption_coefficient(10000.0)
            .build()
        )

        assert mat.absorption_coefficient == 10000.0


class TestLiquidFuelBuilder:
    """Test liquid fuel material building."""

    def test_basic_liquid_fuel(self):
        """Test basic liquid fuel configuration."""
        matl = (
            MaterialBuilder("ETHANOL")
            .density(794)
            .thermal_conductivity(0.17)
            .specific_heat(2.44)
            .as_liquid_fuel(boiling_temperature=78.5, spec_id="ETHANOL")
            .build()
        )

        assert matl.boiling_temperature == 78.5
        assert matl.reactions is not None
        assert len(matl.reactions) == 1
        assert matl.reactions[0].products[0].spec_id == "ETHANOL"

    def test_liquid_fuel_with_all_params(self):
        """Test liquid fuel with all optional parameters.

        Note: In FDS, heat_of_vaporization is passed as heat_of_reaction
        on the evaporation reaction, not as a separate Material parameter.
        """
        matl = (
            MaterialBuilder("METHANOL")
            .thermal_conductivity(0.2)
            .specific_heat(2.51)
            .density(792)
            .as_liquid_fuel(
                boiling_temperature=64.7,
                spec_id="METHANOL",
                mw=32.04,
                heat_of_vaporization=1100,
                absorption_coefficient=140,
            )
            .build()
        )

        assert matl.boiling_temperature == 64.7
        assert matl.mw == 32.04
        # heat_of_vaporization is set as heat_of_reaction on the evaporation reaction
        assert matl.reactions is not None
        assert matl.reactions[0].heat_of_reaction == 1100
        assert matl.absorption_coefficient == 140
        assert matl.reactions[0].products[0].spec_id == "METHANOL"
