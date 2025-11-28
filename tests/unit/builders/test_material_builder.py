"""Tests for MaterialBuilder class."""

import pytest

from pyfds.builders import MaterialBuilder


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
        mat = (
            MaterialBuilder("FOAM")
            .density(40)
            .thermal_conductivity(0.04)
            .specific_heat(1.5)
            .add_pyrolysis_reaction(
                a=1e10, e=80000, heat_of_reaction=1000, product_species="FUEL_VAPOR"
            )
            .build()
        )

        assert mat.n_reactions == 1
        assert mat.a == [1e10]
        assert mat.e == [80000]
        assert mat.heat_of_reaction == [1000]
        assert mat.spec_id == ["FUEL_VAPOR"]
        assert mat.nu_spec == [1.0]

    def test_pyrolysis_multi_reaction(self):
        """Test material with multiple pyrolysis reactions."""
        mat = (
            MaterialBuilder("COMPLEX")
            .density(500)
            .thermal_conductivity(0.15)
            .specific_heat(2.0)
            .add_pyrolysis_reaction(
                a=1e10, e=80000, heat_of_reaction=1000, product_species="VAPOR_1"
            )
            .add_pyrolysis_reaction(a=5e8, e=120000, heat_of_reaction=1500, residue_material="CHAR")
            .build()
        )

        assert mat.n_reactions == 2
        assert len(mat.a) == 2
        assert len(mat.e) == 2
        assert mat.spec_id == ["VAPOR_1", ""]  # None values replaced with empty strings
        assert mat.nu_spec == [1.0, 0.0]  # Default yields for species, 0.0 for no species
        assert mat.matl_id_products == ["", "CHAR"]  # None values replaced with empty strings
        assert mat.nu_matl == [0.0, 1.0]  # Default yields for residue materials, 0.0 for no residue

    def test_structured_pyrolysis_reaction(self):
        """Test material with structured PyrolysisReaction."""
        from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction

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
        # Should not have old-style array parameters when using structured reactions
        assert mat.a is None
        assert mat.e is None

    def test_mixed_reaction_apis_error(self):
        """Test error when mixing structured and legacy reaction APIs."""
        from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction

        reaction = PyrolysisReaction(
            a=1e10,
            e=150000,
            heat_of_reaction=500,
            products=[PyrolysisProduct(spec_id="GAS", nu_spec=1.0)],
        )

        with pytest.raises(ValueError, match=r"Cannot mix structured reactions.*legacy reactions"):
            (
                MaterialBuilder("WOOD")
                .density(500)
                .thermal_conductivity(0.13)
                .specific_heat(2.5)
                .add_reaction(reaction)
                .add_pyrolysis_reaction(
                    a=5e8, e=120000, heat_of_reaction=600, product_species="GAS2"
                )
                .build()
            )

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
        """Test predefined concrete material."""
        concrete = MaterialBuilder.concrete()

        assert concrete.id == "CONCRETE"
        assert concrete.density == 2400
        assert concrete.conductivity == 1.6
        assert concrete.specific_heat == 0.88

    def test_predefined_gypsum(self):
        """Test predefined gypsum material."""
        gypsum = MaterialBuilder.gypsum()

        assert gypsum.id == "GYPSUM"
        assert gypsum.density == 930
        assert gypsum.conductivity == 0.48

    def test_predefined_steel(self):
        """Test predefined steel material."""
        steel = MaterialBuilder.steel()

        assert steel.id == "STEEL"
        assert steel.density == 7850
        assert steel.conductivity == 45.8
        assert steel.emissivity == 0.7

    def test_predefined_aluminum(self):
        """Test predefined aluminum material."""
        aluminum = MaterialBuilder.aluminum()

        assert aluminum.id == "ALUMINUM"
        assert aluminum.density == 2700
        assert aluminum.conductivity == 237

    def test_predefined_brick(self):
        """Test predefined brick material."""
        brick = MaterialBuilder.brick()

        assert brick.id == "BRICK"
        assert brick.density == 1920

    def test_predefined_wood(self):
        """Test predefined wood material."""
        wood = MaterialBuilder.wood()

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

    def test_reference_temperature(self):
        """Test setting reference temperature."""
        mat = (
            MaterialBuilder("TEST")
            .density(1000)
            .thermal_conductivity(1.0)
            .specific_heat(1.0)
            .reference_temperature(20.0)
            .build()
        )

        assert mat.reference_temperature == 20.0

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
        assert matl.spec_id == "ETHANOL"

    def test_liquid_fuel_with_all_params(self):
        """Test liquid fuel with all optional parameters."""
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
        assert matl.heat_of_vaporization == 1100
        assert matl.absorption_coefficient == 140
