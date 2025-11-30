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
            part_id=["SOOT", "SMOKE"],
            nu_part=[0.01, 0.005],
            a=[1e6, 2e6],  # Add reaction parameters to indicate 2 reactions
            e=[80000, 90000],
        )
        fds_str = mat.to_fds()
        assert "PART_ID='SOOT','SMOKE'" in fds_str
        assert "NU_PART=0.01,0.005" in fds_str


class TestMaterialMultiReaction:
    """Tests for multi-reaction pyrolysis."""

    def test_multi_reaction_2d_arrays(self):
        """Test 2D array syntax for products."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            a=[1e10, 5e8],
            e=[100000, 120000],
            heat_of_reaction=[1800, 500],
            spec_id=[["VAPOR"], ["CO2", "H2O"]],
            nu_spec=[[0.82], [0.1, 0.08]],
            matl_id=[["CHAR"], []],
            nu_matl=[[0.18], []],
        )
        fds = mat.to_fds()
        # Check species are flattened correctly
        assert "SPEC_ID='VAPOR','CO2','H2O'" in fds
        assert "NU_SPEC=0.82,0.1,0.08" in fds
        # Check material products - empty second reaction is omitted
        assert "MATL_ID_PRODUCTS='CHAR'" in fds
        assert "NU_MATL=0.18" in fds

    def test_yield_sum_validation(self):
        """Test that yields are validated."""
        with pytest.raises(ValidationError, match="yield"):
            Material(
                id="BAD",
                density=500.0,
                conductivity=0.13,
                specific_heat=2.5,
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
            spec_id=["CO", "CO2", "H2O"],
            nu_spec=[0.1, 0.3, 0.4],
            matl_id="CHAR",
            nu_matl=0.2,
        )
        fds = mat.to_fds()
        assert "SPEC_ID='CO','CO2','H2O'" in fds
        assert "NU_SPEC=0.1,0.3,0.4" in fds
        assert "MATL_ID_PRODUCTS='CHAR'" in fds
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


class TestMaterialReferenceTemperature:
    """Tests for REFERENCE_TEMPERATURE array support."""

    def test_reference_temperature_single(self):
        """Test single reference temperature (backward compatibility)."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reference_temperature=300.0,
        )
        assert mat.reference_temperature == 300.0
        fds = mat.to_fds()
        assert "REFERENCE_TEMPERATURE=300" in fds

    def test_reference_temperature_array(self):
        """Test multi-reaction with array of reference temperatures."""
        mat = Material(
            id="MULTI_PYRO",
            density=500,
            conductivity=0.1,
            specific_heat=1.5,
            reference_temperature=[300.0, 450.0],
            heat_of_reaction=[500.0, 800.0],
            nu_spec=[0.8, 0.2],
            spec_id=["FUEL1", "FUEL2"],
        )
        assert len(mat.reference_temperature) == 2
        fds = mat.to_fds()
        assert "REFERENCE_TEMPERATURE=300.0,450.0" in fds

    def test_reference_temperature_array_mismatch(self):
        """Test error when reference_temperature array length mismatches n_reactions."""
        with pytest.raises(ValidationError, match="must have 2 values"):
            Material(
                id="BAD_PYRO",
                density=500,
                conductivity=0.1,
                specific_heat=1.5,
                reference_temperature=[300.0],  # Should be 2 values
                heat_of_reaction=[500.0, 800.0],  # This indicates 2 reactions
            )

    def test_reference_temperature_single_reaction_list_error(self):
        """Test error when single reaction has list with wrong length."""
        with pytest.raises(ValidationError, match="must have length 1"):
            Material(
                id="BAD",
                density=500,
                conductivity=0.1,
                specific_heat=1.5,
                reference_temperature=[300.0, 400.0],  # Should be length 1 for single reaction
            )


class TestMaterialDelaminationRemoved:
    """Verify delamination parameters are not on Material."""

    def test_delamination_not_on_material(self):
        """Verify delamination parameters are not on Material."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        assert not hasattr(mat, "delamination_tmp")
        assert not hasattr(mat, "delamination_density")


class TestMaterialReacRateDelta:
    """Tests for REAC_RATE_DELTA parameter."""

    def test_reac_rate_delta_FdsField_exists(self):
        """Test that reac_rate_delta FdsField exists on Material."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        assert hasattr(mat, "reac_rate_delta")
        assert mat.reac_rate_delta is None

    def test_reac_rate_delta_assignment(self):
        """Test assigning reac_rate_delta value."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reac_rate_delta=0.1,
        )
        assert mat.reac_rate_delta == 0.1

    def test_reac_rate_delta_in_fds_output(self):
        """Test that reac_rate_delta appears in FDS output."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reac_rate_delta=0.05,
        )
        fds_str = mat.to_fds()
        assert "REAC_RATE_DELTA=0.05" in fds_str

    def test_reac_rate_delta_not_in_fds_when_none(self):
        """Test that reac_rate_delta is not included when None."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        fds_str = mat.to_fds()
        assert "REAC_RATE_DELTA" not in fds_str


class TestMaterialStructuredReactions:
    """Tests for structured reactions support."""

    def test_reactions_FdsField_exists(self):
        """Test that reactions FdsField exists on Material."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        assert hasattr(mat, "reactions")
        assert mat.reactions is None

    def test_structured_reactions_creation(self):
        """Test creating Material with structured reactions."""
        from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction

        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        reaction = PyrolysisReaction(heat_of_reaction=500.0, products=products, a=1e6, e=80000.0)
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[reaction],
        )
        assert len(mat.reactions) == 1
        assert mat.reactions[0].heat_of_reaction == 500.0

    def test_structured_reactions_fds_output(self):
        """Test FDS output for structured reactions."""
        from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction

        products = [
            PyrolysisProduct(spec_id="CO2", nu_spec=0.3),
            PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
        ]
        reaction = PyrolysisReaction(
            heat_of_reaction=500.0,
            products=products,
            a=1e6,
            e=80000.0,
        )
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[reaction],
        )
        fds_str = mat.to_fds()
        assert "&MATL ID='WOOD'" in fds_str
        assert "REACTIONS=" in fds_str
        assert "heat_of_reaction=500.0" in fds_str
        assert "a=1000000.0" in fds_str
        assert "e=80000.0" in fds_str
        assert "spec_id='CO2'" in fds_str
        assert "nu_spec=0.3" in fds_str
        assert "matl_id='CHAR'" in fds_str
        assert "nu_matl=0.2" in fds_str

    def test_validation_mixed_formats_not_allowed(self):
        """Test that mixing structured and array formats raises error."""
        from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction

        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        reaction = PyrolysisReaction(heat_of_reaction=500.0, products=products, a=1e6, e=80000.0)

        with pytest.raises(
            ValidationError, match=r"Cannot mix 'reactions' list with array parameters"
        ):
            Material(
                id="WOOD",
                density=500.0,
                conductivity=0.13,
                specific_heat=2.5,
                reactions=[reaction],
                a=[1e6],  # Array format
            )

    def test_multiple_structured_reactions(self):
        """Test multiple structured reactions."""
        from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction

        # First reaction: moisture evaporation
        moisture_products = [PyrolysisProduct(spec_id="H2O", nu_spec=0.1)]
        moisture_rxn = PyrolysisReaction(
            heat_of_reaction=2260.0,
            products=moisture_products,
            reference_temperature=100.0,
            pyrolysis_range=20.0,
        )

        # Second reaction: pyrolysis
        pyrolysis_products = [
            PyrolysisProduct(spec_id="CO2", nu_spec=0.3),
            PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
        ]
        pyrolysis_rxn = PyrolysisReaction(
            heat_of_reaction=500.0,
            products=pyrolysis_products,
            a=1e8,
            e=100000.0,
        )

        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[moisture_rxn, pyrolysis_rxn],
        )

        fds_str = mat.to_fds()
        assert "REACTIONS=" in fds_str
        assert "heat_of_reaction=2260.0" in fds_str
        assert "reference_temperature=100.0" in fds_str
        assert "heat_of_reaction=500.0" in fds_str
        assert "a=100000000.0" in fds_str
