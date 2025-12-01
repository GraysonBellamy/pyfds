"""Unit tests for MATL namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.models import PyrolysisProduct, PyrolysisReaction
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
        """Test particle products for single reaction using structured format."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[
                PyrolysisReaction(
                    products=[PyrolysisProduct(part_id="SOOT", nu_part=0.01)],
                )
            ],
        )
        assert mat.reactions[0].products[0].part_id == "SOOT"
        assert mat.reactions[0].products[0].nu_part == 0.01
        fds_str = mat.to_fds()
        assert "PART_ID='SOOT'" in fds_str
        assert "NU_PART=0.01" in fds_str

    def test_particle_products_multi_reaction(self):
        """Test particle products for multi-reaction material using structured format."""
        mat = Material(
            id="PLASTIC",
            density=1000.0,
            conductivity=0.2,
            specific_heat=2.0,
            reactions=[
                PyrolysisReaction(
                    a=1e6,
                    e=80000.0,
                    products=[PyrolysisProduct(part_id="SOOT", nu_part=0.01)],
                ),
                PyrolysisReaction(
                    a=2e6,
                    e=90000.0,
                    products=[PyrolysisProduct(part_id="SMOKE", nu_part=0.005)],
                ),
            ],
        )
        fds_str = mat.to_fds()
        assert "PART_ID='SOOT','SMOKE'" in fds_str
        assert "NU_PART=0.01,0.005" in fds_str


class TestMaterialMultiReaction:
    """Tests for multi-reaction pyrolysis using structured format."""

    def test_multi_reaction_with_products(self):
        """Test multi-reaction with gas and solid products."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[
                PyrolysisReaction(
                    a=1e10,
                    e=100000.0,
                    heat_of_reaction=1800.0,
                    products=[
                        PyrolysisProduct(spec_id="VAPOR", nu_spec=0.82),
                        PyrolysisProduct(matl_id="CHAR", nu_matl=0.18),
                    ],
                ),
                PyrolysisReaction(
                    a=5e8,
                    e=120000.0,
                    heat_of_reaction=500.0,
                    products=[
                        PyrolysisProduct(spec_id="CO2", nu_spec=0.1),
                        PyrolysisProduct(spec_id="H2O", nu_spec=0.08),
                    ],
                ),
            ],
        )
        fds = mat.to_fds()
        # Check species are output correctly
        assert "SPEC_ID='VAPOR','CO2','H2O'" in fds
        assert "NU_SPEC=0.82,0.1,0.08" in fds
        # Check material products
        assert "MATL_ID='CHAR'" in fds
        assert "NU_MATL=0.18" in fds
        # Check N_REACTIONS
        assert "N_REACTIONS=2" in fds

    def test_yield_sum_validation(self):
        """Test that yields are validated in PyrolysisReaction."""
        with pytest.raises(ValidationError, match="yield"):
            PyrolysisReaction(
                products=[
                    PyrolysisProduct(spec_id="GAS", nu_spec=0.6),
                    PyrolysisProduct(matl_id="CHAR", nu_matl=0.6),  # Sum = 1.2 > 1.0
                ],
            )

    def test_single_reaction_with_multiple_species(self):
        """Test single reaction with multiple gaseous products."""
        mat = Material(
            id="PLASTIC",
            density=1000.0,
            conductivity=0.2,
            specific_heat=2.0,
            reactions=[
                PyrolysisReaction(
                    products=[
                        PyrolysisProduct(spec_id="CO", nu_spec=0.1),
                        PyrolysisProduct(spec_id="CO2", nu_spec=0.3),
                        PyrolysisProduct(spec_id="H2O", nu_spec=0.4),
                        PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
                    ],
                )
            ],
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
            absorption_coefficient=1140,
            reactions=[
                PyrolysisReaction(
                    heat_of_reaction=837.0,
                    products=[PyrolysisProduct(spec_id="ETHANOL", nu_spec=1.0)],
                )
            ],
        )
        assert ethanol.boiling_temperature == 78.5
        fds = ethanol.to_fds()
        assert "BOILING_TEMPERATURE=78.5" in fds

    def test_liquid_fuel_with_heat_of_reaction(self):
        """Test liquid fuel with heat of reaction (vaporization).

        Note: FDS uses HEAT_OF_REACTION in the reaction, not a separate
        HEAT_OF_VAPORIZATION parameter on MATL.
        """
        methanol = Material(
            id="METHANOL_LIQUID",
            density=792,
            conductivity=0.2,
            specific_heat=2.51,
            boiling_temperature=64.7,
            absorption_coefficient=140,
            reactions=[
                PyrolysisReaction(
                    heat_of_reaction=1100,  # This is the heat of vaporization
                    products=[PyrolysisProduct(spec_id="METHANOL", nu_spec=1.0)],
                )
            ],
        )
        assert methanol.reactions[0].heat_of_reaction == 1100
        fds = methanol.to_fds()
        assert "HEAT_OF_REACTION=1100" in fds

    def test_liquid_fuel_requires_gas_product(self):
        """Test that liquid fuel requires a gas species product."""
        with pytest.raises(ValidationError, match="gas species product"):
            Material(
                id="BAD_LIQUID",
                density=792,
                conductivity=0.2,
                specific_heat=2.51,
                boiling_temperature=64.7,
            )


class TestMaterialReferenceTemperature:
    """Tests for REFERENCE_TEMPERATURE in structured reactions."""

    def test_reference_temperature_single(self):
        """Test single reference temperature in reaction."""
        mat = Material(
            id="WOOD",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[
                PyrolysisReaction(
                    reference_temperature=300.0,
                    products=[PyrolysisProduct(spec_id="GAS", nu_spec=0.8)],
                )
            ],
        )
        assert mat.reactions[0].reference_temperature == 300.0
        fds = mat.to_fds()
        assert "REFERENCE_TEMPERATURE=300" in fds

    def test_reference_temperature_array(self):
        """Test multi-reaction with different reference temperatures."""
        mat = Material(
            id="MULTI_PYRO",
            density=500,
            conductivity=0.1,
            specific_heat=1.5,
            reactions=[
                PyrolysisReaction(
                    reference_temperature=300.0,
                    heat_of_reaction=500.0,
                    products=[PyrolysisProduct(spec_id="FUEL1", nu_spec=0.8)],
                ),
                PyrolysisReaction(
                    reference_temperature=450.0,
                    heat_of_reaction=800.0,
                    products=[PyrolysisProduct(spec_id="FUEL2", nu_spec=0.2)],
                ),
            ],
        )
        assert len(mat.reactions) == 2
        fds = mat.to_fds()
        assert "REFERENCE_TEMPERATURE=300.0,450.0" in fds


class TestMaterialDelaminationRemoved:
    """Verify delamination parameters are not on Material."""

    def test_delamination_not_on_material(self):
        """Verify delamination parameters are not on Material."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        assert not hasattr(mat, "delamination_tmp")
        assert not hasattr(mat, "delamination_density")


class TestMaterialLegacyFieldsRemoved:
    """Verify legacy array-format fields have been removed."""

    def test_legacy_pyrolysis_fields_removed(self):
        """Verify legacy pyrolysis array fields are not on Material.

        These reaction-level fields should be on PyrolysisReaction, not Material.
        Material-level fields like reac_rate_delta ARE valid per FDS User Guide.
        """
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        # Reaction kinetics - should be on PyrolysisReaction, not Material
        assert not hasattr(mat, "a")
        assert not hasattr(mat, "e")
        assert not hasattr(mat, "n_s")
        assert not hasattr(mat, "n_t")
        assert not hasattr(mat, "n_o2")
        assert not hasattr(mat, "reference_temperature")
        assert not hasattr(mat, "reference_rate")
        assert not hasattr(mat, "pyrolysis_range")
        assert not hasattr(mat, "heating_rate")
        assert not hasattr(mat, "heat_of_reaction")
        # Note: reac_rate_delta IS on Material per FDS User Guide Table

    def test_legacy_product_fields_removed(self):
        """Verify legacy product array fields are not on Material."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        # These fields should no longer exist
        assert not hasattr(mat, "spec_id")
        assert not hasattr(mat, "nu_spec")
        assert not hasattr(mat, "matl_id_products")
        assert not hasattr(mat, "nu_matl")
        assert not hasattr(mat, "part_id")
        assert not hasattr(mat, "nu_part")


class TestMaterialStructuredReactions:
    """Tests for structured reactions support."""

    def test_reactions_field_exists(self):
        """Test that reactions field exists on Material."""
        mat = Material(id="WOOD", density=500.0, conductivity=0.13, specific_heat=2.5)
        assert hasattr(mat, "reactions")
        assert mat.reactions is None

    def test_structured_reactions_creation(self):
        """Test creating Material with structured reactions."""
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
        """Test FDS output for structured reactions converts to array format."""
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
        # Should output in FDS array format, not structured
        assert "A=1000000.0" in fds_str
        assert "E=80000.0" in fds_str
        assert "HEAT_OF_REACTION=500.0" in fds_str
        assert "SPEC_ID='CO2'" in fds_str
        assert "NU_SPEC=0.3" in fds_str
        assert "MATL_ID='CHAR'" in fds_str
        assert "NU_MATL=0.2" in fds_str
        # Should NOT output raw reactions list
        assert "REACTIONS=" not in fds_str

    def test_multiple_structured_reactions(self):
        """Test multiple structured reactions."""
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
        assert "N_REACTIONS=2" in fds_str
        assert "HEAT_OF_REACTION=2260.0,500.0" in fds_str
        assert "REFERENCE_TEMPERATURE=100.0" in fds_str
        assert "PYROLYSIS_RANGE=20.0" in fds_str
        assert "A=100000000.0" in fds_str

    def test_n_reactions_property(self):
        """Test n_reactions property returns correct count."""
        mat = Material(id="INERT", density=1000, conductivity=1.0, specific_heat=1.0)
        assert mat.n_reactions == 0

        mat_with_rxn = Material(
            id="PYRO",
            density=500.0,
            conductivity=0.13,
            specific_heat=2.5,
            reactions=[
                PyrolysisReaction(
                    products=[PyrolysisProduct(spec_id="GAS", nu_spec=0.8)],
                ),
                PyrolysisReaction(
                    products=[PyrolysisProduct(spec_id="GAS2", nu_spec=0.2)],
                ),
            ],
        )
        assert mat_with_rxn.n_reactions == 2


class TestMaterialShrinkSwell:
    """Tests for shrinking/swelling control."""

    def test_shrink_swell_defaults(self):
        """Test default shrink/swell values."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        assert mat.allow_shrinking is True
        assert mat.allow_swelling is True

    def test_disable_shrinking(self):
        """Test disabling shrinking."""
        mat = Material(
            id="RIGID",
            density=1000,
            conductivity=1.0,
            specific_heat=1.0,
            allow_shrinking=False,
        )
        assert mat.allow_shrinking is False
        fds = mat.to_fds()
        assert "ALLOW_SHRINKING=.FALSE." in fds

    def test_disable_swelling(self):
        """Test disabling swelling."""
        mat = Material(
            id="DENSE",
            density=2000,
            conductivity=1.5,
            specific_heat=0.9,
            allow_swelling=False,
        )
        assert mat.allow_swelling is False
        fds = mat.to_fds()
        assert "ALLOW_SWELLING=.FALSE." in fds


class TestMaterialEnergyConservation:
    """Tests for energy conservation parameters."""

    def test_adjust_h_default(self):
        """Test ADJUST_H default value."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        assert mat.adjust_h is True

    def test_disable_adjust_h(self):
        """Test disabling enthalpy adjustment."""
        mat = Material(
            id="TEST",
            density=1000,
            conductivity=1.0,
            specific_heat=1.0,
            adjust_h=False,
        )
        assert mat.adjust_h is False
        fds = mat.to_fds()
        assert "ADJUST_H=.FALSE." in fds

    def test_reference_enthalpy(self):
        """Test setting reference enthalpy."""
        mat = Material(
            id="CUSTOM",
            density=1500,
            conductivity=0.5,
            specific_heat=1.2,
            reference_enthalpy=100.0,
            reference_enthalpy_temperature=300.0,
        )
        assert mat.reference_enthalpy == 100.0
        assert mat.reference_enthalpy_temperature == 300.0
        fds = mat.to_fds()
        assert "REFERENCE_ENTHALPY=100" in fds
        assert "REFERENCE_ENTHALPY_TEMPERATURE=300" in fds


class TestMaterialReactionControl:
    """Tests for reaction control parameters."""

    def test_reac_rate_delta_default(self):
        """Test REAC_RATE_DELTA default value."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        assert mat.reac_rate_delta == 0.05

    def test_reac_rate_delta_custom(self):
        """Test setting custom REAC_RATE_DELTA."""
        mat = Material(
            id="PYRO",
            density=500,
            conductivity=0.13,
            specific_heat=2.5,
            reac_rate_delta=0.1,
        )
        assert mat.reac_rate_delta == 0.1
        fds = mat.to_fds()
        assert "REAC_RATE_DELTA=0.1" in fds


class TestMaterialOxidationModel:
    """Tests for oxidation model parameters."""

    def test_surface_oxidation_default(self):
        """Test SURFACE_OXIDATION_MODEL default value."""
        mat = Material(id="TEST", density=1000, conductivity=1.0, specific_heat=1.0)
        assert mat.surface_oxidation_model is False

    def test_enable_surface_oxidation(self):
        """Test enabling surface oxidation model."""
        mat = Material(
            id="VEGETATION",
            density=400,
            conductivity=0.1,
            specific_heat=1.8,
            surface_oxidation_model=True,
        )
        assert mat.surface_oxidation_model is True
        fds = mat.to_fds()
        assert "SURFACE_OXIDATION_MODEL=.TRUE." in fds

    def test_x_o2_pyro(self):
        """Test X_O2_PYRO parameter."""
        mat = Material(
            id="POROUS",
            density=600,
            conductivity=0.15,
            specific_heat=2.0,
            x_o2_pyro=True,
        )
        assert mat.x_o2_pyro is True
        fds = mat.to_fds()
        assert "X_O2_PYRO=.TRUE." in fds
