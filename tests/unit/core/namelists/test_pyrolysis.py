"""Unit tests for pyrolysis reaction classes."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists.pyrolysis import PyrolysisProduct, PyrolysisReaction


class TestPyrolysisProduct:
    """Tests for PyrolysisProduct class."""

    def test_gas_product_creation(self):
        """Test creation of gas product."""
        product = PyrolysisProduct(spec_id="CO2", nu_spec=0.3, heat_of_combustion=1000.0)
        assert product.spec_id == "CO2"
        assert product.nu_spec == 0.3
        assert product.heat_of_combustion == 1000.0
        assert product.matl_id is None
        assert product.nu_matl is None
        assert product.part_id is None
        assert product.nu_part is None

    def test_solid_product_creation(self):
        """Test creation of solid residue product."""
        product = PyrolysisProduct(matl_id="CHAR", nu_matl=0.2)
        assert product.matl_id == "CHAR"
        assert product.nu_matl == 0.2
        assert product.spec_id is None
        assert product.nu_spec is None
        assert product.part_id is None
        assert product.nu_part is None

    def test_particle_product_creation(self):
        """Test creation of particle product."""
        product = PyrolysisProduct(part_id="SOOT", nu_part=0.01)
        assert product.part_id == "SOOT"
        assert product.nu_part == 0.01
        assert product.spec_id is None
        assert product.nu_spec is None
        assert product.matl_id is None
        assert product.nu_matl is None

    def test_validation_no_product_type(self):
        """Test validation fails when no product type is specified."""
        with pytest.raises(ValidationError, match="Product must specify"):
            PyrolysisProduct()

    def test_validation_gas_without_yield(self):
        """Test validation fails when gas product lacks yield."""
        with pytest.raises(ValidationError, match="spec_id requires nu_spec"):
            PyrolysisProduct(spec_id="CO2")

    def test_validation_solid_without_yield(self):
        """Test validation fails when solid product lacks yield."""
        with pytest.raises(ValidationError, match="matl_id requires nu_matl"):
            PyrolysisProduct(matl_id="CHAR")

    def test_validation_particle_without_yield(self):
        """Test validation fails when particle product lacks yield."""
        with pytest.raises(ValidationError, match="part_id requires nu_part"):
            PyrolysisProduct(part_id="SOOT")

    def test_validation_invalid_yield_range(self):
        """Test validation of yield ranges."""
        with pytest.raises(ValidationError):
            PyrolysisProduct(spec_id="CO2", nu_spec=-0.1)

        with pytest.raises(ValidationError):
            PyrolysisProduct(spec_id="CO2", nu_spec=1.5)


class TestPyrolysisReaction:
    """Tests for PyrolysisReaction class."""

    def test_basic_reaction_creation(self):
        """Test basic reaction creation with Arrhenius kinetics."""
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
        assert reaction.heat_of_reaction == 500.0
        assert len(reaction.products) == 2
        assert reaction.a == 1e6
        assert reaction.e == 80000.0
        assert reaction.reference_temperature is None
        assert reaction.reference_rate is None
        assert reaction.n_s == 1.0
        assert reaction.n_t == 0.0
        assert reaction.n_o2 == 0.0

    def test_default_heat_of_reaction(self):
        """Test that heat_of_reaction defaults to 0.0."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        reaction = PyrolysisReaction(products=products, a=1e6, e=80000.0)
        assert reaction.heat_of_reaction == 0.0

    def test_simplified_kinetics_with_range(self):
        """Test reaction creation with simplified kinetics using PYROLYSIS_RANGE."""
        products = [PyrolysisProduct(spec_id="H2O", nu_spec=0.1)]
        reaction = PyrolysisReaction(
            heat_of_reaction=2260.0,
            products=products,
            reference_temperature=100.0,
            pyrolysis_range=50.0,
        )
        assert reaction.reference_temperature == 100.0
        assert reaction.pyrolysis_range == 50.0
        assert reaction.reference_rate is None
        assert reaction.a is None
        assert reaction.e is None

    def test_simplified_kinetics_with_rate(self):
        """Test reaction creation with simplified kinetics using REFERENCE_RATE."""
        products = [PyrolysisProduct(spec_id="FUEL", nu_spec=0.8)]
        reaction = PyrolysisReaction(
            heat_of_reaction=1800.0,
            products=products,
            reference_temperature=300.0,
            reference_rate=0.002,
            heating_rate=5.0,
        )
        assert reaction.reference_temperature == 300.0
        assert reaction.reference_rate == 0.002
        assert reaction.pyrolysis_range is None
        assert reaction.heating_rate == 5.0
        assert reaction.a is None
        assert reaction.e is None

    def test_simplified_kinetics_temperature_only(self):
        """Test reaction with only REFERENCE_TEMPERATURE (FDS auto-derives A and E)."""
        products = [PyrolysisProduct(spec_id="FUEL", nu_spec=1.0)]
        reaction = PyrolysisReaction(
            products=products,
            reference_temperature=350.0,
        )
        assert reaction.reference_temperature == 350.0
        assert reaction.reference_rate is None
        assert reaction.pyrolysis_range is None
        assert reaction.a is None
        assert reaction.e is None

    def test_validation_no_products(self):
        """Test validation fails when no products specified."""
        with pytest.raises(ValidationError):
            PyrolysisReaction(heat_of_reaction=500.0, products=[])

    def test_validation_arrhenius_incomplete(self):
        """Test validation fails when Arrhenius parameters are incomplete."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        with pytest.raises(ValidationError, match="Arrhenius kinetics requires both A and E"):
            PyrolysisReaction(
                heat_of_reaction=500.0,
                products=products,
                a=1e6,  # Missing E
            )

    def test_validation_conflicting_kinetics(self):
        """Test validation fails when both Arrhenius and simplified kinetics specified."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        with pytest.raises(ValidationError, match="Cannot specify both Arrhenius"):
            PyrolysisReaction(
                heat_of_reaction=500.0,
                products=products,
                a=1e6,
                e=80000.0,
                reference_temperature=100.0,
            )

    def test_validation_reference_rate_and_pyrolysis_range(self):
        """Test validation fails when both REFERENCE_RATE and PYROLYSIS_RANGE specified."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        with pytest.raises(
            ValidationError, match="Cannot specify both REFERENCE_RATE and PYROLYSIS_RANGE"
        ):
            PyrolysisReaction(
                products=products,
                reference_temperature=300.0,
                reference_rate=0.002,
                pyrolysis_range=80.0,
            )

    def test_validation_reference_rate_without_temperature(self):
        """Test validation fails when REFERENCE_RATE specified without REFERENCE_TEMPERATURE."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        with pytest.raises(
            ValidationError,
            match="REFERENCE_RATE and PYROLYSIS_RANGE require REFERENCE_TEMPERATURE",
        ):
            PyrolysisReaction(
                products=products,
                reference_rate=0.002,
            )

    def test_validation_pyrolysis_range_without_temperature(self):
        """Test validation fails when PYROLYSIS_RANGE specified without REFERENCE_TEMPERATURE."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        with pytest.raises(
            ValidationError,
            match="REFERENCE_RATE and PYROLYSIS_RANGE require REFERENCE_TEMPERATURE",
        ):
            PyrolysisReaction(
                products=products,
                pyrolysis_range=80.0,
            )

    def test_validation_total_yield_exceeds_one(self):
        """Test validation fails when total product yield exceeds 1.0."""
        products = [
            PyrolysisProduct(spec_id="CO2", nu_spec=0.6),
            PyrolysisProduct(matl_id="CHAR", nu_matl=0.6),
        ]
        with pytest.raises(ValidationError, match=r"Total product yield.*exceeds 1.0"):
            PyrolysisReaction(heat_of_reaction=500.0, products=products)

    def test_get_gas_products(self):
        """Test filtering gas products."""
        products = [
            PyrolysisProduct(spec_id="CO2", nu_spec=0.3),
            PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
            PyrolysisProduct(spec_id="CO", nu_spec=0.1),
        ]
        reaction = PyrolysisReaction(heat_of_reaction=500.0, products=products)
        gas_products = reaction.get_gas_products()
        assert len(gas_products) == 2
        assert gas_products[0].spec_id == "CO2"
        assert gas_products[1].spec_id == "CO"

    def test_get_solid_products(self):
        """Test filtering solid products."""
        products = [
            PyrolysisProduct(spec_id="CO2", nu_spec=0.3),
            PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
            PyrolysisProduct(matl_id="ASH", nu_matl=0.1),
        ]
        reaction = PyrolysisReaction(heat_of_reaction=500.0, products=products)
        solid_products = reaction.get_solid_products()
        assert len(solid_products) == 2
        assert solid_products[0].matl_id == "CHAR"
        assert solid_products[1].matl_id == "ASH"

    def test_get_particle_products(self):
        """Test filtering particle products."""
        products = [
            PyrolysisProduct(spec_id="CO2", nu_spec=0.3),
            PyrolysisProduct(part_id="SOOT", nu_part=0.01),
            PyrolysisProduct(part_id="SMOKE", nu_part=0.005),
        ]
        reaction = PyrolysisReaction(heat_of_reaction=500.0, products=products)
        particle_products = reaction.get_particle_products()
        assert len(particle_products) == 2
        assert particle_products[0].part_id == "SOOT"
        assert particle_products[1].part_id == "SMOKE"

    def test_custom_parameters(self):
        """Test custom reaction order and advanced parameters."""
        products = [PyrolysisProduct(spec_id="CO2", nu_spec=0.3)]
        reaction = PyrolysisReaction(
            heat_of_reaction=500.0,
            products=products,
            a=1e6,
            e=80000.0,
            n_s=0.5,
            n_t=2.0,
            n_o2=1.0,
            gas_diffusion_depth=0.001,
            max_reaction_rate=100.0,
        )
        assert reaction.n_s == 0.5
        assert reaction.n_t == 2.0
        assert reaction.n_o2 == 1.0
        assert reaction.gas_diffusion_depth == 0.001
        assert reaction.max_reaction_rate == 100.0
