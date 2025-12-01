"""Integration tests for pyrolysis modeling scenarios.

Tests complete pyrolysis workflows including material definitions,
cross-references, and simulation setup.
"""

from pyfds.core.models import PyrolysisProduct, PyrolysisReaction
from pyfds.core.namelists import Material
from pyfds.core.simulation import Simulation


class TestPyrolysisScenarios:
    """Integration tests for pyrolysis modeling."""

    def test_charring_wood_simulation(self):
        """Test complete charring wood setup."""
        sim = Simulation(chid="char_wood", title="Charring Wood")

        # Define char residue
        char = Material(id="CHAR", density=150.0, conductivity=0.1, specific_heat=1.0)

        # Define wood with charring reaction
        wood = Material(
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
                        PyrolysisProduct(spec_id="WOOD_GAS", nu_spec=0.75),
                        PyrolysisProduct(matl_id="CHAR", nu_matl=0.25),
                    ],
                )
            ],
        )

        sim.add(char)
        sim.add(wood)

        # Validate cross-references
        warnings = sim.validate()
        assert not any("CHAR" in w for w in warnings)

    def test_liquid_pool_fire(self):
        """Test liquid pool fire setup."""
        sim = Simulation(chid="pool_fire", title="Methanol Pool")

        methanol = Material(
            id="METHANOL_LIQUID",
            density=792.0,
            conductivity=0.2,
            specific_heat=2.51,
            boiling_temperature=64.7,
            absorption_coefficient=140.0,
            reactions=[
                PyrolysisReaction(
                    heat_of_reaction=837.0,
                    products=[PyrolysisProduct(spec_id="METHANOL", nu_spec=1.0)],
                )
            ],
        )

        sim.add(methanol)

        # Validate liquid fuel parameters
        assert methanol.boiling_temperature == 64.7
        assert methanol.reactions[0].heat_of_reaction == 837.0

    def test_multi_reaction_composite(self):
        """Test composite material with multiple reactions."""
        sim = Simulation(chid="composite", title="Multi-Reaction Composite")

        # Define residue materials
        char = Material(id="CHAR", density=200.0, conductivity=0.15, specific_heat=1.2)

        ash = Material(id="ASH", density=100.0, conductivity=0.05, specific_heat=0.8)

        # Define composite with two reactions
        composite = Material(
            id="COMPOSITE",
            density=800.0,
            conductivity=0.2,
            specific_heat=2.0,
            reactions=[
                PyrolysisReaction(
                    a=1e12,
                    e=120000.0,
                    heat_of_reaction=2000.0,
                    products=[
                        PyrolysisProduct(spec_id="VOLATILE_1", nu_spec=0.4),
                        PyrolysisProduct(matl_id="CHAR", nu_matl=0.3),
                    ],
                ),
                PyrolysisReaction(
                    a=5e8,
                    e=140000.0,
                    heat_of_reaction=500.0,
                    products=[
                        PyrolysisProduct(spec_id="VOLATILE_2", nu_spec=0.15),
                        PyrolysisProduct(matl_id="ASH", nu_matl=0.05),
                    ],
                ),
            ],
        )

        sim.add(char)
        sim.add(ash)
        sim.add(composite)

        # Validate yields sum correctly for each reaction
        # Reaction 1: 0.4 + 0.3 = 0.7
        # Reaction 2: 0.15 + 0.05 = 0.2
        rxn1_products = composite.reactions[0].products
        rxn2_products = composite.reactions[1].products
        reaction1_yield = sum(p.nu_spec or 0 for p in rxn1_products) + sum(
            p.nu_matl or 0 for p in rxn1_products
        )
        reaction2_yield = sum(p.nu_spec or 0 for p in rxn2_products) + sum(
            p.nu_matl or 0 for p in rxn2_products
        )
        assert abs(reaction1_yield - 0.7) < 1e-6
        assert abs(reaction2_yield - 0.2) < 1e-6

        # Validate cross-references
        warnings = sim.validate()
        assert not any("CHAR" in w or "ASH" in w for w in warnings)

    def test_cone_calorimeter_setup(self):
        """Test cone calorimeter simulation setup."""
        sim = Simulation(chid="cone_test", title="Cone Calorimeter")

        # Define test material
        sample = Material(
            id="SAMPLE",
            density=600.0,
            conductivity=0.18,
            specific_heat=2.3,
            reactions=[
                PyrolysisReaction(
                    a=2e9,
                    e=90000.0,
                    heat_of_reaction=1500.0,
                    products=[
                        PyrolysisProduct(spec_id="FUEL_GAS", nu_spec=0.8),
                        PyrolysisProduct(matl_id="CHAR", nu_matl=0.2),
                    ],
                )
            ],
        )

        sim.add(sample)

        # Validate material properties
        assert sample.density == 600.0
        # Verify reaction structure
        rxn = sample.reactions[0]
        assert rxn.products[0].nu_spec == 0.8
        assert rxn.products[1].nu_matl == 0.2
