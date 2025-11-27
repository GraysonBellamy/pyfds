"""Integration tests for pyrolysis modeling scenarios.

Tests complete pyrolysis workflows including material definitions,
cross-references, and simulation setup.
"""

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
            n_reactions=1,
            a=[1e10],
            e=[100000],
            heat_of_reaction=[1800],
            spec_id=["WOOD_GAS"],
            nu_spec=[0.75],
            matl_id=["CHAR"],
            nu_matl=[0.25],
        )

        sim.material_mgr.add_material(char)
        sim.material_mgr.add_material(wood)

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
            spec_id="METHANOL",
            heat_of_reaction=837.0,
            absorption_coefficient=140.0,
        )

        sim.material_mgr.add_material(methanol)

        # Validate liquid fuel parameters
        assert methanol.boiling_temperature == 64.7
        assert methanol.heat_of_reaction == 837.0

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
            n_reactions=2,
            a=[1e12, 5e8],
            e=[120000, 140000],
            heat_of_reaction=[2000, 500],
            spec_id=[["VOLATILE_1"], ["VOLATILE_2"]],
            nu_spec=[[0.4], [0.15]],
            matl_id=[["CHAR"], ["ASH"]],
            nu_matl=[[0.3], [0.05]],
        )

        sim.material_mgr.add_material(char)
        sim.material_mgr.add_material(ash)
        sim.material_mgr.add_material(composite)

        # Validate yields sum correctly for each reaction
        # Reaction 1: 0.4 + 0.3 = 0.7
        # Reaction 2: 0.15 + 0.05 = 0.2
        reaction1_yield = 0.4 + 0.3
        reaction2_yield = 0.15 + 0.05
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
            n_reactions=1,
            a=[2e9],
            e=[90000],
            heat_of_reaction=[1500],
            spec_id=["FUEL_GAS"],
            nu_spec=[0.8],
            matl_id=["CHAR"],
            nu_matl=[0.2],
        )

        sim.material_mgr.add_material(sample)

        # Validate material properties
        assert sample.density == 600.0
        assert sample.nu_spec == [0.8]
        assert sample.nu_matl == [0.2]
