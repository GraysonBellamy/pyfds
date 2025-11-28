"""Integration tests for species functionality."""

from pyfds import Simulation
from pyfds.builders.libraries import create_standard_air
from pyfds.core.namelists import Species


class TestSpeciesIntegration:
    """Integration tests for species in simulations."""

    def test_simple_chemistry_simulation(self):
        """Test simulation with simple chemistry."""
        sim = Simulation(chid="test_simple_chem")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        sim.reaction(fuel="PROPANE", soot_yield=0.01, co_yield=0.02)

        fds = sim.to_fds()
        assert "&REAC" in fds
        assert "FUEL='PROPANE'" in fds

    def test_custom_species_simulation(self):
        """Test simulation with custom species."""
        sim = Simulation(chid="test_custom_spec")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Define custom fuel
        sim.species(id="MY_FUEL", formula="C3H8O3N4")
        sim.reaction(fuel="MY_FUEL", heat_of_combustion=46124)

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "FORMULA='C3H8O3N4'" in fds

    def test_lumped_species_simulation(self):
        """Test simulation with lumped species."""
        sim = Simulation(chid="test_lumped")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Define component species
        sim.species(id="NITROGEN", lumped_component_only=True)
        sim.species(id="OXYGEN", lumped_component_only=True)
        sim.species(
            id="AIR",
            background=True,
            spec_id=["NITROGEN", "OXYGEN"],
            volume_fraction=[0.79, 0.21],
        )

        fds = sim.to_fds()
        assert "BACKGROUND=.TRUE." in fds
        assert "LUMPED_COMPONENT_ONLY=.TRUE." in fds

    def test_species_library_integration(self):
        """Test integration with species library."""
        sim = Simulation(chid="test_lib_integration")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Use species library to create air
        air_dict = create_standard_air(humidity=50.0)
        air_species = Species(**air_dict)
        sim.add_species(air_species)

        # Add a reaction
        sim.reaction(fuel="PROPANE")

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "&REAC" in fds
        assert "BACKGROUND=.TRUE." in fds
        assert "FUEL='PROPANE'" in fds

    def test_combustion_parameters_integration(self):
        """Test combustion parameters integration."""
        sim = Simulation(chid="test_combustion")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Add species and reaction
        sim.species(id="PROPANE", formula="C3H8")
        sim.reaction(fuel="PROPANE")

        # Set combustion parameters
        sim.combustion(extinction_model="EXTINCTION 2", initial_unmixed_fraction=0.8)

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "&REAC" in fds
        assert "&COMB" in fds
        assert "EXTINCTION_MODEL='EXTINCTION 2'" in fds
        assert "INITIAL_UNMIXED_FRACTION=0.8" in fds

    def test_background_species_validation(self):
        """Test background species validation in simulation."""
        sim = Simulation(chid="test_background")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Add background species
        sim.species(id="AIR", background=True, spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])

        # Try to add another background species (should produce warning, not error)
        sim.species(id="AIR2", background=True, spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])

        # Validate should catch the issue
        warnings = sim.validate()
        assert len(warnings) > 0
        assert any("Multiple background species" in w for w in warnings)

    def test_species_output_ordering(self):
        """Test that species appear in correct order in FDS output."""
        sim = Simulation(chid="test_ordering")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))

        # Add species in specific order
        sim.species(id="FUEL", formula="CH4")
        sim.species(id="AIR", background=True, spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])
        sim.reaction(fuel="FUEL")
        sim.combustion(extinction_model="EXTINCTION 1")

        fds = sim.to_fds()

        # Find positions of key sections
        spec_pos = fds.find("! --- Species ---")
        reac_pos = fds.find("! --- Reactions ---")
        comb_pos = fds.find("! --- Combustion Parameters ---")

        # Verify ordering: SPEC before REAC before COMB
        assert spec_pos < reac_pos < comb_pos

    def test_complete_fire_simulation(self):
        """Test a complete fire simulation with all species features."""
        sim = Simulation(chid="complete_fire", title="Complete Fire Test")
        sim.time(t_end=300.0)
        sim.mesh(ijk=(20, 20, 10), xb=(0, 4, 0, 4, 0, 2))

        # Add air background using library
        air_dict = create_standard_air(humidity=40.0)
        sim.add_species(Species(**air_dict))

        # Add fuel species
        sim.species(id="PROPANE", formula="C3H8", mw=44.0)

        # Add reaction
        sim.reaction(
            fuel="PROPANE",
            heat_of_combustion=46300,
            soot_yield=0.015,
            co_yield=0.005,
            radiative_fraction=0.30,
        )

        # Add combustion parameters
        sim.combustion(
            extinction_model="EXTINCTION 2", initial_unmixed_fraction=0.9, zz_min_global=1e-10
        )

        # Add fire source
        sim.surface(id="FIRE", hrrpua=500.0, color="RED")
        sim.obstruction(xb=(1.8, 2.2, 1.8, 2.2, 0, 0.1), surf_id="FIRE")

        # Add monitoring
        sim.device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=(2, 2, 1))

        fds = sim.to_fds()

        # Verify all components are present
        assert "&HEAD" in fds
        assert "&TIME" in fds
        assert "&MESH" in fds
        assert "&SPEC" in fds
        assert "&REAC" in fds
        assert "&COMB" in fds
        assert "&SURF" in fds
        assert "&OBST" in fds
        assert "&DEVC" in fds

        # Verify species content
        assert "BACKGROUND=.TRUE." in fds
        assert "FUEL='PROPANE'" in fds
        assert "EXTINCTION_MODEL='EXTINCTION 2'" in fds
