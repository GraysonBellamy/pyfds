"""Integration tests for species functionality."""

from pyfds import Simulation
from pyfds.builders.libraries import create_standard_air
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Combustion,
    Device,
    Mesh,
    Obstruction,
    Reaction,
    Species,
    Surface,
    Time,
)


class TestSpeciesIntegration:
    """Integration tests for species in simulations."""

    def test_simple_chemistry_simulation(self):
        """Test simulation with simple chemistry."""
        sim = Simulation(chid="test_simple_chem")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        sim.add(Reaction(fuel="PROPANE", soot_yield=0.01, co_yield=0.02))

        fds = sim.to_fds()
        assert "&REAC" in fds
        assert "FUEL='PROPANE'" in fds

    def test_custom_species_simulation(self):
        """Test simulation with custom species."""
        sim = Simulation(chid="test_custom_spec")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Define custom fuel
        sim.add(Species(id="MY_FUEL", formula="C3H8O3N4"))
        sim.add(Reaction(fuel="MY_FUEL", heat_of_combustion=46124))

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "FORMULA='C3H8O3N4'" in fds

    def test_lumped_species_simulation(self):
        """Test simulation with lumped species."""
        sim = Simulation(chid="test_lumped")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Define component species
        sim.add(Species(id="NITROGEN", lumped_component_only=True))
        sim.add(Species(id="OXYGEN", lumped_component_only=True))
        sim.add(
            Species(
                id="AIR",
                background=True,
                spec_id=["NITROGEN", "OXYGEN"],
                volume_fraction=[0.79, 0.21],
            )
        )

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "BACKGROUND=.TRUE." in fds

    def test_species_library_integration(self):
        """Test integration with species library."""
        sim = Simulation(chid="test_lib_integration")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Use species library to create air
        air_dict = create_standard_air(humidity=50.0)
        sim.add(Species(**air_dict))

        # Add a reaction
        sim.add(Reaction(fuel="PROPANE"))

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "&REAC" in fds

    def test_combustion_parameters_integration(self):
        """Test combustion parameters integration."""
        sim = Simulation(chid="test_combustion")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Add species and reaction
        sim.add(Species(id="PROPANE", formula="C3H8"))
        sim.add(Reaction(fuel="PROPANE"))

        # Set combustion parameters
        sim.add(Combustion(extinction_model="EXTINCTION 2", initial_unmixed_fraction=0.8))

        fds = sim.to_fds()
        assert "&SPEC" in fds
        assert "&REAC" in fds
        assert "&COMB" in fds
        assert "INITIAL_UNMIXED_FRACTION=0.8" in fds

    def test_background_species_validation(self):
        """Test background species validation in simulation."""
        sim = Simulation(chid="test_background")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        sim.add(Species(id="FUEL", formula="CH4"))
        sim.add(
            Species(id="AIR", background=True, spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])
        )
        sim.add(Reaction(fuel="FUEL"))

        # Try to add another background species (should produce warning, not error)
        sim.add(
            Species(id="AIR2", background=True, spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])
        )

        # Validate should catch the issue
        warnings = sim.validate()
        assert len(warnings) > 0
        assert any("Multiple background species" in w for w in warnings)

    def test_species_output_ordering(self):
        """Test that species appear in correct order in FDS output."""
        sim = Simulation(chid="test_ordering")
        sim.add(Time(t_end=10.0))

        # Add species in specific order
        sim.add(Species(id="FUEL", formula="CH4"))
        sim.add(
            Species(id="AIR", background=True, spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])
        )
        sim.add(Reaction(fuel="FUEL"))
        sim.add(Combustion(extinction_model="EXTINCTION 1"))

        fds = sim.to_fds()

        # Find positions of key sections
        comb_pos = fds.find("&COMB")
        spec_pos = fds.find("&SPEC")
        reac_pos = fds.find("&REAC")

        # Verify ordering: SPEC before REAC before COMB (FDS standard order)
        assert spec_pos < reac_pos < comb_pos

    def test_complete_fire_simulation(self):
        """Test a complete fire simulation with all species features."""
        sim = Simulation(chid="complete_fire", title="Complete Fire Test")
        sim.add(Time(t_end=300.0))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 4, 0, 4, 0, 2)))

        # Add air background using library
        air_dict = create_standard_air(humidity=40.0)
        sim.add(Species(**air_dict))

        # Add fuel species
        sim.add(Species(id="PROPANE", formula="C3H8", mw=44.0))

        # Add reaction
        sim.add(
            Reaction(
                fuel="PROPANE",
                heat_of_combustion=46300,
                soot_yield=0.015,
                co_yield=0.005,
                radiative_fraction=0.30,
            )
        )

        # Add combustion parameters
        sim.add(
            Combustion(
                extinction_model="EXTINCTION 2",
                initial_unmixed_fraction=0.9,
                zz_min_global=1e-10,
            )
        )

        # Add fire source
        sim.add(Surface(id="FIRE", hrrpua=500.0, color="RED"))
        sim.add(Obstruction(xb=Bounds3D.of(1.8, 2.2, 1.8, 2.2, 0, 0.1), surf_id="FIRE"))

        # Add monitoring
        sim.add(Device(id="TEMP_CENTER", quantity="TEMPERATURE", xyz=Point3D.of(2, 2, 1)))

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
