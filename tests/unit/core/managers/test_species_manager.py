"""Tests for SpeciesManager."""

from pyfds.core.managers.species import SpeciesManager
from pyfds.core.namelists import Species
from pyfds.core.namelists.comb import Combustion


class TestSpeciesManager:
    """Test SpeciesManager functionality."""

    def test_initialization(self):
        """Test SpeciesManager initialization."""
        mgr = SpeciesManager()
        assert mgr.species == []
        assert mgr.combustion is None

    def test_add_species(self):
        """Test adding species."""
        mgr = SpeciesManager()
        spec = Species(id="OXYGEN")
        mgr.add_species(spec)
        assert len(mgr.species) == 1
        assert mgr.species[0].id == "OXYGEN"

    def test_get_species(self):
        """Test getting species by ID."""
        mgr = SpeciesManager()
        spec1 = Species(id="OXYGEN")
        spec2 = Species(fuel="PROPANE")
        mgr.add_species(spec1)
        mgr.add_species(spec2)

        # Get by ID
        found = mgr.get_species("OXYGEN")
        assert found is not None
        assert found.id == "OXYGEN"

        # Get by fuel name
        found = mgr.get_species("PROPANE")
        assert found is not None
        assert found.fuel == "PROPANE"

        # Get non-existent
        found = mgr.get_species("NONEXISTENT")
        assert found is None

    def test_get_background_species(self):
        """Test getting background species."""
        mgr = SpeciesManager()
        spec1 = Species(id="OXYGEN")
        spec2 = Species(id="AIR", background=True)
        mgr.add_species(spec1)
        mgr.add_species(spec2)

        bg = mgr.get_background_species()
        assert bg is not None
        assert bg.id == "AIR"

    def test_set_combustion(self):
        """Test setting combustion parameters."""
        mgr = SpeciesManager()

        # Set with object
        comb = Combustion(extinction_model="EXTINCTION 2")
        mgr.set_combustion(comb)
        assert mgr.combustion is not None
        assert mgr.combustion.extinction_model == "EXTINCTION 2"

        # Set with kwargs
        mgr.set_combustion(initial_unmixed_fraction=0.5)
        assert mgr.combustion.initial_unmixed_fraction == 0.5

    def test_validate_no_warnings(self):
        """Test validation with no issues."""
        mgr = SpeciesManager()
        spec = Species(id="OXYGEN")
        mgr.add_species(spec)
        warnings = mgr.validate()
        assert warnings == []

    def test_validate_duplicate_ids(self):
        """Test validation detects duplicate species IDs."""
        mgr = SpeciesManager()
        spec1 = Species(id="OXYGEN")
        spec2 = Species(id="OXYGEN")  # Duplicate
        mgr.add_species(spec1)
        mgr.add_species(spec2)
        warnings = mgr.validate()
        assert len(warnings) == 1
        assert "Duplicate species IDs" in warnings[0]

    def test_validate_multiple_background(self):
        """Test validation detects multiple background species."""
        mgr = SpeciesManager()
        spec1 = Species(id="AIR1", background=True)
        spec2 = Species(id="AIR2", background=True)
        mgr.add_species(spec1)
        mgr.add_species(spec2)
        warnings = mgr.validate()
        assert len(warnings) == 1
        assert "Multiple background species" in warnings[0]

    def test_validate_lumped_references(self):
        """Test validation of lumped species references."""
        mgr = SpeciesManager()
        # Add component species
        comp1 = Species(id="N2", lumped_component_only=True)
        comp2 = Species(id="O2", lumped_component_only=True)
        mgr.add_species(comp1)
        mgr.add_species(comp2)

        # Add lumped species that references them
        lumped = Species(id="AIR", spec_id=["N2", "O2"], mass_fraction=[0.79, 0.21])
        mgr.add_species(lumped)

        warnings = mgr.validate()
        assert warnings == []  # Should be valid

        # Add lumped species with invalid reference
        invalid_lumped = Species(id="INVALID", spec_id=["N2", "UNKNOWN"], mass_fraction=[0.5, 0.5])
        mgr.add_species(invalid_lumped)

        warnings = mgr.validate()
        assert len(warnings) == 1
        assert "references unknown component" in warnings[0]
