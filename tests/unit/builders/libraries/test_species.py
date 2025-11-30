"""Tests for the species library."""

import pytest

from pyfds.builders.libraries.species import (
    PREDEFINED_SPECIES,
    create_standard_air,
    get_species_formula,
    get_species_info,
    get_species_molecular_weight,
    is_predefined,
    list_predefined_species,
)


class TestPredefinedSpecies:
    """Test the predefined species database."""

    def test_predefined_species_count(self):
        """Test that we have a reasonable number of predefined species."""
        assert len(PREDEFINED_SPECIES) >= 30  # Should have at least 30 species

    def test_common_species_present(self):
        """Test that essential species are present."""
        essential_species = [
            "NITROGEN",
            "OXYGEN",
            "CARBON_DIOXIDE",
            "WATER_VAPOR",
            "METHANE",
            "PROPANE",
            "HYDROGEN",
            "SOOT",
        ]
        for species in essential_species:
            assert species in PREDEFINED_SPECIES

    def test_species_have_required_FdsFields(self):
        """Test that all species have required FdsFields."""
        for _name, info in PREDEFINED_SPECIES.items():
            assert "formula" in info
            assert "mw" in info
            assert "description" in info
            assert isinstance(info["mw"], (int, float))
            assert info["mw"] > 0

    def test_soot_is_aerosol(self):
        """Test that soot is marked as aerosol."""
        assert PREDEFINED_SPECIES["SOOT"]["aerosol"] is True


class TestListPredefinedSpecies:
    """Test the list_predefined_species function."""

    def test_returns_list(self):
        """Test that function returns a list."""
        species = list_predefined_species()
        assert isinstance(species, list)

    def test_list_is_sorted(self):
        """Test that the list is sorted alphabetically."""
        species = list_predefined_species()
        assert species == sorted(species)

    def test_all_species_in_list(self):
        """Test that all predefined species are in the list."""
        species_list = list_predefined_species()
        for name in PREDEFINED_SPECIES:
            assert name in species_list


class TestGetSpeciesInfo:
    """Test the get_species_info function."""

    def test_get_known_species(self):
        """Test getting info for known species."""
        info = get_species_info("PROPANE")
        assert info["formula"] == "C3H8"
        assert abs(info["mw"] - 44.0956) < 0.001
        assert "description" in info

    def test_case_insensitive(self):
        """Test that function is case insensitive."""
        info1 = get_species_info("propane")
        info2 = get_species_info("PROPANE")
        assert info1 == info2

    def test_unknown_species_raises_error(self):
        """Test that unknown species raise ValueError."""
        with pytest.raises(ValueError, match="Unknown species"):
            get_species_info("UNKNOWN_SPECIES")

    def test_error_message_lists_available_species(self):
        """Test that error message includes available species."""
        with pytest.raises(ValueError) as exc_info:
            get_species_info("NONEXISTENT")
        error_msg = str(exc_info.value)
        assert "Available species:" in error_msg
        # Should contain some known species
        assert "PROPANE" in error_msg


class TestIsPredefined:
    """Test the is_predefined function."""

    def test_known_species_returns_true(self):
        """Test that known species return True."""
        assert is_predefined("METHANE") is True
        assert is_predefined("methane") is True  # Case insensitive

    def test_unknown_species_returns_false(self):
        """Test that unknown species return False."""
        assert is_predefined("CUSTOM_FUEL") is False
        assert is_predefined("unknown") is False


class TestCreateStandardAir:
    """Test the create_standard_air function."""

    def test_default_humidity(self):
        """Test creating air with default humidity."""
        air = create_standard_air()
        assert air["id"] == "AIR"
        assert air["background"] is True
        assert "spec_id" in air
        assert "mass_fraction" in air
        assert len(air["spec_id"]) == len(air["mass_fraction"])

    def test_custom_humidity(self):
        """Test creating air with custom humidity."""
        air = create_standard_air(humidity=50.0)
        assert air["id"] == "AIR"
        assert "humidity: 50.0%" in air["description"]

    def test_humidity_validation(self):
        """Test humidity validation."""
        with pytest.raises(ValueError, match="Humidity must be between"):
            create_standard_air(humidity=-10)

        with pytest.raises(ValueError, match="Humidity must be between"):
            create_standard_air(humidity=150)

    def test_mass_fractions_sum_to_one(self):
        """Test that mass fractions sum to approximately 1.0."""
        air = create_standard_air()
        total_mass = sum(air["mass_fraction"])
        assert abs(total_mass - 1.0) < 1e-10

    def test_contains_expected_components(self):
        """Test that air contains expected components."""
        air = create_standard_air()
        expected_components = ["NITROGEN", "OXYGEN", "ARGON", "CARBON_DIOXIDE", "WATER_VAPOR"]
        for component in expected_components:
            assert component in air["spec_id"]

    def test_humidity_affects_composition(self):
        """Test that humidity affects the composition."""
        dry_air = create_standard_air(humidity=0)
        humid_air = create_standard_air(humidity=80)

        # Should have different compositions
        assert dry_air["mass_fraction"] != humid_air["mass_fraction"]

        # Humid air should have more water vapor
        dry_water_idx = dry_air["spec_id"].index("WATER_VAPOR")
        humid_water_idx = humid_air["spec_id"].index("WATER_VAPOR")
        assert humid_air["mass_fraction"][humid_water_idx] > dry_air["mass_fraction"][dry_water_idx]


class TestGetSpeciesMolecularWeight:
    """Test the get_species_molecular_weight function."""

    def test_known_species(self):
        """Test getting molecular weight for known species."""
        mw = get_species_molecular_weight("WATER_VAPOR")
        assert abs(mw - 18.0153) < 0.001

    def test_case_insensitive(self):
        """Test case insensitivity."""
        mw1 = get_species_molecular_weight("oxygen")
        mw2 = get_species_molecular_weight("OXYGEN")
        assert mw1 == mw2

    def test_unknown_species_raises_error(self):
        """Test that unknown species raise error."""
        with pytest.raises(ValueError):
            get_species_molecular_weight("UNKNOWN")


class TestGetSpeciesFormula:
    """Test the get_species_formula function."""

    def test_known_species(self):
        """Test getting formula for known species."""
        formula = get_species_formula("PROPANE")
        assert formula == "C3H8"

    def test_case_insensitive(self):
        """Test case insensitivity."""
        formula1 = get_species_formula("methane")
        formula2 = get_species_formula("METHANE")
        assert formula1 == formula2

    def test_unknown_species_raises_error(self):
        """Test that unknown species raise error."""
        with pytest.raises(ValueError):
            get_species_formula("UNKNOWN")


class TestSpeciesDataAccuracy:
    """Test accuracy of species data."""

    def test_oxygen_properties(self):
        """Test oxygen properties."""
        info = get_species_info("OXYGEN")
        assert info["formula"] == "O2"
        assert abs(info["mw"] - 31.9988) < 0.001

    def test_nitrogen_properties(self):
        """Test nitrogen properties."""
        info = get_species_info("NITROGEN")
        assert info["formula"] == "N2"
        assert abs(info["mw"] - 28.0134) < 0.001

    def test_water_vapor_properties(self):
        """Test water vapor properties."""
        info = get_species_info("WATER_VAPOR")
        assert info["formula"] == "H2O"
        assert abs(info["mw"] - 18.0153) < 0.001

    def test_soot_properties(self):
        """Test soot properties."""
        info = get_species_info("SOOT")
        assert info["formula"] == "C"
        assert abs(info["mw"] - 12.011) < 0.001
        assert info["aerosol"] is True
