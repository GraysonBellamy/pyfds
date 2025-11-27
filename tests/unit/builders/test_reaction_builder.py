"""Tests for ReactionBuilder class."""

import pytest

from pyfds.builders import ReactionBuilder


class TestReactionBuilder:
    """Test ReactionBuilder functionality."""

    def test_predefined_fuel_propane(self):
        """Test using predefined PROPANE fuel."""
        reac = ReactionBuilder().fuel("PROPANE").build()

        assert reac.c == 3
        assert reac.h == 8
        assert reac.o is None  # Not included when zero
        assert reac.n is None  # Not included when zero
        assert reac.heat_of_combustion == 46000
        assert reac.soot_yield == 0.010  # From database

    def test_predefined_fuel_methane(self):
        """Test using predefined METHANE fuel."""
        reac = ReactionBuilder().fuel("METHANE").build()

        assert reac.c == 1
        assert reac.h == 4
        assert reac.soot_yield == 0.001

    def test_predefined_fuel_wood(self):
        """Test using predefined WOOD fuel."""
        reac = ReactionBuilder().fuel("WOOD").build()

        assert reac.c == 3.4
        assert reac.h == 6.2
        assert reac.o == 2.5

    def test_predefined_fuel_case_insensitive(self):
        """Test that fuel names are case-insensitive."""
        reac1 = ReactionBuilder().fuel("propane").build()
        reac2 = ReactionBuilder().fuel("PROPANE").build()
        reac3 = ReactionBuilder().fuel("ProPaNe").build()

        assert reac1.c == reac2.c == reac3.c == 3

    def test_unknown_fuel_error(self):
        """Test error for unknown fuel."""
        with pytest.raises(ValueError, match="Unknown fuel"):
            ReactionBuilder().fuel("UNKNOWN_FUEL").build()

    def test_custom_fuel(self):
        """Test defining custom fuel composition."""
        reac = ReactionBuilder().custom_fuel(c=7, h=16, heat_of_combustion=44600).build()

        assert reac.c == 7
        assert reac.h == 16
        assert reac.heat_of_combustion == 44600

    def test_custom_fuel_with_oxygen_nitrogen(self):
        """Test custom fuel with oxygen and nitrogen."""
        reac = (
            ReactionBuilder()
            .custom_fuel(c=3.52, h=5.48, o=0.88, n=0.32, heat_of_combustion=23200)
            .build()
        )

        assert reac.c == 3.52
        assert reac.h == 5.48
        assert reac.o == 0.88
        assert reac.n == 0.32

    def test_override_soot_yield(self):
        """Test overriding database soot yield."""
        reac = ReactionBuilder().fuel("PROPANE").soot_yield(0.020).build()

        assert reac.soot_yield == 0.020  # Overridden value

    def test_set_co_yield(self):
        """Test setting CO yield."""
        reac = ReactionBuilder().fuel("PROPANE").co_yield(0.015).build()

        assert reac.co_yield == 0.015

    def test_set_yields_both(self):
        """Test setting both soot and CO yields."""
        reac = ReactionBuilder().fuel("PROPANE").yields(soot=0.015, co=0.02).build()

        assert reac.soot_yield == 0.015
        assert reac.co_yield == 0.02

    def test_radiative_fraction(self):
        """Test setting radiative fraction."""
        reac = ReactionBuilder().fuel("PROPANE").radiative_fraction(0.35).build()

        assert reac.radiative_fraction == 0.35

    def test_auto_ignition_temperature(self):
        """Test setting auto-ignition temperature."""
        reac = ReactionBuilder().fuel("PROPANE").auto_ignition_temperature(450).build()

        assert reac.auto_ignition_temperature == 450

    def test_complete_reaction(self):
        """Test building complete reaction with all parameters."""
        reac = (
            ReactionBuilder()
            .fuel("POLYURETHANE")
            .yields(soot=0.10, co=0.03)
            .radiative_fraction(0.30)
            .auto_ignition_temperature(350)
            .build()
        )

        assert reac.c == 3.52
        assert reac.soot_yield == 0.10
        assert reac.co_yield == 0.03
        assert reac.radiative_fraction == 0.30
        assert reac.auto_ignition_temperature == 350

    def test_no_fuel_error(self):
        """Test error when no fuel specified."""
        with pytest.raises(ValueError, match="Must specify fuel composition"):
            ReactionBuilder().soot_yield(0.01).build()

    def test_builder_reuse_error(self):
        """Test that builder cannot be reused."""
        builder = ReactionBuilder().fuel("PROPANE")
        builder.build()

        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()

    def test_list_fuels(self):
        """Test listing available fuels."""
        fuels = ReactionBuilder.list_fuels()

        assert isinstance(fuels, list)
        assert len(fuels) > 0
        assert "PROPANE" in fuels
        assert "METHANE" in fuels
        assert "WOOD" in fuels
        # Should be sorted
        assert fuels == sorted(fuels)

    def test_get_fuel_info(self):
        """Test getting fuel information."""
        info = ReactionBuilder.get_fuel_info("PROPANE")

        assert info["c"] == 3
        assert info["h"] == 8
        assert info["hoc"] == 46000
        assert "soot_yield" in info

    def test_get_fuel_info_case_insensitive(self):
        """Test fuel info lookup is case-insensitive."""
        info1 = ReactionBuilder.get_fuel_info("propane")
        info2 = ReactionBuilder.get_fuel_info("PROPANE")

        assert info1["c"] == info2["c"]

    def test_get_fuel_info_unknown_error(self):
        """Test error for unknown fuel in get_fuel_info."""
        with pytest.raises(ValueError, match="Unknown fuel"):
            ReactionBuilder.get_fuel_info("UNKNOWN")

    def test_fds_output_format(self):
        """Test FDS output format."""
        reac = ReactionBuilder().fuel("PROPANE").build()

        fds_output = reac.to_fds()
        assert "&REAC" in fds_output
        assert "C=3" in fds_output
        assert "H=8" in fds_output
        assert "HEAT_OF_COMBUSTION=46000" in fds_output

    def test_hydrogen_fuel(self):
        """Test hydrogen fuel (no carbon, not included in params since FDS requires c>0)."""
        # Note: Hydrogen (c=0) is in the database but FDS Reaction requires c > 0
        # So we test that it's excluded from params
        reac = ReactionBuilder().fuel("HYDROGEN").build()

        assert reac.c is None  # Not included since c=0
        assert reac.h == 2
        assert reac.soot_yield == 0.0  # No soot from hydrogen
