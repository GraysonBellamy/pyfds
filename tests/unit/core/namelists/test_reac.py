"""Unit tests for REAC namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.namelists import Reaction


class TestReaction:
    """Tests for Reaction namelist."""

    def test_basic_creation(self):
        """Test basic reaction creation."""
        reac = Reaction(fuel="PROPANE")
        assert reac.fuel == "PROPANE"

    def test_custom_reaction(self):
        """Test custom fuel composition."""
        reac = Reaction(c=7, h=16, heat_of_combustion=44600)
        assert reac.c == 7
        assert reac.h == 16

    def test_reaction_validation_yields(self):
        """Test that yields cannot exceed 1.0."""
        with pytest.raises(ValidationError, match=r"yields.*exceeds 1.0"):
            Reaction(fuel="PROPANE", soot_yield=0.6, co_yield=0.5)

    def test_reaction_to_fds(self):
        """Test FDS output format."""
        reac = Reaction(fuel="PROPANE")
        fds_str = reac.to_fds()
        assert "&REAC" in fds_str
        assert "FUEL='PROPANE'" in fds_str
