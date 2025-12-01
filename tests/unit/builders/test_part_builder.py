"""Unit tests for PartBuilder."""

import pytest

from pyfds.builders import PartBuilder
from pyfds.core.namelists import Particle


class TestPartBuilderBasicUsage:
    """Test basic PartBuilder usage."""

    def test_simple_build(self):
        """Test building a simple particle."""
        part = PartBuilder("TEST").with_diameter(0.001).with_density(1000.0).build()
        assert isinstance(part, Particle)
        assert part.id == "TEST"
        assert part.diameter == 0.001
        assert part.density == 1000.0

    def test_builder_cannot_be_reused(self):
        """Test that builder cannot be used twice."""
        builder = PartBuilder("TEST")
        builder.build()
        with pytest.raises(RuntimeError, match="already been used"):
            builder.build()


class TestPartBuilderWaterDroplet:
    """Test water droplet configuration."""

    def test_as_water_droplet(self):
        """Test as_water_droplet method."""
        part = PartBuilder("WATER").as_water_droplet(diameter=0.001, temp=20.0).build()
        assert part.liquid_droplet is True
        assert part.diameter == 0.001
        assert part.density == 1000.0
        assert part.initial_temperature == 20.0
        assert part.boiling_temperature == 100.0
        assert part.heat_of_vaporization == 2260.0

    def test_water_droplet_with_breakup(self):
        """Test water droplet with breakup enabled."""
        part = PartBuilder("WATER").as_water_droplet(0.001).with_breakup(True).build()
        assert part.breakup is True


class TestPartBuilderAerosol:
    """Test aerosol configuration."""

    def test_as_aerosol(self):
        """Test as_aerosol method."""
        part = PartBuilder("SMOKE").as_aerosol(diameter=0.00001, spec_id="SOOT").build()
        assert part.diameter == 0.00001
        assert part.spec_id == "SOOT"
        assert part.massless is False


class TestPartBuilderTracer:
    """Test tracer configuration."""

    def test_as_tracer(self):
        """Test as_tracer method."""
        part = PartBuilder("TRACER").as_tracer().build()
        assert part.massless is True


class TestPartBuilderVisualization:
    """Test visualization methods."""

    def test_with_color(self):
        """Test with_color method."""
        part = PartBuilder("TEST").with_color("BLUE").build()
        assert part.color == "BLUE"

    def test_with_rgb(self):
        """Test with_rgb method."""
        part = PartBuilder("TEST").with_rgb(128, 128, 128).build()
        assert part.rgb == (128, 128, 128)


class TestPartBuilderLifetime:
    """Test lifetime and age methods."""

    def test_with_lifetime(self):
        """Test with_lifetime method."""
        part = PartBuilder("TEST").with_lifetime(60.0).build()
        assert part.lifetime == 60.0

    def test_with_initial_age(self):
        """Test with_initial_age method."""
        part = PartBuilder("TEST").with_initial_age(10.0).build()
        assert part.age == 10.0


class TestPartBuilderDrag:
    """Test drag law methods."""

    def test_with_drag_law(self):
        """Test with_drag_law method."""
        part = PartBuilder("TEST").with_drag_law("CYLINDER").build()
        assert part.drag_law == "CYLINDER"


class TestPartBuilderMethodChaining:
    """Test method chaining capabilities."""

    def test_chain_multiple_methods(self):
        """Test chaining multiple methods together."""
        part = (
            PartBuilder("COMPLEX")
            .with_diameter(0.0005)
            .with_density(800.0)
            .with_lifetime(120.0)
            .with_color("RED")
            .build()
        )
        assert part.id == "COMPLEX"
        assert part.diameter == 0.0005
        assert part.density == 800.0
        assert part.lifetime == 120.0
        assert part.color == "RED"

    def test_water_droplet_full_chain(self):
        """Test full water droplet configuration chain."""
        part = (
            PartBuilder("SPRINKLER_DROP")
            .as_water_droplet(diameter=0.001, temp=20.0)
            .with_breakup(True)
            .with_breakup_parameters(cns_min=0.5, cns_max=15.0)
            .with_color("BLUE")
            .with_lifetime(30.0)
            .build()
        )
        assert part.liquid_droplet is True
        assert part.breakup is True
        assert part.breakup_cns_min == 0.5
        assert part.breakup_cns_max == 15.0
        assert part.color == "BLUE"
        assert part.lifetime == 30.0


class TestPartBuilderFDSOutput:
    """Test that builder output generates valid FDS."""

    def test_builder_generates_valid_fds(self):
        """Test that PartBuilder generates valid FDS output."""
        part = PartBuilder("WATER").as_water_droplet(0.001, temp=20.0).with_color("BLUE").build()
        fds_output = part.to_fds()
        assert "&PART" in fds_output
        assert "ID='WATER'" in fds_output
        assert "0.001" in fds_output
        assert "BLUE" in fds_output
        assert "/" in fds_output  # FDS namelist terminator

    def test_aerosol_fds_output(self):
        """Test aerosol generates valid FDS."""
        part = (
            PartBuilder("SMOKE")
            .as_aerosol(diameter=0.00001, spec_id="SOOT")
            .with_color("GRAY")
            .build()
        )
        fds_output = part.to_fds()
        assert "&PART" in fds_output
        assert "SMOKE" in fds_output
        assert "SOOT" in fds_output
        assert "GRAY" in fds_output
