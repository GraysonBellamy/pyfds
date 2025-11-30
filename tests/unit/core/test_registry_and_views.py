"""
Tests for the unified registration API, RegistryView, and eager validation.

These tests verify the Phase 1-5 refactoring goals:
1. Single add pattern: Only sim.add() registers namelists
2. Typed views: sim.meshes, sim.surfaces etc. provide read access
3. Builder consistency: All builders work the same way (construct only)
4. Optional eager validation: Users can choose when validation happens
"""

import pytest

from pyfds.builders import MeshBuilder, ObstructionBuilder, SurfaceBuilder, VentBuilder
from pyfds.core.geometry import Bounds3D, Grid3D, Point3D
from pyfds.core.namelists import (
    Device,
    Material,
    Mesh,
    Obstruction,
    Surface,
    Time,
    Vent,
)
from pyfds.core.simulation import Simulation
from pyfds.exceptions import DuplicateIdError


class TestUnifiedAdd:
    """Tests for the unified sim.add() API."""

    def test_add_single_item(self):
        """Test adding a single item returns the simulation for chaining."""
        sim = Simulation(chid="test")
        mesh = Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))

        result = sim.add(mesh)

        assert result is sim  # Returns simulation for chaining
        assert "m1" in sim.meshes
        assert sim.meshes["m1"] is mesh

    def test_add_multiple_items(self):
        """Test adding multiple items in a single call."""
        sim = Simulation(chid="test")
        m1 = Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        m2 = Mesh(id="m2", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(1, 2, 0, 1, 0, 1))
        surf = Surface(id="FIRE", hrrpua=1000.0)

        result = sim.add(m1, m2, surf)

        assert result is sim
        assert len(sim.meshes) == 2
        assert "m1" in sim.meshes
        assert "m2" in sim.meshes
        assert "FIRE" in sim.surfaces

    def test_method_chaining(self):
        """Test fluent API with method chaining."""
        sim = (
            Simulation(chid="test")
            .add(Time(t_end=600.0))
            .add(Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
            .add(Surface(id="FIRE", hrrpua=1000.0))
            .add(
                Obstruction(
                    id="fire_obst", xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0.1), surf_id="FIRE"
                )
            )
        )

        assert sim.time_params.t_end == 600.0
        assert len(sim.meshes) == 1
        assert len(sim.surfaces) == 1
        assert len(sim.obstructions) == 1

    def test_add_duplicate_id_raises_error(self):
        """Test that adding duplicate IDs raises DuplicateIdError."""
        sim = Simulation(chid="test")
        sim.add(Surface(id="FIRE", hrrpua=1000.0))

        with pytest.raises(DuplicateIdError, match="FIRE"):
            sim.add(Surface(id="FIRE", hrrpua=500.0))

    def test_add_duplicate_across_types_raises_error(self):
        """Test that IDs must be globally unique across all namelist types."""
        sim = Simulation(chid="test")
        sim.add(Surface(id="THING", hrrpua=1000.0))

        # FDS requires IDs to be unique across ALL namelists
        with pytest.raises(DuplicateIdError, match="THING"):
            sim.add(Material(id="THING", density=1000, conductivity=1.0, specific_heat=1.0))


class TestRegistryView:
    """Tests for RegistryView read-only access."""

    def test_registry_view_iteration(self):
        """Test iterating over registry view."""
        sim = Simulation(chid="test")
        sim.add(
            Surface(id="A", hrrpua=100.0),
            Surface(id="B", hrrpua=200.0),
            Surface(id="C", hrrpua=300.0),
        )

        ids = [s.id for s in sim.surfaces]
        assert set(ids) == {"A", "B", "C"}

    def test_registry_view_indexing_by_id(self):
        """Test accessing items by ID using bracket notation."""
        sim = Simulation(chid="test")
        fire_surf = Surface(id="FIRE", hrrpua=1000.0)
        sim.add(fire_surf)

        assert sim.surfaces["FIRE"] is fire_surf
        assert sim.surfaces["FIRE"].hrrpua == 1000.0

    def test_registry_view_indexing_by_position(self):
        """Test accessing items by index position."""
        sim = Simulation(chid="test")
        m1 = Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        m2 = Mesh(id="m2", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(1, 2, 0, 1, 0, 1))
        sim.add(m1, m2)

        # Position-based access
        assert sim.meshes[0] in [m1, m2]
        assert sim.meshes[1] in [m1, m2]

    def test_registry_view_len(self):
        """Test len() on registry view."""
        sim = Simulation(chid="test")
        assert len(sim.surfaces) == 0

        sim.add(Surface(id="A", hrrpua=100.0))
        assert len(sim.surfaces) == 1

        sim.add(Surface(id="B", hrrpua=200.0))
        assert len(sim.surfaces) == 2

    def test_registry_view_contains(self):
        """Test 'in' operator on registry view."""
        sim = Simulation(chid="test")
        sim.add(Surface(id="FIRE", hrrpua=1000.0))

        assert "FIRE" in sim.surfaces
        assert "UNKNOWN" not in sim.surfaces

    def test_registry_view_is_readonly(self):
        """Test that registry views don't expose mutation methods."""
        sim = Simulation(chid="test")
        sim.add(Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Views should not have add/remove methods
        assert not hasattr(sim.meshes, "add")
        assert not hasattr(sim.meshes, "remove")
        assert not hasattr(sim.meshes, "clear")

    def test_registry_view_unknown_id_raises_error(self):
        """Test that accessing unknown ID raises appropriate error."""
        from pyfds.exceptions import UnknownIdError

        sim = Simulation(chid="test")
        sim.add(Surface(id="FIRE", hrrpua=1000.0))

        with pytest.raises(UnknownIdError, match="UNKNOWN"):
            _ = sim.surfaces["UNKNOWN"]


class TestBuilderBehavior:
    """Tests verifying builders don't auto-register."""

    def test_mesh_builder_does_not_autoregister(self):
        """Test MeshBuilder creates mesh without registering."""
        mesh = (
            MeshBuilder()
            .with_id("m1")
            .with_bounds(Bounds3D.of(0, 1, 0, 1, 0, 1))
            .with_grid(Grid3D.of(10, 10, 10))
            .build()
        )

        # Mesh exists but is not registered anywhere
        assert mesh.id == "m1"

        # Create simulation - mesh should not be in it
        sim = Simulation(chid="test")
        assert "m1" not in sim.meshes
        assert len(sim.meshes) == 0

        # Explicit registration required
        sim.add(mesh)
        assert "m1" in sim.meshes

    def test_surface_builder_does_not_autoregister(self):
        """Test SurfaceBuilder creates surface without registering."""
        surf = SurfaceBuilder().id("FIRE").burning(hrrpua=1000.0).color("RED").build()

        sim = Simulation(chid="test")
        assert "FIRE" not in sim.surfaces

        sim.add(surf)
        assert "FIRE" in sim.surfaces

    def test_obstruction_builder_does_not_autoregister(self):
        """Test ObstructionBuilder creates obstruction without registering."""
        obst = ObstructionBuilder().id("wall").bounds(Bounds3D.of(0, 0.1, 0, 1, 0, 1)).build()

        sim = Simulation(chid="test")
        assert "wall" not in sim.obstructions

        sim.add(obst)
        assert "wall" in sim.obstructions

    def test_vent_builder_does_not_autoregister(self):
        """Test VentBuilder creates vent without registering."""
        vent = (
            VentBuilder().id("inlet").bounds(Bounds3D.of(0, 0, 0, 1, 0, 1)).surface("OPEN").build()
        )

        sim = Simulation(chid="test")
        assert "inlet" not in sim.vents

        sim.add(vent)
        assert "inlet" in sim.vents

    def test_builder_class_method_pattern(self):
        """Test using .builder() class method pattern."""
        # Some namelists provide .builder() class method
        surf = Surface.builder().id("TEST").burning(hrrpua=500.0).build()

        assert surf.id == "TEST"
        assert surf.hrrpua == 500.0

        sim = Simulation(chid="test")
        assert "TEST" not in sim.surfaces


class TestEagerValidation:
    """Tests for eager validation mode."""

    def test_eager_validation_catches_missing_surf(self):
        """Test eager validation catches undefined SURF_ID references."""
        sim = Simulation(chid="test", eager_validation=True)
        sim.add(Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # This should fail because "burner" surface doesn't exist
        with pytest.raises(ValueError, match="undefined SURF 'burner'"):
            sim.add(
                Obstruction(id="fire", xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0.1), surf_id="burner")
            )

    def test_eager_validation_catches_missing_matl(self):
        """Test eager validation catches undefined MATL_ID references."""
        sim = Simulation(chid="test", eager_validation=True)

        # Surface references undefined material
        with pytest.raises(ValueError, match="undefined MATL 'WOOD'"):
            sim.add(Surface(id="WALL", matl_id="WOOD", thickness=0.01))

    def test_eager_validation_catches_missing_prop(self):
        """Test eager validation catches undefined PROP_ID references."""
        sim = Simulation(chid="test", eager_validation=True)

        with pytest.raises(ValueError, match="undefined PROP 'SPRINKLER'"):
            sim.add(
                Device(
                    id="SPK1",
                    prop_id="SPRINKLER",
                    xyz=Point3D.of(0.5, 0.5, 2.0),
                    quantity="LINK TEMPERATURE",
                )
            )

    def test_eager_validation_catches_missing_ctrl(self):
        """Test eager validation catches undefined CTRL_ID references."""
        sim = Simulation(chid="test", eager_validation=True)

        with pytest.raises(ValueError, match="undefined CTRL 'TIMER'"):
            sim.add(
                Device(
                    id="DEVC1",
                    ctrl_id="TIMER",
                    xyz=Point3D.of(0.5, 0.5, 0.5),
                    quantity="TEMPERATURE",
                )
            )

    def test_lazy_validation_allows_any_order(self):
        """Test lazy validation (default) allows adding items in any order."""
        sim = Simulation(chid="test", eager_validation=False)

        # Can add obstruction before defining the surface it references
        sim.add(
            Obstruction(id="fire", xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0.1), surf_id="burner")
        )
        sim.add(Surface(id="burner", hrrpua=1000.0))

        # Validation happens at write time - should be valid now
        sim.add(Time(t_end=100.0))
        sim.add(Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        issues = sim.validate()
        # Should have no undefined surface issues (burner is now defined)
        assert not any("burner" in str(issue) for issue in issues)

    def test_eager_validation_allows_builtin_surfaces(self):
        """Test eager validation accepts built-in surface IDs."""
        sim = Simulation(chid="test", eager_validation=True)
        sim.add(Mesh(id="m1", ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))

        # Built-in surfaces should not raise errors
        sim.add(Obstruction(id="wall", xb=Bounds3D.of(0, 0.1, 0, 1, 0, 1), surf_id="INERT"))
        sim.add(Vent(id="open_vent", xb=Bounds3D.of(0, 0, 0, 1, 0, 1), surf_id="OPEN"))

        assert "wall" in sim.obstructions
        assert "open_vent" in sim.vents

    def test_eager_validation_with_defined_references(self):
        """Test eager validation works when dependencies are added first."""
        sim = Simulation(chid="test", eager_validation=True)

        # Add dependencies in correct order
        sim.add(Material(id="WOOD", density=500, conductivity=0.13, specific_heat=2.5))
        sim.add(Surface(id="WALL", matl_id="WOOD", thickness=0.01))  # Should work
        sim.add(
            Obstruction(id="wall_obst", xb=Bounds3D.of(0, 0.1, 0, 1, 0, 1), surf_id="WALL")
        )  # Should work

        assert "WOOD" in sim.materials
        assert "WALL" in sim.surfaces
        assert "wall_obst" in sim.obstructions

    def test_eager_validation_default_is_false(self):
        """Test that eager_validation defaults to False."""
        sim = Simulation(chid="test")

        # Should not raise - lazy validation allows forward references
        sim.add(
            Obstruction(
                id="fire", xb=Bounds3D.of(0.4, 0.6, 0.4, 0.6, 0, 0.1), surf_id="undefined_surf"
            )
        )

        # Only fails at validation time
        issues = sim.validate()
        assert any("undefined_surf" in str(issue) for issue in issues)


class TestAllRegistryViews:
    """Test that all registry views work correctly."""

    def test_all_view_properties_exist(self):
        """Test all expected view properties are accessible."""
        sim = Simulation(chid="test")

        # All these should be RegistryView instances
        assert hasattr(sim, "meshes")
        assert hasattr(sim, "surfaces")
        assert hasattr(sim, "materials")
        assert hasattr(sim, "obstructions")
        assert hasattr(sim, "vents")
        assert hasattr(sim, "holes")
        assert hasattr(sim, "devices")
        assert hasattr(sim, "props")
        assert hasattr(sim, "ctrls")
        assert hasattr(sim, "species")
        assert hasattr(sim, "reactions")
        assert hasattr(sim, "ramps")
        assert hasattr(sim, "mults")
        assert hasattr(sim, "inits")

    def test_empty_views_are_empty(self):
        """Test newly created simulation has empty views."""
        sim = Simulation(chid="test")

        assert len(sim.meshes) == 0
        assert len(sim.surfaces) == 0
        assert len(sim.materials) == 0
        assert len(sim.obstructions) == 0
        assert len(sim.vents) == 0
        assert len(sim.devices) == 0

    def test_views_reflect_additions(self):
        """Test that views immediately reflect added items."""
        sim = Simulation(chid="test")

        assert len(sim.surfaces) == 0
        sim.add(Surface(id="A", hrrpua=100.0))
        assert len(sim.surfaces) == 1
        sim.add(Surface(id="B", hrrpua=200.0))
        assert len(sim.surfaces) == 2
