"""Integration tests for hole functionality."""

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists import Hole, Mesh, Obstruction, Time
from pyfds.core.simulation import Simulation


class TestHoleIntegration:
    """Integration tests for hole functionality."""

    def test_hole_in_simulation_output(self):
        """Test that holes appear in FDS output."""
        sim = Simulation(chid="hole_test")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

        # Add wall with door
        sim.add(Obstruction(xb=Bounds3D.of(5, 5.2, 0, 10, 0, 3), surf_id="CONCRETE"))
        sim.add(Hole(xb=Bounds3D.of(5, 5.2, 4, 6, 0, 2.1), id="DOOR"))

        fds_output = sim.to_fds()

        # Check that both OBST and HOLE appear
        assert "&OBST" in fds_output
        assert "&HOLE" in fds_output
        assert "ID='DOOR'" in fds_output

        # Check ordering - OBST should come before HOLE
        obst_pos = fds_output.find("&OBST")
        hole_pos = fds_output.find("&HOLE")
        assert obst_pos < hole_pos

    def test_multiple_holes_in_simulation(self):
        """Test multiple holes in a simulation."""
        sim = Simulation(chid="multi_hole_test")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

        # Add walls with multiple openings
        sim.add(Obstruction(xb=Bounds3D.of(0, 0.2, 0, 10, 0, 5), surf_id="CONCRETE"))  # Left wall
        sim.add(Obstruction(xb=Bounds3D.of(9.8, 10, 0, 10, 0, 5), surf_id="CONCRETE"))  # Right wall

        # Add door and window
        sim.add(Hole(xb=Bounds3D.of(0, 0.2, 3, 5, 0, 2.1), id="DOOR"))
        sim.add(Hole(xb=Bounds3D.of(9.8, 10, 2, 4, 2.5, 4.5), id="WINDOW"))

        fds_output = sim.to_fds()

        # Check both holes are present
        assert fds_output.count("&HOLE") == 2
        assert "ID='DOOR'" in fds_output
        assert "ID='WINDOW'" in fds_output

    def test_controlled_hole_integration(self):
        """Test controlled holes."""
        sim = Simulation(chid="controlled_hole_test")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

        # Add wall
        sim.add(Obstruction(xb=Bounds3D.of(5, 5.2, 0, 10, 0, 5), surf_id="CONCRETE"))

        # Add controlled door
        sim.add(
            Hole(
                xb=Bounds3D.of(5, 5.2, 4, 6, 0, 2.1),
                id="DOOR",
                ctrl_id="DOOR_CONTROL",
                color="GRAY",
            )
        )

        fds_output = sim.to_fds()

        # Check hole components are present
        assert "&HOLE" in fds_output
        assert "CTRL_ID='DOOR_CONTROL'" in fds_output
        assert "COLOR='GRAY'" in fds_output

    def test_hole_with_visualization(self):
        """Test hole with visualization parameters."""
        sim = Simulation(chid="visual_hole_test")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

        # Add wall
        sim.add(Obstruction(xb=Bounds3D.of(5, 5.2, 0, 10, 0, 5), surf_id="CONCRETE"))

        # Add hole with full visualization
        sim.add(
            Hole(
                xb=Bounds3D.of(5, 5.2, 4, 6, 0, 2.1),
                id="WINDOW",
                color="BLUE",
                rgb=(0, 0, 255),
                transparency=0.8,
            )
        )

        fds_output = sim.to_fds()

        # Check visualization parameters
        assert "COLOR='BLUE'" in fds_output
        assert "RGB=0,0,255" in fds_output
        assert "TRANSPARENCY=0.8" in fds_output

    def test_hole_bounds_validation(self):
        """Test that hole bounds are properly validated."""
        sim = Simulation(chid="bounds_test")
        sim.add(Time(t_end=10.0))
        sim.add(Mesh(ijk=Grid3D.of(20, 20, 10), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

        # Add wall
        sim.add(Obstruction(xb=Bounds3D.of(5, 5.2, 0, 10, 0, 5), surf_id="CONCRETE"))

        # Add hole within wall bounds - should work
        sim.add(Hole(xb=Bounds3D.of(5, 5.2, 4, 6, 0, 2.1), id="VALID_HOLE"))

        # This should not raise an error during FDS generation
        fds_output = sim.to_fds()
        assert "&HOLE" in fds_output
        assert "ID='VALID_HOLE'" in fds_output
