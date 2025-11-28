"""Integration tests for MULT functionality."""

from pyfds.core.simulation import Simulation


class TestMultIntegration:
    """Integration tests for MULT functionality."""

    def test_mult_in_simulation_output(self):
        """Test that MULT appears in FDS output."""
        sim = Simulation(chid="mult_test")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(20, 20, 10), xb=(0, 10, 0, 10, 0, 5))

        # Add multiplier for 3x3 array
        sim.mult(id="ARRAY_3X3", dx=2.0, dy=2.0, i_lower=0, i_upper=2, j_lower=0, j_upper=2)

        # Add obstruction that uses the multiplier
        sim.obstruction(xb=(0, 1, 0, 1, 0, 0.5), mult_id="ARRAY_3X3")

        fds_output = sim.to_fds()

        # Check that both MULT and OBST appear
        assert "&MULT" in fds_output
        assert "&OBST" in fds_output
        assert "ID='ARRAY_3X3'" in fds_output
        assert "MULT_ID='ARRAY_3X3'" in fds_output

        # Check ordering - MULT should come before OBST
        mult_pos = fds_output.find("&MULT")
        obst_pos = fds_output.find("&OBST")
        assert mult_pos < obst_pos

    def test_multiple_mults_in_simulation(self):
        """Test multiple multipliers in a simulation."""
        sim = Simulation(chid="multi_mult_test")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(20, 20, 10), xb=(0, 10, 0, 10, 0, 5))

        # Add two different multipliers
        sim.mult(id="ROW_5", dx=1.0, n_lower=0, n_upper=4)
        sim.mult(id="COLUMN_3", dy=2.0, n_lower=0, n_upper=2)

        # Add obstructions using different multipliers
        sim.obstruction(xb=(0, 0.5, 0, 0.5, 0, 0.5), mult_id="ROW_5")
        sim.obstruction(xb=(2, 2.5, 0, 0.5, 0, 0.5), mult_id="COLUMN_3")

        fds_output = sim.to_fds()

        # Check both MULTs are present
        assert fds_output.count("&MULT") == 2
        assert "ID='ROW_5'" in fds_output
        assert "ID='COLUMN_3'" in fds_output
        assert "MULT_ID='ROW_5'" in fds_output
        assert "MULT_ID='COLUMN_3'" in fds_output

    def test_mult_with_skip_ranges(self):
        """Test MULT with skip ranges for creating gaps."""
        sim = Simulation(chid="mult_skip_test")
        sim.time(t_end=10.0)
        sim.mesh(ijk=(20, 20, 10), xb=(0, 10, 0, 10, 0, 5))

        # Add multiplier with skip range (skip indices 2-3)
        sim.mult(id="ARRAY_WITH_GAP", dx=1.0, n_lower=0, n_upper=9, n_lower_skip=2, n_upper_skip=3)

        # Add obstruction using the multiplier
        sim.obstruction(xb=(0, 0.5, 0, 0.5, 0, 0.5), mult_id="ARRAY_WITH_GAP")

        fds_output = sim.to_fds()

        # Check MULT with skip parameters
        assert "&MULT" in fds_output
        assert "ID='ARRAY_WITH_GAP'" in fds_output
        assert "N_LOWER=0" in fds_output
        assert "N_UPPER=9" in fds_output
        assert "N_LOWER_SKIP=2" in fds_output
        assert "N_UPPER_SKIP=3" in fds_output
