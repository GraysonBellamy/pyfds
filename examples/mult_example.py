"""
Example demonstrating MULT namelist for creating arrays of repeated objects.

This example shows how to use the MULT namelist to create regular arrays
of obstructions, holes, vents, and other geometry objects without specifying
each one individually.
"""

from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists.hole import Hole
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.mult import Mult
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.time import Time
from pyfds.core.namelists.vent import Vent
from pyfds.core.simulation import Simulation


def main():
    """Demonstrate MULT functionality with various geometry objects."""

    # Create a basic simulation
    sim = Simulation(chid="mult_example", title="MULT Array Example")
    sim.add(Time(t_end=60.0))
    sim.add(Mesh(ijk=Grid3D.of(100, 100, 50), xb=Bounds3D.of(0, 10, 0, 10, 0, 5)))

    # Example 1: 3x3 array of obstructions (pillars)
    sim.add(Mult(id="PILLARS", dx=3.0, dy=3.0, i_lower=0, i_upper=2, j_lower=0, j_upper=2))
    sim.add(Obstruction(xb=Bounds3D.of(0.2, 0.8, 0.2, 0.8, 0, 3), mult_id="PILLARS"))
    # Example 2: Linear array of windows along a wall
    sim.add(Mult(id="WINDOWS", dx=1.5, n_lower=0, n_upper=4))
    sim.add(Hole(xb=Bounds3D.of(0, 0.1, 1.5, 2.5, 1.8, 2.2), mult_id="WINDOWS"))

    # Example 3: Array with gaps (skip some positions)
    sim.add(
        Mult(id="SPARSE_ARRAY", dx=2.0, n_lower=0, n_upper=9, n_lower_skip=2, n_upper_skip=3)
    )  # Skip positions 2-3
    sim.add(
        Vent(xb=Bounds3D.of(0.1, 0.9, 0.1, 0.9, 5.0, 5.0), surf_id="OPEN", mult_id="SPARSE_ARRAY")
    )

    # Generate FDS input
    fds_content = sim.to_fds()
    print("Generated FDS input:")
    print(fds_content)

    # Save to file
    sim.write("mult_example.fds")
    print("\nFDS input saved to: mult_example.fds")


if __name__ == "__main__":
    main()
