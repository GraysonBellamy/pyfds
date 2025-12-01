"""Tests for Move namelist."""

from pyfds.core.namelists.move import Move


class TestMove:
    """Test Move namelist functionality."""

    def test_basic_move(self):
        """Test basic move creation."""
        move = Move(id="BASIC_MOVE", dx=1.0, dy=2.0, dz=3.0)
        assert move.id == "BASIC_MOVE"
        assert move.dx == 1.0
        assert move.dy == 2.0
        assert move.dz == 3.0

    def test_move_with_rotation(self):
        """Test move with rotation."""
        move = Move(id="ROTATE", axis=(0, 0, 1), rotation_angle=90.0)
        assert move.id == "ROTATE"
        assert move.axis == (0, 0, 1)
        assert move.rotation_angle == 90.0

    def test_move_with_scaling(self):
        """Test move with scaling."""
        move = Move(id="SCALE", scale=(2.0, 1.5, 1.0))
        assert move.id == "SCALE"
        assert move.scale == (2.0, 1.5, 1.0)

    def test_move_combined(self):
        """Test combined translation, rotation, and scaling."""
        move = Move(
            id="COMPLEX",
            dx=1.0,
            dy=1.0,
            dz=0.0,
            axis=(0, 0, 1),
            rotation_angle=45.0,
            scale=(2.0, 2.0, 1.0),
        )
        assert move.id == "COMPLEX"
        assert move.dx == 1.0
        assert move.dy == 1.0
        assert move.dz == 0.0
        assert move.axis == (0, 0, 1)
        assert move.rotation_angle == 45.0
        assert move.scale == (2.0, 2.0, 1.0)

    def test_move_to_fds_basic(self):
        """Test basic FDS output."""
        move = Move(id="TRANSLATE", dx=5.0, dy=0.0, dz=2.0)
        fds_output = move.to_fds()
        assert "&MOVE" in fds_output
        assert "ID='TRANSLATE'" in fds_output
        assert "DX=5.0" in fds_output
        assert "DZ=2.0" in fds_output
        # DY=0.0 is not included when zero
        assert "DY=" not in fds_output

    def test_move_to_fds_rotation(self):
        """Test rotation FDS output."""
        move = Move(id="SPIN", axis=(0, 0, 1), rotation_angle=90.0)
        fds_output = move.to_fds()
        assert "AXIS=0.0,0.0,1.0" in fds_output
        assert "ROTATION_ANGLE=90.0" in fds_output

    def test_move_to_fds_scaling(self):
        """Test scaling FDS output."""
        move = Move(id="STRETCH", scale=(2.0, 1.5, 1.0))
        fds_output = move.to_fds()
        assert "SCALE=2.0,1.5,1.0" in fds_output
