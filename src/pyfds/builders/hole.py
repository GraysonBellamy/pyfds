"""Builder for creating HOLE namelists with fluent API."""

from ..core.geometry import Bounds3D
from ..core.namelists import Hole
from .base import Builder


class HoleBuilder(Builder[Hole]):
    """
    Builder for creating HOLE namelists.

    Provides a fluent API for constructing holes in obstructions such as
    doors, windows, and vents with control and visualization parameters.

    Parameters
    ----------
    xb : Bounds3D | tuple[float, ...]
        Hole bounds (xmin, xmax, ymin, ymax, zmin, zmax)

    Examples
    --------
    >>> # Simple door hole
    >>> door = HoleBuilder((5, 5.1, 2, 4, 0, 2.1)) \\
    ...     .with_id('DOOR') \\
    ...     .build()

    >>> # Controlled window with visualization
    >>> window = HoleBuilder((5, 5.1, 2, 4, 0, 2.1)) \\
    ...     .with_id('WINDOW') \\
    ...     .with_control(ctrl_id='WINDOW_CTRL') \\
    ...     .with_visualization(color='GRAY') \\
    ...     .build()

    >>> # Door using factory method
    >>> door = HoleBuilder.door(wall_x=5, y_min=2, y_max=4, z_min=0, z_max=2.1)
    """

    def __init__(self, xb: Bounds3D | tuple[float, ...]):
        """
        Initialize the HoleBuilder.

        Parameters
        ----------
        xb : Bounds3D | tuple[float, ...]
            Hole bounds (xmin, xmax, ymin, ymax, zmin, zmax)
        """
        super().__init__()
        if isinstance(xb, Bounds3D):
            self._xb = xb
        else:
            if len(xb) != 6:
                raise ValueError(f"Bounds tuple must have exactly 6 elements, got {len(xb)}")
            self._xb = Bounds3D.from_tuple((xb[0], xb[1], xb[2], xb[3], xb[4], xb[5]))
        self._params: dict = {}

    def with_id(self, id: str) -> "HoleBuilder":
        """Set hole identifier."""
        self._params["id"] = id
        return self

    def with_control(self, ctrl_id: str | None = None, devc_id: str | None = None) -> "HoleBuilder":
        """Set control parameters."""
        if ctrl_id:
            self._params["ctrl_id"] = ctrl_id
        if devc_id:
            self._params["devc_id"] = devc_id
        return self

    def with_visualization(
        self,
        color: str | None = None,
        rgb: tuple[int, int, int] | None = None,
        transparency: float | None = None,
    ) -> "HoleBuilder":
        """Set visualization parameters."""
        if color:
            self._params["color"] = color
        if rgb:
            self._params["rgb"] = rgb
        if transparency is not None:
            self._params["transparency"] = transparency
        return self

    def with_multiplier(self, mult_id: str) -> "HoleBuilder":
        """Set multiplier ID for array replication."""
        self._params["mult_id"] = mult_id
        return self

    def build(self) -> Hole:
        """Build the Hole object."""
        self._check_built()
        params = {"xb": self._xb, **self._params}
        hole = Hole(**params)
        self._mark_built()
        return hole

    @classmethod
    def door(
        cls,
        wall_x: float,
        y_min: float,
        y_max: float,
        z_min: float,
        z_max: float,
        thickness: float = 0.1,
        id: str | None = None,
    ) -> Hole:
        """Create a door hole in a wall."""
        xb = Bounds3D(wall_x, wall_x + thickness, y_min, y_max, z_min, z_max)
        builder = cls(xb)
        if id:
            builder.with_id(id)
        else:
            builder.with_id("DOOR")
        return builder.build()

    @classmethod
    def window(
        cls,
        wall_x: float,
        y_min: float,
        y_max: float,
        z_min: float,
        z_max: float,
        thickness: float = 0.1,
        id: str | None = None,
    ) -> Hole:
        """Create a window hole in a wall."""
        xb = Bounds3D(wall_x, wall_x + thickness, y_min, y_max, z_min, z_max)
        builder = cls(xb)
        if id:
            builder.with_id(id)
        else:
            builder.with_id("WINDOW")
        return builder.build()
