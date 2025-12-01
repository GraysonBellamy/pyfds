"""Common hole presets for doors and windows."""

from pyfds.core.geometry import Bounds3D
from pyfds.core.namelists import Hole


class CommonHoles:
    """
    Library of predefined hole configurations.

    Provides factory methods for common openings like doors and windows
    with sensible visualization defaults.

    Examples
    --------
    >>> door = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1))
    >>> window = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2))
    """

    @staticmethod
    def door(
        xb: tuple[float, float, float, float, float, float] | Bounds3D,
        id: str | None = None,
        ctrl_id: str | None = None,
        devc_id: str | None = None,
        color: str = "BROWN",
        mult_id: str | None = None,
    ) -> Hole:
        """
        Create a standard door opening.

        Parameters
        ----------
        xb : tuple or Bounds3D
            Door bounds (xmin, xmax, ymin, ymax, zmin, zmax).
            One dimension should have zero thickness (planar opening).
        id : str, optional
            Hole identifier
        ctrl_id : str, optional
            Control ID for door operation (e.g., open/close logic)
        devc_id : str, optional
            Device ID for door operation
        color : str, optional
            Visualization color (default: 'BROWN')
        mult_id : str, optional
            Multiplier ID for creating arrays of doors

        Returns
        -------
        Hole
            Door hole object

        Examples
        --------
        >>> # Simple door on X-boundary wall
        >>> door = CommonHoles.door(xb=(5, 5, 2, 4, 0, 2.1))

        >>> # Door with control logic
        >>> door = CommonHoles.door(
        ...     xb=(5, 5, 2, 4, 0, 2.1),
        ...     id="MAIN_DOOR",
        ...     ctrl_id="DOOR_OPEN_CTRL"
        ... )

        Notes
        -----
        Standard door dimensions are typically:
        - Width: 0.9-1.2 m
        - Height: 2.0-2.1 m
        """
        if isinstance(xb, tuple):
            xb = Bounds3D.of(*xb)
        return Hole(
            xb=xb,
            id=id,
            ctrl_id=ctrl_id,
            devc_id=devc_id,
            color=color,
            mult_id=mult_id,
        )

    @staticmethod
    def window(
        xb: tuple[float, float, float, float, float, float] | Bounds3D,
        id: str | None = None,
        ctrl_id: str | None = None,
        devc_id: str | None = None,
        color: str = "CYAN",
        transparency: float = 0.5,
        mult_id: str | None = None,
    ) -> Hole:
        """
        Create a standard window opening.

        Parameters
        ----------
        xb : tuple or Bounds3D
            Window bounds (xmin, xmax, ymin, ymax, zmin, zmax).
            One dimension should have zero thickness (planar opening).
        id : str, optional
            Hole identifier
        ctrl_id : str, optional
            Control ID for window operation (e.g., breakage logic)
        devc_id : str, optional
            Device ID for window operation
        color : str, optional
            Visualization color (default: 'CYAN')
        transparency : float, optional
            Visualization transparency 0-1 (default: 0.5)
        mult_id : str, optional
            Multiplier ID for creating arrays of windows

        Returns
        -------
        Hole
            Window hole object

        Examples
        --------
        >>> # Simple window on X-boundary wall
        >>> window = CommonHoles.window(xb=(0, 0, 2, 3, 1, 2))

        >>> # Window with breakage control
        >>> window = CommonHoles.window(
        ...     xb=(0, 0, 2, 3, 1, 2),
        ...     id="WINDOW_1",
        ...     ctrl_id="GLASS_BREAK_CTRL"
        ... )

        Notes
        -----
        Windows are typically placed 0.9-1.0 m above floor level.
        Standard window heights are 1.0-1.5 m.
        """
        if isinstance(xb, tuple):
            xb = Bounds3D.of(*xb)
        return Hole(
            xb=xb,
            id=id,
            ctrl_id=ctrl_id,
            devc_id=devc_id,
            color=color,
            transparency=transparency,
            mult_id=mult_id,
        )
