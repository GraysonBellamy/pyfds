"""
ControlManager - Manages control logic and initial conditions.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Ctrl, Init


class ControlManager(BaseManager):
    """
    Manages control-related simulation components.

    This manager handles control logic (CTRL) and initial conditions (INIT)
    that define simulation behavior and starting states.
    """

    def __init__(self) -> None:
        """Initialize the control manager."""
        super().__init__()
        self._ctrls: list[Ctrl] = []
        self._inits: list[Init] = []

    @property
    def ctrls(self) -> list[Ctrl]:
        """Get the list of controls."""
        return self._ctrls

    @property
    def inits(self) -> list[Init]:
        """Get the list of initial conditions."""
        return self._inits

    def add_ctrl(self, ctrl: Ctrl) -> None:
        """
        Add a Ctrl object to the simulation.

        Parameters
        ----------
        ctrl : Ctrl
            Ctrl object to add
        """
        self._ctrls.append(ctrl)

    def add_init(self, init: Init) -> None:
        """
        Add an Init (initial condition) object to the simulation.

        Parameters
        ----------
        init : Init
            Init object to add
        """
        self._inits.append(init)

    def validate(self) -> list[str]:
        """
        Validate control configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        # Control-specific validation can be added here
        return []
