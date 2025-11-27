"""
PhysicsManager - Manages combustion reactions and physical parameters.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Misc, Reaction


class PhysicsManager(BaseManager):
    """
    Manages physics-related simulation components.

    This manager handles combustion reactions and miscellaneous
    physical parameters that control simulation behavior.
    """

    def __init__(self) -> None:
        """Initialize the physics manager."""
        super().__init__()
        self._reactions: list[Reaction] = []
        self._misc_params: Misc | None = None

    @property
    def reactions(self) -> list[Reaction]:
        """Get the list of reactions."""
        return self._reactions

    @property
    def misc_params(self) -> Misc | None:
        """Get the miscellaneous parameters."""
        return self._misc_params

    def add_reaction(self, reaction: Reaction) -> None:
        """
        Add a Reaction object to the simulation.

        Parameters
        ----------
        reaction : Reaction
            Reaction object to add
        """
        self._reactions.append(reaction)

    def set_misc(self, misc: Misc | None = None, **kwargs: Any) -> None:
        """
        Set MISC parameters for the simulation.

        Can be called with a Misc object or with keyword arguments to create one.

        Parameters
        ----------
        misc : Misc, optional
            Misc object to set (if None, kwargs are used to create one)
        **kwargs
            Keyword arguments to pass to Misc constructor

        Notes
        -----
        Only one MISC namelist is allowed per simulation. Calling this method
        multiple times will overwrite the previous settings.
        """
        if misc is None:
            from ..namelists import Misc

            misc = Misc(**kwargs)
        self._misc_params = misc

    def validate(self) -> list[str]:
        """
        Validate physics configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        # Physics-specific validation can be added here
        return []
