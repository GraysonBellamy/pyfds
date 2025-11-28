"""
CombustionManager - Manages combustion model parameters.

.. deprecated:: 1.0.0
    CombustionManager is deprecated. Use SpeciesManager instead, which provides
    unified management of both species definitions and combustion parameters.
"""

from __future__ import annotations

import warnings
from typing import TYPE_CHECKING, Any

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Combustion


class CombustionManager(BaseManager):
    """
    Manages combustion model parameters.

    .. deprecated:: 1.0.0
        CombustionManager is deprecated. Use SpeciesManager instead, which provides
        unified management of both species definitions and combustion parameters.

    This manager handles global combustion behavior including extinction models,
    turbulent combustion controls, and mixing parameters.
    """

    def __init__(self) -> None:
        """Initialize the combustion manager."""
        warnings.warn(
            "CombustionManager is deprecated. Use SpeciesManager instead, "
            "which provides unified management of both species definitions and combustion parameters.",
            DeprecationWarning,
            stacklevel=2,
        )
        super().__init__()
        self._combustion: Combustion | None = None

    @property
    def combustion(self) -> Combustion | None:
        """Get the combustion parameters."""
        return self._combustion

    def set_combustion(self, combustion: Combustion | None = None, **kwargs: Any) -> None:
        """
        Set COMB parameters for the simulation.

        Can be called with a Combustion object or with keyword arguments to create one.

        Parameters
        ----------
        combustion : Combustion, optional
            Combustion object to set (if None, kwargs are used to create one)
        **kwargs
            Keyword arguments to pass to Combustion constructor

        Notes
        -----
        Only one COMB namelist is allowed per simulation. Calling this method
        multiple times will overwrite the previous settings.
        """
        if combustion is None:
            from ..namelists import Combustion

            combustion = Combustion(**kwargs)
        self._combustion = combustion

    def validate(self) -> list[str]:
        """
        Validate combustion configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        # Combustion-specific validation can be added here
        return []
