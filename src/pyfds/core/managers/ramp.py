"""
RampManager - Manages time-varying and property-varying ramps.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Ramp


class RampManager(BaseManager):
    """
    Manages ramp definitions for time-varying and property-varying functions.

    RAMPs are cross-cutting entities used by multiple namelists:
    - Materials: conductivity_ramp, specific_heat_ramp
    - Surfaces: ramp_q (HRR), ramp_t (temperature)
    - Vents: ramp_v (flow rates)
    - Controls: time-based setpoints
    - Devices: output controls
    """

    def __init__(self) -> None:
        """Initialize the ramp manager."""
        super().__init__()
        self._ramps: list[Ramp] = []

    @property
    def ramps(self) -> list[Ramp]:
        """Get the list of ramps."""
        return self._ramps

    def add_ramp(self, ramp: Ramp) -> None:
        """
        Add a Ramp object to the simulation.

        Parameters
        ----------
        ramp : Ramp
            Ramp object to add
        """
        self._ramps.append(ramp)

    def validate(self) -> list[str]:
        """
        Validate ramp configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        warnings = []

        # Check for duplicate IDs
        ramp_ids = [r.id for r in self._ramps]
        duplicates = {rid for rid in ramp_ids if ramp_ids.count(rid) > 1}
        for dup_id in duplicates:
            warnings.append(f"Duplicate RAMP ID '{dup_id}'")

        return warnings

    def validate_ramp_references(self, referenced_ids: set[str]) -> list[str]:
        """
        Validate that referenced ramp IDs exist.

        Parameters
        ----------
        referenced_ids : set[str]
            Set of ramp IDs referenced by other namelists

        Returns
        -------
        List[str]
            List of validation warnings
        """
        warnings = []
        ramp_ids = {r.id for r in self._ramps}

        for ref_id in referenced_ids:
            if ref_id not in ramp_ids:
                warnings.append(f"Referenced undefined RAMP '{ref_id}'")

        return warnings
