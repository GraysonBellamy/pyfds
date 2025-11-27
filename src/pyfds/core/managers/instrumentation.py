"""
InstrumentationManager - Manages devices and props.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from .base import BaseManager

if TYPE_CHECKING:
    from ..namelists import Device, Prop


class InstrumentationManager(BaseManager):
    """
    Manages instrumentation-related simulation components.

    This manager handles devices (measurement points) and props
    (device properties like sprinklers, detectors, etc.).
    """

    def __init__(self) -> None:
        """Initialize the instrumentation manager."""
        super().__init__()
        self._devices: list[Device] = []
        self._props: list[Prop] = []

    @property
    def devices(self) -> list[Device]:
        """Get the list of devices."""
        return self._devices

    @property
    def props(self) -> list[Prop]:
        """Get the list of props."""
        return self._props

    def add_device(self, device: Device) -> None:
        """
        Add a Device object to the simulation.

        Parameters
        ----------
        device : Device
            Device object to add
        """
        self._devices.append(device)

    def add_prop(self, prop: Prop) -> None:
        """
        Add a Prop object to the simulation.

        Parameters
        ----------
        prop : Prop
            Prop object to add
        """
        self._props.append(prop)

    def validate(self) -> list[str]:
        """
        Validate instrumentation configuration.

        Returns
        -------
        List[str]
            List of validation warnings
        """
        warnings = []

        # Check for device ID uniqueness
        device_ids = [d.id for d in self._devices]
        if len(device_ids) != len(set(device_ids)):
            warnings.append("Duplicate device IDs found")

        return warnings
