"""
FDS CTRL namelist.

Control logic for devices.
"""

from enum import Enum
from typing import Any

from pydantic import Field, model_validator

from pyfds.core.namelists.base import NamelistBase


class ControlFunction(str, Enum):
    """Control function types."""

    ANY = "ANY"
    ALL = "ALL"
    ONLY = "ONLY"
    TIME_DELAY = "TIME_DELAY"
    CUSTOM = "CUSTOM"
    KILL = "KILL"
    RESTART = "RESTART"


class Ctrl(NamelistBase):
    """
    FDS CTRL namelist - control logic.

    Implements control logic for devices based on inputs from other devices
    or control functions.

    Parameters
    ----------
    id : str
        Unique control identifier
    function_type : ControlFunction
        Type of control function
    input_id : str | list[str], optional
        Input device or control ID(s)
    delay : float, optional
        Time delay [s], default: 0.0
    initial_state : bool, optional
        Initial state, default: False
    latch : bool, optional
        Whether to latch on activation, default: True

    Examples
    --------
    >>> # ANY logic - activate if any input is true
    >>> ctrl = Ctrl(
    ...     id='SMOKE_ALARM',
    ...     function_type=ControlFunction.ANY,
    ...     input_id=['SD_1', 'SD_2', 'SD_3']
    ... )

    >>> # Time delay
    >>> ctrl = Ctrl(
    ...     id='DELAYED_ACTIVATION',
    ...     function_type=ControlFunction.TIME_DELAY,
    ...     input_id='SPRINKLER_1',
    ...     delay=5.0
    ... )
    """

    id: str = Field(..., description="Control identifier")
    function_type: ControlFunction = Field(..., description="Function type")
    input_id: str | list[str] | None = Field(None, description="Input device ID(s)")
    delay: float = Field(0.0, ge=0, description="Time delay [s]")
    initial_state: bool = Field(False, description="Initial state")
    latch: bool = Field(True, description="Latch on activation")

    @model_validator(mode="after")
    def validate_ctrl(self) -> "Ctrl":
        """Validate control parameters."""
        # ANY and ALL require multiple inputs
        if self.function_type in [ControlFunction.ANY, ControlFunction.ALL] and not isinstance(
            self.input_id, list
        ):
            raise ValueError(
                f"Control '{self.id}': {self.function_type.value} requires multiple INPUT_ID"
            )

        return self

    def to_fds(self) -> str:
        """Generate FDS CTRL namelist."""
        params: dict[str, Any] = {"id": self.id, "function_type": self.function_type.value}

        if self.input_id:
            params["input_id"] = self.input_id
        if self.delay > 0:
            params["delay"] = self.delay
        if self.initial_state:
            params["initial_state"] = self.initial_state
        if not self.latch:
            params["latch"] = self.latch

        return self._build_namelist("CTRL", params)
