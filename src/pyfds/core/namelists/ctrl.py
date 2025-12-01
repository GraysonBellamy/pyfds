"""FDS CTRL namelist for control logic.

Implements control logic for devices based on inputs from other devices
or control functions.

Field Groups:
    identification: Control ID
    logic: Function type (ANY, ALL, etc.)
    inputs: Input device references
    timing: Delay parameters
    state: Initial state and latching
"""

from pydantic import model_validator

from pyfds.core.enums import ControlFunction
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Control"]


class Control(NamelistBase):
    """FDS CTRL namelist - control logic.

    Implements control logic for devices based on inputs from other devices
    or control functions.

    Parameters
    ----------
    id : str
        Unique control identifier.
    function_type : ControlFunction
        Type of control function.
    input_id : str | list[str], optional
        Input device or control ID(s).
    delay : float, optional
        Time delay [s], default: 0.0.
    initial_state : bool, optional
        Initial state, default: False.
    latch : bool, optional
        Whether to latch on activation, default: True.

    Examples
    --------
    >>> ctrl = Control(
    ...     id='SMOKE_ALARM',
    ...     function_type=ControlFunction.ANY,
    ...     input_id=['SD_1', 'SD_2', 'SD_3']
    ... )

    See Also
    --------
    Device : Measurement devices that provide control inputs.
    Obstruction : Objects controlled by CTRL logic.
    Vent : Vents controlled by CTRL logic.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "CTRL"

    # --- Identification ---
    id: str = FdsField(..., description="Control identifier", group="identification")
    function_type: ControlFunction = FdsField(..., description="Function type", group="logic")

    # --- Inputs ---
    input_id: str | list[str] | None = FdsField(
        None, description="Input device ID(s)", group="inputs"
    )

    # --- Timing ---
    delay: float = FdsField(0.0, exclude_if=0.0, description="Time delay [s]", group="timing")

    # --- State ---
    initial_state: bool = FdsField(
        False, exclude_if=False, description="Initial state", group="state"
    )
    latch: bool = FdsField(True, exclude_if=True, description="Latch on activation", group="state")

    @model_validator(mode="after")
    def validate_ctrl(self) -> "Control":
        """Validate control parameters."""
        # ANY and ALL require multiple inputs
        if self.function_type in [ControlFunction.ANY, ControlFunction.ALL] and not isinstance(
            self.input_id, list
        ):
            raise ValueError(
                f"Control '{self.id}': {self.function_type.value} requires multiple INPUT_ID"
            )

        return self
