"""Builder for creating CTRL namelists with control logic."""

from ..core.enums import ControlFunction
from ..core.namelists import Ctrl
from .base import Builder


class ControlBuilder(Builder[Ctrl]):
    """
    Builder for creating CTRL namelists.

    Provides convenient methods for creating control logic including
    logic gates (ANY, ALL), time delays, and other control functions.

    Parameters
    ----------
    id : str
        Unique identifier for the control

    Examples
    --------
    >>> # ANY logic (OR) - activate if any input is true
    >>> ctrl = ControlBuilder('SMOKE_ALARM') \\
    ...     .any(['SD_1', 'SD_2', 'SD_3']) \\
    ...     .build()

    >>> # ALL logic (AND) - activate if all inputs are true
    >>> ctrl = ControlBuilder('DUAL_CONDITION') \\
    ...     .all(['TEMP_HIGH', 'SMOKE_DETECTED']) \\
    ...     .build()

    >>> # Time delay
    >>> ctrl = ControlBuilder('DELAYED_SPRINKLER') \\
    ...     .time_delay('HEAT_DETECTOR', delay=10.0) \\
    ...     .build()

    >>> # Custom control with latch and initial state
    >>> ctrl = ControlBuilder('ALARM') \\
    ...     .any(['SD_1', 'SD_2']) \\
    ...     .with_latch(True) \\
    ...     .with_initial_state(False) \\
    ...     .build()
    """

    def __init__(self, id: str):
        """
        Initialize the ControlBuilder.

        Parameters
        ----------
        id : str
            Unique identifier for the control
        """
        super().__init__()
        self._id = id
        self._function_type: ControlFunction | None = None
        self._input_id: str | list[str] | None = None
        self._delay: float = 0.0
        self._initial_state: bool = False
        self._latch: bool = True

    def any(self, input_ids: list[str]) -> "ControlBuilder":
        """
        OR logic - activate if ANY input is true.

        Parameters
        ----------
        input_ids : list[str]
            List of device or control IDs to monitor

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('ALARM').any(['SD_1', 'SD_2', 'SD_3']).build()
        """
        self._function_type = ControlFunction.ANY
        self._input_id = input_ids
        return self

    def all(self, input_ids: list[str]) -> "ControlBuilder":
        """
        AND logic - activate if ALL inputs are true.

        Parameters
        ----------
        input_ids : list[str]
            List of device or control IDs to monitor

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('DUAL_COND').all(['TEMP_HIGH', 'SMOKE']).build()
        """
        self._function_type = ControlFunction.ALL
        self._input_id = input_ids
        return self

    def only(self, input_id: str) -> "ControlBuilder":
        """
        Direct pass-through of single input.

        Parameters
        ----------
        input_id : str
            Device or control ID to pass through

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('PASSTHROUGH').only('DETECTOR_1').build()
        """
        self._function_type = ControlFunction.ONLY
        self._input_id = input_id
        return self

    def time_delay(self, input_id: str, delay: float) -> "ControlBuilder":
        """
        Time-delayed activation.

        Parameters
        ----------
        input_id : str
            Device or control ID to monitor
        delay : float
            Time delay in seconds

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('DELAYED') \\
        ...     .time_delay('DETECTOR', delay=10.0) \\
        ...     .build()
        """
        self._function_type = ControlFunction.TIME_DELAY
        self._input_id = input_id
        self._delay = delay
        return self

    def custom(self, input_id: str | list[str]) -> "ControlBuilder":
        """
        Custom control function.

        Parameters
        ----------
        input_id : str or list[str]
            Device or control ID(s) to monitor

        Returns
        -------
        ControlBuilder
            Self for method chaining
        """
        self._function_type = ControlFunction.CUSTOM
        self._input_id = input_id
        return self

    def kill(self) -> "ControlBuilder":
        """
        Kill function - stops the simulation.

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('KILL_AT_TEMP') \\
        ...     .kill() \\
        ...     .build()
        """
        self._function_type = ControlFunction.KILL
        return self

    def restart(self) -> "ControlBuilder":
        """
        Restart function - triggers simulation restart.

        Returns
        -------
        ControlBuilder
            Self for method chaining
        """
        self._function_type = ControlFunction.RESTART
        return self

    def with_delay(self, delay: float) -> "ControlBuilder":
        """
        Add time delay to current control function.

        Parameters
        ----------
        delay : float
            Time delay in seconds

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('ALARM') \\
        ...     .any(['SD_1', 'SD_2']) \\
        ...     .with_delay(5.0) \\
        ...     .build()
        """
        self._delay = delay
        return self

    def with_initial_state(self, state: bool) -> "ControlBuilder":
        """
        Set initial state of the control.

        Parameters
        ----------
        state : bool
            Initial state (True=on, False=off)

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> ctrl = ControlBuilder('CTRL') \\
        ...     .any(['A', 'B']) \\
        ...     .with_initial_state(True) \\
        ...     .build()
        """
        self._initial_state = state
        return self

    def with_latch(self, latch: bool) -> "ControlBuilder":
        """
        Set whether control latches on activation.

        When latched, the control stays active once triggered.
        When unlatched, it can toggle on and off.

        Parameters
        ----------
        latch : bool
            Whether to latch on activation

        Returns
        -------
        ControlBuilder
            Self for method chaining

        Examples
        --------
        >>> # Latch stays on once activated
        >>> ctrl = ControlBuilder('LATCH').any(['A', 'B']).with_latch(True).build()

        >>> # Can toggle on/off
        >>> ctrl = ControlBuilder('TOGGLE').any(['A', 'B']).with_latch(False).build()
        """
        self._latch = latch
        return self

    def build(self) -> Ctrl:
        """
        Build the Ctrl object.

        Returns
        -------
        Ctrl
            The constructed Ctrl namelist object

        Raises
        ------
        ValueError
            If function type is not specified
        RuntimeError
            If the builder has already been used
        """
        self._check_built()

        if self._function_type is None:
            raise ValueError(f"ControlBuilder '{self._id}': No function type specified")

        ctrl = Ctrl(
            id=self._id,
            function_type=self._function_type,
            input_id=self._input_id,
            delay=self._delay,
            initial_state=self._initial_state,
            latch=self._latch,
        )

        self._mark_built()
        return ctrl
