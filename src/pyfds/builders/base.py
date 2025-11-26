"""Base builder class providing common builder pattern functionality."""

from abc import ABC, abstractmethod
from typing import Generic, TypeVar

T = TypeVar("T")


class Builder(ABC, Generic[T]):
    """
    Abstract base class for all builders.

    Provides common functionality like validation tracking,
    method chaining, and build finalization.

    Type Parameters
    ---------------
    T
        The type of object this builder creates

    Examples
    --------
    >>> class MyBuilder(Builder[MyClass]):
    ...     def __init__(self):
    ...         super().__init__()
    ...         self._value = None
    ...
    ...     def set_value(self, value):
    ...         self._value = value
    ...         return self
    ...
    ...     def build(self) -> MyClass:
    ...         return MyClass(self._value)
    """

    def __init__(self) -> None:
        """Initialize the builder."""
        self._built = False
        self._validation_errors: list[str] = []

    @abstractmethod
    def build(self) -> T:
        """
        Build and return the final object.

        Returns
        -------
        T
            The constructed object

        Raises
        ------
        RuntimeError
            If the builder has already been used to build an object
        ValueError
            If validation fails
        """
        pass

    def _validate(self) -> list[str]:
        """
        Validate current builder state.

        Subclasses can override this to provide custom validation logic.

        Returns
        -------
        list[str]
            List of validation error messages (empty if valid)
        """
        return []

    def _check_built(self) -> None:
        """
        Check if object has already been built.

        Raises
        ------
        RuntimeError
            If the builder has already been used
        """
        if self._built:
            raise RuntimeError("Builder has already been used to build an object")

    def _mark_built(self) -> None:
        """Mark this builder as having been used."""
        self._built = True
