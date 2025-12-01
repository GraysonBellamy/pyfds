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
    ...     def _validate(self) -> list[str]:
    ...         errors = []
    ...         if self._value is None:
    ...             errors.append("Value is required")
    ...         return errors
    ...
    ...     def _create(self) -> MyClass:
    ...         return MyClass(self._value)
    """

    def __init__(self) -> None:
        """Initialize the builder."""
        self._built = False

    def build(self) -> T:
        """
        Build and return the final object.

        Validates the builder state and creates the object.
        Override _validate() to add custom validation logic.
        Override _create() to implement object creation.

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
        self._check_built()

        # Run validation before building
        errors = self._validate()
        if errors:
            raise ValueError(
                f"Validation failed for {self.__class__.__name__}:\n"
                + "\n".join(f"  - {e}" for e in errors)
            )

        self._mark_built()
        return self._create()

    @abstractmethod
    def _create(self) -> T:
        """
        Create the target object.

        Subclasses must implement this method to create and return
        the target object. Called after validation passes.

        Returns
        -------
        T
            The constructed object
        """
        pass

    def _validate(self) -> list[str]:
        """
        Validate current builder state.

        Subclasses can override this to provide custom validation logic.
        Return an empty list if validation passes.

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
