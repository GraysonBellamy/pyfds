"""All PyFDS exceptions live here. Import from here only."""

from typing import Any


class PyFDSError(Exception):
    """Base exception for all PyFDS errors."""

    pass


class ValidationError(PyFDSError):
    """Invalid input or configuration."""

    def __init__(self, message: str, FdsField: str | None = None, value: Any = None):
        self.FdsField = FdsField
        self.value = value
        super().__init__(message)


class ConfigurationError(PyFDSError):
    """Invalid simulation configuration."""

    pass


class DuplicateIdError(ValidationError):
    """Duplicate ID in registry."""

    def __init__(self, id: str, type_name: str):
        super().__init__(f"Duplicate {type_name} ID: '{id}'", FdsField="id", value=id)


class UnknownIdError(ValidationError):
    """Reference to unknown ID."""

    def __init__(self, id: str, type_name: str, available: list[str] | None = None):
        msg = f"Unknown {type_name} ID: '{id}'"
        if available:
            msg += f". Available: {available[:5]}{'...' if len(available) > 5 else ''}"
        super().__init__(msg, FdsField="id", value=id)


class ExecutionError(PyFDSError):
    """FDS execution failed."""

    def __init__(
        self,
        message: str,
        exit_code: int | None = None,
        stdout: str | None = None,
        stderr: str | None = None,
        fds_file: str | None = None,
    ):
        self.exit_code = exit_code
        self.stdout = stdout
        self.stderr = stderr
        self.fds_file = fds_file
        super().__init__(message)

    def __str__(self) -> str:
        """Format error with context information."""
        parts = [super().__str__()]

        if self.exit_code is not None:
            parts.append(f"Exit code: {self.exit_code}")

        if self.stderr:
            parts.append(f"Stderr: {self.stderr}")

        if self.fds_file:
            parts.append(f"FDS file: {self.fds_file}")

        return "\n".join(parts)


class FDSNotFoundError(ExecutionError):
    """FDS executable not found."""

    pass


class FDSTimeoutError(ExecutionError):
    """FDS execution timed out."""

    pass


class ParseError(PyFDSError):
    """Failed to parse FDS file."""

    pass


__all__ = [
    "ConfigurationError",
    "DuplicateIdError",
    "ExecutionError",
    "FDSNotFoundError",
    "FDSTimeoutError",
    "ParseError",
    "PyFDSError",
    "UnknownIdError",
    "ValidationError",
]
