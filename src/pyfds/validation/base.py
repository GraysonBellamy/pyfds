"""Base classes for validation system."""

from collections.abc import Iterator
from dataclasses import dataclass

from pyfds.core.enums import Severity


@dataclass(frozen=True)
class Issue:
    """A validation issue.

    Attributes
    ----------
    severity : Severity
        Issue severity level (ERROR, WARNING, INFO)
    message : str
        Human-readable description of the issue
    namelist : str, optional
        FDS namelist name where issue was found
    field : str, optional
        Field name where issue was found
    """

    severity: Severity
    message: str
    namelist: str | None = None
    field: str | None = None

    def __str__(self) -> str:
        prefix = f"[{self.severity.value.upper()}]"
        location = ""
        if self.namelist:
            location = f" {self.namelist}"
            if self.field:
                location += f".{self.field}"
        return f"{prefix}{location}: {self.message}"


@dataclass
class ValidationResult:
    """Result of a validation operation.

    Attributes
    ----------
    issues : list[Issue]
        All validation issues found
    """

    issues: list[Issue]

    @property
    def errors(self) -> list[Issue]:
        """Get only ERROR-level issues."""
        return [i for i in self.issues if i.severity == Severity.ERROR]

    @property
    def warnings(self) -> list[Issue]:
        """Get only WARNING-level issues."""
        return [i for i in self.issues if i.severity == Severity.WARNING]

    @property
    def infos(self) -> list[Issue]:
        """Get only INFO-level issues."""
        return [i for i in self.issues if i.severity == Severity.INFO]

    @property
    def is_valid(self) -> bool:
        """True if no ERROR-level issues exist."""
        return len(self.errors) == 0

    def __bool__(self) -> bool:
        """True if no issues at all."""
        return len(self.issues) == 0

    def __len__(self) -> int:
        """Return number of issues."""
        return len(self.issues)

    def __iter__(self) -> Iterator[Issue]:
        """Iterate over issues."""
        return iter(self.issues)


__all__ = ["Issue", "ValidationResult"]
