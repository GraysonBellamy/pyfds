"""
Exceptions for FDS execution module.
"""


class FDSExecutionError(Exception):
    """
    Raised when FDS execution fails.

    Attributes
    ----------
    exit_code : int
        Process exit code
    stdout : str
        Standard output from FDS
    stderr : str
        Standard error from FDS
    fds_file : str
        Path to the FDS input file
    """

    def __init__(
        self,
        message: str,
        exit_code: int | None = None,
        stdout: str | None = None,
        stderr: str | None = None,
        fds_file: str | None = None,
    ):
        super().__init__(message)
        self.exit_code = exit_code
        self.stdout = stdout
        self.stderr = stderr
        self.fds_file = fds_file

    def __str__(self) -> str:
        """Format error message with context."""
        parts = [super().__str__()]

        if self.fds_file:
            parts.append(f"FDS file: {self.fds_file}")

        if self.exit_code is not None:
            parts.append(f"Exit code: {self.exit_code}")

        if self.stderr:
            parts.append(f"Error output:\n{self.stderr}")

        return "\n".join(parts)


class FDSNotFoundError(Exception):
    """Raised when FDS executable cannot be found."""

    pass


class FDSTimeoutError(FDSExecutionError):
    """Raised when FDS execution exceeds timeout."""

    pass
