"""Tests for FDS execution exceptions."""

import pytest

from pyfds.execution.exceptions import (
    FDSExecutionError,
    FDSNotFoundError,
    FDSTimeoutError,
)


class TestFDSExecutionError:
    """Tests for FDSExecutionError."""

    def test_basic_error(self):
        """Test creating basic error."""
        error = FDSExecutionError("Test error")
        assert str(error) == "Test error"
        assert error.exit_code is None
        assert error.stdout is None
        assert error.stderr is None
        assert error.fds_file is None

    def test_error_with_exit_code(self):
        """Test error with exit code."""
        error = FDSExecutionError("Failed", exit_code=1)
        assert error.exit_code == 1
        assert "Exit code: 1" in str(error)

    def test_error_with_stdout_stderr(self):
        """Test error with stdout/stderr."""
        error = FDSExecutionError("Failed", stdout="Standard output", stderr="Error output")
        assert error.stdout == "Standard output"
        assert error.stderr == "Error output"
        # Note: __str__ only includes stderr, not stdout
        assert "Error output" in str(error)

    def test_error_with_fds_file(self):
        """Test error with FDS file path."""
        error = FDSExecutionError("Failed", fds_file="/path/to/test.fds")
        assert error.fds_file == "/path/to/test.fds"
        assert "test.fds" in str(error)

    def test_error_with_all_fields(self):
        """Test error with all context fields."""
        error = FDSExecutionError(
            "Simulation failed",
            exit_code=1,
            stdout="Output data",
            stderr="Error data",
            fds_file="/path/test.fds",
        )

        error_str = str(error)
        assert "Simulation failed" in error_str
        assert "Exit code: 1" in error_str
        # Note: __str__ only includes stderr, not stdout
        assert "Error data" in error_str
        assert "test.fds" in error_str

    def test_error_str_formatting(self):
        """Test error string formatting is clear and readable."""
        error = FDSExecutionError("Test error", exit_code=2, fds_file="case.fds")
        error_str = str(error)

        # Should have clear sections
        assert error_str.count("\n") >= 2  # Multi-line output
        assert "Test error" in error_str


class TestFDSTimeoutError:
    """Tests for FDSTimeoutError."""

    def test_timeout_error_inherits_execution_error(self):
        """Test FDSTimeoutError is subclass of FDSExecutionError."""
        error = FDSTimeoutError("Timeout")
        assert isinstance(error, FDSExecutionError)

    def test_timeout_error_basic(self):
        """Test creating basic timeout error."""
        error = FDSTimeoutError("Exceeded timeout")
        assert "Exceeded timeout" in str(error)

    def test_timeout_error_with_context(self):
        """Test timeout error with full context."""
        error = FDSTimeoutError(
            "Timeout after 600s",
            exit_code=None,  # Process was killed
            fds_file="long_sim.fds",
        )

        error_str = str(error)
        assert "Timeout after 600s" in error_str
        assert "long_sim.fds" in error_str


class TestFDSNotFoundError:
    """Tests for FDSNotFoundError."""

    def test_not_found_error_is_exception(self):
        """Test FDSNotFoundError is a plain Exception."""
        error = FDSNotFoundError("FDS not found")
        assert isinstance(error, Exception)

    def test_not_found_error_basic(self):
        """Test creating basic not found error."""
        error = FDSNotFoundError("FDS executable not found")
        assert "not found" in str(error).lower()


class TestExceptionHierarchy:
    """Tests for exception hierarchy and catching."""

    def test_catch_execution_errors(self):
        """Test catching execution-related errors with base class."""
        errors = [
            FDSExecutionError("Generic error"),
            FDSTimeoutError("Timeout"),
        ]

        for error in errors:
            try:
                raise error
            except FDSExecutionError:
                pass  # Should catch FDSExecutionError and FDSTimeoutError
            else:
                pytest.fail(f"Failed to catch {type(error)}")

    def test_specific_error_catching(self):
        """Test catching specific error types."""
        # Should catch timeout specifically
        try:
            raise FDSTimeoutError("Test")
        except FDSTimeoutError:
            pass
        else:
            pytest.fail("Failed to catch FDSTimeoutError")

        # Should catch not found specifically
        try:
            raise FDSNotFoundError("Test")
        except FDSNotFoundError:
            pass
        else:
            pytest.fail("Failed to catch FDSNotFoundError")

    def test_timeout_inherits_from_execution_error(self):
        """Test FDSTimeoutError inherits from FDSExecutionError."""
        error = FDSTimeoutError("Timeout")
        assert isinstance(error, FDSExecutionError)
        assert isinstance(error, Exception)

    def test_error_context_preservation(self):
        """Test that error context is preserved through raising."""
        original = ValueError("Original error")

        try:
            try:
                raise original
            except ValueError as e:
                raise FDSExecutionError("Wrapped error") from e
        except FDSExecutionError as fds_error:
            assert fds_error.__cause__ is original
            assert isinstance(fds_error.__cause__, ValueError)
