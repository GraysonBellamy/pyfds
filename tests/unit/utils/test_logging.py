"""Tests for logging utilities."""

import logging

from pyfds.logging import get_logger, setup_logging


class TestSetupLogging:
    """Tests for setup_logging function."""

    def test_setup_logging_default(self):
        """Test setting up logging with defaults."""
        setup_logging()
        logger = logging.getLogger("pyfds")
        assert logger.level == logging.INFO
        assert len(logger.handlers) > 0

    def test_setup_logging_custom_level(self):
        """Test setting up logging with custom level."""
        setup_logging("DEBUG")
        logger = logging.getLogger("pyfds")
        assert logger.level == logging.DEBUG

    def test_setup_logging_with_file(self, tmp_path):
        """Test logging to file."""
        log_file = tmp_path / "test.log"
        setup_logging("INFO", str(log_file))

        logger = logging.getLogger("pyfds")
        logger.info("Test message")

        # File should exist and contain message
        assert log_file.exists()
        content = log_file.read_text()
        assert "Test message" in content


class TestGetLogger:
    """Tests for get_logger function."""

    def test_get_logger_returns_logger(self):
        """Test get_logger returns a logger instance."""
        logger = get_logger("test_module")
        assert isinstance(logger, logging.Logger)
        assert logger.name == "test_module"

    def test_get_logger_with_module_name(self):
        """Test get_logger with __name__."""
        logger = get_logger(__name__)
        assert "test_logging" in logger.name
