"""Tests for logging utilities."""

import logging

from pyfds.utils.logging import get_logger, set_log_level, setup_logger


class TestSetupLogger:
    """Tests for setup_logger function."""

    def test_default_logger(self):
        """Test creating logger with defaults."""
        logger = setup_logger("test_default")
        assert logger.name == "test_default"
        assert logger.level == logging.INFO
        assert len(logger.handlers) > 0

    def test_custom_level_string(self):
        """Test setting level with string."""
        logger = setup_logger("test_debug", level="DEBUG")
        assert logger.level == logging.DEBUG

        logger = setup_logger("test_warning", level="WARNING")
        assert logger.level == logging.WARNING

    def test_custom_level_int(self):
        """Test setting level with integer."""
        logger = setup_logger("test_int", level=logging.ERROR)
        assert logger.level == logging.ERROR

    def test_file_logging(self, tmp_path):
        """Test logging to file."""
        log_file = tmp_path / "test.log"
        logger = setup_logger("test_file", log_file=log_file)

        logger.info("Test message")

        # File should exist and contain message
        assert log_file.exists()
        content = log_file.read_text()
        assert "Test message" in content
        assert "test_file" in content

    def test_no_propagation(self):
        """Test logger doesn't propagate to root."""
        logger = setup_logger("test_no_prop")
        assert logger.propagate is False

    def test_handlers_cleared_on_setup(self):
        """Test that handlers are cleared when re-setting up same logger."""
        logger = setup_logger("test_clear")
        initial_handlers = len(logger.handlers)

        # Setup again
        logger = setup_logger("test_clear")
        assert len(logger.handlers) == initial_handlers  # Should be same, not doubled


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


class TestSetLogLevel:
    """Tests for set_log_level function."""

    def test_set_level_string(self):
        """Test setting level with string."""
        logger = setup_logger("test_set_level")
        set_log_level("WARNING", "test_set_level")

        assert logger.level == logging.WARNING
        for handler in logger.handlers:
            assert handler.level == logging.WARNING

    def test_set_level_int(self):
        """Test setting level with integer."""
        logger = setup_logger("test_set_int")
        set_log_level(logging.ERROR, "test_set_int")

        assert logger.level == logging.ERROR

    def test_set_root_pyfds_level(self):
        """Test setting root pyfds logger level."""
        set_log_level("DEBUG", "pyfds")
        logger = logging.getLogger("pyfds")
        assert logger.level == logging.DEBUG
