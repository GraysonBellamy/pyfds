"""Logging configuration for PyFDS."""

import logging
import sys
from pathlib import Path
from typing import Literal

# Default log format
DEFAULT_FORMAT = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
DEFAULT_DATE_FORMAT = "%Y-%m-%d %H:%M:%S"

# Log level type
LogLevel = Literal["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]


def setup_logger(
    name: str = "pyfds",
    level: str | int = logging.INFO,
    log_file: Path | str | None = None,
    format_string: str = DEFAULT_FORMAT,
    date_format: str = DEFAULT_DATE_FORMAT,
) -> logging.Logger:
    """
    Set up a logger with consistent formatting.

    Parameters
    ----------
    name : str, optional
        Logger name, by default "pyfds"
    level : str or int, optional
        Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL or logging.DEBUG, etc.),
        by default logging.INFO
    log_file : Path or str, optional
        Path to log file. If None, only logs to console, by default None
    format_string : str, optional
        Log message format string, by default DEFAULT_FORMAT
    date_format : str, optional
        Date format string, by default DEFAULT_DATE_FORMAT

    Returns
    -------
    logging.Logger
        Configured logger instance

    Examples
    --------
    >>> logger = setup_logger("pyfds.simulation", level="DEBUG")
    >>> logger.info("Starting simulation")
    >>> logger.debug("Detailed debug information")

    >>> # With file logging
    >>> logger = setup_logger("pyfds", log_file="pyfds.log")
    >>> logger.error("An error occurred")
    """
    logger = logging.getLogger(name)

    # Convert string level to int if needed
    if isinstance(level, str):
        level = getattr(logging, level.upper())

    logger.setLevel(level)

    # Remove existing handlers to avoid duplicates
    logger.handlers.clear()

    # Create formatter
    formatter = logging.Formatter(format_string, datefmt=date_format)

    # Console handler
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setLevel(level)
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)

    # File handler (optional)
    if log_file is not None:
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(level)
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)

    # Prevent propagation to root logger
    logger.propagate = False

    return logger


def get_logger(name: str) -> logging.Logger:
    """
    Get a logger instance for a module.

    This is a convenience function that returns a logger with the standard
    naming convention: 'pyfds.module_name'.

    Parameters
    ----------
    name : str
        Module name (typically __name__)

    Returns
    -------
    logging.Logger
        Logger instance

    Examples
    --------
    >>> # In a module file
    >>> logger = get_logger(__name__)
    >>> logger.info("Module initialized")
    """
    return logging.getLogger(name)


def set_log_level(level: str | int, logger_name: str = "pyfds") -> None:
    """
    Set the log level for a logger and all its handlers.

    Parameters
    ----------
    level : str or int
        New log level (DEBUG, INFO, WARNING, ERROR, CRITICAL or logging constant)
    logger_name : str, optional
        Name of logger to modify, by default "pyfds"

    Examples
    --------
    >>> set_log_level("DEBUG")
    >>> set_log_level(logging.WARNING, "pyfds.execution")
    """
    if isinstance(level, str):
        level = getattr(logging, level.upper())

    logger = logging.getLogger(logger_name)
    logger.setLevel(level)

    # Update all handlers
    for handler in logger.handlers:
        handler.setLevel(level)


# Initialize the root pyfds logger when module is imported
_root_logger = setup_logger("pyfds", level=logging.INFO)
