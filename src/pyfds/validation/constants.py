"""Shared constants for validation across the pyfds validation module."""

import re

# FDS CHID constraints
CHID_MAX_LENGTH: int = 60  # FDS 6.8+ limit
CHID_PATTERN: re.Pattern[str] = re.compile(r"^[a-zA-Z0-9_-]+$")  # Valid CHID characters

# File size limits for parsing
MAX_INPUT_FILE_SIZE: int = 10 * 1024 * 1024  # 10 MB
MAX_OUTPUT_FILE_SIZE: int = 100 * 1024 * 1024  # 100 MB

__all__ = [
    "CHID_MAX_LENGTH",
    "CHID_PATTERN",
    "MAX_INPUT_FILE_SIZE",
    "MAX_OUTPUT_FILE_SIZE",
]
