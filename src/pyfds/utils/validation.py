"""Input validation and sanitization utilities."""

import re
from pathlib import Path

# Maximum file size for reading output files (100 MB)
MAX_FILE_SIZE_BYTES = 100 * 1024 * 1024

# Maximum CHID length (FDS limitation)
MAX_CHID_LENGTH = 60

# Valid CHID pattern (alphanumeric, underscores, hyphens)
CHID_PATTERN = re.compile(r"^[a-zA-Z0-9_-]+$")


class ValidationError(ValueError):
    """Raised when input validation fails."""

    pass


def validate_chid(chid: str) -> str:
    """
    Validate and sanitize a CHID (case identifier).

    CHID must be:
    - Non-empty
    - 60 characters or less
    - Contain only letters, numbers, underscores, and hyphens
    - Not contain path separators or path traversal sequences

    Parameters
    ----------
    chid : str
        Case identifier to validate

    Returns
    -------
    str
        Validated CHID (unchanged if valid)

    Raises
    ------
    ValidationError
        If CHID is invalid

    Examples
    --------
    >>> validate_chid("my_simulation")
    'my_simulation'
    >>> validate_chid("test-case-123")
    'test-case-123'
    >>> validate_chid("invalid/path")  # doctest: +SKIP
    ValidationError: CHID cannot contain path separators
    """
    if not chid:
        raise ValidationError("CHID cannot be empty")

    if not isinstance(chid, str):
        raise ValidationError(f"CHID must be a string, got {type(chid).__name__}")

    # Check length
    if len(chid) > MAX_CHID_LENGTH:
        raise ValidationError(
            f"CHID must be {MAX_CHID_LENGTH} characters or less, got {len(chid)} characters"
        )

    # Check for path separators and path traversal
    if ".." in chid or "/" in chid or "\\" in chid:
        raise ValidationError(f"CHID cannot contain path separators or '..' sequences: '{chid}'")

    # Check for invalid characters
    if not CHID_PATTERN.match(chid):
        raise ValidationError(
            f"CHID can only contain letters, numbers, underscores, and hyphens. "
            f"Invalid CHID: '{chid}'"
        )

    return chid


def validate_path(
    path: str | Path,
    must_exist: bool = False,
    must_be_file: bool = False,
    must_be_dir: bool = False,
    allow_create: bool = True,
) -> Path:
    """
    Validate and resolve a file system path.

    Parameters
    ----------
    path : str or Path
        Path to validate
    must_exist : bool, optional
        If True, path must already exist, by default False
    must_be_file : bool, optional
        If True, path must be a file (not a directory), by default False
    must_be_dir : bool, optional
        If True, path must be a directory (not a file), by default False
    allow_create : bool, optional
        If True, allow paths that don't exist yet (for output files), by default True

    Returns
    -------
    Path
        Validated, resolved absolute path

    Raises
    ------
    ValidationError
        If path validation fails

    Examples
    --------
    >>> validate_path("/tmp/test.txt")  # doctest: +SKIP
    Path('/tmp/test.txt')

    >>> validate_path("output", must_be_dir=True)  # doctest: +SKIP
    Path('/current/dir/output')
    """
    if not isinstance(path, (str, Path)):
        raise ValidationError(f"Path must be string or Path, got {type(path).__name__}")

    try:
        path = Path(path)
    except (TypeError, ValueError) as e:
        raise ValidationError(f"Invalid path: {e}") from e

    # Resolve to absolute path
    try:
        resolved = path.resolve()
    except (OSError, RuntimeError) as e:
        raise ValidationError(f"Cannot resolve path '{path}': {e}") from e

    # Check existence
    if must_exist and not resolved.exists():
        raise ValidationError(f"Path does not exist: {resolved}")

    if not allow_create and not resolved.exists():
        raise ValidationError(f"Path does not exist: {resolved}")

    # Check type constraints
    if resolved.exists():
        if must_be_file and not resolved.is_file():
            raise ValidationError(f"Path is not a file: {resolved}")
        if must_be_dir and not resolved.is_dir():
            raise ValidationError(f"Path is not a directory: {resolved}")

    # Check parent directory exists (for new files)
    if not resolved.exists() and allow_create and not resolved.parent.exists():
        raise ValidationError(f"Parent directory does not exist: {resolved.parent}")

    return resolved


def validate_file_size(file_path: Path, max_size: int = MAX_FILE_SIZE_BYTES) -> Path:
    """
    Validate that a file is not too large to read safely.

    Parameters
    ----------
    file_path : Path
        Path to file to check
    max_size : int, optional
        Maximum allowed file size in bytes, by default MAX_FILE_SIZE_BYTES (100 MB)

    Returns
    -------
    Path
        The file path (unchanged if valid)

    Raises
    ------
    ValidationError
        If file is too large

    Examples
    --------
    >>> validate_file_size(Path("small_file.txt"))  # doctest: +SKIP
    Path('small_file.txt')
    """
    if not file_path.exists():
        raise ValidationError(f"File does not exist: {file_path}")

    if not file_path.is_file():
        raise ValidationError(f"Path is not a file: {file_path}")

    file_size = file_path.stat().st_size

    if file_size > max_size:
        size_mb = file_size / (1024 * 1024)
        max_mb = max_size / (1024 * 1024)
        raise ValidationError(
            f"File is too large to read: {file_path}\n"
            f"File size: {size_mb:.1f} MB\n"
            f"Maximum allowed: {max_mb:.1f} MB"
        )

    return file_path


def validate_positive_number(value: int | float, name: str = "value") -> int | float:
    """
    Validate that a number is positive (> 0).

    Parameters
    ----------
    value : int or float
        Number to validate
    name : str, optional
        Name of the parameter (for error messages), by default "value"

    Returns
    -------
    int or float
        The value (unchanged if valid)

    Raises
    ------
    ValidationError
        If value is not positive

    Examples
    --------
    >>> validate_positive_number(5, "count")
    5
    >>> validate_positive_number(0, "count")  # doctest: +SKIP
    ValidationError: count must be positive, got 0
    """
    if not isinstance(value, (int, float)):
        raise ValidationError(f"{name} must be a number, got {type(value).__name__}")

    if value <= 0:
        raise ValidationError(f"{name} must be positive, got {value}")

    return value


def validate_non_negative_number(value: int | float, name: str = "value") -> int | float:
    """
    Validate that a number is non-negative (>= 0).

    Parameters
    ----------
    value : int or float
        Number to validate
    name : str, optional
        Name of the parameter (for error messages), by default "value"

    Returns
    -------
    int or float
        The value (unchanged if valid)

    Raises
    ------
    ValidationError
        If value is negative

    Examples
    --------
    >>> validate_non_negative_number(0, "time")
    0
    >>> validate_non_negative_number(-1, "time")  # doctest: +SKIP
    ValidationError: time must be non-negative, got -1
    """
    if not isinstance(value, (int, float)):
        raise ValidationError(f"{name} must be a number, got {type(value).__name__}")

    if value < 0:
        raise ValidationError(f"{name} must be non-negative, got {value}")

    return value


def safe_read_text(
    file_path: Path, max_size: int = MAX_FILE_SIZE_BYTES, encoding: str = "utf-8"
) -> str:
    """
    Safely read a text file with size validation.

    Parameters
    ----------
    file_path : Path
        Path to file to read
    max_size : int, optional
        Maximum allowed file size in bytes, by default MAX_FILE_SIZE_BYTES
    encoding : str, optional
        Text encoding, by default "utf-8"

    Returns
    -------
    str
        File contents

    Raises
    ------
    ValidationError
        If file is too large or cannot be read

    Examples
    --------
    >>> safe_read_text(Path("config.txt"))  # doctest: +SKIP
    'file contents here'
    """
    # Validate file exists and size
    validate_file_size(file_path, max_size)

    try:
        return file_path.read_text(encoding=encoding)
    except UnicodeDecodeError as e:
        raise ValidationError(f"File is not valid {encoding} text: {file_path}") from e
    except OSError as e:
        raise ValidationError(f"Cannot read file {file_path}: {e}") from e
