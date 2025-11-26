"""
Progress monitoring for FDS simulations.
"""

import re
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from threading import Event, Lock, Thread

from ..utils import get_logger, safe_read_text

logger = get_logger(__name__)


@dataclass
class ProgressInfo:
    """
    Container for FDS simulation progress information.

    Attributes
    ----------
    current_time : float
        Current simulation time in seconds
    end_time : float
        End time for simulation in seconds
    time_step : int
        Current time step number
    wall_clock_time : float
        Wall clock time elapsed in seconds
    percent_complete : float
        Percentage of simulation complete (0-100)
    eta_seconds : float, optional
        Estimated time remaining in seconds
    """

    current_time: float
    end_time: float
    time_step: int
    wall_clock_time: float
    percent_complete: float
    eta_seconds: float | None = None

    @property
    def is_complete(self) -> bool:
        """Check if simulation is complete."""
        return self.percent_complete >= 100.0 or self.current_time >= self.end_time


class ProgressMonitor:
    """
    Monitor FDS simulation progress by parsing output file.

    This class runs in a background thread and periodically parses the
    FDS .out file to extract progress information.

    Parameters
    ----------
    out_file : Path
        Path to FDS .out file
    poll_interval : float
        How often to check progress in seconds (default: 2.0)

    Examples
    --------
    >>> monitor = ProgressMonitor(Path("case.out"))
    >>> monitor.start()
    >>> while not monitor.is_complete():
    ...     progress = monitor.get_progress()
    ...     print(f"Progress: {progress.percent_complete:.1f}%")
    ...     time.sleep(5)
    >>> monitor.stop()
    """

    def __init__(self, out_file: Path, poll_interval: float = 2.0):
        self.out_file = out_file
        self.poll_interval = poll_interval
        self._stop_event = Event()
        self._thread: Thread | None = None
        self._progress: ProgressInfo | None = None
        self._progress_lock = Lock()  # Thread-safe access to _progress
        self._callbacks: list[Callable[[ProgressInfo], None]] = []
        logger.debug(f"Created progress monitor for: {out_file}")

    def add_callback(self, callback: Callable[[ProgressInfo], None]) -> None:
        """
        Add a callback to be called when progress updates.

        Parameters
        ----------
        callback : Callable[[ProgressInfo], None]
            Function to call with progress info
        """
        self._callbacks.append(callback)

    def start(self) -> None:
        """Start monitoring in background thread."""
        if self._thread is not None and self._thread.is_alive():
            return

        self._stop_event.clear()
        self._thread = Thread(target=self._monitor_loop, daemon=True)
        self._thread.start()

    def stop(self) -> None:
        """Stop monitoring thread."""
        self._stop_event.set()
        if self._thread is not None:
            self._thread.join(timeout=5.0)

    def get_progress(self) -> ProgressInfo | None:
        """
        Get current progress information (thread-safe).

        Returns
        -------
        ProgressInfo, optional
            Current progress, or None if not available yet
        """
        with self._progress_lock:
            return self._progress

    def is_complete(self) -> bool:
        """Check if simulation is complete."""
        if self._progress is None:
            return False
        return self._progress.is_complete

    def _monitor_loop(self) -> None:
        """Main monitoring loop (runs in background thread)."""
        while not self._stop_event.is_set():
            try:
                if self.out_file.exists():
                    self._parse_out_file()
            except Exception:
                # Ignore parsing errors during monitoring
                pass

            # Wait for next poll interval
            self._stop_event.wait(self.poll_interval)

    def _parse_out_file(self) -> None:
        """Parse the .out file for progress information."""
        try:
            # Use safe_read_text with file size limit to prevent memory issues
            content = safe_read_text(self.out_file)
            progress = self._extract_progress(content)
            if progress is not None:
                # Thread-safe update of progress
                with self._progress_lock:
                    self._progress = progress
                    logger.debug(
                        f"Progress: {progress.percent_complete:.1f}% "
                        f"(T={progress.current_time:.1f}s)"
                    )

                # Notify callbacks (outside lock to avoid deadlock)
                from contextlib import suppress

                for callback in self._callbacks:
                    with suppress(Exception):
                        # Don't let callback errors stop monitoring
                        callback(progress)
        except Exception as e:
            # File might be locked, incomplete, or too large
            logger.debug(f"Failed to parse progress: {e}")
            pass

    def _extract_progress(self, content: str) -> ProgressInfo | None:
        """
        Extract progress information from .out file content.

        Parameters
        ----------
        content : str
            Content of .out file

        Returns
        -------
        ProgressInfo, optional
            Extracted progress info, or None if not enough data
        """
        # Look for time step information
        # FDS prints lines like: "Time Step    1234  November 01,   12:34:56"
        # And: "T= 123.45 s, dt= 0.05 s"

        current_time = None
        end_time = None
        time_step = None
        wall_clock_time = None

        # Find T_END from namelist
        time_match = re.search(r"T_END\s*=\s*([\d.]+)", content)
        if time_match:
            end_time = float(time_match.group(1))

        # Find latest time step
        step_matches = re.findall(r"Time Step\s+(\d+)", content)
        if step_matches:
            time_step = int(step_matches[-1])

        # Find latest simulation time
        t_matches = re.findall(r"T=\s*([\d.]+)\s*s", content)
        if t_matches:
            current_time = float(t_matches[-1])

        # Try to extract wall clock time from timestamps
        # This is approximate - would need more sophisticated parsing
        if current_time is not None and end_time is not None and time_step is not None:
            percent = min(100.0, (current_time / end_time) * 100.0)

            # Simple ETA estimation (can be improved)
            eta = None
            if percent > 0 and wall_clock_time is not None:
                eta = (wall_clock_time / percent) * (100.0 - percent)

            return ProgressInfo(
                current_time=current_time,
                end_time=end_time,
                time_step=time_step or 0,
                wall_clock_time=wall_clock_time or 0.0,
                percent_complete=percent,
                eta_seconds=eta,
            )

        return None


def parse_out_file_for_errors(out_file: Path) -> list[str]:
    """
    Parse .out file for FDS error messages.

    Parameters
    ----------
    out_file : Path
        Path to .out file

    Returns
    -------
    List[str]
        List of error messages found

    Examples
    --------
    >>> errors = parse_out_file_for_errors(Path("case.out"))
    >>> for error in errors:
    ...     print(f"ERROR: {error}")
    """
    errors: list[str] = []

    if not out_file.exists():
        return errors

    try:
        # Use safe_read_text with file size limit
        content = safe_read_text(out_file)

        # Look for ERROR messages
        error_pattern = re.compile(r"ERROR:\s*(.+?)(?:\n|$)", re.MULTILINE)
        for match in error_pattern.finditer(content):
            errors.append(match.group(1).strip())

        # Look for STOP messages
        stop_pattern = re.compile(r"STOP:\s*(.+?)(?:\n|$)", re.MULTILINE)
        for match in stop_pattern.finditer(content):
            errors.append(f"STOP: {match.group(1).strip()}")

    except Exception as e:
        logger.warning(f"Failed to parse errors from {out_file}: {e}")
        pass

    return errors
