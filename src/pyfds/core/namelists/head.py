"""
FDS HEAD namelist.

Simulation identification and metadata.
"""

from pydantic import field_validator

from pyfds.core.namelists.base import FdsField, NamelistBase


class Head(NamelistBase):
    """
    FDS HEAD namelist - simulation identification and metadata.

    Parameters
    ----------
    chid : str
        Case identifier (filename prefix for all output files)
    title : str, optional
        Descriptive title for the simulation

    Examples
    --------
    >>> head = Head(chid='room_fire', title='Room Fire Test')
    >>> print(head.to_fds())
    &HEAD CHID='room_fire', TITLE='Room Fire Test' /
    """

    chid: str = FdsField(..., description="Case identifier")
    title: str | None = FdsField(None, description="Simulation title")

    @field_validator("chid")
    @classmethod
    def validate_chid(cls, v: str) -> str:
        """Validate CHID FdsField."""
        if not v:
            raise ValueError("CHID cannot be empty")
        if len(v) > 60:
            raise ValueError("CHID cannot be longer than 60 characters")
        if " " in v:
            raise ValueError("CHID cannot contain spaces")
        return v

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "HEAD"
