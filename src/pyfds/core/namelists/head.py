"""
FDS HEAD namelist.

Simulation identification and metadata.
"""

from typing import Any

from pydantic import Field, field_validator

from pyfds.core.namelists.base import NamelistBase


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

    chid: str = Field(..., description="Case identifier")
    title: str | None = Field(None, description="Simulation title")

    @field_validator("chid")
    @classmethod
    def validate_chid(cls, v: str) -> str:
        """Validate CHID has no spaces or special characters."""
        if not v:
            raise ValueError("CHID cannot be empty")
        if " " in v:
            raise ValueError("CHID cannot contain spaces")
        if len(v) > 60:
            raise ValueError("CHID should be 60 characters or less")
        return v

    def to_fds(self) -> str:
        """Generate FDS HEAD namelist."""
        params: dict[str, Any] = {"chid": self.chid}
        if self.title:
            params["title"] = self.title
        return self._build_namelist("HEAD", params)
