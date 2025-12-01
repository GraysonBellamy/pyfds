"""FDS HEAD namelist for simulation identification and metadata.

The HEAD namelist defines the case identifier (CHID) and optional title
for the simulation.

Field Groups:
    identification: Case ID (CHID) and title
"""

from pydantic import field_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Head"]


class Head(NamelistBase):
    """FDS HEAD namelist for simulation identification.

    The HEAD namelist is required for every FDS simulation and defines
    the case identifier used for all output files.

    Parameters
    ----------
    chid : str
        Case identifier (filename prefix for all output files).
        Limited to 50 characters. Cannot contain spaces or periods.
    title : str, optional
        Descriptive title for the simulation. Limited to 256 characters.

    Notes
    -----
    CHID cannot contain spaces or periods and is limited to 50 characters.
    Output files will be named <CHID>.out, <CHID>_devc.csv, etc.

    Examples
    --------
    >>> head = Head(chid='room_fire', title='Room Fire Test')

    See Also
    --------
    Time : Simulation timing parameters.
    Misc : Global simulation settings.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "HEAD"

    # --- Identification ---
    chid: str = FdsField(..., description="Case identifier", group="identification")
    title: str | None = FdsField(None, description="Simulation title", group="identification")

    # --- Validators ---
    @field_validator("chid")
    @classmethod
    def validate_chid(cls, v: str) -> str:
        """Validate CHID field.

        CHID must be non-empty, 50 characters or less, and cannot contain
        spaces or periods.
        """
        if not v:
            raise ValueError("CHID cannot be empty")
        if len(v) > 50:
            raise ValueError("CHID cannot be longer than 50 characters")
        if " " in v:
            raise ValueError("CHID cannot contain spaces")
        if "." in v:
            raise ValueError("CHID cannot contain periods")
        return v

    @field_validator("title")
    @classmethod
    def validate_title(cls, v: str | None) -> str | None:
        """Validate TITLE field.

        TITLE must be 256 characters or less.
        """
        if v is not None and len(v) > 256:
            raise ValueError("TITLE cannot be longer than 256 characters")
        return v
