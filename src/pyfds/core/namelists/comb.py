"""FDS COMB namelist for combustion model parameters.

Controls global combustion behavior including extinction models,
turbulent combustion, mixing parameters, and ODE solver configuration.

Field Groups:
    extinction: Extinction model and suppression
    mixing: Turbulent mixing time scales
    thresholds: Species and temperature thresholds
    diagnostics: Diagnostic output options
    ode_solver: ODE solver configuration for chemistry integration
    chemistry: Chemistry integration parameters
"""

from pydantic import field_validator, model_validator

from pyfds.core.enums import ExtinctionModel
from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Combustion"]


class Combustion(NamelistBase):
    """FDS COMB namelist - combustion model parameters.

    Controls global combustion behavior including extinction models,
    turbulent combustion, mixing parameters, and ODE solver configuration.

    Parameters
    ----------
    extinction_model : ExtinctionModel, optional
        Extinction model ('EXTINCTION 1' or 'EXTINCTION 2').
    suppression : bool, optional
        Enable flame suppression model, default: True.
    initial_unmixed_fraction : float, optional
        Initial unmixed fraction (0-1), default: 1.0.
    tau_chem : float, optional
        Minimum bound for mixing time [s], default: 1e-5.
    tau_flame : float, optional
        Maximum bound for mixing time [s], default: 1e10.
    ode_solver : str, optional
        ODE solver for chemistry integration ('EXPLICIT EULER', 'RK2', 'RK2 RICHARDSON', 'RK3', 'CVODE').

    Examples
    --------
    >>> comb = Combustion(extinction_model=ExtinctionModel.EXTINCTION_2)

    See Also
    --------
    Reaction : Combustion reaction chemistry.
    Misc : Additional solver parameters.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "COMB"

    # --- Extinction ---
    extinction_model: ExtinctionModel | None = FdsField(
        None, description="Extinction model", group="extinction"
    )
    suppression: bool = FdsField(
        True, exclude_if=True, description="Enable flame suppression model", group="extinction"
    )
    free_burn_temperature: float = FdsField(
        600.0,
        exclude_if=600.0,
        description="Temperature above which no extinction occurs [°C]",
        group="extinction",
    )

    @field_validator("extinction_model", mode="before")
    @classmethod
    def validate_extinction_model(cls, v: str | ExtinctionModel | None) -> ExtinctionModel | None:
        """Convert extinction model to enum."""
        if v is None:
            return None
        if isinstance(v, ExtinctionModel):
            return v
        if isinstance(v, str):
            # Try exact match first
            try:
                return ExtinctionModel(v.upper())
            except ValueError:
                pass
            # Handle "EXTINCTION_1" -> "EXTINCTION 1" format
            normalized = v.upper().replace("_", " ")
            try:
                return ExtinctionModel(normalized)
            except ValueError:
                valid = [e.value for e in ExtinctionModel]
                raise ValueError(f"EXTINCTION_MODEL must be one of {valid}") from None
        return v

    # --- Turbulent combustion / mixing ---
    initial_unmixed_fraction: float = FdsField(
        1.0,
        ge=0.0,
        le=1.0,
        exclude_if=1.0,
        description="Initial unmixed fraction (0-1)",
        group="mixing",
    )
    ramp_zeta_0: str | None = FdsField(
        None, description="Ramp ID for time-varying initial unmixed fraction", group="mixing"
    )
    fixed_mix_time: float | None = FdsField(
        None, gt=0, description="Fixed mixing time [s]", group="mixing"
    )
    tau_chem: float = FdsField(
        1e-5,
        exclude_if=1e-5,
        gt=0,
        description="Minimum mixing time bound [s]",
        group="mixing",
    )
    tau_flame: float = FdsField(
        1e10,
        exclude_if=1e10,
        gt=0,
        description="Maximum mixing time bound [s]",
        group="mixing",
    )

    # --- Species/reaction thresholds ---
    zz_min_global: float = FdsField(
        1e-10,
        exclude_if=1e-10,
        description="Minimum species mass fraction for reactions",
        group="thresholds",
    )
    finite_rate_min_temp: float = FdsField(
        -273.15,
        exclude_if=-273.15,
        description="Minimum temperature for finite-rate reactions [°C]",
        group="thresholds",
    )

    # --- Diagnostics ---
    compute_adiabatic_flame_temperature: bool = FdsField(
        False,
        exclude_if=False,
        description="Compute and report adiabatic flame temperature",
        group="diagnostics",
    )
    check_realizability: bool = FdsField(
        False,
        exclude_if=False,
        description="Check species mass fractions remain between 0 and 1",
        group="diagnostics",
    )

    # --- ODE Solver Configuration ---
    ode_solver: str | None = FdsField(
        None,
        description="ODE solver for chemistry ('EXPLICIT EULER', 'RK2', 'RK2 RICHARDSON', 'RK3', 'CVODE')",
        group="ode_solver",
    )
    ode_min_atol: float | None = FdsField(
        None, gt=0, description="Minimum absolute tolerance for ODE solver", group="ode_solver"
    )
    ode_rel_error: float | None = FdsField(
        None, gt=0, description="Relative error tolerance for ODE solver", group="ode_solver"
    )
    max_chemistry_substeps: int = FdsField(
        20,
        exclude_if=20,
        ge=1,
        description="Maximum chemistry substeps per time step",
        group="ode_solver",
    )
    n_fixed_chemistry_substeps: int = FdsField(
        -1,
        exclude_if=-1,
        description="Fixed number of chemistry substeps (-1 for auto)",
        group="ode_solver",
    )

    # --- Chemistry Integration Parameters ---
    equiv_ratio_check: bool = FdsField(
        True,
        exclude_if=True,
        description="Only compute chemistry within equivalence ratio bounds",
        group="chemistry",
    )
    min_equiv_ratio: float = FdsField(
        0.2,
        exclude_if=0.2,
        ge=0,
        description="Minimum equivalence ratio for chemistry calculation",
        group="chemistry",
    )
    max_equiv_ratio: float = FdsField(
        10.0,
        exclude_if=10.0,
        gt=0,
        description="Maximum equivalence ratio for chemistry calculation",
        group="chemistry",
    )
    do_chem_load_balance: bool = FdsField(
        False,
        exclude_if=False,
        description="Distribute chemistry load across MPI processes",
        group="chemistry",
    )

    @model_validator(mode="after")
    def validate_combustion(self) -> "Combustion":
        """Validate combustion parameters."""
        # TAU_CHEM must be <= TAU_FLAME
        if self.tau_chem > self.tau_flame:
            raise ValueError("TAU_CHEM must be <= TAU_FLAME")

        # MIN_EQUIV_RATIO must be <= MAX_EQUIV_RATIO
        if self.min_equiv_ratio > self.max_equiv_ratio:
            raise ValueError("MIN_EQUIV_RATIO must be <= MAX_EQUIV_RATIO")

        # Validate ODE_SOLVER values
        valid_ode_solvers = ["EXPLICIT EULER", "RK2", "RK2 RICHARDSON", "RK3", "CVODE"]
        if self.ode_solver is not None and self.ode_solver.upper() not in valid_ode_solvers:
            raise ValueError(f"ODE_SOLVER must be one of {valid_ode_solvers}")

        return self
