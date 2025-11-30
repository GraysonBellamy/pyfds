"""
Main Simulation class for creating and managing FDS simulations.

This module provides the high-level API for creating FDS simulations
programmatically in Python.
"""

from pathlib import Path
from typing import TYPE_CHECKING, Any

from pyfds.core.registry import SimulationRegistry
from pyfds.core.registry_view import RegistryView
from pyfds.validation import Validator

from ..utils import get_logger, validate_chid
from .namelists import (
    Combustion,
    Ctrl,
    Device,
    Head,
    Hole,
    Init,
    Material,
    Mesh,
    Misc,
    Mult,
    NamelistBase,
    Obstruction,
    Prop,
    Ramp,
    Reaction,
    Species,
    Surface,
    Time,
    Vent,
)

if TYPE_CHECKING:
    from ..analysis.results import Results
    from ..config import RunConfig
    from ..execution.runner import Job

logger = get_logger(__name__)


class Simulation:
    """
    Main class for building FDS simulations.

    This class mirrors the structure of an FDS input file, which consists
    of namelist groups like MESH, SURF, MATL, etc. All components are added
    using the unified `add()` method.

    Parameters
    ----------
    chid : str
        Case identifier (filename prefix for all output files).
        Must be 50 characters or less, no spaces or periods (FDS requirement).
    title : str, optional
        Descriptive title for the simulation (256 characters max).
    eager_validation : bool, optional
        If True, validate cross-references when items are added.
        If False (default), validation only occurs at write time.

    Attributes
    ----------
    meshes : RegistryView[Mesh]
        Read-only view of registered meshes
    surfaces : RegistryView[Surface]
        Read-only view of registered surfaces
    materials : RegistryView[Material]
        Read-only view of registered materials
    obstructions : RegistryView[Obstruction]
        Read-only view of registered obstructions
    vents : RegistryView[Vent]
        Read-only view of registered vents
    holes : RegistryView[Hole]
        Read-only view of registered holes
    devices : RegistryView[Device]
        Read-only view of registered devices
    props : RegistryView[Prop]
        Read-only view of registered properties
    ctrls : RegistryView[Ctrl]
        Read-only view of registered controls
    species : RegistryView[Species]
        Read-only view of registered species
    reactions : RegistryView[Reaction]
        Read-only view of registered reactions
    ramps : RegistryView[Ramp]
        Read-only view of registered ramps
    mults : RegistryView[Mult]
        Read-only view of registered multipliers
    inits : RegistryView[Init]
        Read-only view of registered initial conditions

    Examples
    --------
    Create a basic room fire simulation:

    >>> from pyfds import Simulation
    >>> from pyfds.core.geometry import Bounds3D, Grid3D
    >>> from pyfds.core.namelists import Time, Mesh, Surface, Obstruction
    >>>
    >>> sim = Simulation(chid='room_fire', title='Simple Room Fire')
    >>> sim.add(
    ...     Time(t_end=600.0),
    ...     Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)),
    ...     Surface(id='FIRE', hrrpua=1000.0, color='RED'),
    ...     Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')
    ... )
    >>> sim.write('room_fire.fds')

    Notes
    -----
    This class uses a unified registry system to ensure all IDs are unique
    across the entire simulation, matching FDS behavior. Validation is
    performed automatically before writing or running simulations.
    """

    def __init__(
        self,
        chid: str,
        title: str | None = None,
        eager_validation: bool = False,
    ):
        """Initialize a new FDS simulation."""
        # Validate CHID
        chid = validate_chid(chid)
        logger.debug(f"Creating simulation with CHID: {chid}")

        # Initialize registry
        self._registry = SimulationRegistry()
        self._eager_validation = eager_validation

        # Core simulation metadata
        self._head = Head(chid=chid, title=title)
        self._time: Time | None = None

        # Register head in registry
        self._registry.head = self._head

        # Initialize validator
        self._validator = Validator(self)

    # =========================================================================
    # Read-only Registry Views
    # =========================================================================

    @property
    def meshes(self) -> RegistryView[Mesh]:
        """Read-only view of registered meshes."""
        return RegistryView(self._registry.meshes)

    @property
    def surfaces(self) -> RegistryView[Surface]:
        """Read-only view of registered surfaces."""
        return RegistryView(self._registry.surfaces)

    @property
    def materials(self) -> RegistryView[Material]:
        """Read-only view of registered materials."""
        return RegistryView(self._registry.materials)

    @property
    def obstructions(self) -> RegistryView[Obstruction]:
        """Read-only view of registered obstructions."""
        return RegistryView(self._registry.obstructions)

    @property
    def vents(self) -> RegistryView[Vent]:
        """Read-only view of registered vents."""
        return RegistryView(self._registry.vents)

    @property
    def holes(self) -> RegistryView[Hole]:
        """Read-only view of registered holes."""
        return RegistryView(self._registry.holes)

    @property
    def devices(self) -> RegistryView[Device]:
        """Read-only view of registered devices."""
        return RegistryView(self._registry.devices)

    @property
    def props(self) -> RegistryView[Prop]:
        """Read-only view of registered properties."""
        return RegistryView(self._registry.props)

    @property
    def ctrls(self) -> RegistryView[Ctrl]:
        """Read-only view of registered controls."""
        return RegistryView(self._registry.ctrls)

    @property
    def species(self) -> RegistryView[Species]:
        """Read-only view of registered species."""
        return RegistryView(self._registry.species)

    @property
    def reactions(self) -> RegistryView[Reaction]:
        """Read-only view of registered reactions."""
        return RegistryView(self._registry.reactions)

    @property
    def ramps(self) -> RegistryView[Ramp]:
        """Read-only view of registered ramps."""
        return RegistryView(self._registry.ramps)

    @property
    def mults(self) -> RegistryView[Mult]:
        """Read-only view of registered multipliers."""
        return RegistryView(self._registry.mults)

    @property
    def inits(self) -> RegistryView[Init]:
        """Read-only view of registered initial conditions."""
        return RegistryView(self._registry.inits)

    # =========================================================================
    # Singleton Accessors
    # =========================================================================

    @property
    def head(self) -> Head:
        """Get the HEAD namelist."""
        return self._head

    @property
    def time_config(self) -> Time | None:
        """Get the TIME namelist."""
        return self._time

    @property
    def misc_config(self) -> Misc | None:
        """Get the MISC namelist."""
        return self._registry.misc

    @property
    def combustion_config(self) -> Combustion | None:
        """Get the COMB namelist."""
        return self._registry.combustion

    @property
    def chid(self) -> str:
        """Get the case identifier."""
        return self._head.chid

    # =========================================================================
    # Unified Add Method
    # =========================================================================

    def add(self, *items: NamelistBase) -> "Simulation":
        """
        Add one or more namelist objects to the simulation.

        This is the unified API for adding any type of namelist to the simulation.
        The method automatically routes each item to the appropriate registry.

        Parameters
        ----------
        *items : NamelistBase
            One or more namelist objects to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.add(
        ...     Time(t_end=600.0),
        ...     Mesh(ijk=Grid3D.of(50, 50, 25), xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5)),
        ...     Surface(id='FIRE', hrrpua=1000.0)
        ... )

        >>> # Method chaining
        >>> sim.add(mesh).add(surface).add(obstruction)

        Raises
        ------
        TypeError
            If an item is not a recognized namelist type
        ValueError
            If eager_validation is True and cross-references are invalid
        """
        for item in items:
            self._add_item(item)
        return self

    def _add_item(self, item: NamelistBase) -> None:
        """Route a single item to the appropriate registry."""
        # Handle special singleton cases first
        if isinstance(item, Time):
            if self._time is not None:
                raise ValueError("TIME namelist already set")
            self._time = item
            self._registry.time = item
            return

        if isinstance(item, Misc):
            if self._registry.misc is not None:
                raise ValueError("MISC namelist already set")
            self._registry.misc = item
            return

        if isinstance(item, Combustion):
            if self._registry.combustion is not None:
                raise ValueError("COMB namelist already set")
            self._registry.combustion = item
            return

        if isinstance(item, Head):
            raise ValueError("HEAD namelist is set automatically via chid parameter")

        # Register in the unified registry
        try:
            self._registry.register(item)
        except TypeError as e:
            raise TypeError(f"Unknown namelist type: {type(item).__name__}") from e

        # Eager validation if enabled
        if self._eager_validation:
            self._validate_item_references(item)

    def _validate_item_references(self, item: NamelistBase) -> None:
        """Validate cross-references for a single item (eager validation)."""
        builtin_surfaces = {"INERT", "OPEN", "MIRROR", "PERIODIC"}

        # Validate SURF_ID references
        if isinstance(item, (Obstruction, Vent)):
            for attr in ["surf_id", "surf_id_top", "surf_id_bottom", "surf_id_sides"]:
                surf_id = getattr(item, attr, None)
                if (
                    surf_id
                    and surf_id not in builtin_surfaces
                    and surf_id not in self._registry.surfaces
                ):
                    item_id = getattr(item, "id", "unnamed")
                    raise ValueError(
                        f"{type(item).__name__} '{item_id}' references undefined SURF '{surf_id}'"
                    )

        # Validate MATL_ID references
        if isinstance(item, Surface) and getattr(item, "matl_id", None):
            matl_id = item.matl_id
            matl_ids = matl_id if isinstance(matl_id, list) else [matl_id]
            for mid in matl_ids:
                if isinstance(mid, list):
                    for m in mid:
                        if m and m not in self._registry.materials:
                            raise ValueError(f"SURF '{item.id}' references undefined MATL '{m}'")
                elif mid and mid not in self._registry.materials:
                    raise ValueError(f"SURF '{item.id}' references undefined MATL '{mid}'")

        # Validate PROP_ID references
        if isinstance(item, Device) and item.prop_id and item.prop_id not in self._registry.props:
            raise ValueError(f"DEVC '{item.id}' references undefined PROP '{item.prop_id}'")

        # Validate CTRL_ID references
        if isinstance(item, Device) and item.ctrl_id and item.ctrl_id not in self._registry.ctrls:
            raise ValueError(f"DEVC '{item.id}' references undefined CTRL '{item.ctrl_id}'")

    # =========================================================================
    # Convenience Methods for Singletons
    # =========================================================================

    def set_time(
        self,
        t_end: float,
        t_begin: float = 0.0,
        dt: float | None = None,
        wall_clock_time: float | None = None,
    ) -> "Simulation":
        """
        Set time parameters for the simulation.

        Parameters
        ----------
        t_end : float
            End time for the simulation in seconds
        t_begin : float, optional
            Start time in seconds (default: 0.0)
        dt : float, optional
            Initial time step in seconds
        wall_clock_time : float, optional
            Maximum wall clock time in seconds

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.set_time(t_end=600.0, dt=0.1)
        """
        time_obj = Time(t_end=t_end, t_begin=t_begin, dt=dt, wall_clock_time=wall_clock_time)
        self.add(time_obj)
        return self

    def set_misc(self, misc: Misc | None = None, **kwargs: Any) -> "Simulation":
        """
        Set MISC parameters for the simulation.

        Can be called with a Misc object or with keyword arguments to create one.

        Parameters
        ----------
        misc : Misc, optional
            Misc object to set (if None, kwargs are used to create one)
        **kwargs
            Keyword arguments to pass to Misc constructor

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Using a Misc object
        >>> misc = Misc(tmpa=25.0, humidity=70.0)
        >>> sim.set_misc(misc)

        >>> # Using keyword arguments
        >>> sim.set_misc(tmpa=25.0, humidity=70.0, solid_phase_only=True)

        Notes
        -----
        Only one MISC namelist is allowed per simulation. Calling this method
        multiple times will raise an error.
        """
        if misc is None:
            misc = Misc(**kwargs)
        self.add(misc)
        return self

    def set_combustion(self, combustion: Combustion | None = None, **kwargs: Any) -> "Simulation":
        """
        Set COMB parameters for the simulation.

        Can be called with a Combustion object or with keyword arguments to create one.

        Parameters
        ----------
        combustion : Combustion, optional
            Combustion object to set (if None, kwargs are used to create one)
        **kwargs
            Keyword arguments to pass to Combustion constructor

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Enable extinction model 2
        >>> sim.set_combustion(extinction_model='EXTINCTION 2')

        >>> # Premixed combustion
        >>> sim.set_combustion(initial_unmixed_fraction=0.0)

        Notes
        -----
        Only one COMB namelist is allowed per simulation. Calling this method
        multiple times will raise an error.
        """
        if combustion is None:
            combustion = Combustion(**kwargs)
        self.add(combustion)
        return self

    # =========================================================================
    # Output Generation
    # =========================================================================

    def to_fds(self) -> str:
        """
        Generate the complete FDS input file content.

        Returns
        -------
        str
            FDS input file content

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.add(Time(t_end=100.0))
        >>> sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        >>> content = sim.to_fds()
        """
        lines = []

        # Add header comment
        lines.append("! FDS input file generated by PyFDS")
        if self._head.title:
            lines.append(f"! {self._head.title}")
        lines.append("")

        # HEAD namelist
        lines.append(self._head.to_fds())

        # TIME namelist
        if self._time:
            lines.append(self._time.to_fds())

        # MISC namelist (should appear near top)
        if self._registry.misc:
            lines.append("! --- Miscellaneous Parameters ---")
            lines.append(self._registry.misc.to_fds())

        # MESH namelists
        if self.meshes:
            lines.append("! --- Meshes ---")
            for mesh in self.meshes:
                lines.append(mesh.to_fds())

        # MULT namelists (must come before objects that reference them)
        if self.mults:
            lines.append("! --- Multipliers ---")
            for mult in self.mults:
                lines.append(mult.to_fds())

        # RAMP namelists (must come before MATL that reference them)
        if self.ramps:
            lines.append("! --- Ramps ---")
            for ramp in self.ramps:
                lines.append(ramp.to_fds())

        # SPEC namelists (after RAMP, before REAC)
        if self.species:
            lines.append("! --- Species ---")
            for spec in self.species:
                lines.append(spec.to_fds())

        # REAC namelists (allow multiple REAC entries)
        if self.reactions:
            lines.append("! --- Reactions ---")
            for reac in self.reactions:
                lines.append(reac.to_fds())

        # COMB namelist (after REAC)
        if self._registry.combustion:
            lines.append("! --- Combustion Parameters ---")
            lines.append(self._registry.combustion.to_fds())

        # MATL namelists
        if self.materials:
            lines.append("! --- Materials ---")
            for material in self.materials:
                lines.append(material.to_fds())

        # SURF namelists
        if self.surfaces:
            lines.append("! --- Surfaces ---")
            for surface in self.surfaces:
                lines.append(surface.to_fds())

        # OBST namelists
        if self.obstructions:
            lines.append("! --- Obstructions ---")
            for obst in self.obstructions:
                lines.append(obst.to_fds())

        # HOLE namelists (must come after OBST)
        if self.holes:
            lines.append("! --- Holes ---")
            for hole in self.holes:
                lines.append(hole.to_fds())

        # VENT namelists (after OBST and SURF)
        if self.vents:
            lines.append("! --- Vents ---")
            for vent in self.vents:
                lines.append(vent.to_fds())

        # PROP namelists
        if self.props:
            lines.append("! --- Device Properties ---")
            for prop in self.props:
                lines.append(prop.to_fds())

        # DEVC namelists
        if self.devices:
            lines.append("! --- Devices ---")
            for device in self.devices:
                lines.append(device.to_fds())

        # CTRL namelists
        if self.ctrls:
            lines.append("! --- Controls ---")
            for ctrl in self.ctrls:
                lines.append(ctrl.to_fds())

        # INIT namelists
        if self.inits:
            lines.append("! --- Initial Conditions ---")
            for init in self.inits:
                lines.append(init.to_fds())

        # TAIL namelist
        lines.append("\n&TAIL /")

        return "\n".join(lines)

    def write(self, filename: str | Path) -> Path:
        """
        Write the FDS input file to disk.

        Parameters
        ----------
        filename : str or Path
            Path to output file (should have .fds extension)

        Returns
        -------
        Path
            Path to the written file

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.add(Time(t_end=100.0))
        >>> sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        >>> sim.write('test.fds')
        """
        # Run validation
        warnings = self.validate()
        if warnings:
            for warning in warnings:
                logger.warning(warning)

        content = self.to_fds()

        # Write to file
        path = Path(filename)
        if path.suffix != ".fds":
            path = path.with_suffix(".fds")
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

        logger.info(f"Wrote FDS input file: {path}")
        return path

    def validate(self) -> list[str]:
        """
        Validate the simulation configuration.

        Returns
        -------
        List[str]
            List of validation warnings (empty if no issues)

        Examples
        --------
        >>> sim = Simulation('test')
        >>> warnings = sim.validate()
        >>> if warnings:
        ...     for w in warnings:
        ...         print(f"Warning: {w}")
        """
        issues = self._validator.validate()
        return [str(issue) for issue in issues]

    def run(
        self,
        config: "RunConfig | None" = None,
        **kwargs: Any,
    ) -> "Results | Job":
        """
        Write FDS file and execute simulation.

        This is a convenience method that combines write() and execution
        in a single call. The FDS file is written to a temporary location
        or specified output directory, then executed.

        Parameters
        ----------
        config : RunConfig, optional
            Execution configuration (default: RunConfig())
        **kwargs
            Additional keyword arguments passed to RunConfig()

        Returns
        -------
        Results or Job
            Results object if wait=True, Job object if wait=False

        Raises
        ------
        ValueError
            If validation fails and strict=True
        ExecutionError
            If FDS execution fails
        FDSNotFoundError
            If FDS executable cannot be found

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.add(Time(t_end=100.0))
        >>> sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
        >>> results = sim.run(n_threads=4)
        >>> print(f"Peak HRR: {results.hrr['HRR'].max()}")

        >>> # Non-blocking execution
        >>> job = sim.run(wait=False, monitor=True)
        >>> while job.is_running():
        ...     print(f"Progress: {job.progress}%")
        ...     time.sleep(5)
        >>> results = job.get_results()
        """
        # Import here to avoid circular import
        from ..config import RunConfig
        from ..execution import FDSRunner

        # Create config from kwargs if not provided
        if config is None:
            config = RunConfig(**kwargs)
        elif kwargs:
            raise ValueError("Cannot specify both 'config' and keyword arguments")

        # Validate if requested
        if config.validate:
            warnings = self.validate()
            if warnings:
                logger.warning(f"Simulation validation found {len(warnings)} warning(s)")
                for warning in warnings:
                    logger.warning(f"  - {warning}")
                if config.strict:
                    raise ValueError(
                        "Simulation validation failed:\n" + "\n".join(f"  - {w}" for w in warnings)
                    )

        # Determine output directory
        output_dir = Path.cwd() if config.output_dir is None else Path(config.output_dir)

        output_dir.mkdir(parents=True, exist_ok=True)

        # Write FDS file
        fds_file = output_dir / f"{self.chid}.fds"
        self.write(fds_file)

        logger.info(f"Running simulation: {self.chid}")
        logger.debug(f"  Threads: {config.n_threads}, MPI processes: {config.n_mpi}")
        logger.debug(f"  Output directory: {output_dir}")

        # Create runner and execute
        runner = FDSRunner(fds_executable=config.fds_executable)
        return runner.run(
            fds_file=fds_file,
            n_threads=config.n_threads,
            n_mpi=config.n_mpi,
            mpiexec_path=config.mpiexec_path,
            output_dir=output_dir,
            monitor=config.monitor,
            wait=config.wait,
            timeout=config.timeout,
            simulation=self,  # Pass simulation for parallel validation
        )

    # =========================================================================
    # Legacy Compatibility Properties (Deprecated - to be removed)
    # =========================================================================

    @property
    def time_params(self) -> Time | None:
        """Deprecated: use time_config instead."""
        return self._time

    @time_params.setter
    def time_params(self, value: Time | None) -> None:
        """Deprecated: use add(Time(...)) instead."""
        if value is not None:
            self._time = value
            self._registry.time = value
