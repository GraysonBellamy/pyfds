"""
Main Simulation class for creating and managing FDS simulations.

This module provides the high-level API for creating FDS simulations
programmatically in Python.
"""

from pathlib import Path
from typing import TYPE_CHECKING

from .namelist import Device, Head, Mesh, Obstruction, Surface, Time

if TYPE_CHECKING:
    from ..analysis.results import Results
    from ..execution.runner import Job


class Simulation:
    """
    Main class for building FDS simulations.

    This class provides a Pythonic interface for creating FDS input files.
    It manages all namelist groups and provides methods for adding simulation
    components.

    Parameters
    ----------
    chid : str
        Case identifier (filename prefix for all output files)
    title : str, optional
        Descriptive title for the simulation

    Examples
    --------
    >>> sim = Simulation(chid='room_fire', title='Room Fire Test')
    >>> sim.time(t_end=600.0)
    >>> sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
    >>> sim.write('room_fire.fds')

    Attributes
    ----------
    head : Head
        HEAD namelist group
    time_params : Time, optional
        TIME namelist group
    meshes : List[Mesh]
        List of MESH namelist groups
    surfaces : List[Surface]
        List of SURF namelist groups
    obstructions : List[Obstruction]
        List of OBST namelist groups
    devices : List[Device]
        List of DEVC namelist groups
    """

    def __init__(self, chid: str, title: str | None = None):
        """Initialize a new FDS simulation."""
        self.head = Head(chid=chid, title=title)
        self.time_params: Time | None = None
        self.meshes: list[Mesh] = []
        self.surfaces: list[Surface] = []
        self.obstructions: list[Obstruction] = []
        self.devices: list[Device] = []

    @property
    def chid(self) -> str:
        """Get the case identifier."""
        return self.head.chid

    def time(
        self,
        t_end: float,
        t_begin: float | None = None,
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
            Start time for output (default: 0.0)
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
        >>> sim.time(t_end=600.0, dt=0.1)
        """
        self.time_params = Time(
            t_end=t_end, t_begin=t_begin, dt=dt, wall_clock_time=wall_clock_time
        )
        return self

    def mesh(
        self,
        ijk: tuple[int, int, int],
        xb: tuple[float, float, float, float, float, float],
        id: str | None = None,
        mpi_process: int | None = None,
    ) -> "Simulation":
        """
        Add a computational mesh to the simulation.

        Parameters
        ----------
        ijk : Tuple[int, int, int]
            Number of grid cells in x, y, z directions
        xb : Tuple[float, float, float, float, float, float]
            Domain bounds (xmin, xmax, ymin, ymax, zmin, zmax)
        id : str, optional
            Mesh identifier for multi-mesh simulations
        mpi_process : int, optional
            MPI process number for this mesh

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.mesh(ijk=(100, 100, 50), xb=(0, 10, 0, 10, 0, 5))

        Notes
        -----
        For best accuracy, grid cells should be cubic or near-cubic.
        """
        mesh_obj = Mesh(ijk=ijk, xb=xb, id=id, mpi_process=mpi_process)
        self.meshes.append(mesh_obj)
        return self

    def add_mesh(self, mesh: Mesh) -> "Simulation":
        """
        Add a Mesh object to the simulation.

        Parameters
        ----------
        mesh : Mesh
            Mesh object to add

        Returns
        -------
        Simulation
            Self for method chaining
        """
        self.meshes.append(mesh)
        return self

    def surface(
        self,
        id: str,
        rgb: tuple[int, int, int] | None = None,
        color: str | None = None,
        hrrpua: float | None = None,
        tmp_front: float | None = None,
        matl_id: str | None = None,
        thickness: float | None = None,
    ) -> "Simulation":
        """
        Add a surface definition to the simulation.

        Parameters
        ----------
        id : str
            Unique surface identifier
        rgb : Tuple[int, int, int], optional
            RGB color values (0-255)
        color : str, optional
            Named color (e.g., 'RED', 'BLUE')
        hrrpua : float, optional
            Heat release rate per unit area (kW/m²)
        tmp_front : float, optional
            Front surface temperature (°C)
        matl_id : str, optional
            Material identifier
        thickness : float, optional
            Material thickness (m)

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.surface(id='FIRE', hrrpua=1000.0, color='RED')
        """
        surf_obj = Surface(
            id=id,
            rgb=rgb,
            color=color,
            hrrpua=hrrpua,
            tmp_front=tmp_front,
            matl_id=matl_id,
            thickness=thickness,
        )
        self.surfaces.append(surf_obj)
        return self

    def add_surface(self, surface: Surface) -> "Simulation":
        """
        Add a Surface object to the simulation.

        Parameters
        ----------
        surface : Surface
            Surface object to add

        Returns
        -------
        Simulation
            Self for method chaining
        """
        self.surfaces.append(surface)
        return self

    def obstruction(
        self,
        xb: tuple[float, float, float, float, float, float],
        surf_id: str | None = None,
        surf_id_top: str | None = None,
        surf_id_bottom: str | None = None,
        surf_id_sides: str | None = None,
        color: str | None = None,
    ) -> "Simulation":
        """
        Add an obstruction to the simulation.

        Parameters
        ----------
        xb : Tuple[float, float, float, float, float, float]
            Obstruction bounds (xmin, xmax, ymin, ymax, zmin, zmax)
        surf_id : str, optional
            Surface ID for all faces
        surf_id_top : str, optional
            Surface ID for top face
        surf_id_bottom : str, optional
            Surface ID for bottom face
        surf_id_sides : str, optional
            Surface ID for side faces
        color : str, optional
            Named color

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.obstruction(xb=(4, 6, 4, 6, 0, 0.5), surf_id='FIRE')
        """
        obst_obj = Obstruction(
            xb=xb,
            surf_id=surf_id,
            surf_id_top=surf_id_top,
            surf_id_bottom=surf_id_bottom,
            surf_id_sides=surf_id_sides,
            color=color,
        )
        self.obstructions.append(obst_obj)
        return self

    def add_obstruction(self, obstruction: Obstruction) -> "Simulation":
        """
        Add an Obstruction object to the simulation.

        Parameters
        ----------
        obstruction : Obstruction
            Obstruction object to add

        Returns
        -------
        Simulation
            Self for method chaining
        """
        self.obstructions.append(obstruction)
        return self

    def device(
        self,
        id: str,
        quantity: str,
        xyz: tuple[float, float, float] | None = None,
        xb: tuple[float, float, float, float, float, float] | None = None,
    ) -> "Simulation":
        """
        Add a measurement device to the simulation.

        Parameters
        ----------
        id : str
            Unique device identifier
        quantity : str
            FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY')
        xyz : Tuple[float, float, float], optional
            Device location (x, y, z) in meters
        xb : Tuple[float, float, float, float, float, float], optional
            Device bounds for spatial averaging

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.device(id='TEMP1', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.0))

        Notes
        -----
        Either xyz or xb must be specified, but not both.
        """
        if xyz is None and xb is None:
            raise ValueError("Either xyz or xb must be specified")
        if xyz is not None and xb is not None:
            raise ValueError("Cannot specify both xyz and xb")

        dev_obj = Device(id=id, quantity=quantity, xyz=xyz, xb=xb)
        self.devices.append(dev_obj)
        return self

    def add_device(self, device: Device) -> "Simulation":
        """
        Add a Device object to the simulation.

        Parameters
        ----------
        device : Device
            Device object to add

        Returns
        -------
        Simulation
            Self for method chaining
        """
        self.devices.append(device)
        return self

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
        >>> sim.time(t_end=100.0)
        >>> sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        >>> content = sim.to_fds()
        """
        lines = []

        # Add header comment
        lines.append("! FDS input file generated by PyFDS")
        if self.head.title:
            lines.append(f"! {self.head.title}")
        lines.append("")

        # HEAD namelist
        lines.append(self.head.to_fds())

        # TIME namelist
        if self.time_params:
            lines.append(self.time_params.to_fds())

        # MESH namelists
        if self.meshes:
            lines.append("! --- Meshes ---")
            for mesh in self.meshes:
                lines.append(mesh.to_fds())

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

        # DEVC namelists
        if self.devices:
            lines.append("! --- Devices ---")
            for device in self.devices:
                lines.append(device.to_fds())

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
        >>> sim.time(t_end=100.0)
        >>> sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        >>> sim.write('test.fds')
        """
        filepath = Path(filename)

        # Ensure .fds extension
        if filepath.suffix != ".fds":
            filepath = filepath.with_suffix(".fds")

        # Create parent directories if needed
        filepath.parent.mkdir(parents=True, exist_ok=True)

        # Write file
        content = self.to_fds()
        filepath.write_text(content)

        return filepath

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
        warnings = []

        # Check for required components
        if not self.meshes:
            warnings.append("No meshes defined - at least one mesh is required")

        if not self.time_params:
            warnings.append("No time parameters defined - TIME namelist required")

        # Check for surface ID references
        surface_ids = {s.id for s in self.surfaces}
        for obst in self.obstructions:
            if (
                obst.surf_id
                and obst.surf_id not in surface_ids
                and obst.surf_id not in ["INERT", "OPEN", "MIRROR"]
            ):
                warnings.append(f"Obstruction references undefined surface '{obst.surf_id}'")

        # Check for device ID uniqueness
        device_ids = [d.id for d in self.devices]
        if len(device_ids) != len(set(device_ids)):
            warnings.append("Duplicate device IDs found")

        # Check mesh cell aspect ratios
        for i, mesh in enumerate(self.meshes):
            dx, dy, dz = mesh.get_cell_size()
            max_ratio = max(dx / dy, dy / dx, dx / dz, dz / dx, dy / dz, dz / dy)
            if max_ratio > 2.0:
                warnings.append(
                    f"Mesh {i} has non-cubic cells (aspect ratio {max_ratio:.2f}). "
                    "Consider using more cubic cells for better accuracy."
                )

        return warnings

    def run(
        self,
        n_threads: int = 1,
        n_mpi: int = 1,
        mpiexec_path: str = "mpiexec",
        output_dir: Path | str | None = None,
        fds_executable: Path | None = None,
        monitor: bool = True,
        wait: bool = True,
        timeout: float | None = None,
        validate: bool = True,
        strict: bool = False,
    ) -> "Results | Job":
        """
        Write FDS file and execute simulation.

        This is a convenience method that combines write() and execution
        in a single call. The FDS file is written to a temporary location
        or specified output directory, then executed.

        Parameters
        ----------
        n_threads : int
            Number of OpenMP threads (default: 1)
        n_mpi : int
            Number of MPI processes (default: 1)
        mpiexec_path : str
            Path to mpiexec command (default: 'mpiexec')
        output_dir : Path or str, optional
            Output directory (default: current directory)
        fds_executable : Path, optional
            Path to FDS executable (auto-detected if not provided)
        monitor : bool
            Enable progress monitoring (default: True)
        wait : bool
            Wait for completion (default: True)
        timeout : float, optional
            Timeout in seconds (only used if wait=True)
        validate : bool
            Validate simulation before running (default: True)
        strict : bool
            Raise exception on validation warnings (default: False)

        Returns
        -------
        Results or Job
            Results object if wait=True, Job object if wait=False

        Raises
        ------
        ValueError
            If validation fails and strict=True
        FDSExecutionError
            If FDS execution fails
        FDSNotFoundError
            If FDS executable cannot be found

        Examples
        --------
        >>> sim = Simulation('test')
        >>> sim.time(t_end=100.0)
        >>> sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
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
        from ..execution import FDSRunner

        # Validate if requested
        if validate:
            warnings = self.validate()
            if warnings:
                if strict:
                    raise ValueError(
                        "Simulation validation failed:\n" + "\n".join(f"  - {w}" for w in warnings)
                    )
                # Print warnings but continue
                print("Validation warnings:")
                for warning in warnings:
                    print(f"  - {warning}")

        # Determine output directory
        output_dir = Path.cwd() if output_dir is None else Path(output_dir)

        output_dir.mkdir(parents=True, exist_ok=True)

        # Write FDS file
        fds_file = output_dir / f"{self.chid}.fds"
        self.write(fds_file)

        # Create runner and execute
        runner = FDSRunner(fds_executable=fds_executable)
        return runner.run(
            fds_file=fds_file,
            n_threads=n_threads,
            n_mpi=n_mpi,
            mpiexec_path=mpiexec_path,
            output_dir=output_dir,
            monitor=monitor,
            wait=wait,
            timeout=timeout,
        )
