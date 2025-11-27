"""
Main Simulation class for creating and managing FDS simulations.

This module provides the high-level API for creating FDS simulations
programmatically in Python.
"""

from pathlib import Path
from typing import TYPE_CHECKING, Any

from pyfds.core.geometry import Point3D
from pyfds.core.managers import (
    ControlManager,
    GeometryManager,
    InstrumentationManager,
    MaterialManager,
    OutputManager,
    PhysicsManager,
    RampManager,
)

from ..utils import get_logger, validate_chid
from .namelists import (
    ControlFunction,
    Ctrl,
    Device,
    Head,
    Init,
    Material,
    Mesh,
    Misc,
    Obstruction,
    Prop,
    Ramp,
    Reaction,
    Surface,
    Time,
    Vent,
)
from .validator import BasicValidationStrategy, ValidationStrategy

if TYPE_CHECKING:
    from ..analysis.results import Results
    from ..execution.runner import Job

logger = get_logger(__name__)


class Simulation:
    """
    Main class for building FDS simulations.

    This class provides a Pythonic interface for creating FDS input files.
    It manages all namelist groups through specialized manager classes.

    Parameters
    ----------
    chid : str
        Case identifier (filename prefix for all output files)
    title : str, optional
        Descriptive title for the simulation
    validation_strategy : ValidationStrategy, optional
        Strategy for validating the simulation configuration.
        Defaults to BasicValidationStrategy.

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
    geometry : GeometryManager
        Manager for meshes, obstructions, and vents
    material_mgr : MaterialManager
        Manager for materials, surfaces, and ramps
    physics : PhysicsManager
        Manager for reactions and misc parameters
    instrumentation : InstrumentationManager
        Manager for devices and props
    controls : ControlManager
        Manager for controls and initial conditions
    """

    def __init__(
        self,
        chid: str,
        title: str | None = None,
        validation_strategy: ValidationStrategy | None = None,
    ):
        """Initialize a new FDS simulation."""
        # Validate CHID
        chid = validate_chid(chid)
        logger.debug(f"Creating simulation with CHID: {chid}")

        # Core simulation metadata
        self.head = Head(chid=chid, title=title)
        self.time_params: Time | None = None

        # Set validation strategy
        self._validation_strategy = validation_strategy or BasicValidationStrategy()

        # Initialize managers
        self._geometry = GeometryManager()
        self._material_mgr = MaterialManager()
        self._physics = PhysicsManager()
        self._instrumentation = InstrumentationManager()
        self._controls = ControlManager()
        self._ramps = RampManager()

    @property
    def geometry(self) -> GeometryManager:
        """Get the geometry manager."""
        return self._geometry

    @property
    def material_mgr(self) -> MaterialManager:
        """Get the material manager."""
        return self._material_mgr

    @property
    def physics(self) -> PhysicsManager:
        """Get the physics manager."""
        return self._physics

    @property
    def instrumentation(self) -> InstrumentationManager:
        """Get the instrumentation manager."""
        return self._instrumentation

    @property
    def controls(self) -> ControlManager:
        """Get the control manager."""
        return self._controls

    @property
    def ramps(self) -> RampManager:
        """Get the ramp manager."""
        return self._ramps

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

        This is the primary API for creating meshes. For advanced use cases,
        use add_mesh() with a pre-constructed Mesh object.

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

        See Also
        --------
        add_mesh : Advanced API for pre-constructed Mesh objects

        Notes
        -----
        For best accuracy, grid cells should be cubic or near-cubic.
        """
        mesh_obj = Mesh(ijk=ijk, xb=xb, id=id, mpi_process=mpi_process)
        self._geometry.add_mesh(mesh_obj)
        return self

    def add_mesh(self, mesh: Mesh) -> "Simulation":
        """
        Add a Mesh object to the simulation (advanced API).

        For most use cases, prefer the mesh() builder method.

        Parameters
        ----------
        mesh : Mesh
            Mesh object to add

        Returns
        -------
        Simulation
            Self for method chaining

        See Also
        --------
        mesh : Primary API for creating meshes
        """
        self._geometry.add_mesh(mesh)
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

        This is the primary API for creating surfaces. For advanced use cases,
        use add_surface() with a pre-constructed Surface object.

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

        See Also
        --------
        add_surface : Advanced API for pre-constructed Surface objects
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
        self._material_mgr.add_surface(surf_obj)
        return self

    def add_surface(self, surface: Surface) -> "Simulation":
        """
        Add a Surface object to the simulation (advanced API).

        For most use cases, prefer the surface() builder method.

        Parameters
        ----------
        surface : Surface
            Surface object to add

        Returns
        -------
        Simulation
            Self for method chaining

        See Also
        --------
        surface : Primary API for creating surfaces
        """
        self._material_mgr.add_surface(surface)
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

        This is the primary API for creating obstructions. For advanced use cases,
        use add_obstruction() with a pre-constructed Obstruction object.

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

        See Also
        --------
        add_obstruction : Advanced API for pre-constructed Obstruction objects
        """
        obst_obj = Obstruction(
            xb=xb,
            surf_id=surf_id,
            surf_id_top=surf_id_top,
            surf_id_bottom=surf_id_bottom,
            surf_id_sides=surf_id_sides,
            color=color,
        )
        self._geometry.add_obstruction(obst_obj)
        return self

    def add_obstruction(self, obstruction: Obstruction) -> "Simulation":
        """
        Add an Obstruction object to the simulation (advanced API).

        For most use cases, prefer the obstruction() builder method.

        Parameters
        ----------
        obstruction : Obstruction
            Obstruction object to add

        Returns
        -------
        Simulation
            Self for method chaining

        See Also
        --------
        obstruction : Primary API for creating obstructions
        """
        self._geometry.add_obstruction(obstruction)
        return self

    def device(
        self,
        id: str,
        quantity: str,
        xyz: Point3D | tuple[float, float, float] | None = None,
        xb: tuple[float, float, float, float, float, float] | None = None,
    ) -> "Simulation":
        """
        Add a measurement device to the simulation.

        This is the primary API for creating devices. For advanced use cases,
        use add_device() with a pre-constructed Device object.

        Parameters
        ----------
        id : str
            Unique device identifier
        quantity : str
            FDS quantity to measure (e.g., 'TEMPERATURE', 'VELOCITY')
        xyz : Point3D or tuple[float, float, float], optional
            Device location (x, y, z) in meters
        xb : Tuple[float, float, float, float, float, float], optional
            Device bounds for spatial averaging

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> from pyfds.core.geometry import Point3D
        >>> sim = Simulation('test')
        >>> sim.device(id='TEMP1', quantity='TEMPERATURE', xyz=Point3D(2.5, 2.5, 2.0))
        >>> # Or using tuple (converted automatically)
        >>> sim.device(id='TEMP2', quantity='TEMPERATURE', xyz=(2.5, 2.5, 2.0))

        See Also
        --------
        add_device : Advanced API for pre-constructed Device objects

        Notes
        -----
        Either xyz or xb must be specified, but not both.
        """
        if xyz is None and xb is None:
            raise ValueError("Either xyz or xb must be specified")
        if xyz is not None and xb is not None:
            raise ValueError("Cannot specify both xyz and xb")

        # Convert tuple to Point3D if necessary
        if isinstance(xyz, tuple):
            xyz = Point3D.from_tuple(xyz)

        dev_obj = Device(id=id, quantity=quantity, xyz=xyz, xb=xb)
        self._instrumentation.add_device(dev_obj)
        return self

    def add_device(self, device: Device) -> "Simulation":
        """
        Add a Device object to the simulation (advanced API).

        For most use cases, prefer the device() builder method.

        Parameters
        ----------
        device : Device
            Device object to add

        Returns
        -------
        Simulation
            Self for method chaining

        See Also
        --------
        device : Primary API for creating devices
        """
        self._instrumentation.add_device(device)
        return self

    def ramp(self, id: str, points: list[tuple[float, float]]) -> "Simulation":
        """
        Add a ramp (time-varying function) to the simulation.

        This is the primary API for creating ramps. For advanced use cases,
        use add_ramp() with a pre-constructed Ramp object.

        Parameters
        ----------
        id : str
            Unique ramp identifier
        points : list[tuple[float, float]]
            List of (T, F) points defining the ramp function

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Linear HRR ramp from 0 to 1000 kW/m² over 300 seconds
        >>> sim.ramp(id='FIRE_RAMP', points=[(0, 0), (300, 1000)])

        >>> # Temperature ramp
        >>> sim.ramp(id='TEMP_RAMP', points=[(20, 1.0), (100, 1.5), (200, 2.0)])

        See Also
        --------
        add_ramp : Advanced API for pre-constructed Ramp objects
        """
        ramp_obj = Ramp(id=id, points=points)
        self._ramps.add_ramp(ramp_obj)
        return self

    def add_ramp(self, ramp: Ramp) -> "Simulation":
        """
        Add a Ramp object to the simulation (advanced API).

        For most use cases, prefer the ramp() builder method.

        Parameters
        ----------
        ramp : Ramp
            Ramp object to add

        Returns
        -------
        Simulation
            Self for method chaining

        See Also
        --------
        ramp : Primary API for creating ramps
        """
        self._ramps.add_ramp(ramp)
        return self

    def reaction(
        self,
        fuel: str | None = None,
        c: float | None = None,
        h: float | None = None,
        o: float | None = None,
        n: float | None = None,
        heat_of_combustion: float | None = None,
        soot_yield: float = 0.01,
        co_yield: float = 0.0,
        radiative_fraction: float | None = None,
        **kwargs: Any,
    ) -> "Simulation":
        """
        Add a combustion reaction to the simulation.

        This is the primary API for creating reactions. For advanced use cases,
        use add_reaction() with a pre-constructed Reaction object.

        Parameters
        ----------
        fuel : str, optional
            Fuel name for predefined fuels (e.g., 'PROPANE', 'METHANE')
        c : float, optional
            Number of carbon atoms in fuel molecule
        h : float, optional
            Number of hydrogen atoms in fuel molecule
        o : float, optional
            Number of oxygen atoms in fuel molecule
        n : float, optional
            Number of nitrogen atoms in fuel molecule
        heat_of_combustion : float, optional
            Heat of combustion [kJ/kg]
        soot_yield : float, optional
            Soot yield [kg soot/kg fuel], default: 0.01
        co_yield : float, optional
            CO yield [kg CO/kg fuel], default: 0.0
        radiative_fraction : float, optional
            Fraction of energy radiated, default: 0.35
        **kwargs
            Additional Reaction parameters

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Use predefined fuel
        >>> sim.reaction(fuel='PROPANE')

        >>> # Custom fuel with specific properties
        >>> sim.reaction(c=7, h=16, heat_of_combustion=44600, soot_yield=0.015)

        See Also
        --------
        add_reaction : Advanced API for pre-constructed Reaction objects

        Notes
        -----
        Only one reaction is allowed per simulation.
        """
        reaction_obj = Reaction(
            fuel=fuel,
            c=c,
            h=h,
            o=o,
            n=n,
            heat_of_combustion=heat_of_combustion,
            soot_yield=soot_yield,
            co_yield=co_yield,
            radiative_fraction=radiative_fraction,
            **kwargs,
        )
        self._physics.add_reaction(reaction_obj)
        return self

    def add_reaction(self, reaction: Reaction) -> "Simulation":
        """
        Add a Reaction object to the simulation (advanced API).

        For most use cases, prefer the reaction() builder method.

        Parameters
        ----------
        reaction : Reaction
            Reaction object to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> reac = Reaction(fuel='PROPANE', soot_yield=0.01)
        >>> sim.add_reaction(reac)

        See Also
        --------
        reaction : Primary API for creating reactions
        """
        self._physics.add_reaction(reaction)
        return self

    def material(
        self,
        id: str,
        density: float,
        conductivity: float | None = None,
        conductivity_ramp: str | None = None,
        specific_heat: float | None = None,
        specific_heat_ramp: str | None = None,
        emissivity: float = 0.9,
        **kwargs: Any,
    ) -> "Simulation":
        """
        Add a material definition to the simulation.

        This is the primary API for creating materials. For advanced use cases,
        use add_material() with a pre-constructed Material object.

        Parameters
        ----------
        id : str
            Unique material identifier
        density : float
            Material density [kg/m³]
        conductivity : float, optional
            Thermal conductivity [W/(m·K)]
        conductivity_ramp : str, optional
            RAMP ID for temperature-dependent conductivity
        specific_heat : float, optional
            Specific heat capacity [kJ/(kg·K)]
        specific_heat_ramp : str, optional
            RAMP ID for temperature-dependent specific heat
        emissivity : float, optional
            Surface emissivity [0-1], default: 0.9
        **kwargs
            Additional Material parameters

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Simple material with constant properties
        >>> sim.material(id='PINE', density=500, conductivity=0.13, specific_heat=2.5)

        >>> # Material with temperature-dependent conductivity
        >>> sim.material(
        ...     id='STEEL',
        ...     density=7850,
        ...     conductivity_ramp='STEEL_K',
        ...     specific_heat=0.46
        ... )

        See Also
        --------
        add_material : Advanced API for pre-constructed Material objects
        """
        material_obj = Material(
            id=id,
            density=density,
            conductivity=conductivity,
            conductivity_ramp=conductivity_ramp,
            specific_heat=specific_heat,
            specific_heat_ramp=specific_heat_ramp,
            emissivity=emissivity,
            **kwargs,
        )
        self._material_mgr.add_material(material_obj)
        return self

    def add_material(self, material: Material) -> "Simulation":
        """
        Add a Material object to the simulation (advanced API).

        For most use cases, prefer the material() builder method.

        Parameters
        ----------
        material : Material
            Material object to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> mat = Material(id='WOOD', density=500, conductivity=0.13, specific_heat=2.5)
        >>> sim.add_material(mat)

        See Also
        --------
        material : Primary API for creating materials
        """
        self._material_mgr.add_material(material)
        return self

    def prop(
        self,
        id: str,
        quantity: str | None = None,
        activation_temperature: float | None = None,
        activation_obscuration: float | None = None,
        rti: float | None = None,
        flow_rate: float | None = None,
        **kwargs: Any,
    ) -> "Simulation":
        """
        Add a device property (PROP) to the simulation.

        This is the primary API for creating props. For advanced use cases,
        use add_prop() with a pre-constructed Prop object.

        Parameters
        ----------
        id : str
            Unique property identifier
        quantity : str, optional
            Measured quantity (e.g., 'TEMPERATURE', 'CHAMBER_OBSCURATION')
        activation_temperature : float, optional
            Activation temperature for heat detectors/sprinklers [°C]
        activation_obscuration : float, optional
            Activation obscuration for smoke detectors [%/m]
        rti : float, optional
            Response Time Index [m^0.5·s^0.5]
        flow_rate : float, optional
            Sprinkler flow rate [L/min]
        **kwargs
            Additional Prop parameters

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Sprinkler property
        >>> sim.prop(id='SPRINKLER', activation_temperature=68, rti=50, flow_rate=60)

        >>> # Smoke detector property
        >>> sim.prop(
        ...     id='SMOKE_DETECTOR',
        ...     quantity='CHAMBER_OBSCURATION',
        ...     activation_obscuration=3.28
        ... )

        See Also
        --------
        add_prop : Advanced API for pre-constructed Prop objects
        """
        prop_obj = Prop(
            id=id,
            quantity=quantity,
            activation_temperature=activation_temperature,
            activation_obscuration=activation_obscuration,
            rti=rti,
            flow_rate=flow_rate,
            **kwargs,
        )
        self._instrumentation.add_prop(prop_obj)
        return self

    def add_prop(self, prop: Prop) -> "Simulation":
        """
        Add a Prop (device property) object to the simulation (advanced API).

        For most use cases, prefer the prop() builder method.

        Parameters
        ----------
        prop : Prop
            Prop object to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> prop = Prop(id='SPRINKLER', activation_temperature=68, rti=50)
        >>> sim.add_prop(prop)

        See Also
        --------
        prop : Primary API for creating props
        """
        self._instrumentation.add_prop(prop)
        return self

    def ctrl(
        self,
        id: str,
        function_type: "ControlFunction",
        input_id: str | list[str] | None = None,
        delay: float = 0.0,
        initial_state: bool = False,
        latch: bool = True,
        **kwargs: Any,
    ) -> "Simulation":
        """
        Add a control logic (CTRL) to the simulation.

        This is the primary API for creating controls. For advanced use cases,
        use add_ctrl() with a pre-constructed Ctrl object.

        Parameters
        ----------
        id : str
            Unique control identifier
        function_type : ControlFunction
            Type of control function
        input_id : str | list[str], optional
            Input device or control ID(s)
        delay : float, optional
            Time delay [s], default: 0.0
        initial_state : bool, optional
            Initial state, default: False
        latch : bool, optional
            Whether to latch on activation, default: True
        **kwargs
            Additional Ctrl parameters

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> from pyfds.core.namelists.ctrl import ControlFunction
        >>> # ANY logic - activate if any input is true
        >>> sim.ctrl(
        ...     id='SMOKE_ALARM',
        ...     function_type=ControlFunction.ANY,
        ...     input_id=['SD_1', 'SD_2', 'SD_3']
        ... )

        >>> # Time delay
        >>> sim.ctrl(
        ...     id='DELAYED_ACTIVATION',
        ...     function_type=ControlFunction.TIME_DELAY,
        ...     input_id='SPRINKLER_1',
        ...     delay=5.0
        ... )

        See Also
        --------
        add_ctrl : Advanced API for pre-constructed Ctrl objects
        """
        ctrl_obj = Ctrl(
            id=id,
            function_type=function_type,
            input_id=input_id,
            delay=delay,
            initial_state=initial_state,
            latch=latch,
            **kwargs,
        )
        self._controls.add_ctrl(ctrl_obj)
        return self

    def add_ctrl(self, ctrl: Ctrl) -> "Simulation":
        """
        Add a Ctrl (control logic) object to the simulation (advanced API).

        For most use cases, prefer the ctrl() builder method.

        Parameters
        ----------
        ctrl : Ctrl
            Ctrl object to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> from pyfds.core.namelists.ctrl import ControlFunction
        >>> ctrl = Ctrl(id='ALARM', function_type=ControlFunction.ANY, input_id=['SD_1', 'SD_2'])
        >>> sim.add_ctrl(ctrl)

        See Also
        --------
        ctrl : Primary API for creating controls
        """
        self._controls.add_ctrl(ctrl)
        return self

    def init(
        self,
        xb: tuple[float, float, float, float, float, float] | None = None,
        xyz: tuple[float, float, float] | None = None,
        temperature: float | None = None,
        density: float | None = None,
        mass_fraction: list[float] | None = None,
        volume_fraction: list[float] | None = None,
        spec_id: list[str] | None = None,
        **kwargs: Any,
    ) -> "Simulation":
        """
        Add an initial condition (INIT) to the simulation.

        This is the primary API for creating initial conditions. For advanced use cases,
        use add_init() with a pre-constructed Init object.

        Parameters
        ----------
        xb : tuple[float, float, float, float, float, float], optional
            Region bounds (xmin, xmax, ymin, ymax, zmin, zmax)
        xyz : tuple[float, float, float], optional
            Point location (x, y, z)
        temperature : float, optional
            Initial temperature [°C]
        density : float, optional
            Initial density [kg/m³]
        mass_fraction : list[float], optional
            Species mass fractions
        volume_fraction : list[float], optional
            Species volume fractions
        spec_id : list[str], optional
            Species identifiers
        **kwargs
            Additional Init parameters

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Hot gas region
        >>> sim.init(
        ...     xb=(0, 10, 0, 10, 0, 0.1),
        ...     temperature=500,
        ...     spec_id=['PROPANE'],
        ...     mass_fraction=[0.05]
        ... )

        >>> # Single point initialization
        >>> sim.init(xyz=(5, 5, 1), temperature=300)

        See Also
        --------
        add_init : Advanced API for pre-constructed Init objects
        """
        init_obj = Init(
            xb=xb,
            xyz=xyz,
            temperature=temperature,
            density=density,
            mass_fraction=mass_fraction,
            volume_fraction=volume_fraction,
            spec_id=spec_id,
            **kwargs,
        )
        self._controls.add_init(init_obj)
        return self

    def add_init(self, init: Init) -> "Simulation":
        """
        Add an Init (initial condition) object to the simulation (advanced API).

        For most use cases, prefer the init() builder method.

        Parameters
        ----------
        init : Init
            Init object to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> init = Init(xb=(0, 10, 0, 10, 0, 0.1), temperature=500)
        >>> sim.add_init(init)

        See Also
        --------
        init : Primary API for creating initial conditions
        """
        self._controls.add_init(init)
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
        multiple times will overwrite the previous settings.
        """
        self._physics.set_misc(misc, **kwargs)
        return self

    def add_vent(self, vent: Vent) -> "Simulation":
        """
        Add a Vent object to the simulation (advanced API).

        For most use cases, prefer the vent() builder method.

        Parameters
        ----------
        vent : Vent
            Vent object to add

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Opening to ambient
        >>> door = Vent(xb=(5, 5, 2, 4, 0, 3), surf_id='OPEN')
        >>> sim.add_vent(door)

        >>> # HVAC supply vent
        >>> supply = Vent(xb=(5, 6, 5, 6, 3, 3), surf_id='HVAC', volume_flow=0.5)
        >>> sim.add_vent(supply)

        >>> # Circular burner
        >>> burner = Vent(
        ...     xb=(-1, 1, -1, 1, 0, 0),
        ...     surf_id='FIRE',
        ...     xyz=(0, 0, 0),
        ...     radius=0.5
        ... )
        >>> sim.add_vent(burner)

        See Also
        --------
        vent : Primary API for creating vents
        """
        self._geometry.add_vent(vent)
        return self

    def vent(
        self,
        xb: tuple[float, float, float, float, float, float] | None = None,
        mb: str | None = None,
        surf_id: str = "INERT",
        **kwargs: Any,
    ) -> "Simulation":
        """
        Add a vent to the simulation.

        This is the primary API for creating vents. For advanced use cases,
        use add_vent() with a pre-constructed Vent object.

        Parameters
        ----------
        xb : tuple[float, float, float, float, float, float], optional
            Bounding box coordinates (xmin, xmax, ymin, ymax, zmin, zmax)
        mb : str, optional
            Mesh boundary location ('XMIN', 'XMAX', etc.)
        surf_id : str, optional
            Surface properties ID, default: 'INERT'
        **kwargs
            Additional vent parameters (xyz, radius, volume_flow, etc.)

        Returns
        -------
        Simulation
            Self for method chaining

        Examples
        --------
        >>> # Opening to ambient
        >>> sim.vent(xb=(5, 5, 2, 4, 0, 3), surf_id='OPEN')

        >>> # HVAC supply vent
        >>> sim.vent(xb=(5, 6, 5, 6, 3, 3), surf_id='HVAC', volume_flow=0.5)

        See Also
        --------
        add_vent : Advanced API for pre-constructed Vent objects
        """
        vent_obj = Vent(xb=xb, mb=mb, surf_id=surf_id, **kwargs)
        self._geometry.add_vent(vent_obj)
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
        # Create output manager with all managers and current state
        output_mgr = OutputManager(
            self._geometry,
            self._material_mgr,
            self._physics,
            self._instrumentation,
            self._controls,
            self._ramps,
            self.head,
            self.time_params,
        )
        return output_mgr.to_fds()

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
        content = self.to_fds()

        # Create output manager for file writing
        output_mgr = OutputManager(
            self._geometry,
            self._material_mgr,
            self._physics,
            self._instrumentation,
            self._controls,
            self._ramps,
            self.head,
            self.time_params,
        )
        return output_mgr.write(filename, content)

    def validate(self) -> list[str]:
        """
        Validate the simulation configuration using the configured validation strategy.

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
        return self._validation_strategy.validate(self)

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
                logger.warning(f"Simulation validation found {len(warnings)} warning(s)")
                for warning in warnings:
                    logger.warning(f"  - {warning}")
                if strict:
                    raise ValueError(
                        "Simulation validation failed:\n" + "\n".join(f"  - {w}" for w in warnings)
                    )

        # Determine output directory
        output_dir = Path.cwd() if output_dir is None else Path(output_dir)

        output_dir.mkdir(parents=True, exist_ok=True)

        # Write FDS file
        fds_file = output_dir / f"{self.chid}.fds"
        self.write(fds_file)

        logger.info(f"Running simulation: {self.chid}")
        logger.debug(f"  Threads: {n_threads}, MPI processes: {n_mpi}")
        logger.debug(f"  Output directory: {output_dir}")

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
