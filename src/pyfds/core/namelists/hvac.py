"""FDS HVAC namelist for Heating, Ventilation, and Air Conditioning systems.

HVAC systems model ductwork, fans, filters, dampers, air coils, and nodes
that connect to the computational domain through vents.

Field Groups:
    identification: Component ID and type
    duct: Duct geometry and flow properties
    fan: Fan performance parameters
    filter: Filter efficiency and loading
    aircoil: Air coil heat exchange parameters
    node: Node location and connections
    damper: Damper control parameters
    leak: Leakage parameters
    control: Device and control activation
    output: Output quantities
"""

from typing import Literal

from pydantic import field_validator, model_validator

from pyfds.core.namelists.base import FdsField, NamelistBase

__all__ = ["Hvac"]


# Valid HVAC component types
HvacType = Literal["DUCT", "NODE", "FAN", "FILTER", "AIRCOIL", "LEAK"]


class Hvac(NamelistBase):
    """FDS HVAC namelist for HVAC system components.

    HVAC systems consist of ducts, nodes, fans, filters, dampers, and air coils.
    Each HVAC line defines one component of the system.

    Parameters
    ----------
    id : str
        Component identifier.
    type_id : str
        Component type: 'DUCT', 'NODE', 'FAN', 'FILTER', 'AIRCOIL', or 'LEAK'.
    node_id : tuple[str, str], optional
        Node IDs at duct endpoints (for DUCT type).
    area : float, optional
        Duct cross-sectional area [m²].
    length : float, optional
        Duct length [m].

    Examples
    --------
    >>> # Define a duct
    >>> duct = Hvac(
    ...     id='DUCT1',
    ...     type_id='DUCT',
    ...     node_id=('NODE1', 'NODE2'),
    ...     area=0.04,
    ...     length=5.0,
    ...     roughness=0.001
    ... )

    >>> # Define a node
    >>> node = Hvac(
    ...     id='NODE1',
    ...     type_id='NODE',
    ...     xyz=(5.0, 5.0, 2.0),
    ...     vent_id='VENT1'
    ... )

    >>> # Define a fan
    >>> fan = Hvac(
    ...     id='FAN1',
    ...     type_id='FAN',
    ...     max_flow=1.0,
    ...     max_pressure=500.0,
    ...     volume_flow=0.5
    ... )

    See Also
    --------
    Vent : Vents that connect HVAC nodes to the domain.
    Device : Devices that can control HVAC components.
    """

    def _get_namelist_name(self) -> str:
        """Get the FDS namelist name."""
        return "HVAC"

    # --- Identification ---
    id: str = FdsField(..., description="Component identifier", group="identification")
    type_id: str | None = FdsField(
        None,
        description="Component type: DUCT, NODE, FAN, FILTER, AIRCOIL, or LEAK",
        group="identification",
    )
    network_id: str | None = FdsField(
        None, description="Network identifier for grouping components", group="identification"
    )

    # --- Duct Parameters ---
    node_id: tuple[str, str] | None = FdsField(
        None, description="Node IDs at duct endpoints", group="duct"
    )
    area: float | None = FdsField(None, gt=0, description="Cross-sectional area [m²]", group="duct")
    diameter: float | None = FdsField(None, gt=0, description="Duct diameter [m]", group="duct")
    perimeter: float | None = FdsField(None, gt=0, description="Duct perimeter [m]", group="duct")
    length: float | None = FdsField(None, gt=0, description="Duct length [m]", group="duct")
    roughness: float | None = FdsField(
        None, ge=0, description="Surface roughness [m]", group="duct"
    )
    loss: list[float] | None = FdsField(
        None, description="Loss coefficients (forward, reverse)", group="duct"
    )
    round: bool | None = FdsField(None, description="Round cross-section", group="duct")
    square: bool | None = FdsField(None, description="Square cross-section", group="duct")
    reverse: bool | None = FdsField(
        None, description="Reverse positive flow direction", group="duct"
    )
    n_cells: int | None = FdsField(
        None, ge=1, description="Number of cells for mass transport", group="duct"
    )
    waypoints: list[float] | None = FdsField(
        None, description="Waypoint coordinates for duct path [m]", group="duct"
    )
    volume_flow: float | None = FdsField(
        None, description="Fixed volume flow rate [m³/s]", group="duct"
    )
    mass_flow: float | None = FdsField(
        None, description="Fixed mass flow rate [kg/s]", group="duct"
    )
    tau_vf: float | None = FdsField(
        None, gt=0, description="Volume flow ramp-up time constant [s]", group="duct"
    )

    # --- Fan Parameters ---
    fan_id: str | None = FdsField(None, description="Fan ID for duct", group="fan")
    max_flow: float | None = FdsField(
        None, gt=0, description="Maximum fan flow rate [m³/s]", group="fan"
    )
    max_pressure: float | None = FdsField(
        None, gt=0, description="Maximum fan pressure [Pa]", group="fan"
    )
    tau_fan: float | None = FdsField(
        None, gt=0, description="Fan ramp-up time constant [s]", group="fan"
    )

    # --- Filter Parameters ---
    filter_id: str | None = FdsField(None, description="Filter ID for node", group="filter")
    clean_loss: float | None = FdsField(
        None, ge=0, description="Clean filter loss coefficient", group="filter"
    )
    efficiency: list[float] | None = FdsField(
        None, description="Filter efficiency per species", group="filter"
    )
    loading: list[float] | None = FdsField(
        None, description="Initial filter loading per species [kg]", group="filter"
    )
    loading_multiplier: list[float] | None = FdsField(
        None, description="Loading multiplier per species [1/kg]", group="filter"
    )
    spec_id: list[str] | None = FdsField(None, description="Species IDs for filter", group="filter")

    # --- Air Coil Parameters ---
    aircoil_id: str | None = FdsField(None, description="Air coil ID for duct", group="aircoil")
    coolant_mass_flow: float | None = FdsField(
        None, gt=0, description="Coolant mass flow rate [kg/s]", group="aircoil"
    )
    coolant_specific_heat: float | None = FdsField(
        None, gt=0, description="Coolant specific heat [kJ/(kg·K)]", group="aircoil"
    )
    coolant_temperature: float | None = FdsField(
        None, description="Coolant inlet temperature [°C]", group="aircoil"
    )
    fixed_q: float | None = FdsField(
        None, description="Fixed heat transfer rate [kW]", group="aircoil"
    )
    tau_ac: float | None = FdsField(
        None, gt=0, description="Air coil time constant [s]", group="aircoil"
    )

    # --- Node Parameters ---
    xyz: tuple[float, float, float] | None = FdsField(
        None, description="Node location [m]", group="node"
    )
    vent_id: str | None = FdsField(
        None, description="Vent ID connecting node to domain", group="node"
    )
    vent2_id: str | None = FdsField(
        None, description="Second vent ID for local leakage", group="node"
    )
    duct_id: list[str] | None = FdsField(
        None, description="Duct IDs connected to node", group="node"
    )
    ambient: bool | None = FdsField(None, description="Node at ambient conditions", group="node")

    # --- Damper Parameters ---
    damper: bool | None = FdsField(None, description="Duct is a damper", group="damper")
    ramp_loss: str | None = FdsField(
        None, description="Ramp ID for variable loss coefficient", group="damper"
    )

    # --- Leak Parameters ---
    discharge_coefficient: float | None = FdsField(
        None, gt=0, description="Discharge coefficient", group="leak"
    )
    leak_enthalpy: bool | None = FdsField(
        None, description="Include enthalpy in leak calculation", group="leak"
    )
    leak_pressure_exponent: float | None = FdsField(
        None, description="Pressure exponent for leak flow", group="leak"
    )
    leak_reference_pressure: float | None = FdsField(
        None, gt=0, description="Reference pressure for leak [Pa]", group="leak"
    )
    transport_particles: bool | None = FdsField(
        None, description="Transport particles through leak", group="leak"
    )

    # --- Control Parameters ---
    ctrl_id: str | None = FdsField(None, description="Control ID for activation", group="control")
    devc_id: str | None = FdsField(None, description="Device ID for activation", group="control")
    ramp_id: str | None = FdsField(
        None, description="Ramp ID for time-varying parameters", group="control"
    )

    # --- Output Parameters ---
    quantity: list[str] | None = FdsField(None, description="Output quantities", group="output")
    quantity_spec_id: list[str] | None = FdsField(
        None, description="Species IDs for output quantities", group="output"
    )
    dry: list[bool] | None = FdsField(None, description="Dry measurement flags", group="output")

    # --- Geometry Visualization ---
    geom: bool | None = FdsField(
        None, description="Generate geometry for duct visualization", group="visualization"
    )
    geom2: bool | None = FdsField(
        None, description="Alternative geometry for duct visualization", group="visualization"
    )

    # --- Validators ---
    @field_validator("type_id")
    @classmethod
    def validate_type_id(cls, v: str | None) -> str | None:
        """Validate component type."""
        if v is not None:
            valid_types = ["DUCT", "NODE", "FAN", "FILTER", "AIRCOIL", "LEAK"]
            if v.upper() not in valid_types:
                raise ValueError(f"TYPE_ID must be one of: {', '.join(valid_types)}")
            return v.upper()
        return v

    @field_validator("node_id")
    @classmethod
    def validate_node_id(cls, v: tuple[str, str] | None) -> tuple[str, str] | None:
        """Validate node_id has exactly 2 elements."""
        if v is not None and len(v) != 2:
            raise ValueError("NODE_ID must have exactly 2 node identifiers")
        return v

    @model_validator(mode="after")
    def validate_hvac_component(self) -> "Hvac":
        """Validate HVAC component configuration."""
        # If type_id is specified, validate required parameters
        if self.type_id == "DUCT" and self.node_id is None:
            raise ValueError("DUCT requires NODE_ID to specify endpoints")

        if self.type_id == "FAN" and self.max_flow is None and self.volume_flow is None:
            raise ValueError("FAN requires MAX_FLOW or VOLUME_FLOW")

        return self
