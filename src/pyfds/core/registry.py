"""Unified registry for all FDS simulation components."""

from collections.abc import Iterator
from typing import TYPE_CHECKING, Generic, TypeVar

from pyfds.exceptions import DuplicateIdError, UnknownIdError

if TYPE_CHECKING:
    from pyfds.core.namelists import (
        Combustion,
        Ctrl,
        Device,
        Head,
        Hole,
        Material,
        Mesh,
        Misc,
        Mult,
        Prop,
        Ramp,
        Reaction,
        Species,
        Surface,
        Time,
    )
    from pyfds.core.namelists.init import Init
    from pyfds.core.namelists.obst import Obstruction
    from pyfds.core.namelists.vent import Vent

T = TypeVar("T")


class Registry(Generic[T]):
    """
    Type-safe registry for FDS namelist objects.

    Unlike WeakValueDictionary, this uses strong references to ensure
    objects are not garbage collected unexpectedly.
    """

    def __init__(self, type_name: str) -> None:
        self._type_name = type_name
        self._items: dict[str, T] = {}
        self._items_without_id: list[T] = []

    def add(self, item: T) -> None:
        """Add an item to the registry."""
        if not hasattr(item, "id"):
            raise AttributeError(f"{type(item).__name__} must have 'id' attribute")

        item_id = item.id
        if item_id is not None:
            if item_id in self._items:
                raise DuplicateIdError(item_id, self._type_name)
            self._items[item_id] = item
        else:
            self._items_without_id.append(item)

    def get(self, item_id: str) -> T:
        """Get an item by ID."""
        if item_id not in self._items:
            raise UnknownIdError(item_id, self._type_name, list(self._items.keys()))
        return self._items[item_id]

    def remove(self, item_id: str) -> None:
        """Remove an item by ID."""
        self._items.pop(item_id, None)

    def contains(self, item_id: str) -> bool:
        """Check if ID exists."""
        return item_id in self._items

    def list_ids(self) -> list[str]:
        """Get all registered IDs."""
        return list(self._items.keys())

    def list_items(self) -> list[T]:
        """Get all registered items."""
        return list(self._items.values()) + self._items_without_id

    def clear(self) -> None:
        """Remove all items."""
        self._items.clear()
        self._items_without_id.clear()

    def __len__(self) -> int:
        return len(self._items) + len(self._items_without_id)

    def __iter__(self) -> Iterator[T]:
        return iter(list(self._items.values()) + self._items_without_id)

    def __contains__(self, item_id: str) -> bool:
        return item_id in self._items


class SimulationRegistry:
    """
    Centralized registry for all simulation components.

    All IDs must be unique across ALL namelist types (FDS requirement).
    This mirrors FDS behavior where IDs like 'INERT' could refer to
    a SURF, MATL, or other component depending on context.
    """

    def __init__(self) -> None:
        # Import here to avoid circular imports

        # Typed registries - organized by FDS namelist group
        self.meshes: Registry[Mesh] = Registry("MESH")
        self.surfaces: Registry[Surface] = Registry("SURF")
        self.materials: Registry[Material] = Registry("MATL")
        self.ramps: Registry[Ramp] = Registry("RAMP")
        self.holes: Registry[Hole] = Registry("HOLE")
        self.species: Registry[Species] = Registry("SPEC")
        self.reactions: Registry[Reaction] = Registry("REAC")
        self.devices: Registry[Device] = Registry("DEVC")
        self.props: Registry[Prop] = Registry("PROP")
        self.ctrls: Registry[Ctrl] = Registry("CTRL")
        self.mults: Registry[Mult] = Registry("MULT")
        self.obstructions: Registry[Obstruction] = Registry("OBST")
        self.vents: Registry[Vent] = Registry("VENT")
        self.inits: Registry[Init] = Registry("INIT")

        # Singleton namelists (only one allowed per simulation)
        # These mirror FDS behavior where only one HEAD, TIME, MISC, COMB is allowed.
        # REAC is intentionally stored in the `reactions` registry (allows multiple entries),
        # but we keep a legacy slot for backward compatibility.
        self.time: Time | None = None
        self.combustion: Combustion | None = None
        self.misc: Misc | None = None
        self.head: Head | None = None

        # Track all IDs for global uniqueness
        self._all_ids: set[str] = set()

    def _check_global_uniqueness(self, item_id: str) -> None:
        """Ensure ID is unique across all registries."""
        if item_id in self._all_ids:
            raise DuplicateIdError(item_id, "global")

    def register(self, item: object) -> None:
        """Register any item to appropriate registry."""
        # Import here to avoid circular imports
        from pyfds.core.namelists import (
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
            Obstruction,
            Prop,
            Ramp,
            Reaction,
            Species,
            Surface,
            Time,
            Vent,
        )

        # Check global uniqueness for ID-based items
        if hasattr(item, "id") and getattr(item, "id", None) is not None:
            item_id = item.id
            self._check_global_uniqueness(item_id)
            self._all_ids.add(item_id)

        # Route to appropriate registry or singleton
        match item:
            case Mesh():
                self.meshes.add(item)
            case Surface():
                self.surfaces.add(item)
            case Material():
                self.materials.add(item)
            case Ramp():
                self.ramps.add(item)
            case Hole():
                self.holes.add(item)
            case Species():
                self.species.add(item)
            case Reaction():
                # Allow multiple REAC entries; store in reactions registry
                self.reactions.add(item)
            case Device():
                self.devices.add(item)
            case Prop():
                self.props.add(item)
            case Ctrl():
                self.ctrls.add(item)
            case Mult():
                self.mults.add(item)
            case Obstruction():
                self.obstructions.add(item)
            case Vent():
                self.vents.add(item)
            case Init():
                self.inits.add(item)
            case Time():
                if self.time is not None:
                    raise ValueError("TIME already set")
                self.time = item
            case Combustion():
                if self.combustion is not None:
                    raise ValueError("COMB already set")
                self.combustion = item
            case Misc():
                if self.misc is not None:
                    raise ValueError("MISC already set")
                self.misc = item
            case Head():
                if self.head is not None:
                    raise ValueError("HEAD already set")
                self.head = item
            case _:
                raise TypeError(f"Unknown item type: {type(item).__name__}")

    def get_by_id(self, item_id: str) -> object:
        """Get any item by ID from any registry."""
        registries: list[Registry] = [
            self.meshes,
            self.surfaces,
            self.materials,
            self.ramps,
            self.holes,
            self.species,
            self.reactions,
            self.devices,
            self.props,
            self.ctrls,
            self.mults,
            self.obstructions,
            self.vents,
            self.inits,
        ]

        for registry in registries:
            if item_id in registry:
                return registry.get(item_id)

        raise UnknownIdError(item_id, "any", list(self._all_ids))

    def clear(self) -> None:
        """Clear all registries and singletons."""
        self.meshes.clear()
        self.surfaces.clear()
        self.materials.clear()
        self.ramps.clear()
        self.holes.clear()
        self.species.clear()
        self.reactions.clear()
        self.devices.clear()
        self.props.clear()
        self.ctrls.clear()
        self.mults.clear()
        self.obstructions.clear()
        self.vents.clear()
        self.inits.clear()

        self.time = None
        self.combustion = None
        self.misc = None
        self.head = None
        self._all_ids.clear()

    def all_namelists(self) -> list[object]:
        """Get all namelists in FDS output order."""
        from typing import Any

        namelists: list[Any] = []

        # Order matches FDS User Guide Table convention:
        # HEAD, TIME, MISC, MESH, MULT, RAMP, SPEC, REAC, COMB, MATL, SURF,
        # OBST, HOLE, VENT, PROP, DEVC, CTRL, INIT, TAIL

        # Singleton namelists (handled in specific order in Simulation.to_fds())
        # TIME and MISC are handled separately in Simulation.to_fds()

        # Registry-based namelists in FDS order
        namelists.extend(self.meshes.list_items())  # MESH
        namelists.extend(self.mults.list_items())  # MULT
        namelists.extend(self.ramps.list_items())  # RAMP (before SPEC/REAC/MATL)
        namelists.extend(self.species.list_items())  # SPEC
        namelists.extend(self.reactions.list_items())  # REAC

        # COMB (after REAC, before MATL)
        if self.combustion:
            namelists.append(self.combustion)

        namelists.extend(self.materials.list_items())  # MATL (after COMB, before SURF)
        namelists.extend(self.surfaces.list_items())  # SURF
        namelists.extend(self.obstructions.list_items())  # OBST
        namelists.extend(self.holes.list_items())  # HOLE
        namelists.extend(self.vents.list_items())  # VENT
        namelists.extend(self.props.list_items())  # PROP (before DEVC)
        namelists.extend(self.devices.list_items())  # DEVC
        namelists.extend(self.ctrls.list_items())  # CTRL
        namelists.extend(self.inits.list_items())  # INIT

        return namelists
