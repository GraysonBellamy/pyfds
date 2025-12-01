"""Unit tests for INIT namelist."""

import pytest
from pydantic import ValidationError

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists import Initialization


class TestInit:
    """Tests for Initialization namelist."""

    def test_basic_creation_with_xb(self):
        """Test init with region bounds."""
        init = Initialization(xb=Bounds3D.of(0, 10, 0, 10, 0, 0.1), temperature=500)
        assert init.xb == Bounds3D.of(0, 10, 0, 10, 0, 0.1)
        assert init.temperature == 500

    def test_basic_creation_with_xyz(self):
        """Test init with point location."""
        init = Initialization(xyz=Point3D(1.0, 2.0, 3.0), part_id="particles", n_particles=10)
        assert init.xyz == Point3D(1.0, 2.0, 3.0)
        assert init.part_id == "particles"
        assert init.n_particles == 10

    def test_basic_creation_with_db(self):
        """Test init with domain bounds shortcut."""
        init = Initialization(db="WHOLE DOMAIN", temperature=100)
        assert init.db == "WHOLE DOMAIN"
        assert init.temperature == 100

    def test_validation_requires_location(self):
        """Test that XB, XYZ, or DB is required."""
        with pytest.raises(ValidationError, match="XB, XYZ, or DB"):
            Initialization(temperature=500)

    def test_to_fds_with_temperature(self):
        """Test FDS output format with temperature."""
        init = Initialization(xb=Bounds3D.of(0, 10, 0, 10, 0, 0.1), temperature=500)
        fds_str = init.to_fds()
        assert "&INIT" in fds_str
        assert "XB=" in fds_str
        assert "TEMPERATURE=500" in fds_str

    def test_species_initialization(self):
        """Test species mass fraction initialization."""
        init = Initialization(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            spec_id=["OXYGEN", "PROPANE"],
            mass_fraction=[0.23, 0.06],
        )
        assert init.spec_id == ["OXYGEN", "PROPANE"]
        assert init.mass_fraction == [0.23, 0.06]

    def test_species_volume_fraction(self):
        """Test species volume fraction initialization."""
        init = Initialization(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            spec_id=["METHANE"],
            volume_fraction=[0.05],
        )
        assert init.spec_id == ["METHANE"]
        assert init.volume_fraction == [0.05]

    def test_species_validation_requires_fraction(self):
        """Test SPEC_ID requires MASS_FRACTION or VOLUME_FRACTION."""
        with pytest.raises(ValidationError, match="MASS_FRACTION or VOLUME_FRACTION"):
            Initialization(xb=Bounds3D.of(0, 1, 0, 1, 0, 1), spec_id=["PROPANE"])

    def test_species_validation_length_mismatch(self):
        """Test MASS_FRACTION length must match SPEC_ID length."""
        with pytest.raises(ValidationError, match="length must match"):
            Initialization(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                spec_id=["OXYGEN", "PROPANE"],
                mass_fraction=[0.23],  # Only one value for two species
            )

    def test_hrrpuv_initialization(self):
        """Test HRRPUV (heat release rate per unit volume)."""
        init = Initialization(
            xb=Bounds3D.of(0, 0.1, 0, 0.025, 0, 0.1),
            hrrpuv=1000.0,
            radiative_fraction=0.25,
        )
        assert init.hrrpuv == 1000.0
        assert init.radiative_fraction == 0.25

    def test_radiative_fraction_requires_hrrpuv(self):
        """Test RADIATIVE_FRACTION requires HRRPUV."""
        with pytest.raises(ValidationError, match="RADIATIVE_FRACTION requires HRRPUV"):
            Initialization(
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
                radiative_fraction=0.25,
            )

    def test_particle_insertion(self):
        """Test particle insertion parameters."""
        init = Initialization(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            part_id="my_particles",
            n_particles=100,
            mass_per_volume=2.0,
        )
        assert init.part_id == "my_particles"
        assert init.n_particles == 100
        assert init.mass_per_volume == 2.0

    def test_particle_cone_shape(self):
        """Test cone shape for particle insertion (tree)."""
        init = Initialization(
            xyz=Point3D(0, 0, 0),
            part_id="foliage",
            shape="CONE",
            radius=1.0,
            height=2.0,
            n_particles_per_cell=1,
            cell_centered=True,
        )
        assert init.shape == "CONE"
        assert init.radius == 1.0
        assert init.height == 2.0
        assert init.cell_centered is True

    def test_cone_requires_radius(self):
        """Test CONE shape requires RADIUS."""
        with pytest.raises(ValidationError, match="requires RADIUS"):
            Initialization(
                xyz=Point3D(0, 0, 0),
                part_id="foliage",
                shape="CONE",
                height=2.0,
            )

    def test_cone_requires_height(self):
        """Test CONE shape requires HEIGHT."""
        with pytest.raises(ValidationError, match="requires HEIGHT"):
            Initialization(
                xyz=Point3D(0, 0, 0),
                part_id="foliage",
                shape="CONE",
                radius=1.0,
            )

    def test_particle_offset(self):
        """Test particle offset for creating line of particles."""
        init = Initialization(
            xyz=Point3D(1.2, 3.4, 5.6),
            part_id="target",
            n_particles=10,
            dx=0.1,
        )
        assert init.dx == 0.1
        assert init.n_particles == 10

    def test_periodic_insertion(self):
        """Test periodic particle insertion."""
        init = Initialization(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            part_id="drops",
            n_particles=100,
            dt_insert=0.05,
            mass_per_time=1.0,
        )
        assert init.dt_insert == 0.05
        assert init.mass_per_time == 1.0

    def test_ramp_parameters(self):
        """Test RAMP parameters."""
        init = Initialization(
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            hrrpuv=1000.0,
            ramp_q="heat_ramp",
        )
        assert init.ramp_q == "heat_ramp"

    def test_control_parameters(self):
        """Test control/device activation."""
        init = Initialization(
            xyz=Point3D(0, 0, 0),
            part_id="drops",
            n_particles=1,
            devc_id="nozzle",
            dt_insert=0.05,
        )
        assert init.devc_id == "nozzle"

    def test_tree_parameters(self):
        """Test tree/vegetation parameters."""
        init = Initialization(
            xyz=Point3D(0, 0, 0),
            part_id="tree_foliage",
            tree_height=10.0,
            crown_base_height=3.0,
        )
        assert init.tree_height == 10.0
        assert init.crown_base_height == 3.0

    def test_initial_velocity(self):
        """Test initial particle velocity."""
        init = Initialization(
            xyz=Point3D(0, 0, 0),
            part_id="projectile",
            n_particles=1,
            uvw=(1.0, 0.0, -5.0),
        )
        assert init.uvw == (1.0, 0.0, -5.0)
