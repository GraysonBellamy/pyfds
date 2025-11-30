"""Integration tests for Stage 2: Particle Systems.

Tests real-world fire simulation scenarios using PART, SURF particle generation,
PROP device properties, and MATL pyrolysis enhancements.
"""

from pyfds.builders import MaterialBuilder, PartBuilder, PropBuilder
from pyfds.core.namelists import Material, Part, Prop, Surface


class TestWaterSpraySuppressionSystem:
    """Test water spray and sprinkler suppression systems."""

    def test_water_droplet_particle(self):
        """Test water droplet particle definition."""
        water_drop = (
            PartBuilder("WATER_DROP")
            .as_water_droplet(diameter=0.001)
            .with_density(1000.0)
            .with_color("BLUE")
            .build()
        )

        assert water_drop.id == "WATER_DROP"
        assert water_drop.liquid_droplet is True
        assert water_drop.diameter == 0.001
        assert water_drop.density == 1000.0
        assert water_drop.color == "BLUE"
        assert water_drop.boiling_temperature == 100.0
        assert water_drop.heat_of_vaporization == 2260.0

        fds_output = water_drop.to_fds()
        assert "WATER_DROP" in fds_output
        assert "0.001" in fds_output

    def test_sprinkler_prop(self):
        """Test sprinkler device property."""
        sprinkler_prop = PropBuilder.sprinkler(
            id="QUICK_SPRINKLER",
            activation_temp=68,
            rti=50,
            flow_rate=60,
            spray_angle=(45, 90),
        )

        assert sprinkler_prop.id == "QUICK_SPRINKLER"
        assert sprinkler_prop.activation_temperature == 68
        assert sprinkler_prop.rti == 50
        assert sprinkler_prop.flow_rate == 60
        assert sprinkler_prop.spray_angle == (45, 90)

        fds_output = sprinkler_prop.to_fds()
        assert "QUICK_SPRINKLER" in fds_output
        assert "68" in fds_output

    def test_sprinkler_surface_with_particles(self):
        """Test sprinkler surface that generates water droplets."""
        sprinkler_surf = Surface(
            id="SPRINKLER_HEAD",
            part_id="WATER_DROP",
            particle_mass_flux=0.015,
            median_diameter=0.001,
            vel_part=5.0,
            color="CYAN",
        )

        assert sprinkler_surf.part_id == "WATER_DROP"
        assert sprinkler_surf.particle_mass_flux == 0.015
        assert sprinkler_surf.median_diameter == 0.001
        assert sprinkler_surf.vel_part == 5.0
        assert sprinkler_surf.color == "CYAN"

        fds_output = sprinkler_surf.to_fds()
        assert "SPRINKLER_HEAD" in fds_output
        assert "WATER_DROP" in fds_output
        assert "0.015" in fds_output

    def test_complete_sprinkler_system(self):
        """Test complete sprinkler suppression system."""
        # Water droplet particle
        water = PartBuilder("WATER_DROP").as_water_droplet(diameter=0.001).build()

        # Sprinkler property
        sprinkler_prop = PropBuilder.quick_response_sprinkler(id="QR_SPRINKLER")

        # Sprinkler surface
        sprinkler_surf = Surface(
            id="SPRINKLER",
            part_id="WATER_DROP",
            particle_mass_flux=0.02,
            median_diameter=0.001,
            vel_part=6.0,
        )

        # Verify all components created successfully
        assert isinstance(water, Part)
        assert isinstance(sprinkler_prop, Prop)
        assert isinstance(sprinkler_surf, Surface)

        # Verify linkage
        assert sprinkler_surf.part_id == water.id
        assert sprinkler_prop.flow_rate == 60


class TestSmokeDetectionSystem:
    """Test smoke detection and optical particle systems."""

    def test_smoke_particle(self):
        """Test smoke particle definition."""
        smoke = (
            PartBuilder("SMOKE")
            .as_aerosol(diameter=0.00001, spec_id="SOOT")
            .with_color("GRAY 50")
            .build()
        )

        assert smoke.id == "SMOKE"
        assert smoke.spec_id == "SOOT"
        assert smoke.diameter == 0.00001
        assert smoke.color == "GRAY 50"

    def test_smoke_detector_prop(self):
        """Test smoke detector property."""
        smoke_det = PropBuilder.smoke_detector(
            id="PHOTOELECTRIC_DET", activation_obscuration=3.28, alpha_e=0.5, beta_e=0.3
        )

        assert smoke_det.id == "PHOTOELECTRIC_DET"
        assert smoke_det.quantity == "CHAMBER_OBSCURATION"
        assert smoke_det.activation_obscuration == 3.28
        assert smoke_det.alpha_e == 0.5
        assert smoke_det.beta_e == 0.3

        fds_output = smoke_det.to_fds()
        assert "CHAMBER_OBSCURATION" in fds_output
        assert "3.28" in fds_output

    def test_smoke_generation_surface(self):
        """Test surface that generates smoke particles."""
        smoke_surf = Surface(
            id="SMOKE_SOURCE", part_id="SMOKE", particle_mass_flux=0.001, nppc=3, color="BLACK"
        )

        assert smoke_surf.part_id == "SMOKE"
        assert smoke_surf.particle_mass_flux == 0.001
        assert smoke_surf.nppc == 3
        assert smoke_surf.color == "BLACK"


class TestPyrolysisMaterialSystems:
    """Test material pyrolysis and decomposition."""

    def test_simple_pyrolysis_material(self):
        """Test material with single pyrolysis reaction."""
        pmma = (
            MaterialBuilder("PMMA")
            .density(1200)
            .thermal_conductivity(0.19)
            .specific_heat(1.4)
            .with_pyrolysis_product("MMA_VAPOR", yield_fraction=1.0)
            .with_heat_of_combustion(25000)
            .build()
        )

        assert pmma.id == "PMMA"
        assert pmma.density == 1200
        assert pmma.spec_id == "MMA_VAPOR"
        assert pmma.nu_spec == 1.0
        assert pmma.heat_of_combustion_array == 25000

        fds_output = pmma.to_fds()
        assert "PMMA" in fds_output
        assert "MMA_VAPOR" in fds_output
        assert "25000" in fds_output

    def test_multi_reaction_pyrolysis(self):
        """Test material with multiple pyrolysis reactions."""
        wood = (
            MaterialBuilder("WOOD")
            .density(500)
            .thermal_conductivity(0.13)
            .specific_heat(2.5)
            .add_pyrolysis_reaction(
                a=1e10, e=100000, heat_of_reaction=1800, product_species="WOOD_VAPOR"
            )
            .add_pyrolysis_reaction(a=5e8, e=120000, heat_of_reaction=500, residue_material="CHAR")
            .build()
        )

        assert wood.id == "WOOD"
        assert wood.n_reactions == 2
        assert len(wood.reactions) == 2
        assert wood.reactions[0].a == 1e10
        assert wood.reactions[0].e == 100000
        assert wood.reactions[0].heat_of_reaction == 1800
        assert wood.reactions[0].products[0].spec_id == "WOOD_VAPOR"
        assert wood.reactions[0].products[0].nu_spec == 1.0
        assert wood.reactions[1].a == 5e8
        assert wood.reactions[1].e == 120000
        assert wood.reactions[1].heat_of_reaction == 500
        assert wood.reactions[1].products[0].matl_id == "CHAR"
        assert wood.reactions[1].products[0].nu_matl == 1.0

        fds_output = wood.to_fds()
        assert "WOOD" in fds_output
        assert "REACTIONS=" in fds_output

    def test_predefined_materials(self):
        """Test predefined common materials."""
        concrete = MaterialBuilder.concrete()
        steel = MaterialBuilder.steel()
        wood = MaterialBuilder.wood()
        gypsum = MaterialBuilder.gypsum()

        assert concrete.id == "CONCRETE"
        assert steel.id == "STEEL"
        assert wood.id == "WOOD"
        assert gypsum.id == "GYPSUM"

        assert concrete.density == 2400
        assert steel.conductivity == 45.8
        assert wood.specific_heat == 2.5


class TestFuelVaporCombustionSystem:
    """Test fuel vapor particles and combustion."""

    def test_fuel_vapor_particle(self):
        """Test fuel vapor particle with combustion."""
        propane_vapor = (
            PartBuilder("PROPANE_VAPOR")
            .as_aerosol(diameter=0.0001, spec_id="PROPANE")
            .with_color("YELLOW")
            .build()
        )

        assert propane_vapor.id == "PROPANE_VAPOR"
        assert propane_vapor.spec_id == "PROPANE"
        assert propane_vapor.diameter == 0.0001

    def test_burning_liquid_pool(self):
        """Test liquid pool that generates fuel vapor."""
        pool_surf = Surface(
            id="FUEL_POOL",
            part_id="PROPANE_VAPOR",
            particle_mass_flux=0.005,
            nppc=2,
            hrrpua=2000.0,
            color="ORANGE",
        )

        assert pool_surf.part_id == "PROPANE_VAPOR"
        assert pool_surf.particle_mass_flux == 0.005
        assert pool_surf.nppc == 2
        assert pool_surf.hrrpua == 2000.0


class TestHeatDetectorSystem:
    """Test heat detector systems."""

    def test_heat_detector_prop(self):
        """Test heat detector property."""
        heat_det = PropBuilder.heat_detector(
            id="HEAT_DET",
            activation_temp=74,
            rti=10.0,
            bead_diameter=0.001,
            bead_density=8000,
            bead_specific_heat=0.5,
        )

        assert heat_det.id == "HEAT_DET"
        assert heat_det.activation_temperature == 74
        assert heat_det.rti == 10.0
        assert heat_det.bead_diameter == 0.001
        assert heat_det.bead_density == 8000
        assert heat_det.bead_specific_heat == 0.5

        fds_output = heat_det.to_fds()
        assert "HEAT_DET" in fds_output
        assert "74" in fds_output
        assert "0.001" in fds_output

    def test_predefined_sprinklers(self):
        """Test predefined sprinkler types."""
        qr_sprinkler = PropBuilder.quick_response_sprinkler()
        sr_sprinkler = PropBuilder.standard_response_sprinkler()

        assert qr_sprinkler.id == "SPRINKLER_QR"
        assert qr_sprinkler.activation_temperature == 68
        assert qr_sprinkler.rti == 50

        assert sr_sprinkler.id == "SPRINKLER_SR"
        assert sr_sprinkler.activation_temperature == 74
        assert sr_sprinkler.rti == 100


class TestNozzleSpraySystem:
    """Test nozzle and spray systems."""

    def test_nozzle_prop(self):
        """Test nozzle property."""
        nozzle = PropBuilder.nozzle(id="SPRAY_NOZZLE", flow_rate=50, pressure=300000)

        assert nozzle.id == "SPRAY_NOZZLE"
        assert nozzle.flow_rate == 50
        assert nozzle.pressure == 300000

    def test_spray_surface_with_distribution(self):
        """Test spray surface with droplet distribution."""
        spray_surf = Surface(
            id="SPRAY",
            part_id="WATER_DROP",
            particle_mass_flux=0.01,
            nppc=5,
            median_diameter=0.0008,
            gamma_d=2.4,
            spray_pattern="GAUSSIAN",
            particle_velocity=(0.0, 0.0, -3.0),
        )

        assert spray_surf.median_diameter == 0.0008
        assert spray_surf.gamma_d == 2.4
        assert spray_surf.spray_pattern == "GAUSSIAN"
        assert spray_surf.particle_velocity == (0.0, 0.0, -3.0)


class TestAdvancedParticleProperties:
    """Test advanced particle properties."""

    def test_particle_with_basic_properties(self):
        """Test particle with basic properties."""
        particle = (
            PartBuilder("CUSTOM_DROP")
            .with_diameter(0.002)
            .with_density(1000)
            .with_color("BLUE")
            .build()
        )

        assert particle.id == "CUSTOM_DROP"
        assert particle.diameter == 0.002
        assert particle.density == 1000
        assert particle.color == "BLUE"

    def test_particle_with_lifetime(self):
        """Test particle with lifetime properties."""
        particle = (
            PartBuilder("TEMP_PARTICLE")
            .with_diameter(0.001)
            .with_density(1000)
            .with_lifetime(10.0)
            .build()
        )

        assert particle.lifetime == 10.0

    def test_particle_with_drag_law(self):
        """Test particle with drag law specification."""
        particle = (
            PartBuilder("DRAG_PARTICLE")
            .with_diameter(0.001)
            .with_density(1000)
            .with_drag_law("SPHERE")
            .build()
        )

        assert particle.drag_law == "SPHERE"

    def test_static_particle(self):
        """Test static particle definition."""
        particle = PartBuilder("STATIC_PART").with_diameter(0.001).as_static().build()

        assert particle.static is True


class TestComplexFireSuppressionScenario:
    """Test complex integrated fire suppression scenario."""

    def test_compartment_fire_with_sprinkler_suppression(self):
        """Test complete compartment fire with sprinkler suppression system."""
        # Combustible material with pyrolysis
        wood_material = (
            MaterialBuilder("PINE_WOOD")
            .density(500)
            .thermal_conductivity(0.13)
            .specific_heat(2.5)
            .add_pyrolysis_reaction(
                a=1e10, e=100000, heat_of_reaction=1800, product_species="WOOD_VAPOR"
            )
            .build()
        )

        # Smoke particles
        smoke = PartBuilder("SMOKE").as_aerosol(diameter=0.00001, spec_id="SOOT").build()

        # Water droplet particles
        water = (
            PartBuilder("WATER_DROP").as_water_droplet(diameter=0.001).with_color("BLUE").build()
        )

        # Fire surface generating smoke
        fire_surf = Surface(
            id="BURNING_WOOD",
            part_id="SMOKE",
            particle_mass_flux=0.002,
            nppc=2,
            hrrpua=500.0,
            ramp_q="fire_growth",
            color="ORANGE",
        )

        # Sprinkler property
        sprinkler_prop = PropBuilder.quick_response_sprinkler(id="QR_SPRINKLER")

        # Sprinkler surface
        sprinkler_surf = Surface(
            id="SPRINKLER_HEAD",
            part_id="WATER_DROP",
            particle_mass_flux=0.02,
            median_diameter=0.001,
            vel_part=6.0,
        )

        # Smoke detector
        smoke_detector = PropBuilder.smoke_detector(id="SMOKE_DET", activation_obscuration=3.28)

        # Verify all components created successfully
        assert isinstance(wood_material, Material)
        assert isinstance(smoke, Part)
        assert isinstance(water, Part)
        assert isinstance(fire_surf, Surface)
        assert isinstance(sprinkler_prop, Prop)
        assert isinstance(sprinkler_surf, Surface)
        assert isinstance(smoke_detector, Prop)

        # Verify linkage between components
        assert fire_surf.part_id == smoke.id
        assert sprinkler_surf.part_id == water.id
        assert wood_material.reactions[0].products[0].spec_id == "WOOD_VAPOR"
        assert wood_material.reactions[0].products[0].nu_spec == 1.0

        # Verify FDS output generation
        assert all(
            obj.to_fds()
            for obj in [
                wood_material,
                smoke,
                water,
                fire_surf,
                sprinkler_prop,
                sprinkler_surf,
                smoke_detector,
            ]
        )


class TestParticleVelocityControls:
    """Test particle velocity and orientation controls."""

    def test_particle_with_velocity_magnitude(self):
        """Test surface with particle velocity magnitude."""
        surf = Surface(id="VENT", vel_part=5.0)

        assert surf.vel_part == 5.0
        assert surf.particle_velocity is None

    def test_particle_with_velocity_vector(self):
        """Test surface with particle velocity vector."""
        surf = Surface(id="SPRAY", particle_velocity=(1.0, 0.0, -2.0))

        assert surf.particle_velocity == (1.0, 0.0, -2.0)
        assert surf.vel_part is None

    def test_spray_with_angle_control(self):
        """Test spray with angle control."""
        sprinkler = Surface(
            id="SPRINKLER",
            part_id="WATER_DROP",
            particle_mass_flux=0.015,
            median_diameter=0.001,
            vel_part=5.0,
            gamma_d=2.4,
            spray_pattern="GAUSSIAN",
        )

        assert sprinkler.spray_pattern == "GAUSSIAN"
        assert sprinkler.gamma_d == 2.4


class TestBackwardCompatibility:
    """Test that Stage 2 enhancements don't break existing functionality."""

    def test_simple_part_still_works(self):
        """Test that simple PART creation still works."""
        part = Part(id="SIMPLE", diameter=0.001, density=1000)
        assert part.id == "SIMPLE"
        assert part.diameter == 0.001

    def test_simple_prop_still_works(self):
        """Test that simple PROP creation still works."""
        prop = Prop(id="SIMPLE_PROP", activation_temperature=74, rti=50)
        assert prop.id == "SIMPLE_PROP"
        assert prop.activation_temperature == 74

    def test_simple_material_still_works(self):
        """Test that simple MATL creation still works."""
        matl = Material(id="SIMPLE_MATL", density=1000, conductivity=1.0, specific_heat=1.0)
        assert matl.id == "SIMPLE_MATL"
        assert matl.density == 1000

    def test_surf_without_particles_still_works(self):
        """Test that SURF without particles still works."""
        surf = Surface(id="SIMPLE_SURF", hrrpua=500.0, color="RED")
        assert surf.id == "SIMPLE_SURF"
        assert surf.hrrpua == 500.0
