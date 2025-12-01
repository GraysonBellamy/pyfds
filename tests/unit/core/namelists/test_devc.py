"""Unit tests for DEVC namelist."""

import pytest

from pyfds.core.geometry import Bounds3D, Point3D
from pyfds.core.namelists import Device


class TestDevice:
    """Tests for Device namelist."""

    def test_basic_creation_xyz(self):
        """Test basic Device creation with XYZ."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 2.0))
        assert dev.id == "TEMP1"
        assert dev.quantity == "TEMPERATURE"
        assert dev.xyz == Point3D.of(1.0, 1.0, 2.0)

    def test_basic_creation_xb(self):
        """Test basic Device creation with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        assert dev.xb == Bounds3D.of(0, 1, 0, 1, 0, 1)

    def test_to_fds_xyz(self):
        """Test FDS output format with XYZ."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 2.0))
        fds_str = dev.to_fds()
        assert "&DEVC" in fds_str
        assert "ID='TEMP1'" in fds_str
        assert "QUANTITY='TEMPERATURE'" in fds_str
        assert "XYZ=1.0,1.0,2.0" in fds_str

    def test_to_fds_xb(self):
        """Test FDS output format with XB."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xb=Bounds3D.of(0, 1, 0, 1, 0, 1))
        fds_str = dev.to_fds()
        assert "XB=" in fds_str
        assert "0" in fds_str and "1" in fds_str


class TestDeviceLocationValidation:
    """Tests for device location validation."""

    def test_requires_location(self):
        """Test that device requires at least one location specification."""
        with pytest.raises(
            ValueError, match="requires XYZ, XB, XBP, DB, INIT_ID, DUCT_ID, or NODE_ID"
        ):
            Device(id="TEMP1", quantity="TEMPERATURE")

    def test_cannot_have_xyz_and_xb(self):
        """Test that cannot specify both XYZ and XB."""
        with pytest.raises(ValueError, match="Cannot specify both XYZ and XB"):
            Device(
                id="TEMP1",
                quantity="TEMPERATURE",
                xyz=Point3D.of(1.0, 1.0, 2.0),
                xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            )

    def test_db_is_valid_location(self):
        """Test that DB is a valid location specification."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", db="WHOLE DOMAIN")
        assert dev.db == "WHOLE DOMAIN"

    def test_xbp_is_valid_location(self):
        """Test that XBP is a valid location specification."""
        dev = Device(id="LINE1", quantity="TEMPERATURE", xbp=Bounds3D.of(0, 1, 0, 0, 0, 0))
        assert dev.xbp == Bounds3D.of(0, 1, 0, 0, 0, 0)

    def test_init_id_is_valid_location(self):
        """Test that INIT_ID is a valid location specification."""
        dev = Device(id="PART1", quantity="TEMPERATURE", init_id="INIT_PARTICLES")
        assert dev.init_id == "INIT_PARTICLES"

    def test_xyz_and_xb_allowed_with_xbp(self):
        """Test that XYZ and XB can be specified together with XBP for line output."""
        dev = Device(
            id="LINE1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            xbp=Bounds3D.of(0, 1, 0, 0, 0, 0),
        )
        assert dev.xyz is not None
        assert dev.xb is not None
        assert dev.xbp is not None


class TestDeviceIorValidation:
    """Tests for IOR (index of orientation) validation."""

    @pytest.mark.parametrize("ior", [-3, -2, -1, 1, 2, 3])
    def test_valid_ior_values(self, ior):
        """Test valid IOR values."""
        dev = Device(id="TEMP1", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 2.0), ior=ior)
        assert dev.ior == ior

    @pytest.mark.parametrize("ior", [-4, 0, 4, 5, 10])
    def test_invalid_ior_values(self, ior):
        """Test invalid IOR values."""
        with pytest.raises(ValueError, match="IOR must be"):
            Device(id="TEMP1", quantity="TEMPERATURE", xyz=Point3D.of(1.0, 1.0, 2.0), ior=ior)


class TestDeviceTripDirectionValidation:
    """Tests for TRIP_DIRECTION validation."""

    @pytest.mark.parametrize("trip_dir", [-1, 0, 1])
    def test_valid_trip_direction_values(self, trip_dir):
        """Test valid TRIP_DIRECTION values."""
        dev = Device(
            id="DET1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            setpoint=74.0,
            trip_direction=trip_dir,
        )
        assert dev.trip_direction == trip_dir

    @pytest.mark.parametrize("trip_dir", [-2, 2, 3])
    def test_invalid_trip_direction_values(self, trip_dir):
        """Test invalid TRIP_DIRECTION values."""
        with pytest.raises(ValueError, match="TRIP_DIRECTION must be"):
            Device(
                id="DET1",
                quantity="TEMPERATURE",
                xyz=Point3D.of(1.0, 1.0, 2.0),
                setpoint=74.0,
                trip_direction=trip_dir,
            )


class TestDeviceControlParameters:
    """Tests for control-related parameters."""

    def test_control_device(self):
        """Test device with control parameters."""
        dev = Device(
            id="DETECTOR1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(3.0, 5.6, 2.3),
            prop_id="K-11",
            setpoint=74.0,
            initial_state=False,
            latch=True,
            trip_direction=1,
            delay=5.0,
        )
        assert dev.setpoint == 74.0
        assert dev.initial_state is False
        assert dev.latch is True
        assert dev.trip_direction == 1
        assert dev.delay == 5.0

    def test_smoothing_parameters(self):
        """Test smoothing parameters."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            smoothing_factor=0.5,
            smoothing_time=2.0,
        )
        assert dev.smoothing_factor == 0.5
        assert dev.smoothing_time == 2.0


class TestDeviceOutputParameters:
    """Tests for output-related parameters."""

    def test_output_control(self):
        """Test output control parameters."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            output=True,
            time_averaged=True,
            time_history=True,
            hide_coordinates=False,
        )
        assert dev.output is True
        assert dev.time_averaged is True
        assert dev.time_history is True
        assert dev.hide_coordinates is False

    def test_conversion_parameters(self):
        """Test conversion parameters."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            units="°F",
            conversion_factor=1.8,
            conversion_addend=32.0,
        )
        assert dev.units == "°F"
        assert dev.conversion_factor == 1.8
        assert dev.conversion_addend == 32.0

    def test_relative_and_absolute_value(self):
        """Test relative and absolute value parameters."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            relative=True,
            absolute_value=True,
        )
        assert dev.relative is True
        assert dev.absolute_value is True


class TestDeviceStatisticsParameters:
    """Tests for statistics-related parameters."""

    def test_spatial_statistics(self):
        """Test spatial statistics parameters."""
        dev = Device(
            id="AVG_TEMP",
            quantity="TEMPERATURE",
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            spatial_statistic="MEAN",
        )
        assert dev.spatial_statistic == "MEAN"

    def test_temporal_statistics(self):
        """Test temporal statistics parameters."""
        dev = Device(
            id="RMS_TEMP",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            temporal_statistic="RMS",
            statistics_start=10.0,
            statistics_end=100.0,
        )
        assert dev.temporal_statistic == "RMS"
        assert dev.statistics_start == 10.0
        assert dev.statistics_end == 100.0

    def test_quantity_range(self):
        """Test quantity range parameter."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            spatial_statistic="MEAN",
            quantity_range=(20.0, 100.0),
        )
        assert dev.quantity_range == (20.0, 100.0)

    def test_time_integral_parameters(self):
        """Test time integral parameters."""
        dev = Device(
            id="DOSE1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            temporal_statistic="TIME INTEGRAL",
            n_intervals=20,
            time_period=60.0,
        )
        assert dev.n_intervals == 20
        assert dev.time_period == 60.0


class TestDeviceMeasurementParameters:
    """Tests for measurement-related parameters."""

    def test_species_measurement(self):
        """Test species measurement parameters."""
        dev = Device(
            id="CO2_1",
            quantity="MASS FRACTION",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            spec_id="CARBON DIOXIDE",
            dry=True,
        )
        assert dev.spec_id == "CARBON DIOXIDE"
        assert dev.dry is True

    def test_particle_measurement(self):
        """Test particle measurement parameters."""
        dev = Device(
            id="PART_TEMP1",
            quantity="PARTICLE TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            part_id="WATER DROPS",
            lp_tag=5,
        )
        assert dev.part_id == "WATER DROPS"
        assert dev.lp_tag == 5

    def test_material_measurement(self):
        """Test material measurement parameters."""
        dev = Device(
            id="MATL1",
            quantity="SOLID DENSITY",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            ior=3,
            matl_id="WOOD",
            depth=0.01,
        )
        assert dev.matl_id == "WOOD"
        assert dev.depth == 0.01

    def test_orientation_parameters(self):
        """Test orientation parameters."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            orientation=(0.0, 1.0, 0.0),
            rotation=45.0,
        )
        assert dev.orientation == (0.0, 1.0, 0.0)
        assert dev.rotation == 45.0

    def test_velocity_index(self):
        """Test velocity index parameter."""
        dev = Device(
            id="VEL1",
            quantity="VELOCITY",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            velo_index=1,
        )
        assert dev.velo_index == 1

    def test_force_direction(self):
        """Test force direction parameter."""
        dev = Device(
            id="FORCE1",
            quantity="FORCE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            force_direction=(0.0, 0.0, 1.0),
        )
        assert dev.force_direction == (0.0, 0.0, 1.0)


class TestDeviceLineFileParameters:
    """Tests for line file output parameters."""

    def test_line_file_basic(self):
        """Test basic line file parameters."""
        dev = Device(
            id="LINE1",
            quantity="TEMPERATURE",
            xbp=Bounds3D.of(0, 1, 0.5, 0.5, 0.5, 0.5),
            points=10,
            quantity2="VELOCITY",
        )
        assert dev.points == 10
        assert dev.quantity2 == "VELOCITY"

    def test_line_file_points_arrays(self):
        """Test line file points array parameters."""
        dev = Device(
            id="LINE1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(0.5, 0.5, 0.5),
            points_array_x=[0.0, 0.5, 1.0],
            points_array_y=[0.5, 0.5, 0.5],
            points_array_z=[0.5, 0.5, 0.5],
        )
        assert dev.points_array_x == [0.0, 0.5, 1.0]
        assert dev.points_array_y == [0.5, 0.5, 0.5]
        assert dev.points_array_z == [0.5, 0.5, 0.5]

    def test_line_file_offsets(self):
        """Test line file offset parameters."""
        dev = Device(
            id="LINE1",
            quantity="TEMPERATURE",
            xbp=Bounds3D.of(0, 1, 0.5, 0.5, 0.5, 0.5),
            dx=0.1,
            dy=0.0,
            dz=0.0,
        )
        assert dev.dx == 0.1
        assert dev.dy == 0.0
        assert dev.dz == 0.0

    def test_line_file_id_parameters(self):
        """Test line file ID parameters."""
        dev = Device(
            id="LINE1",
            quantity="TEMPERATURE",
            xbp=Bounds3D.of(0, 1, 0.5, 0.5, 0.5, 0.5),
            x_id="X_COORD",
            y_id="Y_COORD",
            z_id="Z_COORD",
            d_id="DIST",
            r_id="RADIUS",
            xyz_units="cm",
            coord_factor=100.0,
        )
        assert dev.x_id == "X_COORD"
        assert dev.y_id == "Y_COORD"
        assert dev.z_id == "Z_COORD"
        assert dev.d_id == "DIST"
        assert dev.r_id == "RADIUS"
        assert dev.xyz_units == "cm"
        assert dev.coord_factor == 100.0


class TestDeviceHvacParameters:
    """Tests for HVAC-related parameters."""

    def test_hvac_duct_device(self):
        """Test HVAC duct device."""
        dev = Device(
            id="DUCT_TEMP1",
            quantity="DUCT TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            duct_id="MAIN_DUCT",
            cell_l=0.1,
        )
        assert dev.duct_id == "MAIN_DUCT"
        assert dev.cell_l == 0.1

    def test_hvac_node_device(self):
        """Test HVAC node device."""
        dev = Device(
            id="NODE_FLOW",
            quantity="NODE TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            node_id=("NODE1", "NODE2"),
        )
        assert dev.node_id == ("NODE1", "NODE2")

    def test_aspiration_detector(self):
        """Test aspiration detector parameters."""
        dev = Device(
            id="ASD1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            flowrate=0.01,
            bypass_flowrate=0.005,
            delay=30.0,
        )
        assert dev.flowrate == 0.01
        assert dev.bypass_flowrate == 0.005
        assert dev.delay == 30.0


class TestDeviceFreezeParameters:
    """Tests for device freeze/update parameters."""

    def test_no_update_devc_id(self):
        """Test NO_UPDATE_DEVC_ID parameter."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            no_update_devc_id="FREEZE_DEVICE",
        )
        assert dev.no_update_devc_id == "FREEZE_DEVICE"

    def test_no_update_ctrl_id(self):
        """Test NO_UPDATE_CTRL_ID parameter."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            no_update_ctrl_id="FREEZE_CONTROL",
        )
        assert dev.no_update_ctrl_id == "FREEZE_CONTROL"


class TestDeviceRelationships:
    """Tests for device relationship parameters."""

    def test_device_references(self):
        """Test device reference parameters."""
        dev = Device(
            id="SPR1",
            xyz=Point3D.of(3.0, 5.6, 2.3),
            prop_id="K-11",
            ctrl_id="CTRL1",
            devc_id="REF_DEVICE",
            move_id="MOVE1",
            surf_id="SURF1",
        )
        assert dev.prop_id == "K-11"
        assert dev.ctrl_id == "CTRL1"
        assert dev.devc_id == "REF_DEVICE"
        assert dev.move_id == "MOVE1"
        assert dev.surf_id == "SURF1"

    def test_reaction_reference(self):
        """Test reaction reference parameter."""
        dev = Device(
            id="HRR1",
            quantity="HRR",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            reac_id="PROPANE",
        )
        assert dev.reac_id == "PROPANE"

    def test_pipe_index(self):
        """Test pipe index for sprinkler pressure."""
        dev = Device(
            id="PRES1",
            quantity="PIPE PRESSURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            pipe_index=2,
        )
        assert dev.pipe_index == 2


class TestDeviceFdsOutput:
    """Tests for FDS output generation."""

    def test_fds_output_with_control(self):
        """Test FDS output with control parameters."""
        dev = Device(
            id="DETECTOR1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(3.0, 5.6, 2.3),
            prop_id="K-11",
            setpoint=74.0,
            latch=True,
            trip_direction=1,
        )
        fds_str = dev.to_fds()
        assert "ID='DETECTOR1'" in fds_str
        assert "QUANTITY='TEMPERATURE'" in fds_str
        assert "XYZ=3.0,5.6,2.3" in fds_str
        assert "PROP_ID='K-11'" in fds_str
        assert "SETPOINT=74.0" in fds_str
        assert "LATCH=.TRUE." in fds_str
        assert "TRIP_DIRECTION=1" in fds_str

    def test_fds_output_with_statistics(self):
        """Test FDS output with statistics parameters."""
        dev = Device(
            id="AVG_TEMP",
            quantity="TEMPERATURE",
            xb=Bounds3D.of(0, 1, 0, 1, 0, 1),
            spatial_statistic="MEAN",
            statistics_start=10.0,
            statistics_end=100.0,
        )
        fds_str = dev.to_fds()
        assert "SPATIAL_STATISTIC='MEAN'" in fds_str
        assert "STATISTICS_START=10.0" in fds_str
        assert "STATISTICS_END=100.0" in fds_str

    def test_fds_output_with_orientation(self):
        """Test FDS output with orientation parameters."""
        dev = Device(
            id="TEMP1",
            quantity="TEMPERATURE",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            ior=-3,
            orientation=(0.0, 1.0, 0.0),
            rotation=45.0,
        )
        fds_str = dev.to_fds()
        assert "IOR=-3" in fds_str
        assert "ROTATION=45.0" in fds_str

    def test_fds_output_with_dry(self):
        """Test FDS output with dry measurement."""
        dev = Device(
            id="CO2_1",
            quantity="MASS FRACTION",
            xyz=Point3D.of(1.0, 1.0, 2.0),
            spec_id="CARBON DIOXIDE",
            dry=True,
        )
        fds_str = dev.to_fds()
        assert "SPEC_ID='CARBON DIOXIDE'" in fds_str
        assert "DRY=.TRUE." in fds_str
