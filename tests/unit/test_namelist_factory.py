"""
Unit tests for the NamelistFactory.
"""

import pytest

from pyfds.core.namelists import Head, Mesh, NamelistFactory, Surface, Time


class TestNamelistFactory:
    """Test NamelistFactory functionality."""

    def test_create_head(self):
        """Test creating HEAD namelist."""
        head = NamelistFactory.create("head", chid="test", title="Test Case")
        assert isinstance(head, Head)
        assert head.chid == "test"
        assert head.title == "Test Case"

    def test_create_mesh(self):
        """Test creating MESH namelist."""
        mesh = NamelistFactory.create("mesh", ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
        assert isinstance(mesh, Mesh)
        assert mesh.ijk == (10, 10, 10)
        assert mesh.xb == (0, 1, 0, 1, 0, 1)

    def test_create_unknown_namelist(self):
        """Test creating unknown namelist raises error."""
        with pytest.raises(ValueError, match="Unknown namelist type 'unknown'"):
            NamelistFactory.create("unknown")

    def test_from_dict_head(self):
        """Test creating HEAD from dictionary."""
        data = {"chid": "test_dict", "title": "Dict Test"}
        head = NamelistFactory.from_dict("head", data)
        assert isinstance(head, Head)
        assert head.chid == "test_dict"
        assert head.title == "Dict Test"

    def test_from_dict_case_insensitive(self):
        """Test dictionary keys are case insensitive."""
        data = {"CHID": "upper", "TITLE": "Upper Case"}
        head = NamelistFactory.from_dict("HEAD", data)
        assert isinstance(head, Head)
        assert head.chid == "upper"
        assert head.title == "Upper Case"

    def test_parse_fds_namelist_simple(self):
        """Test parsing simple FDS namelist."""
        fds_text = "&HEAD CHID='simple' /"
        head = NamelistFactory.parse_fds_namelist(fds_text)
        assert isinstance(head, Head)
        assert head.chid == "simple"
        assert head.title is None

    def test_parse_fds_namelist_with_arrays(self):
        """Test parsing FDS namelist with arrays."""
        fds_text = "&MESH XB=0,5,0,5,0,5, IJK=10,10,10 /"
        mesh = NamelistFactory.parse_fds_namelist(fds_text)
        assert isinstance(mesh, Mesh)
        assert mesh.xb == (0.0, 5.0, 0.0, 5.0, 0.0, 5.0)
        assert mesh.ijk == (10, 10, 10)

    def test_parse_fds_namelist_multiple_params(self):
        """Test parsing FDS namelist with multiple parameters."""
        fds_text = "&TIME T_END=100.0, DT=0.1 /"
        time = NamelistFactory.parse_fds_namelist(fds_text)
        assert isinstance(time, Time)
        assert time.t_end == 100.0
        assert time.dt == 0.1

    def test_parse_fds_namelist_invalid(self):
        """Test parsing invalid FDS namelist raises error."""
        with pytest.raises(ValueError):
            NamelistFactory.parse_fds_namelist("INVALID")

    def test_parse_fds_file(self, tmp_path):
        """Test parsing complete FDS file."""
        fds_content = """&HEAD CHID='test_file' /
&MESH XB=0,10,0,10,0,10, IJK=20,20,20 /
&TIME T_END=60.0 /
&SURF ID='floor', RGB=128,128,128 /
&TAIL /
"""
        fds_file = tmp_path / "test.fds"
        fds_file.write_text(fds_content)

        namelists = NamelistFactory.parse_fds_file(str(fds_file))

        assert "HEAD" in namelists
        assert "MESH" in namelists
        assert "TIME" in namelists
        assert "SURFACE" in namelists

        assert len(namelists["HEAD"]) == 1
        assert len(namelists["MESH"]) == 1
        assert len(namelists["TIME"]) == 1
        assert len(namelists["SURFACE"]) == 1

        assert isinstance(namelists["HEAD"][0], Head)
        assert isinstance(namelists["MESH"][0], Mesh)
        assert isinstance(namelists["SURFACE"][0], Surface)

    def test_round_trip_dict(self):
        """Test creating from dict and converting back."""
        original_data = {"chid": "round_trip", "title": "Round Trip Test"}
        head = NamelistFactory.from_dict("head", original_data)
        assert isinstance(head, Head)

        # Convert back to FDS and parse again
        fds_text = head.to_fds()
        parsed_head = NamelistFactory.parse_fds_namelist(fds_text.strip())
        assert isinstance(parsed_head, Head)

        assert parsed_head.chid == head.chid
        assert parsed_head.title == head.title

    def test_parse_fds_value_string(self):
        """Test parsing various FDS value types."""
        # String
        assert NamelistFactory._parse_fds_value("'test'") == "test"
        assert NamelistFactory._parse_fds_value('"test"') == "test"

        # Boolean
        assert NamelistFactory._parse_fds_value(".TRUE.") is True
        assert NamelistFactory._parse_fds_value(".FALSE.") is False

        # Numbers
        assert NamelistFactory._parse_fds_value("42") == 42
        assert NamelistFactory._parse_fds_value("3.14") == 3.14

        # Arrays
        assert NamelistFactory._parse_fds_value("1,2,3") == [1, 2, 3]
        assert NamelistFactory._parse_fds_value("(1,2,3)") == [1, 2, 3]

    def test_split_fds_parameters(self):
        """Test splitting FDS parameters."""
        params_text = "XB=0,10,0,10,0,10, IJK=20,20,20"
        parts = NamelistFactory._split_fds_parameters(params_text)
        assert len(parts) == 2
        assert "XB=0,10,0,10,0,10" in parts
        assert "IJK=20,20,20" in parts
