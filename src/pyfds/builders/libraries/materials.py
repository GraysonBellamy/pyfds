"""Common building and structural materials library."""

from ...core.namelist import Material
from ..material import MaterialBuilder


class CommonMaterials:
    """
    Library of predefined common materials.

    Provides easy access to standard building and structural materials
    with typical thermal properties.

    Examples
    --------
    >>> concrete = CommonMaterials.concrete()
    >>> steel = CommonMaterials.steel()
    >>> wood = CommonMaterials.wood()
    """

    @staticmethod
    def concrete() -> Material:
        """
        Standard concrete.

        Properties
        ----------
        - Density: 2400 kg/m³
        - Thermal conductivity: 1.6 W/(m·K)
        - Specific heat: 0.88 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Concrete material object
        """
        return (
            MaterialBuilder("CONCRETE")
            .density(2400)
            .thermal_conductivity(1.6)
            .specific_heat(0.88)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def gypsum() -> Material:
        """
        Gypsum board (drywall).

        Properties
        ----------
        - Density: 930 kg/m³
        - Thermal conductivity: 0.48 W/(m·K)
        - Specific heat: 0.84 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Gypsum board material object
        """
        return (
            MaterialBuilder("GYPSUM")
            .density(930)
            .thermal_conductivity(0.48)
            .specific_heat(0.84)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def steel() -> Material:
        """
        Structural steel.

        Properties
        ----------
        - Density: 7850 kg/m³
        - Thermal conductivity: 45.8 W/(m·K)
        - Specific heat: 0.46 kJ/(kg·K)
        - Emissivity: 0.7

        Returns
        -------
        Material
            Steel material object
        """
        return (
            MaterialBuilder("STEEL")
            .density(7850)
            .thermal_conductivity(45.8)
            .specific_heat(0.46)
            .emissivity(0.7)
            .build()
        )

    @staticmethod
    def aluminum() -> Material:
        """
        Aluminum.

        Properties
        ----------
        - Density: 2700 kg/m³
        - Thermal conductivity: 237 W/(m·K)
        - Specific heat: 0.90 kJ/(kg·K)
        - Emissivity: 0.2

        Returns
        -------
        Material
            Aluminum material object
        """
        return (
            MaterialBuilder("ALUMINUM")
            .density(2700)
            .thermal_conductivity(237)
            .specific_heat(0.90)
            .emissivity(0.2)
            .build()
        )

    @staticmethod
    def brick() -> Material:
        """
        Standard brick.

        Properties
        ----------
        - Density: 1920 kg/m³
        - Thermal conductivity: 0.69 W/(m·K)
        - Specific heat: 0.84 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Brick material object
        """
        return (
            MaterialBuilder("BRICK")
            .density(1920)
            .thermal_conductivity(0.69)
            .specific_heat(0.84)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def wood() -> Material:
        """
        Wood (pine).

        Properties
        ----------
        - Density: 500 kg/m³
        - Thermal conductivity: 0.13 W/(m·K)
        - Specific heat: 2.5 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Wood material object
        """
        return (
            MaterialBuilder("WOOD")
            .density(500)
            .thermal_conductivity(0.13)
            .specific_heat(2.5)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def fiberglass_insulation() -> Material:
        """
        Fiberglass insulation.

        Properties
        ----------
        - Density: 12 kg/m³
        - Thermal conductivity: 0.04 W/(m·K)
        - Specific heat: 0.84 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Fiberglass insulation material object
        """
        return (
            MaterialBuilder("FIBERGLASS")
            .density(12)
            .thermal_conductivity(0.04)
            .specific_heat(0.84)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def ceramic() -> Material:
        """
        Ceramic.

        Properties
        ----------
        - Density: 2300 kg/m³
        - Thermal conductivity: 1.5 W/(m·K)
        - Specific heat: 0.90 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Ceramic material object
        """
        return (
            MaterialBuilder("CERAMIC")
            .density(2300)
            .thermal_conductivity(1.5)
            .specific_heat(0.90)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def glass() -> Material:
        """
        Glass.

        Properties
        ----------
        - Density: 2500 kg/m³
        - Thermal conductivity: 0.80 W/(m·K)
        - Specific heat: 0.84 kJ/(kg·K)
        - Emissivity: 0.9

        Returns
        -------
        Material
            Glass material object
        """
        return (
            MaterialBuilder("GLASS")
            .density(2500)
            .thermal_conductivity(0.80)
            .specific_heat(0.84)
            .emissivity(0.9)
            .build()
        )

    @staticmethod
    def copper() -> Material:
        """
        Copper.

        Properties
        ----------
        - Density: 8930 kg/m³
        - Thermal conductivity: 401 W/(m·K)
        - Specific heat: 0.39 kJ/(kg·K)
        - Emissivity: 0.03

        Returns
        -------
        Material
            Copper material object
        """
        return (
            MaterialBuilder("COPPER")
            .density(8930)
            .thermal_conductivity(401)
            .specific_heat(0.39)
            .emissivity(0.03)
            .build()
        )
