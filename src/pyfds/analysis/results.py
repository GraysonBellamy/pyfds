"""
Results container for FDS simulation outputs.
"""

from pathlib import Path
from typing import Any

import polars as pl

from pyfds.io.parsers.csv_parser import CSVParser


class Results:
    """
    Container for FDS simulation results.

    Provides easy access to simulation output data including device
    measurements, heat release rate, and other CSV outputs.

    Parameters
    ----------
    chid : str
        Case identifier (CHID from FDS input)
    output_dir : Path or str
        Directory containing output files (default: current directory)

    Examples
    --------
    >>> results = Results("room_fire")
    >>> print(f"Peak HRR: {results.hrr['HRR'].max():.1f} kW")
    >>> temp_data = results.devices["TEMP_1"]
    >>> print(f"Max temp: {temp_data['TEMP_1'].max():.1f} C")

    Attributes
    ----------
    chid : str
        Case identifier
    output_dir : Path
        Output directory
    """

    def __init__(self, chid: str, output_dir: Path | str = "."):
        self.chid = chid
        self.output_dir = Path(output_dir)
        self._parser = CSVParser()
        self._devc_df: pl.DataFrame | None = None
        self._hrr_df: pl.DataFrame | None = None

    @property
    def devc_file(self) -> Path:
        """Get path to device CSV file."""
        return self.output_dir / f"{self.chid}_devc.csv"

    @property
    def hrr_file(self) -> Path:
        """Get path to HRR CSV file."""
        return self.output_dir / f"{self.chid}_hrr.csv"

    @property
    def out_file(self) -> Path:
        """Get path to .out diagnostic file."""
        return self.output_dir / f"{self.chid}.out"

    @property
    def devc(self) -> pl.DataFrame:
        """
        Get device data as Polars DataFrame.

        Returns
        -------
        pl.DataFrame
            Device data with Time column and device columns

        Raises
        ------
        FileNotFoundError
            If device CSV file doesn't exist

        Examples
        --------
        >>> results = Results("case")
        >>> df = results.devc
        >>> print(df.columns)
        ['Time', 'TEMP_1', 'TEMP_2', ...]
        """
        if self._devc_df is None:
            if not self.devc_file.exists():
                raise FileNotFoundError(
                    f"Device CSV file not found: {self.devc_file}\n"
                    "Make sure the simulation completed successfully."
                )
            self._devc_df = self._parser.parse_devc(self.devc_file)
        return self._devc_df

    @property
    def hrr(self) -> pl.DataFrame:
        """
        Get heat release rate data as Polars DataFrame.

        Returns
        -------
        pl.DataFrame
            HRR data with Time and HRR columns

        Raises
        ------
        FileNotFoundError
            If HRR CSV file doesn't exist

        Examples
        --------
        >>> results = Results("case")
        >>> hrr_df = results.hrr
        >>> peak_hrr = hrr_df["HRR"].max()
        """
        if self._hrr_df is None:
            if not self.hrr_file.exists():
                raise FileNotFoundError(
                    f"HRR CSV file not found: {self.hrr_file}\n"
                    "Make sure the simulation completed successfully."
                )
            self._hrr_df = self._parser.parse_hrr(self.hrr_file)
        return self._hrr_df

    @property
    def devices(self) -> dict[str, pl.DataFrame]:
        """
        Get dictionary of device data indexed by device ID.

        Returns
        -------
        Dict[str, pl.DataFrame]
            Dictionary mapping device IDs to their data

        Examples
        --------
        >>> results = Results("case")
        >>> temp_data = results.devices["TEMP_1"]
        >>> print(temp_data["TEMP_1"].max())
        """
        devc_df = self.devc
        device_ids = self._parser.list_devices(devc_df)

        devices_dict = {}
        for device_id in device_ids:
            devices_dict[device_id] = self._parser.get_device_data(devc_df, device_id)

        return devices_dict

    def get_device(self, device_id: str) -> pl.DataFrame:
        """
        Get data for a specific device.

        Parameters
        ----------
        device_id : str
            Device identifier

        Returns
        -------
        pl.DataFrame
            DataFrame with Time and device columns

        Examples
        --------
        >>> results = Results("case")
        >>> temp = results.get_device("TEMP_1")
        >>> max_temp = temp["TEMP_1"].max()
        """
        return self._parser.get_device_data(self.devc, device_id)

    def list_devices(self) -> list[str]:
        """
        List all available device IDs.

        Returns
        -------
        List[str]
            List of device IDs

        Examples
        --------
        >>> results = Results("case")
        >>> devices = results.list_devices()
        >>> print(f"Found {len(devices)} devices: {devices}")
        """
        return self._parser.list_devices(self.devc)

    def plot_hrr(self, save_as: str | Path | None = None) -> None:
        """
        Plot heat release rate over time.

        Parameters
        ----------
        save_as : str or Path, optional
            Save plot to file instead of displaying

        Examples
        --------
        >>> results = Results("case")
        >>> results.plot_hrr()  # Display plot
        >>> results.plot_hrr("hrr_plot.png")  # Save to file
        """
        import matplotlib.pyplot as plt

        hrr_df = self.hrr

        plt.figure(figsize=(10, 6))
        plt.plot(hrr_df["Time"], hrr_df["HRR"], linewidth=2)
        plt.xlabel("Time (s)")
        plt.ylabel("Heat Release Rate (kW)")
        plt.title(f"Heat Release Rate - {self.chid}")
        plt.grid(True, alpha=0.3)

        if save_as:
            plt.savefig(save_as, dpi=300, bbox_inches="tight")
            plt.close()
        else:
            plt.show()

    def plot_device(self, device_id: str, save_as: str | Path | None = None) -> None:
        """
        Plot device data over time.

        Parameters
        ----------
        device_id : str
            Device identifier
        save_as : str or Path, optional
            Save plot to file instead of displaying

        Examples
        --------
        >>> results = Results("case")
        >>> results.plot_device("TEMP_1")
        >>> results.plot_device("TEMP_1", "temp_plot.png")
        """
        import matplotlib.pyplot as plt

        device_df = self.get_device(device_id)

        plt.figure(figsize=(10, 6))
        plt.plot(device_df["Time"], device_df[device_id], linewidth=2)
        plt.xlabel("Time (s)")
        plt.ylabel(device_id)
        plt.title(f"{device_id} - {self.chid}")
        plt.grid(True, alpha=0.3)

        if save_as:
            plt.savefig(save_as, dpi=300, bbox_inches="tight")
            plt.close()
        else:
            plt.show()

    def summary(self) -> dict[str, Any]:
        """
        Get summary statistics for the simulation.

        Returns
        -------
        Dict[str, Any]
            Dictionary containing summary statistics

        Examples
        --------
        >>> results = Results("case")
        >>> summary = results.summary()
        >>> print(f"Peak HRR: {summary['peak_hrr']:.1f} kW")
        >>> print(f"Simulation duration: {summary['duration']:.1f} s")
        """
        summary: dict[str, Any] = {
            "chid": self.chid,
            "output_dir": str(self.output_dir),
        }

        # HRR statistics
        if self.hrr_file.exists():
            hrr_df = self.hrr
            # Ensure HRR and Time columns are numeric
            if hrr_df["HRR"].dtype != pl.Float64:
                hrr_df = hrr_df.with_columns(pl.col("HRR").cast(pl.Float64, strict=False))
            if hrr_df["Time"].dtype != pl.Float64:
                hrr_df = hrr_df.with_columns(pl.col("Time").cast(pl.Float64, strict=False))

            # After type conversion to Float64, these are safe to convert to float
            # Note: type ignores needed for CI mypy; may show as unused locally
            peak = hrr_df["HRR"].max()
            summary["peak_hrr"] = float(peak) if peak is not None else 0.0  # type: ignore[arg-type,unused-ignore]

            hrr_sum = hrr_df["HRR"].sum()
            time_delta = hrr_df["Time"][1] - hrr_df["Time"][0]
            summary["total_energy"] = float(hrr_sum * time_delta) if hrr_sum is not None else 0.0

            duration = hrr_df["Time"].max()
            summary["duration"] = float(duration) if duration is not None else 0.0  # type: ignore[arg-type,unused-ignore]

        # Device statistics
        if self.devc_file.exists():
            device_ids = self.list_devices()
            summary["num_devices"] = len(device_ids)
            summary["device_ids"] = device_ids

        return summary

    def __repr__(self) -> str:
        """String representation of Results."""
        return f"Results(chid='{self.chid}', output_dir='{self.output_dir}')"
