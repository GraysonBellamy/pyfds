"""
CSV file parser for FDS output data.
"""

from pathlib import Path

import polars as pl


class CSVParser:
    """
    Parse FDS CSV output files.

    FDS generates several types of CSV files:
    - *_devc.csv: Device output data
    - *_hrr.csv: Heat release rate data
    - Custom CSV files from other outputs

    All are parsed into Polars DataFrames for efficient analysis.

    Examples
    --------
    >>> parser = CSVParser()
    >>> df = parser.parse("case_devc.csv")
    >>> print(df.columns)
    ['Time', 'TEMP_1', 'TEMP_2', ...]
    """

    def parse(self, csv_file: str | Path) -> pl.DataFrame:
        """
        Parse an FDS CSV file.

        Parameters
        ----------
        csv_file : str or Path
            Path to CSV file

        Returns
        -------
        pl.DataFrame
            Parsed data as Polars DataFrame

        Raises
        ------
        FileNotFoundError
            If CSV file doesn't exist
        ValueError
            If file format is invalid

        Examples
        --------
        >>> parser = CSVParser()
        >>> df = parser.parse("case_devc.csv")
        >>> max_temp = df["TEMP_1"].max()
        """
        csv_file = Path(csv_file)

        if not csv_file.exists():
            raise FileNotFoundError(f"CSV file not found: {csv_file}")

        try:
            # FDS CSV files typically have:
            # - First line: column headers
            # - Subsequent lines: data
            # - May have units in second row (s, C, kW, etc.)

            # Read the first line to detect format
            with csv_file.open() as f:
                first_line = f.readline().strip()

            # Check if first line looks like units (s, kW, C, etc.) rather than headers
            # FDS newer versions put column names first, then units second
            # But sometimes units might be on first line
            skip_rows = 0
            if first_line and "," in first_line:
                first_cols = [col.strip() for col in first_line.split(",")]
                # If most columns are single letters or units, skip this row
                unit_like = sum(
                    1
                    for col in first_cols
                    if len(col) <= 5
                    and col.lower() in ["s", "kw", "c", "k", "m", "kg/s", "pa", "mw", "w"]
                )
                if unit_like > len(first_cols) // 2:
                    skip_rows = 1

            # Read with Polars
            df = pl.read_csv(
                csv_file,
                has_header=True,
                skip_rows=skip_rows,
                null_values=["", "null", "NULL"],
                try_parse_dates=False,
                truncate_ragged_lines=True,  # Handle malformed lines
            )

            # Check if second row (now first row after skip) contains units
            if len(df) > 0:
                first_row = df.row(0)
                # If first row is all strings (units), skip it
                if all(isinstance(val, str) for val in first_row):
                    df = df.slice(1)

            # FDS uses 's' for the time column, rename it to 'Time'
            if "s" in df.columns and "Time" not in df.columns:
                df = df.rename({"s": "Time"})

            # Strip whitespace from all string columns
            for col in df.columns:
                if df[col].dtype == pl.String:
                    df = df.with_columns(pl.col(col).str.strip_chars())

            # Convert all columns to numeric where possible
            from contextlib import suppress

            for col in df.columns:
                with suppress(Exception):
                    df = df.with_columns(pl.col(col).cast(pl.Float64, strict=False))

            return df

        except Exception as e:
            raise ValueError(f"Failed to parse CSV file {csv_file}: {e}") from e

    def parse_devc(self, devc_file: str | Path) -> pl.DataFrame:
        """
        Parse device output CSV file.

        Parameters
        ----------
        devc_file : str or Path
            Path to *_devc.csv file

        Returns
        -------
        pl.DataFrame
            Device data with Time column and device columns

        Examples
        --------
        >>> parser = CSVParser()
        >>> df = parser.parse_devc("case_devc.csv")
        >>> temps = df.select(["Time", "TEMP_1", "TEMP_2"])
        """
        df = self.parse(devc_file)

        # Validate that we have a Time column
        if "Time" not in df.columns and "TIME" not in df.columns:
            # Try to find time column (case-insensitive)
            time_cols = [c for c in df.columns if c.lower() == "time"]
            if time_cols:
                df = df.rename({time_cols[0]: "Time"})
            else:
                raise ValueError("No Time column found in device CSV file")

        return df

    def parse_hrr(self, hrr_file: str | Path) -> pl.DataFrame:
        """
        Parse heat release rate CSV file.

        Parameters
        ----------
        hrr_file : str or Path
            Path to *_hrr.csv file

        Returns
        -------
        pl.DataFrame
            HRR data with Time and HRR columns

        Examples
        --------
        >>> parser = CSVParser()
        >>> df = parser.parse_hrr("case_hrr.csv")
        >>> peak_hrr = df["HRR"].max()
        >>> print(f"Peak HRR: {peak_hrr:.1f} kW")
        """
        df = self.parse(hrr_file)

        # Validate HRR file structure
        if "Time" not in df.columns and "TIME" not in df.columns:
            time_cols = [c for c in df.columns if c.lower() == "time"]
            if time_cols:
                df = df.rename({time_cols[0]: "Time"})

        # HRR column is typically "HRR" but could be "Total HRR"
        if "HRR" not in df.columns:
            hrr_cols = [c for c in df.columns if "hrr" in c.lower()]
            if hrr_cols and hrr_cols[0] != "HRR":
                df = df.rename({hrr_cols[0]: "HRR"})

        return df

    def get_device_data(self, df: pl.DataFrame, device_id: str) -> pl.DataFrame:
        """
        Extract data for a specific device.

        Parameters
        ----------
        df : pl.DataFrame
            Full device DataFrame
        device_id : str
            Device ID to extract

        Returns
        -------
        pl.DataFrame
            DataFrame with Time and device columns

        Examples
        --------
        >>> parser = CSVParser()
        >>> df = parser.parse_devc("case_devc.csv")
        >>> temp_data = parser.get_device_data(df, "TEMP_1")
        """
        if device_id not in df.columns:
            available = [c for c in df.columns if c != "Time"]
            raise ValueError(f"Device '{device_id}' not found. Available devices: {available}")

        return df.select(["Time", device_id])

    def list_devices(self, df: pl.DataFrame) -> list[str]:
        """
        List all device IDs in a device DataFrame.

        Parameters
        ----------
        df : pl.DataFrame
            Device DataFrame

        Returns
        -------
        List[str]
            List of device IDs

        Examples
        --------
        >>> parser = CSVParser()
        >>> df = parser.parse_devc("case_devc.csv")
        >>> devices = parser.list_devices(df)
        >>> print(f"Found {len(devices)} devices")
        """
        # Return all columns except Time
        return [c for c in df.columns if c.lower() != "time"]
