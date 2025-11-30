"""PyFDS command-line interface."""

import argparse
from pathlib import Path

from pyfds import __version__
from pyfds.execution import run_fds
from pyfds.io import parse_fds


def main() -> int:
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        prog="pyfds",
        description="Python interface to NIST Fire Dynamics Simulator",
    )
    parser.add_argument("--version", action="version", version=f"PyFDS {__version__}")

    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # Validate command
    validate_parser = subparsers.add_parser("validate", help="Validate FDS file")
    validate_parser.add_argument("file", type=Path, help="FDS input file")

    # Run command
    run_parser = subparsers.add_parser("run", help="Run FDS simulation")
    run_parser.add_argument("file", type=Path, help="FDS input file")
    run_parser.add_argument("-n", "--threads", type=int, default=1, help="Number of OpenMP threads")
    run_parser.add_argument("--mpi", type=int, default=1, help="Number of MPI processes")

    args = parser.parse_args()

    if args.command == "validate":
        return cmd_validate(args.file)
    if args.command == "run":
        return cmd_run(args.file, args.threads, args.mpi)
    parser.print_help()
    return 0


def cmd_validate(file: Path) -> int:
    """Validate an FDS file."""
    try:
        sim = parse_fds(file)
        issues = sim.validate()
        if issues:
            for issue in issues:
                print(issue)
            return 1
        print(f"✓ {file} is valid")
        return 0
    except Exception as e:
        print(f"Error: {e}")
        return 1


def cmd_run(file: Path, threads: int, mpi: int) -> int:
    """Run an FDS simulation."""
    from pyfds.config import RunConfig

    try:
        config = RunConfig(n_threads=threads, n_mpi=mpi)
        result = run_fds(file, config)
        print(f"Simulation complete: {result.chid}")
        return 0
    except Exception as e:
        print(f"Error: {e}")
        return 1


__all__ = ["main"]
