"""
Example demonstrating the Unified Validation System in PyFDS.

This example shows how to use the unified Validator class
for comprehensive simulation validation.
"""

from pyfds import Severity, Simulation, Validator
from pyfds.core.geometry import Bounds3D, Grid3D
from pyfds.core.namelists.mesh import Mesh
from pyfds.core.namelists.obst import Obstruction
from pyfds.core.namelists.surf import Surface
from pyfds.core.namelists.time import Time


def main():
    print("PyFDS Unified Validation System Example")
    print("=" * 40)

    print("\n1. Creating a simulation with missing required components:")
    sim = Simulation("example_basic")
    validator = Validator()
    issues = validator.validate(sim)

    print(f"   Found {len(issues)} validation issues:")
    for issue in issues:
        print(f"   [{issue.severity.value.upper()}] {issue.message}")

    print("\n2. Adding required components (TIME and MESH):")
    sim.add(Time(t_end=100.0))
    sim.add(Mesh(ijk=Grid3D.of(10, 10, 10), xb=Bounds3D.of(0, 1, 0, 1, 0, 1)))
    issues = validator.validate(sim)

    print(f"   Found {len(issues)} validation issues:")
    for issue in issues:
        print(f"   [{issue.severity.value.upper()}] {issue.message}")

    print("\n3. Adding a surface and obstruction with mesh quality warnings:")
    sim.add(Surface(id="FIRE", hrrpua=1000.0))
    sim.add(Obstruction(xb=Bounds3D.of(0.3, 0.7, 0.3, 0.7, 0, 0.5), surf_id="FIRE"))
    issues = validator.validate(sim)

    print(f"   Found {len(issues)} validation issues:")
    for issue in issues:
        print(f"   [{issue.severity.value.upper()}] {issue.message}")

    print("\n4. Using the Simulation.validate() convenience method:")
    warnings = sim.validate()
    print(f"   Warnings: {warnings}")

    print("\n5. Filtering issues by severity:")
    all_issues = validator.validate(sim)
    errors = [i for i in all_issues if i.severity == Severity.ERROR]
    warnings = [i for i in all_issues if i.severity == Severity.WARNING]
    infos = [i for i in all_issues if i.severity == Severity.INFO]

    print(f"   Errors: {len(errors)}")
    print(f"   Warnings: {len(warnings)}")
    print(f"   Info: {len(infos)}")

    print("\nUnified validation provides:")
    print("- Structured ValidationIssue objects with severity levels")
    print("- Comprehensive checks for physical reasonableness")
    print("- Cross-reference validation between components")
    print("- Mesh quality assessment and recommendations")
    print("- Easy filtering and processing of validation results")


if __name__ == "__main__":
    main()
