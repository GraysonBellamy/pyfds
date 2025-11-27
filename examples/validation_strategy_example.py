"""
Example demonstrating the Validation Strategy Pattern in PyFDS.

This example shows how to use different validation strategies
with the Simulation class.
"""

from pyfds import BasicValidationStrategy, ComprehensiveValidationStrategy, Simulation


def main():
    print("PyFDS Validation Strategy Pattern Example")
    print("=" * 40)

    print("\n1. Using Basic Validation Strategy (default):")
    basic_strategy = BasicValidationStrategy()
    sim_basic = Simulation("example_basic", validation_strategy=basic_strategy)
    warnings = sim_basic.validate()
    print(f"   Warnings: {warnings}")

    print("\n2. Using Comprehensive Validation Strategy:")
    comprehensive_strategy = ComprehensiveValidationStrategy()
    sim_comprehensive = Simulation(
        "example_comprehensive", validation_strategy=comprehensive_strategy
    )
    warnings = sim_comprehensive.validate()
    print(f"   Warnings: {warnings}")

    print("\n3. Default behavior (uses BasicValidationStrategy):")
    sim_default = Simulation("example_default")
    warnings = sim_default.validate()
    print(f"   Warnings: {warnings}")

    print("\n4. Valid simulation with all required components:")
    valid_sim = Simulation("valid_example")
    valid_sim.time(t_end=100.0)
    valid_sim.mesh(ijk=(10, 10, 10), xb=(0, 1, 0, 1, 0, 1))
    warnings = valid_sim.validate()
    print(f"   Warnings: {warnings}")

    print("\nStrategy pattern allows flexible validation approaches!")
    print("- BasicValidationStrategy: Fast, lightweight validation")
    print("- ComprehensiveValidationStrategy: Thorough validation with detailed checks")
    print("- Easy to add new strategies for specific validation needs")


if __name__ == "__main__":
    main()
