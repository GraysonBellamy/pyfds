# Parametric Studies

Automate sensitivity analysis and parameter sweeps with PyFDS.

## Overview

Parametric studies help you:

- Understand sensitivity to input parameters
- Perform grid convergence studies
- Optimize designs
- Generate data for meta-models
- Validate simulation approaches

```python
# Simple parameter sweep
for hrr in [500, 1000, 1500, 2000]:
    sim = Simulation(chid=f'fire_{hrr}kW')
    sim.surface(id='FIRE', hrrpua=hrr)
    # ... rest of setup
    sim.write(f'fire_{hrr}kW.fds')
```

## Grid Convergence Study

Verify numerical accuracy by refining mesh resolution.

```python
from pyfds import Simulation
from pyfds.core.geometry import Point3D
import numpy as np

def create_grid_study(chid_base, mesh_multiplier):
    """Create simulation with scaled mesh resolution."""
    sim = Simulation(chid=f'{chid_base}_mesh{mesh_multiplier}')
    sim.add(Time(t_end=300.0)

    # Base resolution: 0.1m cells
    # Multiply to get finer/coarser meshes
    base_ijk = (50, 50, 25)  # 5m × 5m × 2.5m domain
    ijk = tuple(int(n * mesh_multiplier) for n in base_ijk)

    sim.add(Mesh(ijk=ijk, xb=Bounds3D.of(0, 5, 0, 5, 0, 2.5))

    # Fire (same in all cases)
    sim.surface(id='FIRE', hrrpua=1000.0)
    sim.add(Obstruction(xb=Bounds3D.of(2, 3, 2, 3, 0, 0.1), surf_id='FIRE')

    # Measurement devices
    sim.device(id='TEMP_CEIL', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 2.4))
    sim.device(id='TEMP_MID', quantity='TEMPERATURE', xyz=Point3D.of(2.5, 2.5, 1.25))

    return sim

# Create grid refinement study
# Coarse, medium, fine, very fine
multipliers = [0.5, 1.0, 2.0, 4.0]
cell_sizes = [0.2, 0.1, 0.05, 0.025]  # meters

for mult, dx in zip(multipliers, cell_sizes):
    sim = create_grid_study('convergence', mult)
    sim.write(f'convergence_dx{int(dx*1000)}mm.fds')
    print(f"Created mesh with Δx = {dx*1000:.0f} mm")

# After running all cases, analyze convergence
# Expected: Results should converge as mesh refines
```

**Analysis**:
```python
# Compare peak temperatures across meshes
import polars as pl
import matplotlib.pyplot as plt

results = {}
for dx in [0.2, 0.1, 0.05, 0.025]:
    chid = f'convergence_dx{int(dx*1000)}mm'
    res = FDSResults(chid)
    temp = res.get_device('TEMP_CEIL')
    results[dx] = temp['Value'].max()

# Plot convergence
plt.figure(figsize=(10, 6))
plt.plot(list(results.keys()), list(results.values()), 'o-')
plt.xlabel('Cell Size (m)')
plt.ylabel('Peak Ceiling Temperature (°C)')
plt.title('Grid Convergence Study')
plt.grid(True)
plt.xscale('log')
plt.savefig('grid_convergence.png')
```

## Fire Size Sensitivity

Vary fire heat release rate.

```python
from pyfds import Simulation

def create_fire_sensitivity(hrr):
    """Create simulation with specified HRR."""
    sim = Simulation(chid=f'fire_hrr{hrr}')
    sim.add(Time(t_end=600.0)
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

    # Variable fire intensity
    sim.surface(id='FIRE', hrrpua=hrr, color='ORANGE')
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

    # Fixed measurements
    sim.device(id='TEMP_CEIL', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4))
    sim.device(id='HF_WALL', quantity='GAUGE HEAT FLUX', xyz=Point3D.of(0.1, 2.5, 1.5), ior=1)
    sim.device(id='VIS', quantity='VISIBILITY', xyz=Point3D.of(3, 2.5, 1.5))

    # Door
    sim.add(Vent(xb=Bounds3D.of(6, 6, 2, 3, 0, 2.1), surf_id='OPEN')

    return sim

# Parameter sweep: Fire size
hrr_values = [250, 500, 750, 1000, 1500, 2000, 2500]

for hrr in hrr_values:
    sim = create_fire_sensitivity(hrr)
    sim.write(f'fire_hrr{hrr}.fds')
    print(f"Created simulation with HRR = {hrr} kW/m²")

# Total HRR varies: 250-2500 kW (1m² fire area)
```

**Analysis**:
```python
# Create sensitivity plot
hrr_values = [250, 500, 750, 1000, 1500, 2000, 2500]
peak_temps = []
peak_hf = []

for hrr in hrr_values:
    res = FDSResults(f'fire_hrr{hrr}')
    peak_temps.append(res.get_device('TEMP_CEIL')['Value'].max())
    peak_hf.append(res.get_device('HF_WALL')['Value'].max())

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

ax1.plot(hrr_values, peak_temps, 'ro-', linewidth=2)
ax1.set_xlabel('HRRPUA (kW/m²)')
ax1.set_ylabel('Peak Ceiling Temp (°C)')
ax1.grid(True)

ax2.plot(hrr_values, peak_hf, 'bs-', linewidth=2)
ax2.set_xlabel('HRRPUA (kW/m²)')
ax2.set_ylabel('Peak Wall Heat Flux (kW/m²)')
ax2.grid(True)

plt.tight_layout()
plt.savefig('fire_sensitivity.png')
```

## Ventilation Study

Vary door/window opening sizes.

```python
from pyfds import Simulation

def create_ventilation_study(door_width):
    """Create simulation with variable door width."""
    sim = Simulation(chid=f'vent_w{int(door_width*10)}')
    sim.add(Time(t_end=600.0)
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

    # Fixed fire
    sim.surface(id='FIRE', hrrpua=1000.0)
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

    # Variable door width (centered at y=2.5, height 2.1m)
    y_center = 2.5
    y_min = y_center - door_width / 2
    y_max = y_center + door_width / 2

    sim.add(Vent(xb=Bounds3D.of(6, 6, y_min, y_max, 0, 2.1), surf_id='OPEN')

    # Measurements
    sim.device(id='TEMP_CEIL', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4))
    sim.device(id='LAYER_HT', quantity='LAYER HEIGHT', xyz=Point3D.of(3, 2.5, 1.25))
    sim.device(id='O2', quantity='VOLUME FRACTION', spec_id='OXYGEN', xyz=Point3D.of(3, 2.5, 1.5))

    return sim

# Sweep door width from 0.5m to 2.0m
door_widths = [0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0]

for width in door_widths:
    sim = create_ventilation_study(width)
    sim.write(f'vent_w{int(width*10)}.fds')
    print(f"Created simulation with door width = {width} m")
```

## Material Property Sensitivity

Vary wall material properties.

```python
from pyfds import Simulation

def create_material_study(conductivity, label):
    """Create simulation with variable wall conductivity."""
    sim = Simulation(chid=f'wall_{label}')
    sim.add(Time(t_end=600.0)
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

    # Variable wall material
    sim.material(
        id='WALL_MATL',
        conductivity=conductivity,
        specific_heat=0.84,
        density=1440.0
    )

    sim.surface(
        id='WALL_SURF',
        matl_id='WALL_MATL',
        thickness=0.0127
    )

    # Wall
    sim.add(Obstruction(xb=Bounds3D.of(0, 0.15, 0, 5, 0, 2.5), surf_id='WALL_SURF')

    # Fire
    sim.surface(id='FIRE', hrrpua=1000.0)
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

    # Wall temperature measurements
    sim.device(id='WALL_TEMP_FRONT', quantity='WALL TEMPERATURE', xyz=Point3D.of(0.1, 2.5, 1.5), ior=1)
    sim.device(id='WALL_TEMP_BACK', quantity='WALL TEMPERATURE', xyz=Point3D.of(0.14, 2.5, 1.5), ior=-1)

    return sim

# Sweep thermal conductivity (W/m·K)
materials = {
    'insulation': 0.04,
    'wood': 0.12,
    'gypsum': 0.48,
    'concrete': 1.8,
    'steel': 45.8
}

for label, k in materials.items():
    sim = create_material_study(k, label)
    sim.write(f'wall_{label}.fds')
    print(f"Created simulation with k = {k} W/m·K ({label})")
```

## Multi-Parameter Study

Combine multiple parameters (design of experiments).

```python
from pyfds import Simulation
import itertools

def create_multi_param_study(hrr, door_width, wall_k):
    """Create simulation with multiple parameters."""
    chid = f'multi_h{hrr}_w{int(door_width*10)}_k{int(wall_k*100)}'
    sim = Simulation(chid=chid)
    sim.add(Time(t_end=600.0)
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5))

    # Wall material
    sim.material(id='WALL', conductivity=wall_k, specific_heat=0.84, density=1440.0)
    sim.surface(id='WALL_SURF', matl_id='WALL', thickness=0.0127)
    sim.add(Obstruction(xb=Bounds3D.of(0, 0.15, 0, 5, 0, 2.5), surf_id='WALL_SURF')

    # Fire
    sim.surface(id='FIRE', hrrpua=hrr)
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE')

    # Door
    y_center = 2.5
    sim.add(Vent(xb=Bounds3D.of(6, 6, y_center-door_width/2, y_center+door_width/2, 0, 2.1), surf_id='OPEN')

    # Measurements
    sim.device(id='TEMP_CEIL', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4))
    sim.device(id='LAYER_HT', quantity='LAYER HEIGHT', xyz=Point3D.of(3, 2.5, 1.25))

    return sim

# Define parameter ranges
hrr_values = [500, 1000, 1500]
door_widths = [0.75, 1.0, 1.5]
wall_conductivities = [0.12, 0.48, 1.8]

# Full factorial design: 3³ = 27 simulations
cases = list(itertools.product(hrr_values, door_widths, wall_conductivities))

print(f"Creating {len(cases)} simulations...")

for i, (hrr, width, k) in enumerate(cases):
    sim = create_multi_param_study(hrr, width, k)
    sim.write(f'case_{i+1:03d}.fds')
    print(f"Case {i+1}/{len(cases)}: HRR={hrr}, Width={width}, k={k}")
```

**Analysis with Polars**:
```python
import polars as pl

# Collect results from all cases
results = []

for i, (hrr, width, k) in enumerate(cases):
    chid = f'multi_h{hrr}_w{int(width*10)}_k{int(k*100)}'
    res = FDSResults(chid)

    temp_data = res.get_device('TEMP_CEIL')
    layer_data = res.get_device('LAYER_HT')

    results.append({
        'case': i+1,
        'hrr': hrr,
        'door_width': width,
        'wall_k': k,
        'peak_temp': temp_data['Value'].max(),
        'min_layer_height': layer_data['Value'].min(),
        'time_to_60C': temp_data.filter(pl.col('Value') >= 60.0)['Time'][0] if len(temp_data.filter(pl.col('Value') >= 60.0)) > 0 else None
    })

# Create DataFrame
df = pl.DataFrame(results)

# Save results
df.write_csv('parametric_results.csv')

# Summary statistics
print("\nMean peak temperature by HRR:")
print(df.group_by('hrr').agg(pl.col('peak_temp').mean()))

print("\nMean layer height by door width:")
print(df.group_by('door_width').agg(pl.col('min_layer_height').mean()))
```

## Automated Batch Execution

Run multiple simulations automatically.

```python
from pyfds import Simulation, FDSRunner
import subprocess
from pathlib import Path

def run_parametric_batch(case_files, n_threads=4):
    """
    Run multiple FDS simulations in batch.

    Parameters
    ----------
    case_files : list of str
        List of .fds files to run
    n_threads : int
        OpenMP threads per simulation
    """
    results = {}

    for case_file in case_files:
        print(f"\nRunning {case_file}...")

        # Run FDS
        cmd = f"fds {case_file}"
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            env={'OMP_NUM_THREADS': str(n_threads)}
        )

        if result.returncode == 0:
            print(f"  ✓ Completed successfully")
            results[case_file] = 'success'
        else:
            print(f"  ✗ Failed: {result.stderr}")
            results[case_file] = 'failed'

    return results

# Run all cases
case_files = [f'case_{i:03d}.fds' for i in range(1, 28)]
results = run_parametric_batch(case_files, n_threads=4)

# Summary
successful = sum(1 for r in results.values() if r == 'success')
print(f"\n{successful}/{len(case_files)} simulations completed successfully")
```

## Response Surface Analysis

Fit response surface to results.

```python
import polars as pl
import numpy as np
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

# Load parametric results
df = pl.read_csv('parametric_results.csv').to_pandas()

# Prepare features (HRR, door width, wall conductivity)
X = df[['hrr', 'door_width', 'wall_k']].values
y = df['peak_temp'].values

# Fit quadratic response surface
poly = PolynomialFeatures(degree=2, include_bias=False)
X_poly = poly.fit_transform(X)

model = LinearRegression()
model.fit(X_poly, y)

# Predict on fine grid (for plotting)
hrr_grid = np.linspace(500, 1500, 50)
width_grid = np.linspace(0.75, 1.5, 50)
k_fixed = 0.48  # Fix wall conductivity

HRR, WIDTH = np.meshgrid(hrr_grid, width_grid)
X_grid = np.column_stack([
    HRR.ravel(),
    WIDTH.ravel(),
    np.full(HRR.size, k_fixed)
])
X_grid_poly = poly.transform(X_grid)
predictions = model.predict(X_grid_poly).reshape(HRR.shape)

# Contour plot
plt.figure(figsize=(10, 8))
contour = plt.contourf(HRR, WIDTH, predictions, levels=20, cmap='hot')
plt.colorbar(contour, label='Peak Temperature (°C)')
plt.scatter(df['hrr'], df['door_width'], c='white', edgecolors='black', s=100, label='Simulation Points')
plt.xlabel('HRRPUA (kW/m²)')
plt.ylabel('Door Width (m)')
plt.title(f'Response Surface (k = {k_fixed} W/m·K)')
plt.legend()
plt.savefig('response_surface.png', dpi=300)
```

## Best Practices

### 1. Choose Parameters Wisely

```python
# Focus on uncertain or influential parameters
# HRR, ventilation, material properties typically important
# Geometry changes often require new mesh
```

### 2. Use Appropriate Ranges

```python
# Realistic parameter ranges
hrr_values = [500, 1000, 1500]  # Reasonable fire sizes

# Avoid extreme/unphysical values
hrr_values = [1, 10000]  # Too extreme!
```

### 3. Start with Screening Study

```python
# Run a few cases first to identify trends
# Then refine around interesting regions
```

### 4. Document Everything

```python
# Create parameter log
import json

params = {
    'study_name': 'fire_sensitivity',
    'parameters': {
        'hrr': list(hrr_values),
        'door_width': list(door_widths)
    },
    'date': '-11-26',
    'notes': 'Investigating fire-ventilation interaction'
}

with open('study_parameters.json', 'w') as f:
    json.dump(params, f, indent=2)
```

## Next Steps

- [Basic Examples](basic.md) - Simple simulations
- [Advanced Examples](advanced.md) - Complex scenarios
- [Workflows](workflows.md) - Complete analysis workflows
- [Analysis Guide](../execution/analysis.md) - Results processing

---

[Complete Workflows →](workflows.md){ .md-button .md-button--primary }
