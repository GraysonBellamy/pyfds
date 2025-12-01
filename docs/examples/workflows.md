# Complete Workflows

End-to-end workflows from simulation setup through analysis and reporting.

## Overview

Complete workflows demonstrate:

- Project organization
- Simulation creation and execution
- Results analysis and visualization
- Report generation
- Best practices

## Simple Fire Analysis Workflow

Complete workflow for a basic room fire study.

### 1. Project Setup

```python
from pathlib import Path
import shutil

# Create project structure
project = Path('room_fire_study')
project.mkdir(exist_ok=True)

# Subdirectories
(project / 'inputs').mkdir(exist_ok=True)
(project / 'outputs').mkdir(exist_ok=True)
(project / 'analysis').mkdir(exist_ok=True)
(project / 'reports').mkdir(exist_ok=True)

print(f"Created project: {project}")
```

### 2. Create Simulation

```python
from pyfds import Simulation
from pyfds.core.geometry import Point3D

# simulation_setup.py
def create_room_fire():
    """Create room fire simulation."""
    sim = Simulation(chid='room_fire')
    sim.add(Time(t_end=600.0))

    # Domain
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5)))

    # Materials
    sim.add(Material(id='GYPSUM', conductivity=0.48, specific_heat=0.84, density=1440.0))
    sim.add(Surface(id='GYPSUM_WALL', matl_id='GYPSUM', thickness=0.0127))

    # Walls
    sim.add(Obstruction(xb=Bounds3D.of(0, 0.15, 0, 5, 0, 2.5), surf_id='GYPSUM_WALL'))
    sim.add(Obstruction(xb=Bounds3D.of(5.85, 6, 0, 5, 0, 2.5), surf_id='GYPSUM_WALL'))

    # Fire
    sim.add(Surface(id='FIRE', hrrpua=1000.0, color='ORANGE'))
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE'))

    # Door
    sim.add(Vent(xb=Bounds3D.of(6, 6, 2, 3, 0, 2.1), surf_id='OPEN'))

    # Devices
    for z, label in [(0.5, 'LOW'), (1.5, 'MID'), (2.4, 'HIGH')]:
        sim.add(Device(id=f'TEMP_{label}', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, z)))

    sim.add(Device(id='LAYER_HT', quantity='LAYER HEIGHT', xyz=Point3D.of(3, 2.5, 1.25)))
    sim.add(Device(id='VIS', quantity='VISIBILITY', xyz=Point3D.of(3, 2.5, 1.5)))
    sim.add(Device(id='HF_WALL', quantity='GAUGE HEAT FLUX', xyz=Point3D.of(0.2, 2.5, 1.5), ior=1))

    return sim

# Create and save
sim = create_room_fire()
sim.write('room_fire_study/inputs/room_fire.fds')
print("Simulation file created")
```

### 3. Run Simulation

```python
# run_simulation.py
import subprocess
from pathlib import Path

def run_fds(fds_file, n_threads=4):
    """Run FDS simulation."""
    # Change to input directory
    input_dir = Path(fds_file).parent
    fds_name = Path(fds_file).name

    cmd = f'cd {input_dir} && fds {fds_name}'

    print(f"Running: {cmd}")
    result = subprocess.run(
        cmd,
        shell=True,
        capture_output=True,
        text=True,
        env={'OMP_NUM_THREADS': str(n_threads)}
    )

    if result.returncode == 0:
        print("✓ Simulation completed successfully")
        return True
    else:
        print(f"✗ Simulation failed:\n{result.stderr}")
        return False

# Run
success = run_fds('room_fire_study/inputs/room_fire.fds', n_threads=4)

# Move outputs
if success:
    import shutil
    for ext in ['*.out', '*_devc.csv', '*.smv']:
        for file in Path('room_fire_study/inputs').glob(ext):
            shutil.move(str(file), 'room_fire_study/outputs/')
    print("Moved output files")
```

### 4. Analyze Results

```python
# analyze_results.py
from pyfds.analysis import FDSResults
import polars as pl
import matplotlib.pyplot as plt

# Load results
results = FDSResults('room_fire_study/outputs/room_fire.fds')

# Extract device data
devices = {
    'TEMP_LOW': results.get_device('TEMP_LOW'),
    'TEMP_MID': results.get_device('TEMP_MID'),
    'TEMP_HIGH': results.get_device('TEMP_HIGH'),
    'LAYER_HT': results.get_device('LAYER_HT'),
    'VIS': results.get_device('VIS'),
    'HF_WALL': results.get_device('HF_WALL')
}

# Key metrics
print("=== Key Metrics ===")
print(f"Peak ceiling temp: {devices['TEMP_HIGH']['Value'].max():.1f}°C")
print(f"Min layer height: {devices['LAYER_HT']['Value'].min():.2f}m")
print(f"Min visibility: {devices['VIS']['Value'].min():.1f}m")
print(f"Peak wall heat flux: {devices['HF_WALL']['Value'].max():.1f} kW/m²")

# Time to critical conditions
temp_crit = 60.0  # °C (tenability limit)
vis_crit = 10.0   # m (visibility limit)

temp_high = devices['TEMP_HIGH']
time_to_temp = temp_high.filter(pl.col('Value') >= temp_crit)
if len(time_to_temp) > 0:
    t_temp = time_to_temp['Time'][0]
    print(f"\nTime to {temp_crit}°C: {t_temp:.0f}s")

vis_data = devices['VIS']
time_to_vis = vis_data.filter(pl.col('Value') <= vis_crit)
if len(time_to_vis) > 0:
    t_vis = time_to_vis['Time'][0]
    print(f"Time to {vis_crit}m visibility: {t_vis:.0f}s")

# Create plots
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(14, 10))

# Temperature profiles
for name, label in [('TEMP_LOW', 'Low'), ('TEMP_MID', 'Mid'), ('TEMP_HIGH', 'High')]:
    data = devices[name]
    ax1.plot(data['Time'], data['Value'], label=label, linewidth=2)
ax1.axhline(y=60, color='r', linestyle='--', alpha=0.5, label='Tenability limit')
ax1.set_xlabel('Time (s)')
ax1.set_ylabel('Temperature (°C)')
ax1.set_title('Temperature at Different Heights')
ax1.legend()
ax1.grid(True, alpha=0.3)

# Layer height
layer = devices['LAYER_HT']
ax2.plot(layer['Time'], layer['Value'], 'b-', linewidth=2)
ax2.set_xlabel('Time (s)')
ax2.set_ylabel('Layer Height (m)')
ax2.set_title('Smoke Layer Height')
ax2.grid(True, alpha=0.3)

# Visibility
vis = devices['VIS']
ax3.plot(vis['Time'], vis['Value'], 'g-', linewidth=2)
ax3.axhline(y=10, color='r', linestyle='--', alpha=0.5, label='Critical visibility')
ax3.set_xlabel('Time (s)')
ax3.set_ylabel('Visibility (m)')
ax3.set_title('Visibility at Eye Level')
ax3.legend()
ax3.grid(True, alpha=0.3)

# Heat flux
hf = devices['HF_WALL']
ax4.plot(hf['Time'], hf['Value'], 'orange', linewidth=2)
ax4.set_xlabel('Time (s)')
ax4.set_ylabel('Heat Flux (kW/m²)')
ax4.set_title('Wall Heat Flux')
ax4.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('room_fire_study/analysis/results_summary.png', dpi=300, bbox_inches='tight')
print("\nPlot saved to analysis/results_summary.png")

# Export data
for name, data in devices.items():
    data.write_csv(f'room_fire_study/analysis/{name}.csv')
print("Data exported to CSV files")
```

### 5. Generate Report

```python
# generate_report.py
from datetime import datetime

report = f"""
# Room Fire Analysis Report

**Date**: {datetime.now().strftime('%Y-%m-%d')}
**Project**: Room Fire Study

## Executive Summary

A fire dynamics simulation was performed for a 6m × 5m × 2.5m room with a 1 m² fire
source. The simulation investigated temperature development, smoke layer descent,
visibility degradation, and heat exposure to walls.

## Simulation Setup

- **Domain**: 6m × 5m × 2.5m room
- **Fire**: 1000 kW/m² HRRPUA, 1 m² area (1 MW total)
- **Ventilation**: Single 1m wide door
- **Walls**: Gypsum board (0.0127m thick)
- **Duration**: 600 seconds

## Key Results

### Critical Conditions

| Metric | Value | Time | Criterion |
|--------|-------|------|-----------|
| Peak Ceiling Temp | {devices['TEMP_HIGH']['Value'].max():.1f}°C | - | >60°C |
| Min Layer Height | {devices['LAYER_HT']['Value'].min():.2f}m | - | <2.0m |
| Min Visibility | {devices['VIS']['Value'].min():.1f}m | - | <10m |
| Peak Wall Heat Flux | {devices['HF_WALL']['Value'].max():.1f} kW/m² | - | - |

### Temperature Development

Ceiling temperatures reached {devices['TEMP_HIGH']['Value'].max():.0f}°C, well above
the tenability limit of 60°C. The fire created a strong thermal stratification with
ceiling temperatures significantly higher than floor-level temperatures.

### Smoke Layer

The smoke layer descended to {devices['LAYER_HT']['Value'].min():.2f}m above the floor,
reducing the clear height available for evacuation.

### Visibility

Visibility at eye level (1.5m) decreased to {devices['VIS']['Value'].min():.1f}m,
below the 10m threshold for safe evacuation in unfamiliar spaces.

## Conclusions

1. **Tenability**: Conditions become untenable for occupants within the first few minutes
2. **Evacuation**: Limited time available for safe evacuation
3. **Thermal Exposure**: Walls experience moderate heat flux, structural integrity maintained
4. **Recommendations**: Consider additional ventilation or earlier detection/suppression

## Plots

![Results Summary](../analysis/results_summary.png)

## Appendices

### A. Input File

See `inputs/room_fire.fds`

### B. Raw Data

Device data available in `analysis/` directory as CSV files.

---

**Report Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
"""

# Save report
with open('room_fire_study/reports/analysis_report.md', 'w') as f:
    f.write(report)

print("Report generated: reports/analysis_report.md")
```

## Parametric Study Workflow

Complete workflow for sensitivity analysis.

### Project Structure

```python
# parametric_workflow.py
from pathlib import Path

project = Path('sensitivity_study')
(project / 'inputs').mkdir(parents=True, exist_ok=True)
(project / 'outputs').mkdir(exist_ok=True)
(project / 'analysis').mkdir(exist_ok=True)
(project / 'reports').mkdir(exist_ok=True)
```

### Generate Cases

```python
from pyfds import Simulation
import json

def create_parametric_case(case_id, hrr, door_width):
    """Create parametric case."""
    sim = Simulation(chid=f'case_{case_id:03d}')
    sim.add(Time(t_end=600.0))
    sim.add(Mesh(ijk=Grid3D.of(60, 50, 25), xb=Bounds3D.of(0, 6, 0, 5, 0, 2.5)))

    # Fire (variable)
    sim.add(Surface(id='FIRE', hrrpua=hrr))
    sim.add(Obstruction(xb=Bounds3D.of(2.5, 3.5, 2, 3, 0, 0.1), surf_id='FIRE'))

    # Door (variable width)
    y_center = 2.5
    sim.add(Vent(xb=Bounds3D.of(6, 6, y_center-door_width/2, y_center+door_width/2, 0, 2.1), surf_id='OPEN'))

    # Measurements
    sim.add(Device(id='TEMP_CEIL', quantity='TEMPERATURE', xyz=Point3D.of(3, 2.5, 2.4)))
    sim.add(Device(id='LAYER_HT', quantity='LAYER HEIGHT', xyz=Point3D.of(3, 2.5, 1.25)))

    return sim

# Parameter combinations
import itertools

hrr_values = [500, 1000, 1500]
door_widths = [0.75, 1.0, 1.5]

cases = list(itertools.product(hrr_values, door_widths))

# Create simulations
parameters = []
for i, (hrr, width) in enumerate(cases):
    sim = create_parametric_case(i+1, hrr, width)
    sim.write(f'sensitivity_study/inputs/case_{i+1:03d}.fds')

    parameters.append({
        'case_id': i+1,
        'hrr': hrr,
        'door_width': width
    })

# Save parameter log
with open('sensitivity_study/parameters.json', 'w') as f:
    json.dump(parameters, f, indent=2)

print(f"Created {len(cases)} parametric cases")
```

### Run All Cases

```python
import subprocess
from pathlib import Path
import json

# Load parameters
with open('sensitivity_study/parameters.json') as f:
    parameters = json.load(f)

# Run all cases
for params in parameters:
    case_id = params['case_id']
    fds_file = f'sensitivity_study/inputs/case_{case_id:03d}.fds'

    print(f"\nRunning Case {case_id}/{len(parameters)}...")
    print(f"  HRR: {params['hrr']} kW/m², Door: {params['door_width']} m")

    cmd = f"fds {fds_file}"
    result = subprocess.run(cmd, shell=True, capture_output=True)

    if result.returncode == 0:
        print(f"  ✓ Complete")
    else:
        print(f"  ✗ Failed")
```

### Analyze All Results

```python
import polars as pl
import json

# Load parameters
with open('sensitivity_study/parameters.json') as f:
    parameters = json.load(f)

# Collect results
results = []

for params in parameters:
    case_id = params['case_id']
    chid = f'case_{case_id:03d}'

    try:
        res = FDSResults(f'sensitivity_study/outputs/{chid}')

        temp = res.get_device('TEMP_CEIL')
        layer = res.get_device('LAYER_HT')

        results.append({
            **params,
            'peak_temp': temp['Value'].max(),
            'min_layer': layer['Value'].min()
        })
    except Exception as e:
        print(f"Error loading case {case_id}: {e}")

# Create DataFrame
df = pl.DataFrame(results)
df.write_csv('sensitivity_study/analysis/results_summary.csv')

print("\nResults Summary:")
print(df)

# Statistical analysis
print("\n=== Effect of HRR ===")
print(df.group_by('hrr').agg([
    pl.col('peak_temp').mean().alias('mean_temp'),
    pl.col('min_layer').mean().alias('mean_layer')
]))

print("\n=== Effect of Door Width ===")
print(df.group_by('door_width').agg([
    pl.col('peak_temp').mean().alias('mean_temp'),
    pl.col('min_layer').mean().alias('mean_layer')
]))
```

## Best Practices

### 1. Organize Projects

```
project_name/
├── inputs/          # .fds files
├── outputs/         # FDS outputs
├── analysis/        # Processed data, plots
├── reports/         # Final reports
└── scripts/         # Python scripts
```

### 2. Version Control

```bash
# Initialize git
cd project_name
git init
git add inputs/ scripts/
git commit -m "Initial simulation setup"

# .gitignore for large output files
echo "outputs/*" >> .gitignore
echo "*.smv" >> .gitignore
```

### 3. Document Parameters

```python
# Save metadata
import json
from datetime import datetime

metadata = {
    'project': 'Room Fire Study',
    'date': datetime.now().isoformat(),
    'author': 'Your Name',
    'description': 'Sensitivity study of fire size and ventilation',
    'parameters': {
        'hrr': [500, 1000, 1500],
        'door_width': [0.75, 1.0, 1.5]
    }
}

with open('project_metadata.json', 'w') as f:
    json.dump(metadata, f, indent=2)
```

### 4. Automate Reporting

```python
# Create automated report template
def generate_report(results_df, output_file):
    """Generate analysis report from results."""
    from jinja2 import Template

    template = Template("""
    # Analysis Report

    ## Summary Statistics

    Total cases: {{ n_cases }}
    Parameter ranges:
    - HRR: {{ hrr_min }}-{{ hrr_max }} kW/m²
    - Door width: {{ width_min }}-{{ width_max }} m

    ## Results

    Peak temperature range: {{ temp_min:.1f }}-{{ temp_max:.1f }}°C

    ![Results](plots/summary.png)
    """)

    report = template.render(
        n_cases=len(results_df),
        hrr_min=results_df['hrr'].min(),
        hrr_max=results_df['hrr'].max(),
        width_min=results_df['door_width'].min(),
        width_max=results_df['door_width'].max(),
        temp_min=results_df['peak_temp'].min(),
        temp_max=results_df['peak_temp'].max()
    )

    with open(output_file, 'w') as f:
        f.write(report)
```

## Next Steps

- [Basic Examples](basic.md) - Simple simulations
- [Advanced Examples](advanced.md) - Complex scenarios
- [Parametric Studies](parametric.md) - Sensitivity analysis
- [User Guide](../guide/index.md) - Detailed documentation

---

[Back to Examples →](index.md){ .md-button .md-button--primary }
