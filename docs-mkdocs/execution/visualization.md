# Visualization

Create plots and visualizations from FDS simulation results.

## Overview

PyFDS provides tools for creating:

- Time series plots
- Multi-parameter comparisons
- Contour plots
- 3D visualizations
- Animations
- Publication-quality figures

```python
from pyfds.analysis import FDSResults
import matplotlib.pyplot as plt

results = FDSResults('simulation.fds')
temp = results.get_device('TEMP_1')

plt.plot(temp['Time'], temp['Value'])
plt.xlabel('Time (s)')
plt.ylabel('Temperature (°C)')
plt.show()
```

## Basic Plotting

### Single Device Plot

```python
import matplotlib.pyplot as plt
from pyfds.analysis import FDSResults

results = FDSResults('room_fire')

# Get device data
temp = results.get_device('TEMP_CEILING')

# Create plot
plt.figure(figsize=(10, 6))
plt.plot(temp['Time'], temp['Value'], 'b-', linewidth=2)
plt.xlabel('Time (s)', fontsize=12)
plt.ylabel('Temperature (°C)', fontsize=12)
plt.title('Ceiling Temperature vs Time', fontsize=14)
plt.grid(True, alpha=0.3)
plt.savefig('temperature.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Multiple Devices

```python
# Plot multiple devices on same axes
devices = ['TEMP_LOW', 'TEMP_MID', 'TEMP_HIGH']
colors = ['blue', 'green', 'red']
labels = ['Floor Level', 'Mid-Height', 'Ceiling']

plt.figure(figsize=(12, 6))

for dev, color, label in zip(devices, colors, labels):
    data = results.get_device(dev)
    plt.plot(data['Time'], data['Value'], color=color, label=label, linewidth=2)

plt.xlabel('Time (s)', fontsize=12)
plt.ylabel('Temperature (°C)', fontsize=12)
plt.title('Temperature Stratification', fontsize=14)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.savefig('stratification.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Advanced Plots

### Subplots

```python
import matplotlib.pyplot as plt

fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# Temperature
temp = results.get_device('TEMP_CEILING')
axes[0, 0].plot(temp['Time'], temp['Value'], 'r-', linewidth=2)
axes[0, 0].set_ylabel('Temperature (°C)')
axes[0, 0].set_title('Temperature')
axes[0, 0].grid(True, alpha=0.3)

# Heat Flux
hf = results.get_device('HF_WALL')
axes[0, 1].plot(hf['Time'], hf['Value'], 'orange', linewidth=2)
axes[0, 1].set_ylabel('Heat Flux (kW/m²)')
axes[0, 1].set_title('Wall Heat Flux')
axes[0, 1].grid(True, alpha=0.3)

# Visibility
vis = results.get_device('VIS')
axes[1, 0].plot(vis['Time'], vis['Value'], 'g-', linewidth=2)
axes[1, 0].set_xlabel('Time (s)')
axes[1, 0].set_ylabel('Visibility (m)')
axes[1, 0].set_title('Visibility')
axes[1, 0].grid(True, alpha=0.3)

# Layer Height
layer = results.get_device('LAYER_HEIGHT')
axes[1, 1].plot(layer['Time'], layer['Value'], 'b-', linewidth=2)
axes[1, 1].set_xlabel('Time (s)')
axes[1, 1].set_ylabel('Layer Height (m)')
axes[1, 1].set_title('Smoke Layer')
axes[1, 1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('multi_parameter.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Filled Areas

```python
# Plot with shaded region
temp = results.get_device('TEMP_CEILING')

plt.figure(figsize=(10, 6))
plt.fill_between(
    temp['Time'],
    temp['Value'],
    60,  # Tenability limit
    where=(temp['Value'] >= 60),
    color='red',
    alpha=0.3,
    label='Above tenability limit'
)
plt.plot(temp['Time'], temp['Value'], 'b-', linewidth=2, label='Temperature')
plt.axhline(y=60, color='r', linestyle='--', label='Tenability limit (60°C)')
plt.xlabel('Time (s)')
plt.ylabel('Temperature (°C)')
plt.legend()
plt.grid(True, alpha=0.3)
plt.savefig('filled_area.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Parametric Study Visualizations

### Parameter Sweep Plot

```python
import polars as pl
import matplotlib.pyplot as plt

# Collect results from parametric study
hrr_values = [500, 1000, 1500, 2000]
peak_temps = []

for hrr in hrr_values:
    res = FDSResults(f'fire_{hrr}')
    temp = res.get_device('TEMP_CEILING')
    peak_temps.append(temp['Value'].max())

# Create plot
plt.figure(figsize=(10, 6))
plt.plot(hrr_values, peak_temps, 'ro-', linewidth=2, markersize=10)
plt.xlabel('HRRPUA (kW/m²)', fontsize=12)
plt.ylabel('Peak Ceiling Temperature (°C)', fontsize=12)
plt.title('Fire Size Sensitivity', fontsize=14)
plt.grid(True, alpha=0.3)
plt.savefig('sensitivity.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Heatmap

```python
import numpy as np
import matplotlib.pyplot as plt

# 2D parameter sweep results
hrr_values = [500, 1000, 1500]
door_widths = [0.75, 1.0, 1.5]

# Collect peak temperatures
results_matrix = np.zeros((len(hrr_values), len(door_widths)))

for i, hrr in enumerate(hrr_values):
    for j, width in enumerate(door_widths):
        res = FDSResults(f'case_h{hrr}_w{int(width*10)}')
        temp = res.get_device('TEMP_CEILING')
        results_matrix[i, j] = temp['Value'].max()

# Create heatmap
fig, ax = plt.subplots(figsize=(10, 8))
im = ax.imshow(results_matrix, cmap='hot', aspect='auto')

# Labels
ax.set_xticks(np.arange(len(door_widths)))
ax.set_yticks(np.arange(len(hrr_values)))
ax.set_xticklabels(door_widths)
ax.set_yticklabels(hrr_values)

ax.set_xlabel('Door Width (m)', fontsize=12)
ax.set_ylabel('HRRPUA (kW/m²)', fontsize=12)
ax.set_title('Peak Temperature Heatmap', fontsize=14)

# Colorbar
cbar = plt.colorbar(im, ax=ax)
cbar.set_label('Temperature (°C)', fontsize=12)

# Add values as text
for i in range(len(hrr_values)):
    for j in range(len(door_widths)):
        text = ax.text(j, i, f'{results_matrix[i, j]:.0f}',
                      ha="center", va="center", color="white", fontsize=10)

plt.tight_layout()
plt.savefig('heatmap.png', dpi=300, bbox_inches='tight')
plt.show()
```

## 3D Visualization

### Surface Plot

```python
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

# Create meshgrid
HRR, WIDTH = np.meshgrid(hrr_values, door_widths)

# 3D surface plot
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')

surf = ax.plot_surface(HRR, WIDTH, results_matrix.T, cmap='hot', alpha=0.8)

ax.set_xlabel('HRRPUA (kW/m²)')
ax.set_ylabel('Door Width (m)')
ax.set_zlabel('Peak Temperature (°C)')
ax.set_title('Temperature Response Surface')

fig.colorbar(surf, shrink=0.5)
plt.savefig('surface3d.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Custom Styling

### Publication Quality

```python
import matplotlib.pyplot as plt
import matplotlib as mpl

# Set publication style
mpl.rcParams['font.family'] = 'serif'
mpl.rcParams['font.size'] = 11
mpl.rcParams['axes.linewidth'] = 1.5
mpl.rcParams['xtick.major.width'] = 1.5
mpl.rcParams['ytick.major.width'] = 1.5
mpl.rcParams['legend.frameon'] = True
mpl.rcParams['legend.framealpha'] = 0.9

# Create plot
fig, ax = plt.subplots(figsize=(6, 4))

temp = results.get_device('TEMP_CEILING')
ax.plot(temp['Time'], temp['Value'], 'k-', linewidth=1.5)

ax.set_xlabel('Time (s)')
ax.set_ylabel('Temperature (°C)')
ax.set_title('Ceiling Temperature')
ax.grid(True, linestyle=':', alpha=0.5)

plt.tight_layout()
plt.savefig('publication.pdf', dpi=600, bbox_inches='tight')
plt.savefig('publication.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Custom Color Schemes

```python
# Define custom colors
colors = {
    'fire': '#FF6B35',
    'smoke': '#4A4A4A',
    'cool': '#004E89',
    'hot': '#F77F00'
}

# Use in plots
fig, ax = plt.subplots(figsize=(10, 6))

for dev, color, label in [('TEMP_LOW', colors['cool'], 'Low'),
                           ('TEMP_MID', colors['smoke'], 'Mid'),
                           ('TEMP_HIGH', colors['hot'], 'High')]:
    data = results.get_device(dev)
    ax.plot(data['Time'], data['Value'], color=color, label=label, linewidth=2)

ax.set_xlabel('Time (s)')
ax.set_ylabel('Temperature (°C)')
ax.legend()
ax.grid(True, alpha=0.3)
plt.show()
```

## Animations

### Time-Series Animation

```python
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Get all device data
devices = ['TEMP_1', 'TEMP_2', 'TEMP_3']
data = {dev: results.get_device(dev) for dev in devices}

# Set up figure
fig, ax = plt.subplots(figsize=(10, 6))
lines = {dev: ax.plot([], [], label=dev)[0] for dev in devices}

ax.set_xlim(0, 600)
ax.set_ylim(0, 200)
ax.set_xlabel('Time (s)')
ax.set_ylabel('Temperature (°C)')
ax.legend()
ax.grid(True)

def init():
    for line in lines.values():
        line.set_data([], [])
    return lines.values()

def animate(frame):
    for dev, line in lines.items():
        subset = data[dev].filter(pl.col('Time') <= frame)
        line.set_data(subset['Time'], subset['Value'])
    return lines.values()

# Create animation
anim = animation.FuncAnimation(
    fig, animate, init_func=init,
    frames=range(0, 601, 10),  # Every 10 seconds
    interval=50,  # 50ms between frames
    blit=True
)

# Save animation
anim.save('temperature_animation.mp4', writer='ffmpeg', fps=20, dpi=150)
# or anim.save('temperature_animation.gif', writer='pillow', fps=10)

plt.show()
```

## Comparison Plots

### Before/After Comparison

```python
# Compare two simulations
baseline = FDSResults('baseline')
modified = FDSResults('modified')

baseline_temp = baseline.get_device('TEMP_CEILING')
modified_temp = modified.get_device('TEMP_CEILING')

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# Baseline
ax1.plot(baseline_temp['Time'], baseline_temp['Value'], 'b-', linewidth=2)
ax1.set_xlabel('Time (s)')
ax1.set_ylabel('Temperature (°C)')
ax1.set_title('Baseline')
ax1.grid(True, alpha=0.3)

# Modified
ax2.plot(modified_temp['Time'], modified_temp['Value'], 'r-', linewidth=2)
ax2.set_xlabel('Time (s)')
ax2.set_ylabel('Temperature (°C)')
ax2.set_title('Modified')
ax2.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('comparison.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Difference Plot

```python
# Calculate difference
import polars as pl

# Merge on time
merged = baseline_temp.join(modified_temp, on='Time', suffix='_mod')

# Calculate difference
diff = merged.with_columns([
    (pl.col('Value_mod') - pl.col('Value')).alias('Difference')
])

# Plot difference
plt.figure(figsize=(10, 6))
plt.plot(diff['Time'], diff['Difference'], 'purple', linewidth=2)
plt.axhline(y=0, color='k', linestyle='--', alpha=0.5)
plt.xlabel('Time (s)')
plt.ylabel('Temperature Difference (°C)')
plt.title('Modified - Baseline')
plt.grid(True, alpha=0.3)
plt.savefig('difference.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Best Practices

### 1. Consistent Styling

```python
# Define style once, use everywhere
PLOT_STYLE = {
    'figure.figsize': (10, 6),
    'font.size': 11,
    'axes.labelsize': 12,
    'axes.titlesize': 14,
    'legend.fontsize': 10,
    'lines.linewidth': 2,
    'grid.alpha': 0.3
}

plt.rcParams.update(PLOT_STYLE)
```

### 2. Save Multiple Formats

```python
# Save both raster and vector
plt.savefig('plot.png', dpi=300, bbox_inches='tight')  # For presentations
plt.savefig('plot.pdf', bbox_inches='tight')  # For publications
plt.savefig('plot.svg', bbox_inches='tight')  # For editing
```

### 3. Add Metadata

```python
from datetime import datetime

# Add simulation info to plot
fig, ax = plt.subplots()
# ... create plot ...

fig.text(0.99, 0.01, f'Generated: {datetime.now().strftime("%Y-%m-%d")}',
         ha='right', va='bottom', fontsize=8, alpha=0.5)
```

### 4. Error Bars

```python
# Plot mean with std deviation
import numpy as np

# Multiple runs
runs = [FDSResults(f'run_{i}') for i in range(5)]

# Calculate statistics
times = runs[0].get_device('TEMP')['Time']
values = np.array([run.get_device('TEMP')['Value'] for run in runs])

mean = values.mean(axis=0)
std = values.std(axis=0)

# Plot with error bands
plt.fill_between(times, mean-std, mean+std, alpha=0.3)
plt.plot(times, mean, linewidth=2)
plt.xlabel('Time (s)')
plt.ylabel('Temperature (°C)')
plt.title('Mean ± Std Dev (n=5)')
plt.show()
```

## Next Steps

- [Analysis](analysis.md) - Processing results
- [Examples](../examples/workflows.md) - Complete workflows
- [Parametric Studies](../examples/parametric.md) - Sensitivity analysis
- [Job Management](jobs.md) - Running simulations

---

[Back to Execution →](index.md){ .md-button .md-button--primary }
