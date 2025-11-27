# Results Analysis

Analyze and visualize FDS simulation results with PyFDS.

## Overview

PyFDS provides tools to:

- Load and parse FDS output files
- Extract device data
- Analyze time series
- Create plots and visualizations
- Export results to various formats

```python
from pyfds.analysis import FDSResults

# Load results
results = FDSResults('simulation.fds')

# Get device data
temp_data = results.get_device('TEMP_1')
```

## Loading Results

### From Simulation Object

```python
from pyfds import Simulation

sim = Simulation(chid='room_fire')
# ... configure simulation ...

# Run and get results
results = sim.run(wait=True)

# Access device data
temps = results.devices['TEMP_1']
```

### From Existing Files

```python
from pyfds.analysis import FDSResults

# Load from CHID
results = FDSResults(chid='room_fire')

# Or from .fds file
results = FDSResults('room_fire.fds')
```

## Device Data

### Accessing Device Output

```python
# Get specific device
temp_data = results.get_device('TEMP_CEILING')

# Get all devices
all_devices = results.devices

# List available devices
device_ids = results.list_devices()
print(device_ids)  # ['TEMP_1', 'TEMP_2', 'HF_WALL', ...]
```

### Device Data Structure

Device data is returned as a Polars DataFrame:

```python
import polars as pl

temp_data = results.get_device('TEMP_1')

# DataFrame columns: 'Time', 'Value'
print(temp_data)
# ┌──────┬────────┐
# │ Time │ Value  │
# ├──────┼────────┤
# │ 0.0  │ 20.0   │
# │ 1.0  │ 20.5   │
# │ 2.0  │ 21.2   │
# └──────┴────────┘

# Access time and values
times = temp_data['Time'].to_numpy()
values = temp_data['Value'].to_numpy()
```

## Time Series Analysis

### Basic Statistics

```python
temp_data = results.get_device('TEMP_CEILING')

# Maximum value and time
max_temp = temp_data['Value'].max()
time_at_max = temp_data.filter(
    pl.col('Value') == pl.col('Value').max()
)['Time'][0]

print(f"Peak temperature: {max_temp:.1f}°C at t={time_at_max:.1f}s")

# Mean and standard deviation
mean_temp = temp_data['Value'].mean()
std_temp = temp_data['Value'].std()

# Rate of change
temp_data = temp_data.with_columns([
    (pl.col('Value').diff() / pl.col('Time').diff()).alias('Rate')
])
max_rate = temp_data['Rate'].max()
```

### Time Filtering

```python
# Get data after t=300s
late_data = temp_data.filter(pl.col('Time') >= 300.0)

# Get data in time range
range_data = temp_data.filter(
    (pl.col('Time') >= 100.0) & (pl.col('Time') <= 300.0)
)

# Find when temperature exceeds threshold
threshold = 60.0  # °C
exceedance = temp_data.filter(pl.col('Value') > threshold)
if len(exceedance) > 0:
    first_time = exceedance['Time'][0]
    print(f"Temperature exceeded {threshold}°C at t={first_time:.1f}s")
```

### Comparing Multiple Devices

```python
import polars as pl

# Get multiple temperature devices
devices = ['TEMP_1', 'TEMP_2', 'TEMP_3']
temps = {dev: results.get_device(dev) for dev in devices}

# Combine into single DataFrame
combined = temps['TEMP_1'].select(['Time']).join(
    temps['TEMP_1'].select(pl.col('Value').alias('TEMP_1')),
    left_on='Time', right_on='Time'
)

for dev in devices[1:]:
    data = temps[dev].select([
        pl.col('Time'),
        pl.col('Value').alias(dev)
    ])
    combined = combined.join(data, on='Time', how='outer')

print(combined)
# ┌──────┬────────┬────────┬────────┐
# │ Time │ TEMP_1 │ TEMP_2 │ TEMP_3 │
# ├──────┼────────┼────────┼────────┤
# │ 0.0  │ 20.0   │ 20.0   │ 20.0   │
# │ 1.0  │ 25.3   │ 22.1   │ 21.5   │
# └──────┴────────┴────────┴────────┘
```

## Visualization

### Basic Plotting

```python
import matplotlib.pyplot as plt

# Single device plot
temp_data = results.get_device('TEMP_CEILING')

plt.figure(figsize=(10, 6))
plt.plot(temp_data['Time'], temp_data['Value'], 'b-', linewidth=2)
plt.xlabel('Time (s)')
plt.ylabel('Temperature (°C)')
plt.title('Ceiling Temperature vs Time')
plt.grid(True, alpha=0.3)
plt.savefig('temperature.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Multiple Devices

```python
# Plot multiple devices
devices = ['TEMP_1', 'TEMP_2', 'TEMP_3']
colors = ['red', 'blue', 'green']
labels = ['Location 1', 'Location 2', 'Location 3']

plt.figure(figsize=(12, 6))
for dev, color, label in zip(devices, colors, labels):
    data = results.get_device(dev)
    plt.plot(data['Time'], data['Value'], color=color, label=label, linewidth=2)

plt.xlabel('Time (s)', fontsize=12)
plt.ylabel('Temperature (°C)', fontsize=12)
plt.title('Temperature Comparison', fontsize=14)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.savefig('temp_comparison.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Heat Flux Plot

```python
# Heat flux data
hf_data = results.get_device('HF_WALL')

plt.figure(figsize=(10, 6))
plt.plot(hf_data['Time'], hf_data['Value'], 'r-', linewidth=2)
plt.axhline(y=20.0, color='k', linestyle='--', label='Pain threshold (20 kW/m²)')
plt.xlabel('Time (s)')
plt.ylabel('Heat Flux (kW/m²)')
plt.title('Wall Heat Flux')
plt.legend()
plt.grid(True, alpha=0.3)
plt.savefig('heat_flux.png', dpi=300, bbox_inches='tight')
plt.show()
```

### Subplots for Multi-Parameter Analysis

```python
fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# Temperature
temp = results.get_device('TEMP_1')
axes[0, 0].plot(temp['Time'], temp['Value'], 'r-')
axes[0, 0].set_ylabel('Temperature (°C)')
axes[0, 0].set_title('Temperature')
axes[0, 0].grid(True, alpha=0.3)

# Velocity
vel = results.get_device('VEL_1')
axes[0, 1].plot(vel['Time'], vel['Value'], 'b-')
axes[0, 1].set_ylabel('Velocity (m/s)')
axes[0, 1].set_title('Velocity')
axes[0, 1].grid(True, alpha=0.3)

# Heat Flux
hf = results.get_device('HF_1')
axes[1, 0].plot(hf['Time'], hf['Value'], 'orange')
axes[1, 0].set_ylabel('Heat Flux (kW/m²)')
axes[1, 0].set_xlabel('Time (s)')
axes[1, 0].set_title('Heat Flux')
axes[1, 0].grid(True, alpha=0.3)

# Pressure
pres = results.get_device('PRES_1')
axes[1, 1].plot(pres['Time'], pres['Value'], 'g-')
axes[1, 1].set_ylabel('Pressure (Pa)')
axes[1, 1].set_xlabel('Time (s)')
axes[1, 1].set_title('Pressure')
axes[1, 1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('multi_parameter.png', dpi=300, bbox_inches='tight')
plt.show()
```

## Exporting Results

### Export to CSV

```python
# Single device to CSV
temp_data = results.get_device('TEMP_1')
temp_data.write_csv('temp_1.csv')

# Multiple devices to single CSV
combined = results.combine_devices(['TEMP_1', 'TEMP_2', 'HF_1'])
combined.write_csv('all_results.csv')
```

### Export to Excel

```python
# Export to Excel (requires openpyxl)
temp_data.write_excel('results.xlsx', worksheet='Temperature')

# Multiple sheets
with pd.ExcelWriter('results.xlsx') as writer:
    results.get_device('TEMP_1').to_pandas().to_excel(writer, sheet_name='Temperature')
    results.get_device('HF_1').to_pandas().to_excel(writer, sheet_name='Heat Flux')
    results.get_device('VEL_1').to_pandas().to_excel(writer, sheet_name='Velocity')
```

### Export to JSON

```python
# Export device data to JSON
temp_data.write_json('temp_data.json')

# Custom JSON structure
import json

output = {
    'simulation': 'room_fire',
    'devices': {
        dev_id: results.get_device(dev_id).to_dicts()
        for dev_id in results.list_devices()
    }
}

with open('all_results.json', 'w') as f:
    json.dump(output, f, indent=2)
```

## Complete Analysis Example

```python
from pyfds import Simulation
from pyfds.analysis import FDSResults
import matplotlib.pyplot as plt
import polars as pl

# Run simulation
sim = Simulation(chid='analysis_demo')
sim.time(t_end=600.0)
sim.mesh(ijk=(50, 40, 25), xb=(0, 5, 0, 4, 0, 2.5))

# Fire
sim.surface(id='FIRE', hrrpua=1000.0)
sim.obstruction(xb=(2, 3, 1.5, 2.5, 0, 0.1), surf_id='FIRE')

# Devices
sim.device(id='TEMP_CEIL', quantity='TEMPERATURE', xyz=(2.5, 2, 2.4))
sim.device(id='TEMP_MID', quantity='TEMPERATURE', xyz=(2.5, 2, 1.5))
sim.device(id='TEMP_LOW', quantity='TEMPERATURE', xyz=(2.5, 2, 0.5))
sim.device(id='HF_WALL', quantity='GAUGE HEAT FLUX', xyz=(0.1, 2, 1.5), ior=1)

# Run simulation
results = sim.run(wait=True)

# Analysis
print("=== Simulation Results Analysis ===\n")

# Temperature analysis
for dev in ['TEMP_CEIL', 'TEMP_MID', 'TEMP_LOW']:
    data = results.get_device(dev)
    max_temp = data['Value'].max()
    time_max = data.filter(pl.col('Value') == max_temp)['Time'][0]

    print(f"{dev}:")
    print(f"  Peak: {max_temp:.1f}°C at t={time_max:.1f}s")

    # Time to 60°C
    above_60 = data.filter(pl.col('Value') >= 60.0)
    if len(above_60) > 0:
        t_60 = above_60['Time'][0]
        print(f"  Reached 60°C at t={t_60:.1f}s")
    print()

# Heat flux analysis
hf = results.get_device('HF_WALL')
max_hf = hf['Value'].max()
print(f"HF_WALL:")
print(f"  Peak heat flux: {max_hf:.1f} kW/m²")

# Check for critical threshold
critical_hf = 12.5  # kW/m² (pain threshold for skin)
above_critical = hf.filter(pl.col('Value') >= critical_hf)
if len(above_critical) > 0:
    t_critical = above_critical['Time'][0]
    print(f"  Exceeded {critical_hf} kW/m² at t={t_critical:.1f}s")
print()

# Create summary plot
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 10))

# Temperature plot
for dev, label in [('TEMP_CEIL', 'Ceiling'), ('TEMP_MID', 'Mid-height'), ('TEMP_LOW', 'Low')]:
    data = results.get_device(dev)
    ax1.plot(data['Time'], data['Value'], label=label, linewidth=2)

ax1.axhline(y=60, color='r', linestyle='--', alpha=0.5, label='Tenability limit')
ax1.set_ylabel('Temperature (°C)', fontsize=12)
ax1.set_title('Temperature at Different Heights', fontsize=14)
ax1.legend(fontsize=10)
ax1.grid(True, alpha=0.3)

# Heat flux plot
ax2.plot(hf['Time'], hf['Value'], 'orange', linewidth=2)
ax2.axhline(y=12.5, color='r', linestyle='--', alpha=0.5, label='Pain threshold')
ax2.set_xlabel('Time (s)', fontsize=12)
ax2.set_ylabel('Heat Flux (kW/m²)', fontsize=12)
ax2.set_title('Wall Heat Flux', fontsize=14)
ax2.legend(fontsize=10)
ax2.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('analysis_summary.png', dpi=300, bbox_inches='tight')
print("Summary plot saved to 'analysis_summary.png'")

# Export data
results.export_all_devices('all_devices.csv')
print("All device data exported to 'all_devices.csv'")
```

## Advanced Analysis Techniques

### Peak Finding

```python
import scipy.signal as signal

temp = results.get_device('TEMP_1')
times = temp['Time'].to_numpy()
values = temp['Value'].to_numpy()

# Find peaks
peaks, properties = signal.find_peaks(values, height=50, distance=10)

print(f"Found {len(peaks)} peaks")
for i, peak_idx in enumerate(peaks):
    print(f"  Peak {i+1}: {values[peak_idx]:.1f}°C at t={times[peak_idx]:.1f}s")
```

### Moving Average

```python
# Smooth noisy data with rolling mean
window_size = 10

smoothed = temp_data.with_columns([
    pl.col('Value').rolling_mean(window_size=window_size).alias('Smoothed')
])

plt.plot(smoothed['Time'], smoothed['Value'], 'gray', alpha=0.5, label='Raw')
plt.plot(smoothed['Time'], smoothed['Smoothed'], 'blue', linewidth=2, label='Smoothed')
plt.legend()
plt.show()
```

### Statistical Summary

```python
def summarize_device(results, device_id):
    """Generate statistical summary for a device."""
    data = results.get_device(device_id)

    summary = {
        'device': device_id,
        'count': len(data),
        'mean': data['Value'].mean(),
        'std': data['Value'].std(),
        'min': data['Value'].min(),
        'max': data['Value'].max(),
        'median': data['Value'].median(),
        'q25': data['Value'].quantile(0.25),
        'q75': data['Value'].quantile(0.75),
    }

    return summary

# Summarize all devices
for dev_id in results.list_devices():
    summary = summarize_device(results, dev_id)
    print(f"\n{summary['device']}:")
    print(f"  Mean: {summary['mean']:.2f}")
    print(f"  Std:  {summary['std']:.2f}")
    print(f"  Min:  {summary['min']:.2f}")
    print(f"  Max:  {summary['max']:.2f}")
```

## Best Practices

### 1. Check Data Availability

```python
# Check if device exists before accessing
if 'TEMP_1' in results.list_devices():
    data = results.get_device('TEMP_1')
else:
    print("Device TEMP_1 not found in results")
```

### 2. Handle Missing Data

```python
# Check for null values
data = results.get_device('TEMP_1')
if data['Value'].is_null().any():
    # Fill or drop nulls
    data = data.fill_null(strategy='forward')
```

### 3. Save Publication-Quality Figures

```python
plt.figure(figsize=(10, 6), dpi=300)
# ... plotting code ...
plt.savefig('figure.png', dpi=300, bbox_inches='tight', facecolor='white')
plt.savefig('figure.pdf', bbox_inches='tight')  # Vector format
```

## Next Steps

- [Running Simulations](running.md) - Execute FDS simulations
- [Visualization](visualization.md) - Advanced plotting techniques
- [Examples](../examples/index.md) - Complete analysis examples
- [API Reference](../api/analysis/results.md) - FDSResults API

---

[Visualization →](visualization.md){ .md-button .md-button--primary }
