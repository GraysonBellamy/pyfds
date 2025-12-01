# Multi-Mesh Examples

This directory contains examples demonstrating multi-mesh configurations
for domain decomposition, parallel execution, and MULT for object arrays.

## Examples

### 1. multi_mesh_basic.py
Demonstrates basic multi-mesh domain decomposition.
- **Concepts**: Multiple MESH namelists, mesh boundaries
- **Key Features**:
  - 2x2 mesh array for parallel execution
  - Fire spanning multiple meshes
  - Devices in each mesh region

### 2. mult_obstruction.py
Uses MULT namelist to create obstruction arrays.
- **Concepts**: MULT for efficient array creation
- **Key Features**:
  - 4x3 array of columns using one OBST + MULT
  - Beam array for ceiling structure
  - MULT_ID reference from OBST

### 3. mesh_alignment.py
Demonstrates mesh alignment with different resolutions.
- **Concepts**: Fine/coarse mesh transition
- **Key Features**:
  - Fine mesh (0.05m) around fire source
  - Coarse mesh (0.1m) for extended domain
  - Proper cell alignment at boundaries

## Key PyFDS Features Demonstrated

```python
# Multiple meshes
sim.add(Mesh(id="MESH_1", ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(0, 2, 0, 2, 0, 2)))
sim.add(Mesh(id="MESH_2", ijk=Grid3D.of(20, 20, 20), xb=Bounds3D.of(2, 4, 0, 2, 0, 2)))

# MULT for arrays
sim.add(Multiplier(
    id="COLUMN_ARRAY",
    dx=2.0, dy=2.0,
    i_lower=0, i_upper=3,
    j_lower=0, j_upper=2,
))
sim.add(Obstruction(
    id="COLUMN",
    xb=Bounds3D.of(0.9, 1.1, 0.9, 1.1, 0.0, 3.0),
    mult_id="COLUMN_ARRAY",  # Creates 4x3 = 12 columns
))

# Different resolution meshes
sim.add(Mesh(id="FINE", ijk=Grid3D.of(40, 40, 40), xb=...))  # 0.05m
sim.add(Mesh(id="COARSE", ijk=Grid3D.of(20, 20, 20), xb=...))  # 0.1m
```

## FDS Reference Cases

These examples are inspired by:
- **multi_mesh_***: Multi-mesh verification cases
- **mult_test_***: MULT namelist tests
- **mesh_alignment_***: Mesh boundary alignment tests

## Running the Examples

```bash
# Run individual examples
python examples/multi_mesh/multi_mesh_basic.py
python examples/multi_mesh/mult_obstruction.py
python examples/multi_mesh/mesh_alignment.py
```

## Notes

- Meshes must align at boundaries (no gaps or overlaps)
- Cell sizes should match at mesh interfaces (or be integer ratios)
- MULT creates copies at positions: base + i*dx + j*dy + k*dz
- For parallel execution, assign different MPI_PROCESS to each mesh
