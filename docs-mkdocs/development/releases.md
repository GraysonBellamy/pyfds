# Releases

PyFDS release process and version management.

## Overview

PyFDS follows semantic versioning and maintains a clear release process to ensure stability and predictability for users.

## Versioning

### Semantic Versioning

PyFDS uses [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH
```

- **MAJOR**: Breaking changes to public API
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes, backward compatible

**Examples**:

- `0.1.0` → `0.2.0`: Added new namelists (MINOR)
- `0.2.0` → `0.2.1`: Fixed validation bug (PATCH)
- `0.2.1` → `1.0.0`: Stable API, breaking changes (MAJOR)

### Version File

Version is defined in `src/pyfds/__init__.py`:

```python
__version__ = "0.1.0"
```

## Release Process

### 1. Prepare Release

**Update Version**

```bash
# Edit version
vim src/pyfds/__init__.py

# Update __version__ = "0.2.0"
```

**Update Changelog**

```bash
# Edit CHANGELOG.md
vim CHANGELOG.md
```

Add release notes:

```markdown
## [0.2.0] - 2024-11-26

### Added
- VENT namelist support
- MISC namelist for global settings
- Validation for mesh quality

### Changed
- Improved error messages
- Updated documentation

### Fixed
- Mesh bounds validation
- Reference checking for SURF_ID

### Deprecated
- Old mesh() API (use new signature)
```

### 2. Run Tests

Ensure all tests pass:

```bash
# Run full test suite
uv run pytest

# Run with coverage
uv run pytest --cov=pyfds --cov-report=html

# Check coverage threshold
uv run pytest --cov=pyfds --cov-fail-under=80
```

### 3. Build Documentation

Build and verify documentation:

```bash
# Build docs
uv run mkdocs build --strict

# Serve locally to verify
uv run mkdocs serve

# Check for broken links
# (manually review)
```

### 4. Create Git Tag

```bash
# Commit version changes
git add src/pyfds/__init__.py CHANGELOG.md
git commit -m "Bump version to 0.2.0"

# Create tag
git tag -a v0.2.0 -m "Release version 0.2.0"

# Push tag
git push origin v0.2.0
```

### 5. Build Package

```bash
# Clean previous builds
rm -rf dist/

# Build package
uv build

# Check built package
ls -lh dist/
# pyfds-0.2.0-py3-none-any.whl
# pyfds-0.2.0.tar.gz
```

### 6. Publish to PyPI

**Test PyPI (optional)**

```bash
# Upload to test PyPI
uv publish --repository testpypi

# Test installation
pip install --index-url https://test.pypi.org/simple/ pyfds==0.2.0
```

**Production PyPI**

```bash
# Upload to PyPI
uv publish

# Verify on PyPI
# https://pypi.org/project/pyfds/
```

### 7. GitHub Release

Create release on GitHub:

1. Go to: https://github.com/GraysonBellamy/pyfds/releases/new
2. Select tag: `v0.2.0`
3. Title: "PyFDS v0.2.0"
4. Description: Copy from CHANGELOG.md
5. Attach built packages (optional)
6. Click "Publish release"

### 8. Update Documentation

Documentation is automatically deployed via GitHub Actions when pushing to main.

Verify deployment:
- https://graysonbellamy.github.io/pyfds

## Release Checklist

Use this checklist for each release:

```markdown
## Pre-Release

- [ ] All tests passing
- [ ] Coverage meets threshold (80%+)
- [ ] Documentation builds without warnings
- [ ] CHANGELOG.md updated
- [ ] Version bumped in __init__.py
- [ ] All issues in milestone closed

## Release

- [ ] Git tag created (v0.x.x)
- [ ] Tag pushed to GitHub
- [ ] Package built successfully
- [ ] Uploaded to PyPI
- [ ] GitHub release created
- [ ] Documentation deployed

## Post-Release

- [ ] Verify PyPI package installable
- [ ] Verify documentation live
- [ ] Announcement posted (if major release)
- [ ] Milestone closed
- [ ] Next milestone created
```

## Version Management

### Development Version

During development, use `.dev` suffix:

```python
__version__ = "0.2.0.dev"
```

### Release Candidates

For major releases, create release candidates:

```python
__version__ = "1.0.0rc1"
```

Test thoroughly before final release:

```bash
# Tag RC
git tag -a v1.0.0rc1 -m "Release candidate 1"

# Build and test
uv build
uv publish --repository testpypi

# After testing, release final
__version__ = "1.0.0"
git tag -a v1.0.0 -m "Release version 1.0.0"
```

## Changelog Format

Follow [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) format:

```markdown
# Changelog

All notable changes to PyFDS will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Feature in development

## [0.2.0] - 2024-11-26

### Added
- New features

### Changed
- Improvements

### Deprecated
- Features to be removed

### Removed
- Removed features

### Fixed
- Bug fixes

### Security
- Security fixes

## [0.1.0] - 2024-10-15

Initial release
```

## Automated Releases

### GitHub Actions

Automate releases with GitHub Actions:

```yaml
# .github/workflows/release.yml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  release:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Install uv
      uses: astral-sh/setup-uv@v1

    - name: Set up Python
      run: uv python install 3.11

    - name: Install dependencies
      run: uv sync

    - name: Run tests
      run: uv run pytest --cov=pyfds --cov-fail-under=80

    - name: Build package
      run: uv build

    - name: Publish to PyPI
      env:
        UV_PUBLISH_TOKEN: ${{ secrets.PYPI_API_TOKEN }}
      run: uv publish

    - name: Create GitHub Release
      uses: softprops/action-gh-release@v1
      with:
        files: dist/*
        generate_release_notes: true
```

### Setup PyPI Token

1. Create API token on PyPI
2. Add to GitHub secrets as `PYPI_API_TOKEN`
3. Token is used automatically in workflow

## Deprecation Policy

### Deprecating Features

1. **Announce deprecation** in release notes
2. **Add deprecation warning**:

```python
import warnings

def old_function():
    warnings.warn(
        "old_function is deprecated, use new_function instead",
        DeprecationWarning,
        stacklevel=2
    )
    return new_function()
```

3. **Keep for at least 2 minor versions**
4. **Remove in next major version**

**Example Timeline**:

- v0.3.0: Feature deprecated, warning added
- v0.4.0: Still available with warning
- v0.5.0: Still available with warning
- v1.0.0: Feature removed

## Breaking Changes

### When to Release Major Version

Release major version (1.0.0, 2.0.0) when:

- Breaking API changes
- Removing deprecated features
- Major architectural changes
- Incompatible file format changes

### How to Handle Breaking Changes

1. **Document clearly** in CHANGELOG
2. **Provide migration guide**
3. **Update examples**
4. **Consider providing compatibility layer** (temporarily)

**Migration Guide Example**:

```markdown
## Migrating from 0.x to 1.0

### Mesh API Changed

**Before (0.x)**:
```python
sim.add_mesh(50, 50, 25, 0, 5, 0, 5, 0, 2.5)
```

**After (1.0)**:
```python
sim.mesh(ijk=(50, 50, 25), xb=(0, 5, 0, 5, 0, 2.5))
```

### Surface API Simplified

**Before (0.x)**:
```python
sim.add_surface('FIRE', {'HRRPUA': 1000.0})
```

**After (1.0)**:
```python
sim.surface(id='FIRE', hrrpua=1000.0)
```
```

## Hotfix Releases

### Emergency Fixes

For critical bugs in production:

1. **Create hotfix branch** from tag:

```bash
# From latest release
git checkout -b hotfix/0.2.1 v0.2.0

# Fix bug
vim src/pyfds/...

# Test thoroughly
uv run pytest

# Commit
git commit -m "Fix critical bug in mesh validation"

# Update version
vim src/pyfds/__init__.py  # 0.2.0 → 0.2.1

# Update changelog
vim CHANGELOG.md

# Tag and release
git tag -a v0.2.1 -m "Hotfix release 0.2.1"
git push origin v0.2.1

# Merge back to main
git checkout main
git merge hotfix/0.2.1
git push origin main
```

## Release Cadence

### Planned Schedule

- **Patch releases**: As needed for bugs
- **Minor releases**: Monthly
- **Major releases**: When breaking changes accumulate

### Pre-releases

Before major releases:

- **Alpha**: Very early, may have bugs
- **Beta**: Feature complete, needs testing
- **RC**: Release candidate, nearly final

Example progression:

```
1.0.0a1 (alpha 1)
1.0.0a2 (alpha 2)
1.0.0b1 (beta 1)
1.0.0rc1 (release candidate 1)
1.0.0rc2 (release candidate 2)
1.0.0 (final release)
```

## Communication

### Release Announcements

Announce releases:

1. **GitHub Releases** (automatic)
2. **PyPI** (automatic)
3. **Documentation** (automatic deployment)
4. **Discussion forum** (manual)
5. **Twitter/social media** (optional)

### Release Notes Template

```markdown
# PyFDS v0.2.0

We're excited to announce PyFDS v0.2.0!

## ✨ Highlights

- New VENT namelist support for boundaries
- Enhanced validation with better error messages
- Comprehensive documentation updates

## 📊 Stats

- 50+ commits since v0.1.0
- 15 issues closed
- 3 new contributors

## 🔗 Links

- [Documentation](https://graysonbellamy.github.io/pyfds)
- [Changelog](https://github.com/GraysonBellamy/pyfds/blob/main/CHANGELOG.md)
- [PyPI](https://pypi.org/project/pyfds/)

## 💾 Installation

```bash
pip install --upgrade pyfds
# or
uv pip install --upgrade pyfds
```

## 🙏 Thanks

Thanks to all contributors who made this release possible!
```

## Rollback Procedure

If a release has critical issues:

1. **Yank from PyPI**:

```bash
# Mark as yanked (users can still install with pinned version)
# Done via PyPI web interface
```

2. **Create hotfix** immediately
3. **Release new version** ASAP
4. **Communicate** the issue and fix

## Version Support

### Support Policy

- **Latest major**: Full support
- **Previous major**: Security fixes only
- **Older majors**: No support

Example:
- Current: 2.x.x (full support)
- Previous: 1.x.x (security fixes)
- Old: 0.x.x (no support)

## See Also

- [Contributing](contributing.md) - Development guide
- [Testing](testing.md) - Test requirements
- [Changelog](../about/changelog.md) - Version history

---

[Back to Development →](index.md){ .md-button .md-button--primary }
