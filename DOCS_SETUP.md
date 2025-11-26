# Documentation Setup Guide

This guide explains how to set up and deploy the PyFDS documentation.

## Quick Start

### Local Development

```bash
# 1. Install documentation dependencies
uv sync --extra docs

# 2. Start local server (auto-reload on changes)
uv run mkdocs serve

# 3. Open browser to http://127.0.0.1:8000
```

The site will automatically rebuild when you save changes to any markdown file.

### Building Static Site

```bash
# Build the static HTML site
uv run mkdocs build

# Output will be in the ./site directory
```

### Manual Deployment to GitHub Pages

```bash
# Deploy to GitHub Pages (gh-pages branch)
uv run mkdocs gh-deploy
```

## Automatic Deployment with GitHub Actions

### Setup GitHub Pages

1. **Go to Repository Settings**
   - Navigate to: `https://github.com/GraysonBellamy/pyfds/settings/pages`

2. **Configure GitHub Pages**
   - **Source**: Select "GitHub Actions"
   - Save the settings

3. **Push to Main Branch**
   ```bash
   git add .
   git commit -m "Add documentation and GitHub Actions workflow"
   git push origin main
   ```

4. **Automatic Deployment**
   - GitHub Actions will automatically build and deploy on every push to `main`
   - Changes to `docs-mkdocs/`, `mkdocs.yml`, or `src/pyfds/` trigger builds
   - View progress: `https://github.com/GraysonBellamy/pyfds/actions`

5. **Access Documentation**
   - After deployment: `https://graysonbellamy.github.io/pyfds`

### Workflow Details

The `.github/workflows/docs.yml` workflow:

**Triggers:**
- Push to `main` branch (with relevant file changes)
- Pull requests (validates but doesn't deploy)
- Manual trigger via GitHub UI

**On Main Branch Push:**
- ✅ Installs Python 3.11 and uv
- ✅ Installs documentation dependencies
- ✅ Builds documentation with `mkdocs build --strict`
- ✅ Uploads artifact
- ✅ Deploys to GitHub Pages

**On Pull Requests:**
- ✅ Validates documentation builds
- ❌ Does not deploy

### Monitoring Deployments

View deployment status:
- **Actions tab**: `https://github.com/GraysonBellamy/pyfds/actions`
- **Environments**: `https://github.com/GraysonBellamy/pyfds/deployments`

## Project Structure

```
pyfds/
├── .github/
│   └── workflows/
│       └── docs.yml          # GitHub Actions workflow
├── docs-mkdocs/              # Documentation source
│   ├── index.md             # Home page
│   ├── getting-started/     # Getting Started section
│   ├── guide/               # User Guide
│   ├── execution/           # Execution & Analysis
│   ├── examples/            # Examples
│   ├── api/                 # API Reference
│   ├── development/         # Developer Guide
│   ├── reference/           # Reference Materials
│   ├── about/               # About section
│   ├── stylesheets/         # Custom CSS
│   ├── javascripts/         # Custom JavaScript
│   └── includes/            # Reusable content
├── mkdocs.yml               # MkDocs configuration
└── pyproject.toml           # Documentation dependencies
```

## Writing Documentation

### Creating a New Page

1. **Create the Markdown file**
   ```bash
   touch docs-mkdocs/guide/new-feature.md
   ```

2. **Add content**
   ```markdown
   # New Feature

   Description of the new feature.

   ## Usage

   \`\`\`python
   from pyfds import Simulation
   # Example code
   \`\`\`
   ```

3. **Add to navigation** (edit `mkdocs.yml`)
   ```yaml
   nav:
     - User Guide:
       - guide/index.md
       - New Feature: guide/new-feature.md  # Add this line
   ```

4. **Test locally**
   ```bash
   uv run mkdocs serve
   ```

### Markdown Features

#### Code Blocks with Tabs

```markdown
=== "Python"
    \`\`\`python
    sim = Simulation(chid='test')
    \`\`\`

=== "FDS Output"
    \`\`\`fortran
    &HEAD CHID='test' /
    \`\`\`
```

#### Admonitions

```markdown
!!! tip "Pro Tip"
    This is a helpful tip.

!!! warning "Important"
    Pay attention to this.

!!! example
    Here's an example.
```

#### Grid Cards

```markdown
<div class="grid cards" markdown>

-   :material-fire: **Feature One**

    ---

    Description

    [:octicons-arrow-right-24: Learn More](link.md)

-   :material-check: **Feature Two**

    ---

    Description

    [:octicons-arrow-right-24: Learn More](link.md)

</div>
```

#### Mermaid Diagrams

```markdown
\`\`\`mermaid
graph LR
    A[Start] --> B[Process]
    B --> C[End]
\`\`\`
```

#### API Documentation

```markdown
::: pyfds.core.simulation.Simulation
    options:
      show_root_heading: true
      show_source: true
      heading_level: 3
```

## Troubleshooting

### Build Fails on GitHub Actions

**Check the Actions log:**
1. Go to: `https://github.com/GraysonBellamy/pyfds/actions`
2. Click on the failed workflow
3. Review the build logs

**Common issues:**

1. **Missing dependencies**
   - Ensure `pyproject.toml` has all docs dependencies
   - Check the `uv sync --extra docs` step

2. **Strict mode errors**
   - Fix broken internal links
   - Remove references to non-existent pages
   - Check for malformed markdown

3. **Permission errors**
   - Verify GitHub Pages is enabled
   - Check repository permissions

### Local Build Issues

**mkdocs not found:**
```bash
# Reinstall dependencies
uv sync --extra docs
```

**Changes not showing:**
- Hard refresh browser (Ctrl+Shift+R)
- Check terminal for build errors
- Restart `mkdocs serve`

**Import errors in API docs:**
```bash
# Ensure pyfds is installed
uv sync
```

## Customization

### Changing Theme Colors

Edit `mkdocs.yml`:

```yaml
theme:
  palette:
    - scheme: default
      primary: indigo      # Change this
      accent: blue         # Change this
```

### Adding Custom CSS

Edit `docs-mkdocs/stylesheets/extra.css`:

```css
/* Your custom styles */
.md-typeset h1 {
    color: #custom-color;
}
```

### Adding Plugins

Edit `mkdocs.yml`:

```yaml
plugins:
  - search
  - your-new-plugin
```

Then install:
```bash
# Add to pyproject.toml docs dependencies
uv add mkdocs-your-plugin --optional docs
```

## Best Practices

### 1. Test Before Pushing

Always build and test locally:
```bash
uv run mkdocs build --strict
```

### 2. Write Incrementally

- Create stub pages for navigation structure
- Fill in content gradually
- Keep stubs clearly marked

### 3. Use Consistent Style

- Follow existing page structure
- Use same heading levels
- Keep code examples runnable
- Add cross-references

### 4. Optimize Images

- Use appropriate formats (PNG for screenshots, SVG for diagrams)
- Compress images before adding
- Store in `docs-mkdocs/images/` (create if needed)

### 5. Document as You Code

- Update docs when changing APIs
- Add examples for new features
- Keep changelog current

## Deployment Checklist

Before deploying to production:

- [ ] All pages build without errors
- [ ] No broken internal links
- [ ] Code examples are tested
- [ ] Navigation is logical
- [ ] Search works correctly
- [ ] Mobile layout looks good
- [ ] Dark mode works
- [ ] API docs generate correctly

## Getting Help

- **MkDocs Documentation**: https://www.mkdocs.org/
- **Material for MkDocs**: https://squidfunk.github.io/mkdocs-material/
- **mkdocstrings**: https://mkdocstrings.github.io/
- **PyFDS Issues**: https://github.com/GraysonBellamy/pyfds/issues

## Quick Reference

### Common Commands

```bash
# Serve locally
uv run mkdocs serve

# Build
uv run mkdocs build

# Build with strict mode (fail on warnings)
uv run mkdocs build --strict

# Deploy to GitHub Pages manually
uv run mkdocs gh-deploy

# Clean build artifacts
rm -rf site/
```

### File Locations

- **Configuration**: `mkdocs.yml`
- **Content**: `docs-mkdocs/`
- **Build output**: `site/` (gitignored)
- **GitHub workflow**: `.github/workflows/docs.yml`
- **Custom CSS**: `docs-mkdocs/stylesheets/extra.css`
- **Custom JS**: `docs-mkdocs/javascripts/`

### URLs

- **Local development**: http://127.0.0.1:8000
- **Production**: https://graysonbellamy.github.io/pyfds
- **GitHub Actions**: https://github.com/GraysonBellamy/pyfds/actions
- **Repository**: https://github.com/GraysonBellamy/pyfds

---

For detailed documentation on writing content, see [docs-mkdocs/README.md](docs-mkdocs/README.md).
