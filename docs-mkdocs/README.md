# PyFDS Documentation

This directory contains the source files for the PyFDS documentation site built with [MkDocs Material](https://squidfunk.github.io/mkdocs-material/).

## Building the Documentation

### Prerequisites

Install the documentation dependencies:

```bash
# Using uv (recommended)
uv sync --extra docs

# Or using pip
pip install -e ".[docs]"
```

### Local Development Server

Start a local development server with auto-reload:

```bash
uv run mkdocs serve
```

Then open your browser to [http://127.0.0.1:8000](http://127.0.0.1:8000)

The documentation will automatically rebuild when you save changes to any `.md` file.

### Building Static Site

Build the static HTML site:

```bash
uv run mkdocs build
```

The generated site will be in the `site/` directory.

### Deploying to GitHub Pages

Deploy to GitHub Pages:

```bash
uv run mkdocs gh-deploy
```

This will build the site and push it to the `gh-pages` branch.

## Documentation Structure

```
docs-mkdocs/
├── index.md                    # Home page
├── getting-started/            # Getting Started guides
│   ├── index.md
│   ├── installation.md
│   ├── quickstart.md
│   └── concepts.md
├── guide/                      # User Guide
│   ├── index.md
│   ├── building-simulations.md
│   ├── domain.md
│   └── ...
├── execution/                  # Execution & Analysis
│   ├── index.md
│   ├── running.md
│   └── ...
├── examples/                   # Examples
│   ├── index.md
│   ├── basic.md
│   └── ...
├── api/                       # API Reference
│   ├── index.md
│   ├── core/
│   ├── namelists/
│   └── ...
├── development/               # Developer Guide
│   ├── index.md
│   ├── contributing.md
│   └── ...
├── reference/                 # Reference Material
│   ├── index.md
│   ├── fds-background.md
│   └── ...
├── about/                     # About
│   ├── index.md
│   ├── changelog.md
│   └── ...
├── stylesheets/               # Custom CSS
├── javascripts/               # Custom JavaScript
└── includes/                  # Reusable content
```

## Writing Documentation

### Markdown Files

All documentation is written in Markdown with support for:

- **GitHub Flavored Markdown** (GFM)
- **Material for MkDocs extensions**
- **Python Markdown extensions**
- **Mermaid diagrams**
- **Admonitions**
- **Code tabs**
- **Math equations** (MathJax)

### Code Examples

Use triple backticks with language specifier:

\`\`\`python
from pyfds import Simulation

sim = Simulation(chid='test')
sim.time(t_end=600.0)
\`\`\`

### Admonitions

Create callout boxes:

```markdown
!!! note "Optional Title"
    This is a note admonition.

!!! tip
    This is a tip without a custom title.

!!! warning "Important"
    This is a warning.

!!! example
    This is an example.
```

Types: `note`, `tip`, `warning`, `danger`, `example`, `info`, `success`

### Code Tabs

Group related code snippets:

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

### Mermaid Diagrams

Create flowcharts and diagrams:

\`\`\`mermaid
graph LR
    A[Start] --> B[Process]
    B --> C[End]
\`\`\`

### Grid Cards

Create card layouts:

```markdown
<div class="grid cards" markdown>

-   :material-icon: **Title**

    ---

    Description text

    [:octicons-arrow-right-24: Link](page.md)

-   :material-icon: **Title 2**

    ---

    Description text

    [:octicons-arrow-right-24: Link](page2.md)

</div>
```

### API Documentation

API documentation is auto-generated using mkdocstrings:

```markdown
::: pyfds.core.simulation.Simulation
    options:
      show_root_heading: true
      show_source: true
      heading_level: 3
```

### Cross-References

Link to other pages:

```markdown
See the [Installation Guide](../getting-started/installation.md)
```

Link to specific sections:

```markdown
See [Method Chaining](concepts.md#method-chaining)
```

### Icons

Use Material Design icons:

```markdown
:material-fire: Fire icon
:material-check: Check icon
:fontawesome-brands-github: GitHub icon
```

Browse all icons: [https://squidfunk.github.io/mkdocs-material/reference/icons-emojis/](https://squidfunk.github.io/mkdocs-material/reference/icons-emojis/)

## Configuration

The site is configured in `/mkdocs.yml` in the project root. Key sections:

- **theme**: Material theme configuration
- **plugins**: MkDocs plugins (search, mkdocstrings, etc.)
- **markdown_extensions**: Markdown syntax extensions
- **nav**: Navigation structure

## Adding New Pages

1. Create a new `.md` file in the appropriate directory
2. Add the page to the `nav` section in `mkdocs.yml`
3. Build and verify locally with `mkdocs serve`

Example:

```yaml
nav:
  - Guide:
    - guide/index.md
    - New Page: guide/new-page.md  # Add this line
```

## Style Guide

### Headings

- Use ATX-style headings (`#`, `##`, `###`)
- Don't skip heading levels
- One H1 (`#`) per page (the title)

### Code

- Use inline code for `variable_names`, `function_names()`, and file paths
- Use code blocks for multi-line code
- Always specify language for syntax highlighting
- Keep code examples complete and runnable

### Lists

- Use `-` for unordered lists
- Use `1.` for ordered lists (auto-numbered)
- Indent nested lists with 4 spaces

### Links

- Use descriptive link text (not "click here")
- Prefer relative links for internal pages
- Use absolute URLs for external sites

## Troubleshooting

### MkDocs not found

Install docs dependencies:
```bash
uv sync --extra docs
```

### Changes not showing

Hard refresh your browser (Ctrl+Shift+R or Cmd+Shift+R)

### Build errors

Check for:
- Broken internal links
- Missing files referenced in `mkdocs.yml`
- Syntax errors in Markdown
- Invalid YAML in frontmatter

## Resources

- [MkDocs Documentation](https://www.mkdocs.org/)
- [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/)
- [mkdocstrings](https://mkdocstrings.github.io/)
- [Python Markdown Extensions](https://facelessuser.github.io/pymdown-extensions/)

## Contributing

See the [Contributing Guide](development/contributing.md) for guidelines on contributing to the documentation.

## Questions?

- [GitHub Discussions](https://github.com/GraysonBellamy/pyfds/discussions)
- [GitHub Issues](https://github.com/GraysonBellamy/pyfds/issues)
