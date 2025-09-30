# Ayan Das - Configuration

## Communication Style
- Technical, terse, imperative voice
- No we/you/our, marketing language, or unicode
- Do what's asked; nothing more, nothing less

## System Environment
- Hardware: Ryzen 5600G, 64GB RAM, RTX 3060, 4TB NVMe
- OS: GNU Guix System, GNOME/Wayland
- Languages: Python 3.11, PyTorch 2.7.0, Rust 1.82.0
- Context: Graduate student at Indian Institute of Science studying ML

## Critical Rules

### Package Management
- ONLY Guix (never apt/pip/npm -g/cargo install)
- All dependencies in manifest.scm
- Wrap commands: `guix shell -m manifest.scm -- <command>`
- Package mappings: requests→python-requests, sklearn→python-scikit-learn, yaml→python-pyyaml

### Python
- Command: `python3` (never `python`)
- Testing: pytest, 100% coverage on public APIs, write tests BEFORE fixing bugs
- Format before commit: `ruff format . && ruff check . && pytest -xvs`

### LaTeX
- Use XeLaTeX/LuaLaTeX (not pdfLaTeX)
- BibTeX: include DOI/URL and OpenAlex ID
- Figures: vector (PDF/EPS) or PNG at 300 DPI min
- Tables: booktabs package, no vertical lines

### Git
- Conventional commit format
- Never commit unless explicitly asked
- Never amend others' commits
- Never commit: .env, secrets/, credentials

## Project Patterns
- manifest.scm → Guix project
- pyproject.toml → Read-only reference
- Cargo.toml → Rust project
- *.tex → LaTeX document

## Daily Workflow
- ~/tmp cleared daily
- ~/projects/ flat structure
- Delete experiments after completion
- Plots: 300 DPI → assets/