# User Preferences - Ayan Das

## Specialized Agents

### Agent Usage Instructions
Proactively use specialized agents for focused tasks. Each agent handles ONE specific task well within its context window.

#### Quick Agent Selection
- **Python issues**: Start with `python-import-resolver` for imports, `python-ruff-fixer` for linting, `python-pyright-resolver` for types
- **Rust maintenance**: Use `cargo-dependency-updater` for deps, `rust-clippy-fixer` for idioms
- **LaTeX problems**: Begin with `latex-compiler-fixer`, then specialized agents for math/tables/figures
- **Before commits**: Always run `security-secret-scanner` then `git-commit-formatter`
- **Code cleanup**: Chain `import-cleaner` → language-specific linter → `test-coverage-analyzer`

#### Key Patterns
1. **Dependency updates**: `dependency-patch-updater` (safe) before attempting breaking changes
2. **Refactoring**: `function-renamer` for single functions, `pattern-replacer` for systematic changes
3. **Python setup**: `guix-manifest-updater` → `python-import-resolver` → verify with `manifest-dependency-checker`
4. **Testing gaps**: `test-coverage-analyzer` to identify, then write tests manually
5. **Bibliography**: `bibtex-citation-checker` for consistency, enrichers for improvement

#### Agent Principles
- Analysis agents (`*-analyzer`, `*-checker`, `*-finder`) never modify files
- All Python agents use `guix shell -m manifest.scm` exclusively
- One task per agent invocation for best results
- Chain agents for complex workflows

See `docs/claude/agents.md` for full reference.

## System Environment

### Hardware & OS
- CPU: Ryzen 5600G (12 threads), RAM: 64GB DDR4, Storage: 4TB NVMe Gen4, GPU: RTX 3060 (12GB)
- OS: GNU Guix System, GNOME/Wayland, XDG compliant
- Python 3.11, PyTorch 2.7.0, Rust 1.82.0

### Package Management
- Development: Always `guix shell -m manifest.scm`
- Never: pip, poetry shell, venv, npm -g
- Project deps: manifest.scm at root

## Project Standards

### Repository Rules
- Python: poetry+ruff+pyright+pytest (via Guix only)
- Format→Lint→Type→Test before commit
- No warnings, 100% type coverage on public APIs
- Stateful data: ~/.cache/project/ or ~/.local/share/project/
- Never in repo: __pycache__, *.pyc, node_modules/, data/, models/

### Git Discipline
- Pre-commit hooks mandatory
- No ad-hoc scripts (use scripts/ if needed)
- Clean feature branches after merge
- Conventional commit format
- Guix package commits: "packages: <package-name>: Add/Update to <version>."

## Language Preferences

### Python
- python3 always, pathlib for paths, f-strings
- Black formatting via ruff
- Type hints on signatures only
- pytest -xvs for testing

### Shell
- Long flags (--help), quote variables "${var}"
- Explicit paths, check command existence

### Rust/JS
- cargo/npm OK (project-local only)

## Writing Style

### Documentation
- Technical, terse, no adjectives
- No "we/you/our", use imperatives
- No "robust/high-quality/production-ready"
- README: description→install→usage→license
- Markdown only (not Org mode)
- No unicode characters (ASCII only)

### Examples
BAD: "This robust library provides high-quality processing"
GOOD: "Image processing library"

## Daily Workflow

### Standards
- ~/tmp cleared daily, ~/downloads weekly
- No Desktop files, use ~/projects/ flat
- manifest.scm → git init → hooks → README → commit
- Delete experiments after completion
- Plots: 300 DPI, save to assets/, no plt.show()
- Filenames: lowercase with hyphens

## Current Focus
- Graduate student at Indian Institute of Science
- ML research projects
- Theory-practice bridge

## Emacs Features Reference (emacs-xyz.scm)

```
feature-emacs-appearance: 163-282
feature-emacs-modus-themes: 284-487
feature-emacs-circadian: 489-547
feature-emacs-which-key: 549-585
feature-emacs-keycast: 587-639
feature-emacs-all-the-icons: 641-679
feature-emacs-input-methods: 688-745
feature-emacs-battery: 747-790
feature-emacs-time: 804-869
feature-emacs-calendar: 871-934
feature-emacs-tramp: 936-1064
feature-emacs-dired: 1066-1176
feature-emacs-eat: 1178-1207
feature-emacs-eshell: 1209-1314
feature-emacs-calc: 1316-1352
feature-emacs-re-builder: 1354-1382
feature-emacs-comint: 1384-1418
feature-emacs-shell: 1420-1457
feature-emacs-browse-url: 1459-1610
feature-emacs-tab-bar: 1612-1760
feature-emacs-power-menu: 1762-1891
feature-emacs-completion: 1935-2228
feature-emacs-vertico: 2231-2405
feature-emacs-mct: 2408-2498
feature-emacs-corfu: 2501-2596
feature-emacs-tempel: 2599-2681
feature-emacs-monocle: 2690-2800
feature-emacs-project: 2802-2912
feature-emacs-perspective: 2916-2971
feature-emacs-ednc: 2973-3045
feature-emacs-ace-window: 3047-3077
feature-emacs-smartparens: 3086-3157
feature-emacs-eglot: 3159-3193
feature-emacs-dape: 3195-3250
feature-emacs-flymake: 3252-3272
feature-emacs-elisp: 3274-3321
feature-emacs-git: 3326-3468
feature-emacs-geiser: 3470-3525
feature-emacs-guix: 3527-3589
feature-emacs-xref: 3591-3617
feature-emacs-treebundel: 3619-3640
feature-emacs-pdf-tools: 3649-3732
feature-emacs-nov-el: 3734-3805
feature-emacs-elfeed: 3810-3871
feature-emacs-help: 3873-3922
feature-emacs-info: 3924-3983
feature-emacs-devdocs: 3985-4012
feature-emacs-org: 4022-4275
feature-emacs-org-agenda: 4382-4526
feature-emacs-org-dailies: 4604-4668
feature-emacs-org-ql: 4670-4697
feature-emacs-org-agenda-files-track: 4699-4732
feature-emacs-org-roam: 4735-4882
feature-emacs-citation: 4884-4997
feature-emacs-zotra: 4999-5035
feature-emacs-spelling: 5037-5117
feature-emacs-org-recur: 5119-5150
feature-emacs-graphviz: 5152-5194
feature-emacs-denote: 5196-5308
feature-emacs-telega: 5317-5438
feature-emacs-ebdb: 5440-5511
feature-emacs-elpher: 5513-5547
feature-emacs-dashboard: 5557-5690
feature-emacs-emms: 5692-5986
feature-emacs-nyxt: 5988-6035
feature-emacs-pulseaudio-control: 6037-6106
feature-emacs-webpaste: 6108-6155
feature-emacs-display-wttr: 6157-6189
feature-emacs-daemons: 6191-6214
```