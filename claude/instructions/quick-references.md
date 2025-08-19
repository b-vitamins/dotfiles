# Quick References & Commands

## Frequent Project Commands
```bash
# Python project setup
guix shell -m manifest.scm
pytest -xvs
ruff format . && ruff check .

# LaTeX compilation
xelatex main.tex && bibtex main && xelatex main.tex && xelatex main.tex

# Git workflow
git status && git diff
git add -p  # Interactive staging
git commit  # Will trigger git-commit-formatter hook

# Cleanup
find . -type d -name "__pycache__" -exec rm -rf {} +
find . -name "*.pyc" -delete
```

## Common Guix Package Mappings
- `sklearn` → `python-scikit-learn`
- `cv2` → `opencv` + `python-numpy`
- `PIL/Pillow` → `python-pillow`
- `yaml` → `python-pyyaml`
- `bs4` → `python-beautifulsoup4`
- `dateutil` → `python-dateutil`
- `dotenv` → `python-dotenv`
- `tqdm` → `python-tqdm`
- `jwt` → `python-pyjwt`

## Pytest Markers & Options
```bash
pytest -xvs                    # Stop on first failure, verbose
pytest -k "test_name"          # Run specific test
pytest -m "not slow"           # Skip slow tests
pytest --lf                    # Run last failed
pytest --ff                    # Run failed first
pytest --cov=module            # Coverage report
pytest --cov-report=html       # HTML coverage
pytest --cov-report=term-missing  # Show missing lines
```

## Ruff Configuration Snippets
```toml
# pyproject.toml
[tool.ruff]
line-length = 100
target-version = "py311"

[tool.ruff.lint]
select = ["E", "F", "I", "N", "W", "UP"]
ignore = ["E501"]  # Line too long

[tool.ruff.format]
quote-style = "double"
indent-style = "space"
```

## LaTeX Quick Fixes
```latex
% Wide table
\resizebox{\textwidth}{!}{%
  \begin{tabular}{...}
  ...
  \end{tabular}%
}

% Subfigures
\begin{figure}
  \begin{subfigure}{0.45\textwidth}
    \includegraphics[width=\textwidth]{fig1}
    \caption{First}
  \end{subfigure}
  \hfill
  \begin{subfigure}{0.45\textwidth}
    \includegraphics[width=\textwidth]{fig2}
    \caption{Second}
  \end{subfigure}
\end{figure}

% Algorithm
\usepackage{algorithm2e}
\begin{algorithm}
  \SetAlgoLined
  \KwIn{Input}
  \KwOut{Output}
  ...
\end{algorithm}
```