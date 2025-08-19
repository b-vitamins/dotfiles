# LaTeX Academic Writing Instructions

## Document Standards
- Use XeLaTeX or LuaLaTeX (never pdfLaTeX for modern docs)
- BibTeX entries must include DOI/URL and OpenAlex ID
- Figures: vector (PDF/EPS) > raster (PNG at 300 DPI min)
- Tables: booktabs package, no vertical lines

## LaTeX Agent Workflow

### 1. Initial Setup & Compilation
- `latex-compiler-fixer` → Fix compilation errors first
- `latex-package-resolver` → Resolve package conflicts

### 2. Content Organization
- `latex-figure-organizer` → Manage figure placement
- `latex-table-formatter` → Professional table formatting
- `latex-math-validator` → Ensure proper math mode usage

### 3. Bibliography Management
- `bibtex-citation-checker` → Verify all citations exist
- `bibtex-entry-enricher` → Add DOIs and metadata

### 4. Presentation Creation
- `paper-to-beamer` → Convert papers to presentations
- `latex-beamer-styler` → Professional slide formatting

## Common Fixes
```latex
% Wide tables
\begin{table}
  \small % or \footnotesize
  \begin{adjustbox}{width=\textwidth}
    \begin{tabular}{...}
    ...
    \end{tabular}
  \end{adjustbox}
\end{table}

% Proper math mode
$x$ not x in text
\( \) for inline, \[ \] for display

% Modern packages
\usepackage{booktabs} % tables
\usepackage{subcaption} % subfigures
\usepackage{cleveref} % smart references
```

## Package Order (Critical)
1. hyperref (load last, except...)
2. cleveref (after hyperref)
3. Algorithm packages before hyperref