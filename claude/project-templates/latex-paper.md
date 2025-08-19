# LaTeX Paper Template

## Project Structure
```
paper/
├── manifest.scm          # Guix dependencies
├── Makefile             # Build automation
├── main.tex             # Main document
├── references.bib       # Bibliography
├── sections/            # Paper sections
│   ├── abstract.tex
│   ├── introduction.tex
│   ├── related.tex
│   ├── method.tex
│   ├── experiments.tex
│   ├── results.tex
│   └── conclusion.tex
├── figures/             # Figure files
├── tables/              # Table files
└── supplementary/       # Supplementary material
```

## Standard manifest.scm for LaTeX
```scheme
(specifications->manifest
 '("texlive"
   "biber"
   "python"  ; For plots
   "python-matplotlib"
   "python-numpy"
   "gnuplot"
   "imagemagick"
   "ghostscript"))
```

## Makefile
```makefile
MAIN = main
LATEX = xelatex
BIBTEX = bibtex

.PHONY: all clean

all: $(MAIN).pdf

$(MAIN).pdf: $(MAIN).tex references.bib sections/*.tex
	$(LATEX) $(MAIN)
	$(BIBTEX) $(MAIN)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

clean:
	rm -f *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lot
	rm -f sections/*.aux

watch:
	latexmk -pvc -xelatex $(MAIN).tex

spell:
	aspell -t -c $(MAIN).tex
	for file in sections/*.tex; do aspell -t -c $$file; done

count:
	texcount -inc $(MAIN).tex
```

## Main Document Template
```latex
\documentclass[11pt,a4paper]{article}

% Packages
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{microtype}
\usepackage{amsmath,amssymb,amsthm}
\usepackage{graphicx}
\usepackage{subcaption}
\usepackage{booktabs}
\usepackage{algorithm2e}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{cleveref}

% Bibliography
\usepackage[backend=bibtex,style=ieee,sorting=none]{biblatex}
\addbibresource{references.bib}

% Custom commands
\newcommand{\R}{\mathbb{R}}
\newcommand{\E}{\mathbb{E}}
\DeclareMathOperator*{\argmax}{arg\,max}
\DeclareMathOperator*{\argmin}{arg\,min}

% Title
\title{Paper Title}
\author{Ayan Das\\Indian Institute of Science}
\date{\today}

\begin{document}
\maketitle

\input{sections/abstract}
\input{sections/introduction}
\input{sections/related}
\input{sections/method}
\input{sections/experiments}
\input{sections/results}
\input{sections/conclusion}

\printbibliography

\end{document}
```

## BibTeX Entry Template
```bibtex
@inproceedings{key2024,
  title = {Paper Title},
  author = {Last, First and Other, Author},
  booktitle = {Proceedings of Conference},
  pages = {100--110},
  year = {2024},
  doi = {10.1234/conf.2024.123},
  url = {https://arxiv.org/abs/2024.12345},
  openalex = {W123456789}
}

@article{key2024journal,
  title = {Article Title},
  author = {Last, First and Other, Author},
  journal = {Journal Name},
  volume = {10},
  number = {2},
  pages = {100--110},
  year = {2024},
  publisher = {Publisher},
  doi = {10.1234/journal.2024.123}
}
```

## Figure Best Practices
```latex
\begin{figure}[ht]
  \centering
  \includegraphics[width=0.8\textwidth]{figures/result.pdf}
  \caption{Clear, descriptive caption explaining the figure.}
  \label{fig:result}
\end{figure}

% Reference: As shown in \cref{fig:result}...
```

## Table Best Practices
```latex
\begin{table}[ht]
  \centering
  \caption{Table caption goes above the table.}
  \label{tab:results}
  \begin{tabular}{@{}lcc@{}}
    \toprule
    Method & Accuracy & F1-Score \\
    \midrule
    Baseline & 0.85 & 0.83 \\
    Our Method & \textbf{0.92} & \textbf{0.91} \\
    \bottomrule
  \end{tabular}
\end{table}
```