# Python ML Project Template

## Project Structure
```
project/
├── manifest.scm           # Guix dependencies
├── pyproject.toml         # Project metadata
├── README.md             # Project documentation
├── .gitignore            # Git ignore rules
├── src/                  # Source code
│   └── project/
│       ├── __init__.py
│       ├── models/       # ML models
│       ├── data/         # Data processing
│       └── utils/        # Utilities
├── tests/                # Test suite
│   ├── unit/
│   ├── integration/
│   └── fixtures/
├── experiments/          # Experiment scripts
│   └── exp_001.py
├── notebooks/            # Jupyter notebooks
├── data/                 # Data (gitignored)
│   ├── raw/
│   ├── processed/
│   └── external/
├── models/               # Saved models (gitignored)
├── assets/               # Figures, plots
└── results/              # Experiment results
```

## Standard manifest.scm
```scheme
(specifications->manifest
 '("python"
   "python-pytorch"
   "python-numpy"
   "python-pandas"
   "python-scikit-learn"
   "python-matplotlib"
   "python-seaborn"
   "python-jupyter"
   "python-pytest"
   "python-pytest-cov"
   "python-hypothesis"
   "python-black"
   "python-ruff"
   "python-mypy"
   "python-ipython"))
```

## Standard .gitignore
```
# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
env/
venv/
.eggs/
*.egg-info/
.pytest_cache/
.mypy_cache/
.coverage
htmlcov/

# Data and models
data/
models/
*.pkl
*.h5
*.pt
*.pth
*.onnx

# Jupyter
.ipynb_checkpoints/
*.ipynb_checkpoints

# IDE
.vscode/
.idea/
*.swp
*.swo

# OS
.DS_Store
Thumbs.db

# Project specific
results/*.csv
results/*.json
!results/.gitkeep
```

## Experiment Template
```python
#!/usr/bin/env python3
"""
Experiment: [Name]
Date: [Date]
Author: Ayan Das
Description: [Brief description]
"""
import json
import logging
from pathlib import Path
from datetime import datetime

import numpy as np
import pandas as pd
import torch
from sklearn.metrics import accuracy_score

# Setup
SEED = 42
np.random.seed(SEED)
torch.manual_seed(SEED)

# Paths
PROJECT_ROOT = Path(__file__).parent.parent
DATA_DIR = PROJECT_ROOT / "data"
RESULTS_DIR = PROJECT_ROOT / "results"
ASSETS_DIR = PROJECT_ROOT / "assets"

# Logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def main():
    # Experiment configuration
    config = {
        "seed": SEED,
        "learning_rate": 0.001,
        "batch_size": 32,
        "epochs": 100,
    }
    
    # Log configuration
    logger.info(f"Starting experiment with config: {config}")
    
    # Your experiment code here
    results = {}
    
    # Save results
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    results_file = RESULTS_DIR / f"exp_{timestamp}.json"
    
    with open(results_file, 'w') as f:
        json.dump({
            "config": config,
            "results": results,
            "timestamp": timestamp
        }, f, indent=2)
    
    logger.info(f"Results saved to {results_file}")

if __name__ == "__main__":
    main()
```