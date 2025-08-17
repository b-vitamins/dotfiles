# harvester - ArXiv Paper Aggregator

Download and organize research papers from ArXiv using RSS feeds and OAI-PMH protocol.

## Overview

The `harvester` utility aggregates papers from ArXiv categories, supporting both recent papers (RSS) and historical data (OAI-PMH). Features include keyword filtering, multiple output formats, and configuration management.

## Installation

Ensure the script is in your PATH:
```bash
~/.local/bin/harvester
```

### Dependencies

None! Pure Python standard library implementation.

## Basic Usage

```bash
# Get today's papers from default categories
harvester

# Get papers from specific categories
harvester --categories cs.LG cs.AI stat.ML

# Get last week's papers
harvester --method oai --days-back 7
```

## ArXiv Categories

### Common ML/AI Categories
- `cs.LG` - Machine Learning
- `stat.ML` - Machine Learning (Statistics)
- `cs.AI` - Artificial Intelligence
- `cs.CV` - Computer Vision
- `cs.CL` - Computation and Language
- `cs.NE` - Neural and Evolutionary Computing

### Physics/Math Categories
- `cond-mat.stat-mech` - Statistical Mechanics
- `cond-mat.dis-nn` - Disordered Systems and Neural Networks
- `physics.data-an` - Data Analysis, Statistics
- `math.OC` - Optimization and Control
- `nlin.AO` - Adaptation and Self-Organizing Systems

## Harvesting Methods

### RSS (Recent Papers)
```bash
# Today's papers (default)
harvester

# Specific categories
harvester --categories cs.LG cs.AI
```

Note: RSS feeds are empty on weekends (ArXiv publishing schedule).

### OAI-PMH (Historical Data)
```bash
# Last N days
harvester --method oai --days-back 7

# Specific date range
harvester --method oai --from-date 2024-01-01 --until-date 2024-01-31

# Yesterday's papers
harvester --method oai --days-back 1
```

## Filtering

### Keyword Filtering
```bash
# Include papers with keywords
harvester --keywords transformer attention "neural network"

# Exclude papers
harvester --exclude survey review tutorial

# Combined filtering
harvester --keywords "diffusion model" --exclude survey
```

### Cross-listing Filter
```bash
# Papers in at least 2 categories
harvester --min-categories 2
```

## Output Formats

### JSON (Default)
```bash
harvester --format json
```

### Markdown
```bash
harvester --format markdown

# Custom output file
harvester --format markdown --output papers.md
```

## Configuration

### Configuration File
Create `~/.config/harvester/config.json`:
```json
{
  "categories": ["cs.LG", "cs.AI", "stat.ML"],
  "output_dir": "~/Documents/arxiv-papers",
  "format": "markdown",
  "method": "rss"
}
```

### Generate Example
```bash
harvester --save-config-example
```

## Output Organization

Papers are saved to:
- Default: `~/.local/share/harvester/`
- Custom: `--output-dir /path/to/papers`

Filename format:
- RSS: `arxiv-papers-YYYYMMDD.{format}`
- OAI: `arxiv-papers-YYYYMMDD-YYYYMMDD.{format}`

## Advanced Usage

### Research Workflow
```bash
# Daily ML papers with filtering
harvester --categories cs.LG stat.ML \
          --keywords "reinforcement learning" \
          --exclude survey \
          --format markdown

# Weekly comprehensive harvest
harvester --method oai --days-back 7 \
          --categories cs.LG cs.AI cs.CV cs.CL \
          --format json
```

### Building Paper Database
```bash
# Harvest January 2024 CS papers
harvester --method oai \
          --from-date 2024-01-01 \
          --until-date 2024-01-31 \
          --categories cs.LG cs.AI cs.CV \
          --output-dir ~/research/2024-01

# Filter for specific topics
harvester --method oai --days-back 30 \
          --keywords "large language model" "LLM" \
          --min-categories 2
```

### Conference Preparation
```bash
# Get recent papers on specific topic
harvester --method oai --days-back 90 \
          --keywords "graph neural network" \
          --exclude survey \
          --format markdown \
          --output conference-papers.md
```

## Output Structure

### JSON Format
```json
{
  "2401.00123": {
    "arxiv_id": "2401.00123",
    "title": "Paper Title",
    "authors": ["Author One", "Author Two"],
    "abstract": "Abstract text...",
    "primary_category": "cs.LG",
    "all_categories": ["cs.LG", "cs.AI"],
    "link": "http://arxiv.org/abs/2401.00123",
    "pdf_link": "http://arxiv.org/pdf/2401.00123.pdf",
    "published": "2024-01-15",
    "updated": "2024-01-16"
  }
}
```

### Markdown Format
```markdown
# ArXiv Papers - 2024-01-15

Total papers: 42

## Statistics by Category
- **cs.LG**: 25 papers
- **cs.AI**: 17 papers

## cs.LG

### [Paper Title](http://arxiv.org/abs/2401.00123)
**Authors**: Author One, Author Two
**Categories**: cs.LG, cs.AI
**Abstract**: Abstract text...
[PDF](http://arxiv.org/pdf/2401.00123.pdf) | [arXiv:2401.00123](http://arxiv.org/abs/2401.00123)
```

## Automation

### Daily Harvest (cron)
```bash
# Add to crontab
0 9 * * * /home/user/.local/bin/harvester --categories cs.LG cs.AI --format markdown
```

### Weekly Digest
```bash
#!/bin/bash
# weekly-arxiv.sh
harvester --method oai --days-back 7 \
          --categories cs.LG cs.AI stat.ML \
          --keywords "my research topics" \
          --format markdown \
          --output ~/weekly-papers-$(date +%Y%m%d).md
```

## Tips

### Weekend Usage
RSS feeds are empty on weekends. Use OAI-PMH instead:
```bash
harvester --method oai --days-back 3
```

### Large Date Ranges
OAI-PMH may paginate results for large ranges. The script handles this automatically.

### Rate Limiting
The script implements polite crawling with delays between requests.

### Filtering Strategy
- Use broad categories, filter with keywords
- Exclude common paper types (survey, review)
- Check cross-listings for interdisciplinary work

## Troubleshooting

### No Papers Found (RSS)
- Check if it's a weekend
- Verify categories are valid
- Use OAI-PMH method instead

### Connection Errors
- Check internet connectivity
- ArXiv may be temporarily down
- Script retries automatically (3 attempts)

### Invalid Categories
- Verify category codes at arxiv.org
- Use standard ArXiv taxonomy

### Output Issues
- Check write permissions
- Verify disk space
- Use absolute paths for output

## Exit Codes

- `0` - Success
- `1` - Error occurred