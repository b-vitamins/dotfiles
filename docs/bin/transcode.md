# transcode - Audio Format Conversion Utility

Convert audio files between various formats with parallel processing and resume capability.

## Overview

The `transcode` utility converts audio files between formats while preserving directory structure and metadata. It supports parallel processing, resume capability for interrupted jobs, and various quality settings per format.

## Installation

Ensure the script is in your PATH:
```bash
~/.local/bin/transcode
```

### Dependencies

Required:
- `ffmpeg` - Audio encoding/decoding
- `find` - File discovery

Optional:
- `flac` - WAV to FLAC conversion
- `parallel` - Concurrent processing

## Basic Usage

```bash
# Convert FLAC to MP3 with default quality (V0)
transcode -i /music/flac --from .flac --to .mp3

# Convert with specific bitrate
transcode -i /music/flac --from .flac --to .mp3 -q 320k

# Convert APE to FLAC with maximum compression
transcode -i /music/ape --from .ape --to .flac -q 8
```

## Supported Conversions

| From | To | Description |
|------|-----|-------------|
| .flac | .mp3 | Lossy compression for portability |
| .wav | .flac | Lossless compression |
| .ape | .mp3 | Lossy from lossless |
| .ape | .flac | Lossless to lossless |
| .flac | .ogg | Opus encoding |
| .ape | .ogg | Opus encoding |
| .flac | .m4a | AAC encoding |
| .ape | .m4a | AAC encoding |

## Quality Settings

### MP3
- VBR: `V0` (best) to `V9` (smallest)
- CBR: `128k`, `192k`, `256k`, `320k`

### FLAC
- Compression: `0` (fastest) to `8` (smallest)

### Opus (OGG)
- Bitrate: `96k`, `128k`, `160k`, `256k`

### AAC (M4A)
- Bitrate: `128k`, `192k`, `256k`, `320k`

## Resume Capability

### Automatic Resume Detection
```bash
# Start conversion
transcode -i /music/flac --from .flac --to .mp3

# If interrupted, resume with:
transcode --resume

# Or explicitly:
transcode --resume -i /music/flac --from .flac --to .mp3
```

### Job Management
```bash
# List all conversion jobs
transcode --list-jobs

# Clean up old jobs
transcode --cleanup-stale

# Force new job (ignore resumable)
transcode --no-resume -i /music/flac --from .flac --to .mp3
```

## Advanced Options

### Parallel Processing
```bash
# Use 8 parallel jobs
transcode -i /music --from .flac --to .mp3 -j 8

# Use all CPU cores (default)
transcode -i /music --from .flac --to .mp3
```

### Overwrite Protection
```bash
# Skip existing files (default)
transcode -i /music --from .flac --to .mp3

# Overwrite existing files
transcode -i /music --from .flac --to .mp3 --overwrite
```

### Verbose Output
```bash
# Enable detailed logging
transcode -i /music --from .flac --to .mp3 -v
```

## Configuration

Create a configuration file at `~/.config/transcode/config`:
```bash
# Default settings
input_dir=/music/flac
output_dir=/music/mp3
parallel_jobs=8
overwrite=false
```

Generate example configuration:
```bash
transcode --save-config-example
```

## State Management

Conversion state is stored in `~/.local/state/transcode/`:
- Job tracking files
- Progress information
- Completion records

## Examples

### Convert Music Collection for Car
```bash
transcode -i /music/lossless --from .flac --to .mp3 -q 192k -o /music/car
```

### Archive APE Collection
```bash
transcode -i /downloads/ape --from .ape --to .flac -q 8 --overwrite
```

### Create Mobile Versions
```bash
transcode -i /music/flac --from .flac --to .m4a -q 256k -o /music/mobile
```

### Process Large Collection with Resume
```bash
# Start conversion (may take hours/days)
transcode -i /archive/music --from .flac --to .mp3 -q V0

# Check progress
transcode --list-jobs

# Resume after interruption
transcode --resume
```

## Troubleshooting

### No Files Found
- Check file extensions match exactly (case-sensitive)
- Verify input directory contains matching files
- Use `-v` for verbose file discovery

### Conversion Failures
- Check ffmpeg is installed: `ffmpeg -version`
- Verify input files are valid audio files
- Check disk space in output directory
- Review error messages for codec issues

### Resume Not Working
- Ensure same parameters as original job
- Check state directory permissions
- Use `--list-jobs` to see available jobs
- Clean stale jobs with `--cleanup-stale`

### Performance Issues
- Reduce parallel jobs: `-j 4`
- Check CPU and memory usage
- Consider SSD for temporary files
- Monitor disk I/O bottlenecks

## Exit Codes

- `0` - Success
- `1` - Usage error
- `2` - Configuration error
- `3` - Missing dependencies
- `4` - Operation failed
- `130` - Interrupted by signal