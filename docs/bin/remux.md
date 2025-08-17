# remux - Video Container Conversion Utility

Remux video files between container formats without re-encoding, preserving original quality.

## Overview

The `remux` utility changes video container formats while preserving the original video, audio, and subtitle streams. This is much faster than re-encoding and maintains perfect quality.

## Installation

Ensure the script is in your PATH:
```bash
~/.local/bin/remux
```

### Dependencies

Required:
- `ffmpeg` - Video processing
- `ffprobe` - Stream analysis
- `find` - File discovery

Optional:
- `parallel` - Concurrent processing

## Basic Usage

```bash
# Remux Blu-ray M2TS to MKV (default)
remux -i /media/bluray

# Specify input and output formats
remux -i /videos --from .avi --to .mp4

# Custom output directory
remux -i /media/bluray -o /storage/movies
```

## Supported Formats

All common video containers:
- `.mkv` - Matroska (recommended for complex content)
- `.mp4` - MPEG-4 (widely compatible)
- `.avi` - Audio Video Interleave
- `.mov` - QuickTime
- `.m2ts` - Blu-ray transport stream
- `.ts` / `.mts` - MPEG transport stream
- `.webm` - Web media
- `.mxf` - Material eXchange Format
- `.asf` / `.wmv` - Windows Media

## Stream Selection

### Basic Stream Selection
```bash
# Keep only best streams (video + primary audio + subtitle)
remux -i /videos --select-best

# Select specific audio languages
remux -i /videos --audio-streams eng,jpn

# Select subtitle languages
remux -i /videos --subtitle-streams eng,jpn,ger
```

### Advanced Stream Selection
```bash
# Select by stream index
remux -i /video.mkv --video-streams 0 --audio-streams 0,1 --subtitle-streams 0

# Exclude all subtitles
remux -i /videos --subtitle-streams none

# Keep all streams (default)
remux -i /videos --audio-streams all
```

## Templates

Pre-configured settings for common scenarios:

### bluray-universal
Best video + all audio + English subtitles
```bash
remux -i /bluray --template bluray-universal
```

### movie-collection
All video/audio/subtitle streams for archival
```bash
remux -i /movies --template movie-collection
```

### streaming-optimized
Minimal streams for streaming (MP4 output)
```bash
remux -i /videos --template streaming-optimized
```

### Other Templates
- `tv-series` - Optimized for TV episodes
- `archive-quality` - Preserve everything
- `mobile-device` - Optimized for phones/tablets

## Advanced Features

### Auto-Format Detection
```bash
# Automatically choose best output format
remux -i /videos --auto-format
```

### Stream Information
```bash
# Display detailed stream info before processing
remux -i /video.mkv --show-streams
```

### Verification
```bash
# Verify output after remuxing
remux -i /videos --verify

# Quick duration check only
remux -i /videos --quick-verify
```

### Metadata Preservation
```bash
# Preserve all metadata (default)
remux -i /videos --preserve-metadata

# Preserve chapters
remux -i /videos --preserve-chapters

# Strip metadata
remux -i /videos --no-preserve-metadata
```

## Resume Capability

### Job Management
```bash
# List all remuxing jobs
remux --list-jobs

# Resume interrupted job
remux --resume

# Clean up old jobs
remux --cleanup-stale
```

### Example Resume Workflow
```bash
# Start large remux job
remux -i /media/tv-series --template tv-series

# If interrupted, check status
remux --list-jobs

# Resume from where it stopped
remux --resume
```

## Batch Processing

### Pattern Matching
```bash
# Process only M2TS files
remux -i /media --pattern "*.m2ts"

# Process files matching pattern
remux -i /videos --pattern "*1080p*.mkv" --to .mp4
```

### File List
```bash
# Create file list
find /media -name "*.avi" > videos.txt

# Process from list
remux --file-list videos.txt --to .mp4
```

## Configuration

Create configuration at `~/.config/remux/config`:
```bash
# Default settings
from_ext=.m2ts
to_ext=.mkv
parallel_jobs=4
select_best=false
preserve_metadata=true
preserve_chapters=true
```

## Performance Optimization

### Parallel Processing
```bash
# Use 8 parallel jobs
remux -i /media -j 8

# Optimal for SSD
remux -i /fast-storage -j 16

# Conservative for HDD
remux -i /slow-storage -j 2
```

### Large Collections
```bash
# Process with progress tracking
remux -i /media/collection -v

# Disable progress for background jobs
remux -i /media/collection --no-progress
```

## Examples

### Blu-ray Collection
```bash
# Convert all Blu-rays to MKV
remux -i /media/bluray-rips --template bluray-universal
```

### Anime Collection
```bash
# Multi-language with subtitles
remux -i /anime --audio-streams jpn,eng --subtitle-streams all
```

### Streaming Preparation
```bash
# Convert for Plex/Jellyfin
remux -i /movies --template streaming-optimized --verify
```

### Archive Project
```bash
# Preserve everything with verification
remux -i /archive --template archive-quality --verify
```

## Troubleshooting

### Stream Detection Issues
- Use `--show-streams` to inspect file
- Check ffprobe installation
- Verify file isn't corrupted

### Format Compatibility
- Some codecs work better in specific containers
- H.265 → MKV (better compatibility)
- H.264 → MP4 (wider support)

### Performance Problems
- Reduce parallel jobs
- Check disk I/O bottlenecks
- Monitor CPU usage
- Use SSD for better performance

### Resume Issues
- Ensure same parameters
- Check state directory permissions
- Clean stale jobs

## Container Recommendations

| Content Type | Recommended Container | Reason |
|-------------|----------------------|---------|
| Blu-ray Remux | MKV | Supports all features |
| Web Streaming | MP4 | Wide compatibility |
| Multi-Audio | MKV | Better stream support |
| Simple Video | MP4 | Smaller overhead |
| Archival | MKV | Metadata support |

## Exit Codes

- `0` - Success
- `1` - Usage error
- `2` - Configuration error
- `3` - Missing dependencies
- `4` - Operation failed
- `130` - Interrupted by signal