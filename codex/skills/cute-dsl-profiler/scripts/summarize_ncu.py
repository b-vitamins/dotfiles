#!/usr/bin/env python3
"""Summarize existing Nsight Compute section dumps."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from _ncu_metrics import DEFAULT_SECTIONS, format_summary, parse_ncu_outputs


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "directory",
        type=Path,
        help="Directory containing <section>.txt files produced by run_ncu.py",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    outputs: dict[str, str] = {}
    for section in DEFAULT_SECTIONS:
        path = args.directory / f"{section}.txt"
        if not path.exists():
            raise SystemExit(f"Missing section dump: {path}")
        outputs[section] = path.read_text(encoding="utf-8")
    summary = parse_ncu_outputs(outputs)
    print(format_summary(summary))  # noqa: print
    print(json.dumps(summary, indent=2, sort_keys=True))  # noqa: print

if __name__ == "__main__":
    main()
