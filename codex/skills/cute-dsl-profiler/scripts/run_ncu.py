#!/usr/bin/env python3
"""Run Nsight Compute sections against an arbitrary command and summarize them."""

from __future__ import annotations

import argparse
import json
import subprocess
from datetime import datetime, timezone
from pathlib import Path

from _ncu_metrics import DEFAULT_SECTIONS, format_summary, parse_ncu_outputs


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ncu", default="ncu", help="Nsight Compute executable.")
    parser.add_argument("--label", default="profile", help="Label prefix for output files.")
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Output directory. Defaults to /tmp/cute-dsl-ncu-<timestamp>.",
    )
    parser.add_argument(
        "--section",
        action="append",
        default=[],
        help="NCU section to collect. Repeatable. Defaults to a curated set.",
    )
    parser.add_argument(
        "command",
        nargs=argparse.REMAINDER,
        help="Command to profile after `--`.",
    )
    return parser.parse_args()


def run_section(ncu: str, section: str, command: list[str]) -> str:
    cmd = [
        ncu,
        "--section",
        section,
        "--print-details",
        "all",
        "--profile-from-start",
        "off",
        *command,
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True)
    output = (proc.stdout or "") + (proc.stderr or "")
    if proc.returncode != 0:
        raise RuntimeError(f"NCU failed for section {section}:\n{output}")
    return output


def main() -> None:
    args = parse_args()
    command = [arg for arg in args.command if arg != "--"]
    if not command:
        raise SystemExit("A command is required after `--`.")
    sections = args.section or list(DEFAULT_SECTIONS)
    out_dir = args.out_dir
    if out_dir is None:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        out_dir = Path("/tmp") / f"cute-dsl-ncu-{args.label}-{stamp}"
    out_dir.mkdir(parents=True, exist_ok=True)

    outputs: dict[str, str] = {}
    for section in sections:
        outputs[section] = run_section(args.ncu, section, command)
        (out_dir / f"{section}.txt").write_text(outputs[section], encoding="utf-8")

    summary = parse_ncu_outputs(outputs)
    summary_path = out_dir / "summary.json"
    summary_path.write_text(
        json.dumps(
            {
                "generated_at": datetime.now(timezone.utc).isoformat(),
                "label": args.label,
                "command": command,
                "sections": sections,
                "summary": summary,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    text_path = out_dir / "summary.txt"
    text_path.write_text(format_summary(summary), encoding="utf-8")

    print(f"out_dir: {out_dir}")  # noqa: print
    print(f"summary_json: {summary_path}")  # noqa: print
    print(f"summary_txt: {text_path}")  # noqa: print
    print(format_summary(summary))  # noqa: print

if __name__ == "__main__":
    main()
