#!/usr/bin/env python3
"""Run Nsight Compute source counters and summarize hotspot instructions."""

from __future__ import annotations

import argparse
import csv
import json
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


DEFAULT_TOP_COLUMNS = [
    "L1 Conflicts Shared N-Way",
    "L1 Wavefronts Shared Excessive",
    "L1 Wavefronts Shared",
    "Instructions Executed",
    "stall_long_sb",
    "stall_wait",
    "stall_barrier",
    "L2 Theoretical Sectors Global Excessive",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ncu", default="ncu", help="Nsight Compute executable.")
    parser.add_argument("--label", default="source", help="Label prefix for output files.")
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Output directory. Defaults to /tmp/cute-dsl-ncu-source-<timestamp>.",
    )
    parser.add_argument(
        "--top-k",
        type=int,
        default=10,
        help="How many hotspot rows to keep per summarized metric.",
    )
    parser.add_argument(
        "--print-source",
        default="sass",
        help="NCU source view. Common values are sass or cuda,sass.",
    )
    parser.add_argument(
        "--resolve-source-file",
        action="append",
        default=[],
        help="Additional source files or folders for NCU to resolve.",
    )
    parser.add_argument(
        "--import-source",
        action="store_true",
        help="Ask NCU to import correlated source files when available.",
    )
    parser.add_argument(
        "--source-folder",
        action="append",
        default=[],
        help="Additional source folders for NCU when --import-source is enabled.",
    )
    parser.add_argument(
        "--column",
        action="append",
        default=[],
        help="Metric columns to rank. Defaults to a curated hotspot set.",
    )
    parser.add_argument(
        "command",
        nargs=argparse.REMAINDER,
        help="Command to profile after `--`.",
    )
    return parser.parse_args()


def _clean_command(command: list[str]) -> list[str]:
    return [arg for arg in command if arg != "--"]


def _parse_float(value: str) -> float | None:
    text = value.strip()
    if not text or text == "-":
        return None
    try:
        return float(text.replace(",", ""))
    except ValueError:
        return None


def _extract_warnings(raw_output: str) -> list[str]:
    warnings: list[str] = []
    for line in raw_output.splitlines():
        if line.startswith("==WARNING=="):
            warnings.append(line.removeprefix("==WARNING==").strip())
    return warnings


def _extract_rows(raw_output: str) -> list[dict[str, str]]:
    lines = [line for line in raw_output.splitlines() if not line.startswith("==PROF==")]
    start = None
    for idx, line in enumerate(lines):
        if "Address" in line and "Source" in line:
            start = idx
            break
    if start is None:
        return []
    reader = csv.DictReader(lines[start:])
    rows: list[dict[str, str]] = []
    for row in reader:
        if not row:
            continue
        address = (row.get("Address") or "").strip()
        if not address or address == "Address":
            continue
        rows.append(row)
    return rows


def _top_rows(rows: list[dict[str, str]], column: str, top_k: int) -> list[dict[str, Any]]:
    scored: list[tuple[float, dict[str, str]]] = []
    for row in rows:
        value = _parse_float(row.get(column, ""))
        if value is None or value <= 0:
            continue
        scored.append((value, row))
    scored.sort(key=lambda item: item[0], reverse=True)
    result: list[dict[str, Any]] = []
    for value, row in scored[:top_k]:
        result.append(
            {
                "value": value,
                "address": row.get("Address", ""),
                "source": row.get("Source", "").strip(),
                "instruction_count": _parse_float(row.get("Instructions Executed", "")),
            }
        )
    return result


def build_summary(
    rows: list[dict[str, str]],
    columns: list[str],
    top_k: int,
    warnings: list[str],
) -> dict[str, Any]:
    available_columns = [column for column in columns if rows and column in rows[0]]
    top_by_column = {
        column: _top_rows(rows, column, top_k)
        for column in available_columns
    }
    source_correlation_available = bool(rows)
    return {
        "row_count": len(rows),
        "source_correlation_available": source_correlation_available,
        "warnings": warnings,
        "available_columns": list(rows[0].keys()) if rows else [],
        "top_by_column": top_by_column,
    }


def format_summary(summary: dict[str, Any]) -> str:
    lines = [f"Rows: {summary['row_count']}", ""]
    if summary.get("warnings"):
        lines.append("Warnings")
        for warning in summary["warnings"]:
            lines.append(f"- {warning}")
        lines.append("")
    if not summary.get("source_correlation_available", False):
        lines.append("Source correlation unavailable.")
        lines.append("Re-run with line info enabled, for example: CUTE_DSL_LINEINFO=1")
        lines.append("")
        return "\n".join(lines).rstrip() + "\n"
    for column, rows in summary["top_by_column"].items():
        lines.append(column)
        if not rows:
            lines.append("- no non-zero rows")
            lines.append("")
            continue
        for row in rows:
            inst = row["instruction_count"]
            inst_text = "N/A" if inst is None else f"{inst:.0f}"
            lines.append(
                f"- {row['value']:.2f} @ {row['address']} | inst={inst_text} | {row['source']}"
            )
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def main() -> None:
    args = parse_args()
    command = _clean_command(args.command)
    if not command:
        raise SystemExit("A command is required after `--`.")
    out_dir = args.out_dir
    if out_dir is None:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        out_dir = Path("/tmp") / f"cute-dsl-ncu-source-{args.label}-{stamp}"
    out_dir.mkdir(parents=True, exist_ok=True)

    cmd = [
        args.ncu,
        "--section",
        "SourceCounters",
        "--page",
        "source",
        "--csv",
        "--print-source",
        args.print_source,
        "--profile-from-start",
        "off",
    ]
    if args.import_source:
        cmd.extend(["--import-source", "yes"])
    if args.source_folder:
        cmd.extend(["--source-folders", ",".join(args.source_folder)])
    for path in args.resolve_source_file:
        cmd.extend(["--resolve-source-file", path])
    cmd.extend(command)

    proc = subprocess.run(cmd, capture_output=True, text=True)
    raw_output = (proc.stdout or "") + (proc.stderr or "")
    raw_path = out_dir / "source.csv"
    raw_path.write_text(raw_output, encoding="utf-8")
    if proc.returncode != 0:
        raise RuntimeError(f"NCU source counters failed:\n{raw_output}")

    warnings = _extract_warnings(raw_output)
    rows = _extract_rows(raw_output)
    summary = build_summary(rows, args.column or DEFAULT_TOP_COLUMNS, args.top_k, warnings)
    summary_payload = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "label": args.label,
        "command": command,
        "ncu_command": cmd,
        "summary": summary,
    }
    summary_json = out_dir / "summary.json"
    summary_json.write_text(
        json.dumps(summary_payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    summary_txt = out_dir / "summary.txt"
    summary_txt.write_text(format_summary(summary), encoding="utf-8")

    print(f"out_dir: {out_dir}")  # noqa: print
    print(f"source_csv: {raw_path}")  # noqa: print
    print(f"summary_json: {summary_json}")  # noqa: print
    print(f"summary_txt: {summary_txt}")  # noqa: print
    print(format_summary(summary))  # noqa: print

if __name__ == "__main__":
    main()
