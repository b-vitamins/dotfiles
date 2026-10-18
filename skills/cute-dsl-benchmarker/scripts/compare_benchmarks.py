#!/usr/bin/env python3
"""Compare one or more benchmark.json reports."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reports", nargs="+", type=Path, help="benchmark.json paths")
    parser.add_argument(
        "--metric",
        choices=("min_ms", "mean_ms", "median_ms"),
        default="mean_ms",
        help="Summary metric used for ranking and speedup.",
    )
    return parser.parse_args()


def load_report(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def _flatten_reports(path: Path, report: dict, metric: str) -> list[dict]:
    if "summary" in report:
        return [
            {
                "label": report.get("label", path.stem),
                "metric": report["summary"][metric],
                "path": path,
            }
        ]
    if "results" in report:
        rows: list[dict] = []
        for result in report["results"]:
            nested = result.get("report") or {}
            summary = nested.get("summary") or {}
            if metric not in summary:
                continue
            label = nested.get("label") or result.get("name") or path.stem
            rows.append(
                {
                    "label": label,
                    "metric": summary[metric],
                    "path": path,
                }
            )
        return rows
    raise KeyError(f"Unsupported benchmark report shape in {path}")


def main() -> None:
    args = parse_args()
    rows: list[dict] = []
    for path in args.reports:
        rows.extend(_flatten_reports(path, load_report(path), args.metric))
    if not rows:
        raise SystemExit("No comparable benchmark summaries found.")
    baseline = rows[0]["metric"]

    print("| label | metric_ms | speedup_vs_first | report |")  # noqa: print
    print("| --- | ---: | ---: | --- |")  # noqa: print
    for row in rows:
        metric = row["metric"]
        speedup = baseline / metric if metric else 0.0
        print(  # noqa: print
            f"| {row['label']} | {metric:.6f} | {speedup:.3f}x | {row['path']} |"
        )


if __name__ == "__main__":
    main()
