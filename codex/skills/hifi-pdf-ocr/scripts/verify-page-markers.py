#!/usr/bin/env python3
"""Verify exact `% Page <n>` coverage before marker cleanup."""

from __future__ import annotations

import argparse
import csv
import json
import re
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path


PAGE_MARKER_RE = re.compile(r"^% Page (\d+)\s*$")


@dataclass(frozen=True)
class Spec:
    path: Path
    expected_pages: tuple[int, ...]
    source: str


def extract_markers(path: Path) -> list[int]:
    markers: list[int] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        match = PAGE_MARKER_RE.match(line)
        if match:
            markers.append(int(match.group(1)))
    return markers


def build_single_spec(path: Path, start: int, end: int) -> list[Spec]:
    return [Spec(path=path, expected_pages=tuple(range(start, end + 1)), source=f"{path}:{start}-{end}")]


def build_specs_from_csv(path: Path, base_dir: Path) -> list[Spec]:
    with path.open("r", newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        required = {"start", "end", "target_tex"}
        missing = required - set(reader.fieldnames or [])
        if missing:
            raise SystemExit(f"Missing required columns in {path}: {', '.join(sorted(missing))}")

        expected: dict[Path, set[int]] = defaultdict(set)
        row_sources: dict[Path, list[str]] = defaultdict(list)
        for row_num, row in enumerate(reader, start=2):
            target = (row.get("target_tex") or "").strip()
            if not target:
                raise SystemExit(f"{path}:{row_num}: missing target_tex")
            start = int((row.get("start") or "").strip())
            end = int((row.get("end") or "").strip())
            if start < 1 or end < start:
                raise SystemExit(f"{path}:{row_num}: invalid range {start}-{end}")

            target_path = (base_dir / target).resolve()
            for page in range(start, end + 1):
                if page in expected[target_path]:
                    raise SystemExit(f"{path}:{row_num}: duplicate expected page {page} for {target}")
                expected[target_path].add(page)
            row_sources[target_path].append(f"{target}:{start}-{end}")

    return [
        Spec(path=target, expected_pages=tuple(sorted(pages)), source=" | ".join(row_sources[target]))
        for target, pages in sorted(expected.items(), key=lambda item: str(item[0]))
    ]


def summarize(spec: Spec) -> tuple[bool, str, dict[str, object]]:
    if not spec.path.exists():
        payload = {
            "path": str(spec.path),
            "source": spec.source,
            "ok": False,
            "missing_file": True,
        }
        return False, f"{spec.path}: missing file (expected from {spec.source})", payload

    actual = extract_markers(spec.path)
    expected = list(spec.expected_pages)
    actual_counter = Counter(actual)
    duplicates = sorted(page for page, count in actual_counter.items() if count > 1)
    actual_set = set(actual)
    expected_set = set(expected)
    missing = sorted(expected_set - actual_set)
    extra = sorted(actual_set - expected_set)
    ordered = actual == sorted(actual)

    ok = not duplicates and not missing and not extra and ordered and actual == expected
    details = [
        f"expected={expected[0]}-{expected[-1]}" if expected else "expected=<empty>",
        f"markers={len(actual)}",
        f"duplicates={duplicates or '-'}",
        f"missing={missing or '-'}",
        f"extra={extra or '-'}",
        f"ordered={'yes' if ordered else 'no'}",
    ]
    status = "OK" if ok else "FAIL"
    payload = {
        "path": str(spec.path),
        "source": spec.source,
        "ok": ok,
        "missing_file": False,
        "expected_pages": expected,
        "actual_markers": actual,
        "duplicates": duplicates,
        "missing": missing,
        "extra": extra,
        "ordered": ordered,
    }
    return ok, f"{status} {spec.path}: " + ", ".join(details), payload


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--csv", type=Path, help="CSV with start,end,target_tex columns.")
    parser.add_argument("--base-dir", type=Path, default=Path.cwd(), help="Base directory for resolving target_tex.")
    parser.add_argument("--file", type=Path, help="Single file to validate.")
    parser.add_argument("--start", type=int, help="Expected first page for --file mode.")
    parser.add_argument("--end", type=int, help="Expected last page for --file mode.")
    parser.add_argument("--report", type=Path, help="Optional JSON report output path.")
    args = parser.parse_args()

    if args.csv and args.file:
        raise SystemExit("Use either --csv or --file/--start/--end, not both.")

    specs: list[Spec]
    if args.csv:
        specs = build_specs_from_csv(args.csv, args.base_dir)
    else:
        if not args.file or args.start is None or args.end is None:
            raise SystemExit("Single-file mode requires --file, --start, and --end.")
        if args.start < 1 or args.end < args.start:
            raise SystemExit("Invalid --start/--end range.")
        specs = build_single_spec(args.file.resolve(), args.start, args.end)

    failed = False
    report_specs: list[dict[str, object]] = []
    for spec in specs:
        ok, message, payload = summarize(spec)
        print(message)  # noqa: print
        failed = failed or not ok
        report_specs.append(payload)

    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(
            json.dumps(
                {
                    "ok": not failed,
                    "mode": "csv" if args.csv else "single-file",
                    "source_csv": str(args.csv) if args.csv else None,
                    "specs": report_specs,
                },
                indent=2,
                sort_keys=True,
            )
            + "\n",
            encoding="utf-8",
        )

    return 2 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
