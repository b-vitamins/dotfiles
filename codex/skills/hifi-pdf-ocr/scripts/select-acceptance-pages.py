#!/usr/bin/env python3
"""Select a deterministic acceptance sample from a page audit ledger."""

from __future__ import annotations

import argparse
import csv
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class AuditRow:
    page: int
    status: str
    target_tex: str
    notes: str


def load_rows(path: Path) -> list[AuditRow]:
    with path.open("r", newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        required = {"page", "status", "target_tex"}
        missing = required - set(reader.fieldnames or [])
        if missing:
            raise SystemExit(f"Missing required columns in {path}: {', '.join(sorted(missing))}")

        rows: list[AuditRow] = []
        for row_num, row in enumerate(reader, start=2):
            page_text = (row.get("page") or "").strip()
            status = (row.get("status") or "").strip()
            target_tex = (row.get("target_tex") or "").strip()
            if not page_text or not status or not target_tex:
                raise SystemExit(f"{path}:{row_num}: missing page/status/target_tex")
            rows.append(
                AuditRow(
                    page=int(page_text),
                    status=status,
                    target_tex=target_tex,
                    notes=(row.get("notes") or "").strip(),
                )
            )

    if not rows:
        raise SystemExit(f"No audit rows found in {path}")
    return sorted(rows, key=lambda row: row.page)


def pick_evenly(rows: list[AuditRow], count: int) -> list[AuditRow]:
    if count <= 0 or not rows:
        return []
    if count >= len(rows):
        return rows[:]
    if count == 1:
        return [rows[len(rows) // 2]]

    picks: list[AuditRow] = []
    seen_pages: set[int] = set()
    last_index = len(rows) - 1
    for i in range(count):
        index = round(i * last_index / (count - 1))
        row = rows[index]
        if row.page not in seen_pages:
            picks.append(row)
            seen_pages.add(row.page)

    if len(picks) < count:
        for row in rows:
            if row.page in seen_pages:
                continue
            picks.append(row)
            seen_pages.add(row.page)
            if len(picks) == count:
                break

    return sorted(picks, key=lambda row: row.page)


def choose_sample(rows: list[AuditRow], total: int, min_per_status: int) -> list[tuple[AuditRow, str]]:
    by_status: dict[str, list[AuditRow]] = {}
    for row in rows:
        by_status.setdefault(row.status, []).append(row)

    selected: dict[int, tuple[AuditRow, str]] = {}
    priority_statuses = [status for status in ("fixed", "verified") if status in by_status]

    for status in priority_statuses:
        picks = pick_evenly(by_status[status], min(min_per_status, len(by_status[status])))
        for row in picks:
            selected[row.page] = (row, f"status:{status}")

    if len(selected) < total:
        remaining = [row for row in rows if row.page not in selected]
        needed = total - len(selected)
        for row in pick_evenly(remaining, needed):
            selected[row.page] = (row, "spread-fill")

    sample = sorted(selected.values(), key=lambda item: item[0].page)
    return sample[:total]


def write_csv(path: Path, rows: list[tuple[AuditRow, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["page", "status", "target_tex", "notes", "sample_reason"])
        for row, reason in rows:
            writer.writerow([row.page, row.status, row.target_tex, row.notes, reason])


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--audit-csv", type=Path, required=True, help="Path to plans/page-audit.csv")
    parser.add_argument("--count", type=int, default=12, help="Total sample size.")
    parser.add_argument(
        "--min-per-status",
        type=int,
        default=3,
        help="Minimum sample count for each of fixed and verified when present.",
    )
    parser.add_argument("--out", type=Path, help="Optional CSV output path.")
    args = parser.parse_args()

    if args.count < 1:
        raise SystemExit("--count must be >= 1")
    if args.min_per_status < 0:
        raise SystemExit("--min-per-status must be >= 0")

    rows = load_rows(args.audit_csv)
    sample = choose_sample(rows, args.count, args.min_per_status)

    if args.out:
        write_csv(args.out, sample)

    print("page,status,target_tex,sample_reason")  # noqa: print
    for row, reason in sample:
        print(f"{row.page},{row.status},{row.target_tex},{reason}")  # noqa: print

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
