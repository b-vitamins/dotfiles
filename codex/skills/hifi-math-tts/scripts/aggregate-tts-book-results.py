#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from pathlib import Path


def load_csv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def yes_no(value: bool) -> str:
    return "yes" if value else "no"


def resolve(root: Path, raw_path: str) -> Path:
    path = Path(raw_path)
    return path if path.is_absolute() else root / path


def find_acceptance_review_file(acceptance_sample_path: Path) -> Path | None:
    plans_dir = acceptance_sample_path.parent
    candidates = [
        "tts-acceptance-review.csv",
        "tts-acceptance-review.md",
        "tts-line-by-line-audit.md",
        "tts-line-by-line-audit.csv",
        "tts-audit-report.md",
    ]
    for name in candidates:
        candidate = plans_dir / name
        if candidate.exists() and candidate.read_text(encoding="utf-8").strip():
            return candidate
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description="Aggregate whole-book TTS status from a book manifest.")
    parser.add_argument("--root", default=".", help="Book workspace root.")
    parser.add_argument("--manifest", required=True, help="Book manifest CSV.")
    parser.add_argument("--out", required=True, help="Output book status CSV.")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    manifest_rows = load_csv(resolve(root, args.manifest))
    out_rows: list[dict[str, str]] = []

    ready_count = 0
    for row in manifest_rows:
        plan_rows = load_csv(resolve(root, row["section_plan"]))
        validation_rows = load_csv(resolve(root, row["validation_file"]))
        acceptance_sample_path = resolve(root, row["acceptance_sample_file"])
        acceptance_rows = load_csv(acceptance_sample_path)
        acceptance_review_path = find_acceptance_review_file(acceptance_sample_path)
        source_brief_path = resolve(root, row["source_brief_file"])

        source_covered = bool(plan_rows) and all(
            resolve(root, plan_row["target_txt"]).exists()
            and resolve(root, plan_row["target_txt"]).read_text(encoding="utf-8").strip()
            for plan_row in plan_rows
        )
        validated = bool(validation_rows) and all(validation_row.get("status") in {"ok", "warn"} for validation_row in validation_rows)
        source_briefed = source_brief_path.exists() and bool(source_brief_path.read_text(encoding="utf-8").strip())
        style_audited = bool(acceptance_rows) and acceptance_review_path is not None
        ready_for_tts = source_covered and validated and source_briefed and style_audited
        if ready_for_tts:
            ready_count += 1

        validation_failures = sum(1 for validation_row in validation_rows if validation_row.get("status") == "fail")
        validation_warnings = sum(1 for validation_row in validation_rows if validation_row.get("status") == "warn")

        out_rows.append(
            {
                "seq": row["seq"],
                "source_stem": row["source_stem"],
                "source_tex": row["source_tex"],
                "section_count": str(len(plan_rows)),
                "source-covered": yes_no(source_covered),
                "validated": yes_no(validated),
                "source-briefed": yes_no(source_briefed),
                "style-audited": yes_no(style_audited),
                "ready-for-tts": yes_no(ready_for_tts),
                "validation_failures": str(validation_failures),
                "validation_warnings": str(validation_warnings),
                "validation_file": row["validation_file"],
                "source_brief_file": row["source_brief_file"],
                "acceptance_sample_file": row["acceptance_sample_file"],
                "acceptance_review_file": (
                    str(acceptance_review_path.relative_to(root)) if acceptance_review_path else ""
                ),
            }
        )

    out_path = resolve(root, args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "seq",
                "source_stem",
                "source_tex",
                "section_count",
                "source-covered",
                "validated",
                "source-briefed",
                "style-audited",
                "ready-for-tts",
                "validation_failures",
                "validation_warnings",
                "validation_file",
                "source_brief_file",
                "acceptance_sample_file",
                "acceptance_review_file",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(out_rows)

    print(f"aggregated {len(out_rows)} sources: {ready_count} ready-for-tts")  # noqa: print
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
