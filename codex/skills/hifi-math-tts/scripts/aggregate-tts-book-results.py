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
        lexicon_audit_rows = load_csv(resolve(root, row["lexicon_audit_file"]))
        acceptance_rows = load_csv(resolve(root, row["acceptance_sample_file"]))
        lexicon_audit_path = resolve(root, row["lexicon_audit_file"])

        source_covered = bool(plan_rows) and all(
            resolve(root, plan_row["target_txt"]).exists()
            and resolve(root, plan_row["target_txt"]).read_text(encoding="utf-8").strip()
            for plan_row in plan_rows
        )
        validated = bool(validation_rows) and all(validation_row.get("status") == "ok" for validation_row in validation_rows)
        lexicon_audited = lexicon_audit_path.exists() and all(audit_row.get("covered") == "yes" for audit_row in lexicon_audit_rows)
        style_audited = bool(acceptance_rows)
        ready_for_tts = source_covered and validated and lexicon_audited and style_audited
        if ready_for_tts:
            ready_count += 1

        validation_failures = sum(1 for validation_row in validation_rows if validation_row.get("status") == "fail")
        validation_warnings = sum(1 for validation_row in validation_rows if validation_row.get("status") == "warn")
        uncovered_notation = sum(1 for audit_row in lexicon_audit_rows if audit_row.get("covered") != "yes")

        out_rows.append(
            {
                "seq": row["seq"],
                "source_stem": row["source_stem"],
                "source_tex": row["source_tex"],
                "section_count": str(len(plan_rows)),
                "source-covered": yes_no(source_covered),
                "validated": yes_no(validated),
                "lexicon-audited": yes_no(lexicon_audited),
                "style-audited": yes_no(style_audited),
                "ready-for-tts": yes_no(ready_for_tts),
                "validation_failures": str(validation_failures),
                "validation_warnings": str(validation_warnings),
                "uncovered_notation": str(uncovered_notation),
                "validation_file": row["validation_file"],
                "lexicon_audit_file": row["lexicon_audit_file"],
                "acceptance_sample_file": row["acceptance_sample_file"],
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
                "lexicon-audited",
                "style-audited",
                "ready-for-tts",
                "validation_failures",
                "validation_warnings",
                "uncovered_notation",
                "validation_file",
                "lexicon_audit_file",
                "acceptance_sample_file",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(out_rows)

    print(f"aggregated {len(out_rows)} sources: {ready_count} ready-for-tts")  # noqa: print
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
