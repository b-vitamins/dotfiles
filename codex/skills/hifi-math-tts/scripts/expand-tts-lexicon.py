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


def main() -> int:
    parser = argparse.ArgumentParser(description="Expand a TTS lexicon with uncovered candidates from a lexicon audit.")
    parser.add_argument("--lexicon", required=True, help="Existing lexicon CSV.")
    parser.add_argument("--audit", required=True, help="Lexicon audit CSV.")
    parser.add_argument("--out", required=True, help="Output lexicon CSV.")
    args = parser.parse_args()

    lexicon_rows = load_csv(Path(args.lexicon))
    audit_rows = load_csv(Path(args.audit))

    seen = {row["source_form"] for row in lexicon_rows}
    additions = 0
    for row in audit_rows:
        if row.get("covered") == "yes":
            continue
        source_form = row.get("source_form", "").strip()
        canonical_spoken = row.get("canonical_spoken", "").strip()
        if not source_form or not canonical_spoken or source_form in seen:
            continue
        lexicon_rows.append(
            {
                "source_form": source_form,
                "canonical_spoken": canonical_spoken,
                "forbidden_forms": row.get("forbidden_forms", "").strip(),
                "notes": row.get("notes", "").strip(),
            }
        )
        seen.add(source_form)
        additions += 1

    lexicon_rows.sort(key=lambda row: row["source_form"])

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["source_form", "canonical_spoken", "forbidden_forms", "notes"], lineterminator="\n")
        writer.writeheader()
        writer.writerows(lexicon_rows)

    print(f"expanded lexicon with {additions} new entries")  # noqa: print
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
