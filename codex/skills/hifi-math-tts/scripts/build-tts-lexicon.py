#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from pathlib import Path

from tts_notation import extract_notation_candidates


def add_row(rows: list[dict[str, str]], seen: set[str], source_form: str, canonical_spoken: str, forbidden_forms: str, notes: str) -> None:
    if source_form in seen:
        return
    seen.add(source_form)
    rows.append(
        {
            "source_form": source_form,
            "canonical_spoken": canonical_spoken,
            "forbidden_forms": forbidden_forms,
            "notes": notes,
        }
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Build a chapter-wide spoken notation lexicon from LaTeX source.")
    parser.add_argument("--tex", required=True, help="Canonical source .tex file.")
    parser.add_argument("--out", required=True, help="Output CSV path.")
    args = parser.parse_args()

    tex_path = Path(args.tex)
    out_path = Path(args.out)
    text = tex_path.read_text(encoding="utf-8")

    rows: list[dict[str, str]] = []
    seen: set[str] = set()
    for candidate in extract_notation_candidates(text):
        add_row(rows, seen, candidate.source_form, candidate.canonical_spoken, candidate.forbidden_forms, candidate.notes)

    rows.sort(key=lambda row: row["source_form"])

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["source_form", "canonical_spoken", "forbidden_forms", "notes"], lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)

    print(f"wrote {len(rows)} lexicon entries to {out_path}")  # noqa: print
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
