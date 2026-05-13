#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from collections import Counter
from pathlib import Path

from tts_notation import NotationCandidate, extract_notation_candidates


def load_lexicon(path: Path) -> dict[str, dict[str, str]]:
    if not path.exists():
        return {}
    with path.open("r", encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle))
    return {row["source_form"]: row for row in rows}


def main() -> int:
    parser = argparse.ArgumentParser(description="Audit notation coverage of a TTS lexicon against a LaTeX source.")
    parser.add_argument("--tex", required=True, help="Canonical source .tex file.")
    parser.add_argument("--lexicon", required=True, help="Lexicon CSV.")
    parser.add_argument("--out", required=True, help="Output coverage CSV.")
    args = parser.parse_args()

    tex_path = Path(args.tex)
    lexicon_path = Path(args.lexicon)
    out_path = Path(args.out)
    text = tex_path.read_text(encoding="utf-8")
    lexicon = load_lexicon(lexicon_path)

    candidate_rows = extract_notation_candidates(text)
    counts: Counter[tuple[str, str]] = Counter()
    candidate_map: dict[tuple[str, str], NotationCandidate] = {}
    for candidate in candidate_rows:
        key = (candidate.candidate_type, candidate.source_form)
        counts[key] += 1
        candidate_map.setdefault(key, candidate)

    out_rows: list[dict[str, str]] = []
    uncovered = 0
    for (candidate_type, source_form), count in sorted(counts.items(), key=lambda item: (-item[1], item[0][1])):
        row = lexicon.get(source_form)
        candidate = candidate_map[(candidate_type, source_form)]
        covered = "yes" if row else "no"
        if not row:
            uncovered += 1
        out_rows.append(
            {
                "candidate_type": candidate_type,
                "source_form": source_form,
                "count": str(count),
                "covered": covered,
                "canonical_spoken": row["canonical_spoken"] if row else candidate.canonical_spoken,
                "forbidden_forms": row["forbidden_forms"] if row else candidate.forbidden_forms,
                "notes": row["notes"] if row else candidate.notes,
            }
        )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["candidate_type", "source_form", "count", "covered", "canonical_spoken", "forbidden_forms", "notes"],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(out_rows)

    print(f"audited {len(out_rows)} notation candidates: {len(out_rows) - uncovered} covered, {uncovered} uncovered")  # noqa: print
    return 1 if uncovered else 0


if __name__ == "__main__":
    raise SystemExit(main())
