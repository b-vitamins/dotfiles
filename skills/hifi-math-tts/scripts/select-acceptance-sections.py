#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from collections import Counter, defaultdict
from pathlib import Path


def load_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def recommended_sample_count(total_sections: int, total_equations: int) -> int:
    if total_sections <= 10:
        return total_sections
    count = 6
    if total_sections >= 12 or total_equations >= 40:
        count += 2
    if total_sections >= 18 or total_equations >= 80:
        count += 2
    if total_equations >= 150:
        count += 2
    return min(count, 12)


def main() -> int:
    parser = argparse.ArgumentParser(description="Select a deterministic acceptance sample for TTS scripts.")
    parser.add_argument("--plan", required=True, help="Section plan CSV.")
    parser.add_argument("--checkpoints", required=True, help="Checkpoint CSV.")
    parser.add_argument("--out", required=True, help="Output acceptance sample CSV.")
    parser.add_argument("--count", type=int, default=0, help="Target sample size. Use 0 for automatic sizing.")
    args = parser.parse_args()

    plan_rows = load_csv(Path(args.plan))
    checkpoint_rows = load_csv(Path(args.checkpoints))
    checkpoint_counts: dict[str, Counter[str]] = defaultdict(Counter)
    for row in checkpoint_rows:
        checkpoint_counts[row["section_seq"]][row["kind"]] += 1
        if row["kind"] == "equation" and row.get("notes", "").startswith("tag:"):
            checkpoint_counts[row["section_seq"]]["tagged-equation"] += 1

    enriched = []
    for row in plan_rows:
        counts = checkpoint_counts.get(row["seq"], Counter())
        weight = (
            1
            + 2 * counts.get("equation", 0)
            + 2 * counts.get("figure-caption", 0)
            + 2 * counts.get("algorithm", 0)
            + counts.get("exercise", 0)
            + counts.get("prose-anchor", 0)
            + 3 * counts.get("tagged-equation", 0)
        )
        enriched.append(
            {
                **row,
                "weight": weight,
                "equation_count": counts.get("equation", 0),
                "figure_count": counts.get("figure-caption", 0),
                "algorithm_count": counts.get("algorithm", 0),
                "exercise_count": counts.get("exercise", 0),
                "prose_anchor_count": counts.get("prose-anchor", 0),
                "tagged_equation_count": counts.get("tagged-equation", 0),
            }
        )

    total_equations = sum(int(row["equation_count"]) for row in enriched)
    target_count = args.count if args.count > 0 else recommended_sample_count(len(enriched), total_equations)

    picks: list[dict[str, str | int]] = []
    seen = set()

    def add(row: dict[str, str | int], reason: str) -> None:
        key = row["seq"]
        if key in seen:
            return
        seen.add(key)
        picks.append({**row, "reason": reason})

    if enriched:
        add(enriched[0], "first-section")
        add(enriched[len(enriched) // 2], "midpoint-section")
        add(enriched[-1], "last-section")
        add(max(enriched, key=lambda row: row["equation_count"]), "most-equations")
        add(max(enriched, key=lambda row: row["tagged_equation_count"]), "most-tagged-equations")
        add(max(enriched, key=lambda row: row["figure_count"]), "most-figure-captions")
        add(max(enriched, key=lambda row: row["algorithm_count"]), "most-algorithms")
        add(max(enriched, key=lambda row: row["exercise_count"]), "most-exercises")
        add(max(enriched, key=lambda row: row["prose_anchor_count"]), "most-prose-anchors")
        add(max(enriched, key=lambda row: row["weight"]), "highest-checkpoint-weight")

    for row in sorted(enriched, key=lambda row: (-row["weight"], int(row["seq"]))):
        if len(picks) >= target_count:
            break
        add(row, "high-weight-fill")

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "seq",
                "title",
                "target_txt",
                "reason",
                "weight",
                "equation_count",
                "figure_count",
                "algorithm_count",
                "exercise_count",
                "prose_anchor_count",
                "tagged_equation_count",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        for row in picks[: target_count]:
            writer.writerow(
                {
                    "seq": row["seq"],
                    "title": row["title"],
                    "target_txt": row["target_txt"],
                    "reason": row["reason"],
                    "weight": row["weight"],
                    "equation_count": row["equation_count"],
                    "figure_count": row["figure_count"],
                    "algorithm_count": row["algorithm_count"],
                    "exercise_count": row["exercise_count"],
                    "prose_anchor_count": row["prose_anchor_count"],
                    "tagged_equation_count": row["tagged_equation_count"],
                }
            )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
