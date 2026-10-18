#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
from pathlib import Path


SOURCE_PATTERN = re.compile(r"^(?P<kind>chapter|appendix)-(?P<label>[A-Za-z0-9-]+)\.tex$")
EXCLUDED_DIRS = {
    ".git",
    "__pycache__",
    "auto",
    "build",
    "dist",
    "figures",
    "pages",
    "plans",
    "scripts",
}


def natural_key(value: str) -> list[object]:
    parts = re.split(r"(\d+)", value.lower())
    key: list[object] = []
    for part in parts:
        if not part:
            continue
        key.append(int(part) if part.isdigit() else part)
    return key


def should_skip(path: Path, root: Path) -> bool:
    rel_parts = path.relative_to(root).parts[:-1]
    return any(part in EXCLUDED_DIRS for part in rel_parts)


def discover_sources(root: Path) -> list[Path]:
    matches: list[Path] = []
    for path in root.rglob("*.tex"):
        if should_skip(path, root):
            continue
        if SOURCE_PATTERN.match(path.name):
            matches.append(path)
    return sorted(
        matches,
        key=lambda path: (
            source_order(path),
            source_label(path),
            path.as_posix(),
        ),
    )


def source_kind(path: Path) -> str:
    match = SOURCE_PATTERN.match(path.name)
    if match:
        return match.group("kind")
    return "unknown"


def source_label(path: Path) -> list[object]:
    match = SOURCE_PATTERN.match(path.name)
    if match:
        return natural_key(match.group("label"))
    return natural_key(path.stem)


def source_order(path: Path) -> int:
    kind = source_kind(path)
    order = {
        "chapter": 1,
        "appendix": 2,
    }
    return order.get(kind, 99)


def main() -> int:
    parser = argparse.ArgumentParser(description="Discover chapter and appendix sources for a whole-book TTS run.")
    parser.add_argument("--root", default=".", help="Book workspace root.")
    parser.add_argument("--out", required=True, help="Output manifest CSV.")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    sources = discover_sources(root)
    out_path = Path(args.out)

    rows: list[dict[str, str]] = []
    for seq, source in enumerate(sources):
        stem = source.stem
        plans_dir = Path("plans") / stem
        script_dir = Path("scripts") / stem
        rows.append(
            {
                "seq": str(seq),
                "source_kind": source_kind(source),
                "source_stem": stem,
                "source_tex": str(source.relative_to(root)),
                "plans_dir": str(plans_dir),
                "script_dir": str(script_dir),
                "section_plan": str(plans_dir / "tts-sections.csv"),
                "checkpoint_ledger": str(plans_dir / "tts-checkpoints.csv"),
                "source_brief_file": str(plans_dir / "tts-source-brief.md"),
                "validation_file": str(plans_dir / "tts-validation.csv"),
                "acceptance_sample_file": str(plans_dir / "tts-acceptance-sample.csv"),
            }
        )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "seq",
                "source_kind",
                "source_stem",
                "source_tex",
                "plans_dir",
                "script_dir",
                "section_plan",
                "checkpoint_ledger",
                "source_brief_file",
                "validation_file",
                "acceptance_sample_file",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(rows)

    print(f"discovered {len(rows)} canonical sources")  # noqa: print
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
