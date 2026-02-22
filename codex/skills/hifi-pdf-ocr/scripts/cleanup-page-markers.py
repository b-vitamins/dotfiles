#!/usr/bin/env python3
"""Remove `% Page <n>` markers and stitch likely page-break interruptions."""

from __future__ import annotations

import argparse
import glob
import re
from dataclasses import dataclass
from pathlib import Path


PAGE_MARKER_RE = re.compile(r"^% Page \d+\s*$")


@dataclass
class CleanupResult:
    path: Path
    removed_markers: int = 0
    stitched_boundaries: int = 0
    changed: bool = False


def likely_stitch(prev_line: str, next_line: str) -> bool:
    """Heuristic for joining text split only by a page boundary marker."""
    prev = prev_line.strip()
    nxt = next_line.strip()
    if not prev or not nxt:
        return False
    if prev.startswith("%"):
        return False

    # Don't stitch into section/chapter headers.
    if re.match(r"^\\(chapter|section|subsection|subsubsection|paragraph)\*?\{", nxt):
        return False
    if re.match(r"^\\(chapter|section|subsection|subsubsection|paragraph)\*?\{", prev):
        return False

    # Avoid document boundary joins.
    if re.match(r"^\\(begin|end)\{document\}", prev):
        return False
    if re.match(r"^\\(begin|end)\{document\}", nxt):
        return False

    # Don't stitch if previous line already ends a sentence.
    if re.search(r'[.!?]["\)\]]*$', prev):
        return False

    # Keep colon->environment formatting intact.
    if prev.endswith(":") and re.match(r"^\\begin\{(equation|align|itemize|enumerate|figure|table)", nxt):
        return False

    if re.match(r"^(?:[a-z0-9\(\[]|\\\(|\\textit|\\textbf|\\item\b|\\begin\{(?:itemize|enumerate|description)\})", nxt):
        return True
    if re.search(r"[,;:-]$", prev):
        return True
    if re.search(r"[A-Za-z0-9\)\}]$", prev) and re.match(r"^[a-z]", nxt):
        return True
    if re.match(r"^\\begin\{(?:itemize|enumerate|description)\}", prev) and re.match(r"^\\item\b", nxt):
        return True
    return False


def cleanup_text(lines: list[str], stitch: bool) -> tuple[list[str], int, int]:
    out: list[str] = []
    removed = 0
    stitched = 0
    suppress_blanks = False

    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        if PAGE_MARKER_RE.match(line):
            removed += 1

            p = len(out) - 1
            while p >= 0 and out[p].strip() == "":
                p -= 1
            prev = out[p] if p >= 0 else ""

            j = i + 1
            while j < n and (lines[j].strip() == "" or PAGE_MARKER_RE.match(lines[j])):
                j += 1
            nxt = lines[j] if j < n else ""

            if stitch and likely_stitch(prev, nxt):
                stitched += 1
                while out and out[-1].strip() == "":
                    out.pop()
                suppress_blanks = True

            i += 1
            continue

        if suppress_blanks and line.strip() == "":
            i += 1
            continue

        out.append(line)
        if line.strip():
            suppress_blanks = False
        i += 1

    while out and out[-1].strip() == "":
        out.pop()
    return out, removed, stitched


def expand_patterns(patterns: list[str]) -> list[Path]:
    out: list[Path] = []
    seen: set[Path] = set()
    for pattern in patterns:
        matches = sorted(glob.glob(pattern))
        for m in matches:
            p = Path(m)
            if p.is_file() and p not in seen:
                seen.add(p)
                out.append(p)
    return out


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "paths",
        nargs="*",
        default=["chapter-*.tex"],
        help="File paths or glob patterns (default: chapter-*.tex).",
    )
    parser.add_argument("--no-stitch", action="store_true", help="Remove page markers but do not stitch boundaries.")
    parser.add_argument("--check", action="store_true", help="Check mode: do not write files. Exit nonzero if markers exist.")
    args = parser.parse_args()

    files = expand_patterns(args.paths)
    if not files:
        print("No files matched.")  # noqa: print
        return 1

    total_removed = 0
    total_stitched = 0
    changed_files = 0

    for path in files:
        original = path.read_text(encoding="utf-8").splitlines()
        cleaned, removed, stitched = cleanup_text(original, stitch=not args.no_stitch)

        result = CleanupResult(path=path, removed_markers=removed, stitched_boundaries=stitched)
        result.changed = removed > 0 and cleaned != original

        total_removed += result.removed_markers
        total_stitched += result.stitched_boundaries

        if result.changed and not args.check:
            path.write_text("\n".join(cleaned) + "\n", encoding="utf-8")
            changed_files += 1

        print(  # noqa: print
            f"{path}: removed={result.removed_markers}, "
            f"stitched={result.stitched_boundaries}, changed={'yes' if result.changed else 'no'}"
        )

    print(  # noqa: print
        f"TOTAL files={len(files)}, changed={changed_files}, "
        f"removed={total_removed}, stitched={total_stitched}"
    )

    if args.check and total_removed > 0:
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
