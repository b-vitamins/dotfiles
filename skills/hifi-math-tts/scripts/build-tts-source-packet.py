#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
from pathlib import Path


DISPLAY_BEGIN_PATTERN = re.compile(r"\\begin\{(equation|align|gather|multline|flalign|alignat|eqnarray)\*?\}|\\\[")
DISPLAY_END_PATTERN = re.compile(r"\\end\{(equation|align|gather|multline|flalign|alignat|eqnarray)\*?\}|\\\]")
CALLOUT_PATTERN = re.compile(r"\\(?:textit|emph)\{((?:Exercise|Problem|Section|Chapter|Appendix)\s+[^{}]{0,80})\}")
TAG_OR_LABEL_PATTERN = re.compile(r"\\(?:tag|label)\{[^{}]+\}")


def load_csv(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def normalize_ws(text: str) -> str:
    return " ".join(text.split())


def display_samples(lines: list[str], limit: int) -> list[tuple[int, int]]:
    samples: list[tuple[int, int]] = []
    idx = 1
    while idx <= len(lines):
        if not DISPLAY_BEGIN_PATTERN.search(lines[idx - 1]):
            idx += 1
            continue
        start = idx
        end = idx
        while end <= len(lines) and not DISPLAY_END_PATTERN.search(lines[end - 1]):
            end += 1
        end = min(end, len(lines))
        body = "\n".join(lines[start - 1 : end])
        if TAG_OR_LABEL_PATTERN.search(body) or len(body) > 180 or "\\\\" in body:
            samples.append((start, end))
        if len(samples) >= limit:
            break
        idx = end + 1
    return samples


def environment_counts(text: str) -> list[tuple[str, int]]:
    names = re.findall(r"\\begin\{([A-Za-z*]+)\}", text)
    counts: dict[str, int] = {}
    for name in names:
        counts[name] = counts.get(name, 0) + 1
    return sorted(counts.items(), key=lambda item: (-item[1], item[0]))


def main() -> int:
    parser = argparse.ArgumentParser(description="Build a source evidence packet for model-led TTS briefing.")
    parser.add_argument("--tex", required=True, help="Canonical source .tex file.")
    parser.add_argument("--plan", required=True, help="Section plan CSV.")
    parser.add_argument("--out", required=True, help="Output Markdown packet.")
    parser.add_argument("--max-equation-contexts", type=int, default=16)
    args = parser.parse_args()

    tex_path = Path(args.tex)
    plan_path = Path(args.plan)
    out_path = Path(args.out)

    text = tex_path.read_text(encoding="utf-8")
    lines = text.splitlines()
    plan_rows = load_csv(plan_path)

    parts: list[str] = []
    parts.append("# TTS Source Evidence Packet")
    parts.append("")
    parts.append(f"- Source: `{tex_path}`")
    parts.append(f"- Lines: {len(lines)}")
    parts.append(f"- Section rows: {len(plan_rows)}")
    parts.append("")

    parts.append("## Section Outline")
    for row in plan_rows:
        label = row.get("spoken_label") or row.get("number") or row.get("kind", "")
        title = row.get("title", "")
        parts.append(f"- `{row.get('seq', '')}` lines {row.get('start_line', '')}-{row.get('end_line', '')}: {label} {title}".strip())
    parts.append("")

    parts.append("## Environment Inventory")
    counts = environment_counts(text)
    if counts:
        for name, count in counts[:40]:
            parts.append(f"- `{name}`: {count}")
    else:
        parts.append("- No LaTeX environments detected.")
    parts.append("")

    parts.append("## Possible Layout-Only Callouts")
    callouts = []
    for line_no, line in enumerate(lines, start=1):
        for match in CALLOUT_PATTERN.finditer(line):
            callouts.append((line_no, normalize_ws(match.group(1))))
    if callouts:
        for line_no, callout in callouts[:60]:
            parts.append(f"- line {line_no}: {callout}")
    else:
        parts.append("- None detected.")
    parts.append("")

    parts.append("## Representative Display Math")
    samples = display_samples(lines, args.max_equation_contexts)
    if samples:
        for start, end in samples:
            parts.append(f"### Lines {start}-{end}")
            body = "\n".join(f"{idx}: {lines[idx - 1]}" for idx in range(start, end + 1))
            parts.append(f"```tex\n{body}\n```")
    else:
        parts.append("- No representative display math detected.")
    parts.append("")

    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text("\n".join(parts).rstrip() + "\n", encoding="utf-8")
    print(f"wrote source packet to {out_path}")  # noqa: print
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
