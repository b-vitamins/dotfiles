#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
from dataclasses import dataclass
from pathlib import Path


HEADING_PATTERNS = [
    ("section", re.compile(r"^\s*\\section(\*?)\{(.+?)\}(?:\s*\\label\{[^{}]*\})*\s*(?:%.*)?$")),
    ("subsection", re.compile(r"^\s*\\subsection(\*?)\{(.+?)\}(?:\s*\\label\{[^{}]*\})*\s*(?:%.*)?$")),
    ("subsubsection", re.compile(r"^\s*\\subsubsection(\*?)\{(.+?)\}(?:\s*\\label\{[^{}]*\})*\s*(?:%.*)?$")),
]

CHAPTER_PATTERN = re.compile(r"^\s*\\chapter(\*?)\{(.+?)\}(?:\s*\\label\{[^{}]*\})*\s*(?:%.*)?$")
FILENAME_CHAPTER_PATTERN = re.compile(r"(chapter|appendix)-([A-Za-z0-9]+)")
TAG_PATTERN = re.compile(r"\\tag\{([A-Za-z0-9.]+)\}")
END_DOCUMENT_PATTERN = re.compile(r"^\s*\\end\{document\}\s*$")
BEGIN_DOCUMENT_PATTERN = re.compile(r"^\s*\\begin\{document\}\s*$")
COMMENT_PATTERN = re.compile(r"^\s*%")
PREAMBLE_ONLY_PATTERN = re.compile(r"^\s*\\(?:documentclass|usepackage|input|numberwithin|counterwithin|setcounter)\b")
LAYOUT_ONLY_PATTERN = re.compile(
    r"^\s*\\(?:thispagestyle|clearpage|newpage|null|vfill|bigskip|smallskip|medskip|noindent|frontmatter|mainmatter|backmatter)\b"
)
ENVIRONMENT_ONLY_PATTERN = re.compile(r"^\s*\\(?:begin|end)\{(?:center|minipage|titlepage)\}")
OUT_OF_SCOPE_TRAILING_TITLE_PATTERN = re.compile(
    r"^(?:"
    r"problems?(?:\s+for\b.*)?"
    r"|exercises?(?:\s+for\b.*)?"
    r"|selected\s+problems?"
    r"|answers?(?:\s+(?:to\s+)?(?:selected\s+)?(?:problems?|exercises?))?"
    r"|solutions?(?:\s+(?:to\s+)?(?:selected\s+)?(?:problems?|exercises?))?"
    r"|bibliography"
    r"|references"
    r"|index"
    r")$",
    re.IGNORECASE,
)


@dataclass
class Heading:
    line_no: int
    kind: str
    starred: bool
    title: str
    number: str


def infer_chapter_label(tex_path: Path, lines: list[str], override: str | None) -> str:
    if override:
        return normalize_chapter_label(override)
    match = FILENAME_CHAPTER_PATTERN.search(tex_path.stem)
    if match:
        return normalize_chapter_label(match.group(2))
    for line in lines:
        tag_match = TAG_PATTERN.search(line)
        if tag_match:
            identifier = tag_match.group(1)
            if "." in identifier:
                return normalize_chapter_label(identifier.split(".", 1)[0])
    return tex_path.stem


def normalize_chapter_label(label: str) -> str:
    value = label.strip()
    if value.isdigit():
        return str(int(value))
    return value.upper()


def detect_chapter_title(lines: list[str]) -> tuple[int, str]:
    for idx, line in enumerate(lines, start=1):
        match = CHAPTER_PATTERN.match(line)
        if match:
            return idx, match.group(2).strip()
    return 1, ""


def first_document_body_line(lines: list[str]) -> int:
    for idx, line in enumerate(lines, start=1):
        if BEGIN_DOCUMENT_PATTERN.match(line):
            return idx + 1
    return 1


def is_preface_noise(line: str) -> bool:
    stripped = line.strip()
    return (
        not stripped
        or COMMENT_PATTERN.match(line)
        or PREAMBLE_ONLY_PATTERN.match(line)
        or LAYOUT_ONLY_PATTERN.match(line)
        or ENVIRONMENT_ONLY_PATTERN.match(line)
    )


def has_pre_chapter_content(lines: list[str], body_start: int, chapter_line: int) -> bool:
    if chapter_line <= body_start:
        return False
    return any(not is_preface_noise(lines[idx - 1]) for idx in range(body_start, chapter_line))


def is_substantive_line(line: str) -> bool:
    return bool(line.strip()) and not COMMENT_PATTERN.match(line)


def detect_next_chapter_line(lines: list[str], chapter_line: int) -> int | None:
    for idx in range(chapter_line + 1, len(lines) + 1):
        match = CHAPTER_PATTERN.match(lines[idx - 1])
        if match and not match.group(1):
            return idx
    return None


def detect_content_end_line(lines: list[str], chapter_line: int) -> int:
    next_chapter_line = detect_next_chapter_line(lines, chapter_line)
    content_end = next_chapter_line - 1 if next_chapter_line else len(lines)

    last_end_document = None
    for idx in range(chapter_line, content_end + 1):
        if END_DOCUMENT_PATTERN.match(lines[idx - 1]):
            last_end_document = idx

    if last_end_document is not None:
        trailing_substantive = any(is_substantive_line(lines[idx - 1]) for idx in range(last_end_document + 1, content_end + 1))
        if not trailing_substantive:
            content_end = last_end_document - 1

    while content_end >= chapter_line and not is_substantive_line(lines[content_end - 1]):
        content_end -= 1

    return max(content_end, chapter_line)


def strip_latex_text(text: str) -> str:
    text = re.sub(r"\\[A-Za-z]+\*?(?:\[[^\]]*\])?\{([^{}]*)\}", r"\1", text)
    text = text.replace("{", "").replace("}", "")
    return " ".join(text.split())


def build_headings(lines: list[str], chapter_label: str, max_line: int, include_chapters: bool) -> list[Heading]:
    headings: list[Heading] = []
    counters = [0, 0, 0]
    level_index = {"section": 0, "subsection": 1, "subsubsection": 2}
    for idx, line in enumerate(lines[:max_line], start=1):
        if include_chapters:
            chapter_match = CHAPTER_PATTERN.match(line)
            if chapter_match:
                headings.append(Heading(idx, "chapter", bool(chapter_match.group(1)), strip_latex_text(chapter_match.group(2).strip()), ""))
                continue
        for kind, pattern in HEADING_PATTERNS:
            match = pattern.match(line)
            if not match:
                continue
            starred = bool(match.group(1))
            title = strip_latex_text(match.group(2).strip())
            number = ""
            if not starred:
                pos = level_index[kind]
                counters[pos] += 1
                for reset_pos in range(pos + 1, len(counters)):
                    counters[reset_pos] = 0
                active = [str(value) for value in counters[: pos + 1]]
                number = ".".join([chapter_label] + active)
            headings.append(Heading(idx, kind, starred, title, number))
            break
    return headings


def detect_out_of_scope_tail_line(lines: list[str], start_line: int, end_line: int) -> int | None:
    for idx in range(start_line, end_line + 1):
        line = lines[idx - 1]
        for _kind, pattern in HEADING_PATTERNS:
            match = pattern.match(line)
            if not match:
                continue
            title = strip_latex_text(match.group(2).strip())
            if OUT_OF_SCOPE_TRAILING_TITLE_PATTERN.match(title):
                return idx
    return None


def make_spoken_label(kind: str, number: str, starred: bool, chapter_label: str) -> str:
    if kind == "intro":
        return f"Chapter {chapter_label}"
    if starred or not number:
        return ""
    return f"Section {number}"


def write_plan(
    tex_path: Path,
    out_path: Path,
    script_dir: Path,
    chapter_label: str,
    chapter_title: str,
    content_start_line: int,
    content_end_line: int,
    headings: list[Heading],
) -> None:
    segments = []
    entries = [Heading(content_start_line, "intro", False, chapter_title, chapter_label)] + headings
    for index, entry in enumerate(entries):
        start_line = entry.line_no
        end_line = entries[index + 1].line_no - 1 if index + 1 < len(entries) else content_end_line
        title = entry.title if entry.kind != "intro" else chapter_title
        spoken_label = make_spoken_label(entry.kind, entry.number, entry.starred, chapter_label)
        segments.append(
            {
                "kind": entry.kind,
                "level": entry.kind,
                "title": title,
                "number": entry.number if entry.kind != "intro" else chapter_label,
                "spoken_label": spoken_label,
                "start_line": start_line,
                "end_line": end_line,
            }
        )

    pad = max(2, len(str(len(segments) - 1)))
    script_dir.mkdir(parents=True, exist_ok=True)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "seq",
                "kind",
                "level",
                "title",
                "number",
                "spoken_label",
                "start_line",
                "end_line",
                "source_tex",
                "target_txt",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        for seq, segment in enumerate(segments):
            target_txt = script_dir / f"section-{seq:0{pad}d}-tts.txt"
            writer.writerow(
                {
                    "seq": seq,
                    **segment,
                    "source_tex": str(tex_path),
                    "target_txt": str(target_txt),
                }
            )


def main() -> int:
    parser = argparse.ArgumentParser(description="Create a TTS section plan from a LaTeX chapter.")
    parser.add_argument("--tex", required=True, help="Canonical source .tex file.")
    parser.add_argument("--script-dir", required=True, help="Directory for generated TTS text files.")
    parser.add_argument("--out", required=True, help="Output CSV path.")
    parser.add_argument("--chapter-label", help="Optional override for chapter label, e.g. 20 or A.")
    parser.add_argument(
        "--include-out-of-scope",
        action="store_true",
        help="Include default-out-of-scope trailing blocks such as Problems, Exercises, References, or Index.",
    )
    args = parser.parse_args()

    tex_path = Path(args.tex)
    script_dir = Path(args.script_dir)
    out_path = Path(args.out)
    lines = tex_path.read_text(encoding="utf-8").splitlines()
    chapter_label = infer_chapter_label(tex_path, lines, args.chapter_label)
    chapter_line, chapter_title = detect_chapter_title(lines)
    body_start_line = first_document_body_line(lines)
    include_pre_chapter_content = has_pre_chapter_content(lines, body_start_line, chapter_line)
    content_start_line = body_start_line if include_pre_chapter_content else chapter_line
    content_end_line = detect_content_end_line(lines, chapter_line)
    out_of_scope_tail_line = None if args.include_out_of_scope else detect_out_of_scope_tail_line(lines, content_start_line, content_end_line)
    if out_of_scope_tail_line is not None:
        content_end_line = out_of_scope_tail_line - 1
    if not chapter_title:
        chapter_title = tex_path.stem.replace("-", " ").title()
    if include_pre_chapter_content:
        chapter_title = tex_path.stem.replace("-", " ").title()
    headings = build_headings(lines, chapter_label, content_end_line, include_chapters=include_pre_chapter_content)
    write_plan(tex_path, out_path, script_dir, chapter_label, chapter_title, content_start_line, content_end_line, headings)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
