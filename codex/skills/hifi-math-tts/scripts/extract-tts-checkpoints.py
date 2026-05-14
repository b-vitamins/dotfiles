#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
from pathlib import Path


TAG_PATTERN = re.compile(r"\\tag\{([A-Za-z0-9.\-–—]+)\}")
LABEL_PATTERN = re.compile(r"\\label\{([^}]+)\}")
NUMERIC_IDENTIFIER_PATTERN = re.compile(r"(?P<id>[0-9]+(?:\.[0-9]+)+(?:[a-z])?)")
CAPTION_START = r"\caption{"
ALGORITHM_PATTERN = re.compile(r"\\textbf\{Algorithm\s+([^:}]+):\}\s*(.*)")
EXERCISE_PATTERN = re.compile(
    r"\\(?:noindent\s*)?(?:textbf|paragraph)\{(?:Problem|Exercise)\s+(?P<id>[0-9]+(?:\.[0-9]+)*)(?:[.)]|\s|:)"
)
ANSWER_ENTRY_PATTERN = re.compile(r"^\s*(?P<id>[0-9]+(?:\.[0-9]+)+)\.\s+")
ITEM_PATTERN = re.compile(r"^\s*\\item(?:\s+|$)")
LIST_BEGIN_PATTERN = re.compile(r"^\s*\\begin\{(enumerate|itemize)\}")
LIST_END_PATTERN = re.compile(r"^\s*\\end\{(enumerate|itemize)\}")
DISPLAY_EQUATION_ENVIRONMENTS = {"equation", "align", "gather", "multline", "flalign", "alignat", "eqnarray"}
EQUATION_BEGIN_PATTERN = re.compile(r"^\s*\\begin\{(?P<env>[A-Za-z]+)(?P<star>\*?)\}")
EQUATION_END_PATTERN = re.compile(r"^\s*\\end\{(?P<env>[A-Za-z]+)(?P<star>\*?)\}")
BRACKET_DISPLAY_BEGIN_PATTERN = re.compile(r"(?<!\\)\\\[")
BRACKET_DISPLAY_END_PATTERN = re.compile(r"\\\]")
HEADING_PATTERN = re.compile(r"^\s*\\(chapter|section|subsection|subsubsection)\*?\{")
LAYOUT_PROSE_SKIP_PATTERN = re.compile(
    r"^\s*\\(?:centering|includegraphics|fbox|parbox|minipage|vspace|hspace|rule|captionof)\b"
    r"|Placeholder for Figure"
    r"|TODO\(diagrams\):",
    re.IGNORECASE,
)
def normalize_whitespace(text: str) -> str:
    return " ".join(text.split())


def strip_latex(text: str) -> str:
    prev = None
    value = text
    while prev != value:
        prev = value
        value = re.sub(r"\\[A-Za-z]+\*?(?:\[[^\]]*\])?\{([^{}]*)\}", r"\1", value)
    value = re.sub(r"\\[A-Za-z]+\*?", " ", value)
    value = value.replace("{", " ").replace("}", " ")
    value = value.replace("~", " ")
    value = value.replace("$", " ")
    value = value.replace("_", " ")
    value = value.replace("^", " ")
    return normalize_whitespace(value)


def extract_braced_content(lines: list[str], line_idx: int, start_col: int) -> str:
    pieces: list[str] = []
    depth = 1
    col = start_col
    current_idx = line_idx
    while current_idx < len(lines):
        line = lines[current_idx]
        while col < len(line):
            char = line[col]
            if char == "{":
                depth += 1
            elif char == "}":
                depth -= 1
                if depth == 0:
                    return "".join(pieces)
            pieces.append(char)
            col += 1
        pieces.append("\n")
        current_idx += 1
        col = 0
    return "".join(pieces)


def load_plan(plan_path: Path) -> list[dict[str, str]]:
    with plan_path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def contains_digits(text: str) -> bool:
    return any(char.isdigit() for char in text)


def identifier_from_label(label: str) -> str:
    normalized = label.strip().replace("_", "-").replace(":", "-")
    match = re.search(r"(?:^|-)([0-9]+(?:[-.][0-9]+)+(?:[a-z])?)(?:$|-)", normalized, re.IGNORECASE)
    if not match:
        return ""
    return match.group(1).replace("-", ".")


def identifier_prefix(identifier: str) -> str:
    match = re.match(r"(.+)\.[0-9]+[a-z]?$", identifier)
    return match.group(1) if match else ""


def identifier_counter(identifier: str) -> int | None:
    match = re.search(r"\.([0-9]+)[a-z]?$", identifier)
    return int(match.group(1)) if match else None


def split_number_parts(number: str) -> list[str]:
    return [part for part in number.split(".") if part]


def extract_prose_anchor(paragraph: str, max_words: int = 10, *, tail: bool = False) -> str:
    cleaned = strip_latex(paragraph)
    words = cleaned.split()
    if len(words) < 8:
        return ""
    anchor_words = words[-max_words:] if tail else words[:max_words]
    anchor = " ".join(anchor_words)
    if contains_digits(anchor):
        return ""
    return anchor


def paragraph_has_structural_command(paragraph_lines: list[str]) -> bool:
    stripped_lines = [line.strip() for line in paragraph_lines if line.strip()]
    if not stripped_lines:
        return True
    first = stripped_lines[0]
    if first.startswith("%"):
        return True
    if any(LAYOUT_PROSE_SKIP_PATTERN.search(line) for line in stripped_lines):
        return True
    if HEADING_PATTERN.match(first):
        return True
    if EQUATION_BEGIN_PATTERN.match(first) or EQUATION_END_PATTERN.match(first):
        return True
    if CAPTION_START in first:
        return True
    if ALGORITHM_PATTERN.search(first) or EXERCISE_PATTERN.search(first):
        return True
    for line in stripped_lines:
        if TAG_PATTERN.search(line):
            return True
        if line.startswith(r"\begin{") or line.startswith(r"\end{"):
            return True
    return False


def line_is_structural_for_prose(line: str) -> bool:
    stripped = line.strip()
    if not stripped:
        return False
    if stripped.startswith("%"):
        return True
    if LAYOUT_PROSE_SKIP_PATTERN.search(stripped):
        return True
    if HEADING_PATTERN.match(stripped):
        return True
    if EQUATION_BEGIN_PATTERN.match(stripped) or EQUATION_END_PATTERN.match(stripped):
        return True
    if BRACKET_DISPLAY_BEGIN_PATTERN.search(stripped) or BRACKET_DISPLAY_END_PATTERN.search(stripped):
        return True
    if CAPTION_START in stripped:
        return True
    if ALGORITHM_PATTERN.search(stripped) or EXERCISE_PATTERN.search(stripped):
        return True
    if TAG_PATTERN.search(stripped):
        return True
    if stripped.startswith(r"\begin{") or stripped.startswith(r"\end{"):
        return True
    return False


def collect_prose_anchors(lines: list[str], start_line: int, end_line: int) -> list[tuple[int, str]]:
    anchors: list[tuple[int, str]] = []
    paragraph_lines: list[str] = []
    paragraph_start = start_line
    in_display = False
    display_env = ""

    def flush() -> None:
        nonlocal paragraph_lines, paragraph_start
        if not paragraph_lines:
            return
        if not paragraph_has_structural_command(paragraph_lines):
            paragraph = "\n".join(paragraph_lines)
            anchor = extract_prose_anchor(paragraph)
            if anchor:
                anchors.append((paragraph_start, anchor))
            tail_anchor = extract_prose_anchor(paragraph, tail=True)
            if tail_anchor and tail_anchor != anchor:
                anchors.append((paragraph_start, tail_anchor))
        paragraph_lines = []

    for idx in range(start_line, end_line + 1):
        line = lines[idx - 1]
        if in_display:
            if display_env == "bracket-display":
                if BRACKET_DISPLAY_END_PATTERN.search(line):
                    in_display = False
                    display_env = ""
            else:
                end_match = EQUATION_END_PATTERN.match(line)
                if end_match and end_match.group("env") == display_env:
                    in_display = False
                    display_env = ""
            paragraph_start = idx + 1
            continue

        if not line.strip():
            flush()
            paragraph_start = idx + 1
            continue

        begin_match = EQUATION_BEGIN_PATTERN.match(line)
        if begin_match and begin_match.group("env") in DISPLAY_EQUATION_ENVIRONMENTS:
            flush()
            if not EQUATION_END_PATTERN.match(line):
                in_display = True
                display_env = begin_match.group("env")
            paragraph_start = idx + 1
            continue

        if BRACKET_DISPLAY_BEGIN_PATTERN.search(line):
            flush()
            if not BRACKET_DISPLAY_END_PATTERN.search(line):
                in_display = True
                display_env = "bracket-display"
            paragraph_start = idx + 1
            continue

        if line_is_structural_for_prose(line):
            flush()
            paragraph_start = idx + 1
            continue
        if not paragraph_lines:
            paragraph_start = idx
        paragraph_lines.append(line)
    flush()
    return anchors


def collect_item_lines(lines: list[str], start_line: int, end_line: int, *, top_level_only: bool = False) -> list[int]:
    item_lines: list[int] = []
    depth = 0
    for idx in range(start_line, end_line + 1):
        line = lines[idx - 1]
        if LIST_BEGIN_PATTERN.match(line):
            depth += 1
        if ITEM_PATTERN.match(line):
            if not top_level_only or depth == 1:
                item_lines.append(idx)
        if LIST_END_PATTERN.match(line):
            depth = max(depth - 1, 0)
    return item_lines


def item_span(lines: list[str], item_line: int, section_end_line: int) -> tuple[int, int]:
    depth = 0
    for idx in range(item_line + 1, section_end_line + 1):
        line = lines[idx - 1]
        if LIST_BEGIN_PATTERN.match(line):
            depth += 1
        if LIST_END_PATTERN.match(line):
            if depth == 0:
                return item_line, idx - 1
            depth = max(depth - 1, 0)
        if depth == 0 and ITEM_PATTERN.match(line):
            return item_line, idx - 1
    return item_line, section_end_line


def clean_item_text_for_anchor(item_text: str) -> str:
    cleaned_lines: list[str] = []
    for line in item_text.splitlines():
        if LIST_BEGIN_PATTERN.match(line) or LIST_END_PATTERN.match(line):
            continue
        cleaned_lines.append(ITEM_PATTERN.sub("", line, count=1))
    return "\n".join(cleaned_lines)


def section_is_problem_heavy(row: dict[str, str], lines: list[str]) -> bool:
    title = row.get("title", "").strip().lower()
    return title.startswith("problems") or title.startswith("problem") or title.startswith("exercises") or title.startswith("exercise")


def extract_equation_blocks(lines: list[str]) -> list[dict[str, object]]:
    blocks: list[dict[str, object]] = []
    current_block: dict[str, object] | None = None

    for idx, line in enumerate(lines, start=1):
        if current_block is None:
            begin_match = EQUATION_BEGIN_PATTERN.match(line)
            if begin_match and begin_match.group("env") in DISPLAY_EQUATION_ENVIRONMENTS:
                current_block = {
                    "env": begin_match.group("env"),
                    "star": begin_match.group("star"),
                    "bracket_display": False,
                    "start_line": idx,
                    "lines": [line],
                }
                if EQUATION_END_PATTERN.match(line):
                    body_lines = list(current_block["lines"])
                    labels = [(idx, label) for label in LABEL_PATTERN.findall(line)]
                    tags = [(idx, tag) for tag in TAG_PATTERN.findall(line)]
                    blocks.append(
                        {
                            "env": current_block["env"],
                            "star": current_block["star"],
                            "bracket_display": False,
                            "start_line": idx,
                            "end_line": idx,
                            "body": "\n".join(body_lines),
                            "labels": labels,
                            "tags": tags,
                        }
                    )
                    current_block = None
                continue
            if BRACKET_DISPLAY_BEGIN_PATTERN.search(line):
                current_block = {
                    "env": "bracket-display",
                    "star": "*",
                    "bracket_display": True,
                    "start_line": idx,
                    "lines": [line],
                }
                if BRACKET_DISPLAY_END_PATTERN.search(line):
                    labels = [(idx, label) for label in LABEL_PATTERN.findall(line)]
                    tags = [(idx, tag) for tag in TAG_PATTERN.findall(line)]
                    blocks.append(
                        {
                            "env": "bracket-display",
                            "star": "*",
                            "bracket_display": True,
                            "start_line": idx,
                            "end_line": idx,
                            "body": line,
                            "labels": labels,
                            "tags": tags,
                        }
                    )
                    current_block = None
            continue

        current_block["lines"].append(line)
        end_match = EQUATION_END_PATTERN.match(line)
        block_done = False
        if current_block.get("bracket_display"):
            block_done = bool(BRACKET_DISPLAY_END_PATTERN.search(line))
        elif end_match and end_match.group("env") == current_block["env"] and end_match.group("star") == current_block["star"]:
            block_done = True
        if block_done:
            body_lines = list(current_block["lines"])
            start_line = int(current_block["start_line"])
            labels: list[tuple[int, str]] = []
            tags: list[tuple[int, str]] = []
            for offset, body_line in enumerate(body_lines):
                line_no = start_line + offset
                for label in LABEL_PATTERN.findall(body_line):
                    labels.append((line_no, label))
                for tag in TAG_PATTERN.findall(body_line):
                    tags.append((line_no, tag))
            blocks.append(
                {
                    "env": current_block["env"],
                    "star": current_block["star"],
                    "bracket_display": bool(current_block.get("bracket_display")),
                    "start_line": start_line,
                    "end_line": idx,
                    "body": "\n".join(body_lines),
                    "labels": labels,
                    "tags": tags,
                }
            )
            current_block = None

    return blocks


def extract_subequation_spans(lines: list[str]) -> list[dict[str, object]]:
    spans: list[dict[str, object]] = []
    stack: list[dict[str, object]] = []
    for idx, line in enumerate(lines, start=1):
        if re.search(r"\\begin\{subequations\}", line):
            stack.append({"start_line": idx, "end_line": len(lines), "block_starts": []})
        if re.search(r"\\end\{subequations\}", line) and stack:
            span = stack.pop()
            span["end_line"] = idx
            spans.append(span)
    return spans


def tagged_continuation_starts_with_equals(block: dict[str, object]) -> bool:
    body = str(block.get("body", ""))
    for line in body.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith(r"\[") or stripped.startswith(r"\begin"):
            continue
        return stripped.startswith("=") or stripped.startswith("&=")
    return False


def untagged_blocks_followed_by_tagged_continuation(blocks: list[dict[str, object]]) -> set[int]:
    skip_starts: set[int] = set()
    for idx, block in enumerate(blocks[:-1]):
        if block.get("tags") or block.get("labels"):
            continue
        next_block = blocks[idx + 1]
        if not next_block.get("tags"):
            continue
        if int(next_block["start_line"]) - int(block["end_line"]) > 3:
            continue
        if tagged_continuation_starts_with_equals(next_block):
            skip_starts.add(int(block["start_line"]))
    return skip_starts


def section_for_line(plan_rows: list[dict[str, str]], line_no: int) -> dict[str, str] | None:
    for row in plan_rows:
        if int(row["start_line"]) <= line_no <= int(row["end_line"]):
            return row
    return None


def top_section_number_for_line(plan_rows: list[dict[str, str]], line_no: int) -> str:
    current = ""
    for row in plan_rows:
        start = int(row["start_line"])
        if start > line_no:
            break
        number = row.get("number", "")
        if not number:
            continue
        parts = split_number_parts(number)
        if row.get("kind") == "section" and len(parts) >= 2:
            current = ".".join(parts[:2])
        elif not current and len(parts) >= 2:
            current = ".".join(parts[:2])
    return current


def infer_chapter_prefix(plan_rows: list[dict[str, str]]) -> str:
    for row in plan_rows:
        number = row.get("number", "")
        if "." in number:
            return number.split(".", 1)[0]
    if plan_rows:
        return plan_rows[0].get("number", "")
    return ""


def infer_numbering_scope(lines: list[str], plan_rows: list[dict[str, str]], chapter_prefix: str) -> str:
    identifiers: list[str] = []
    text = "\n".join(lines)
    for match in TAG_PATTERN.finditer(text):
        identifiers.append(match.group(1).replace("–", "-").replace("—", "-"))
    for match in NUMERIC_IDENTIFIER_PATTERN.finditer(text):
        identifiers.append(match.group("id"))
    section_prefixes = {
        ".".join(split_number_parts(row.get("number", ""))[:2])
        for row in plan_rows
        if len(split_number_parts(row.get("number", ""))) >= 2
    }
    section_like = 0
    chapter_like = 0
    for identifier in identifiers:
        prefix = identifier_prefix(identifier)
        if not prefix:
            continue
        parts = split_number_parts(identifier)
        if prefix in section_prefixes and len(parts) >= 3:
            section_like += 1
        elif parts and parts[0] == chapter_prefix and len(parts) == 2:
            chapter_like += 1
    return "section" if section_like > chapter_like else "chapter"


def subequation_span_for_line(spans: list[dict[str, object]], line_no: int) -> dict[str, object] | None:
    for span in spans:
        if int(span["start_line"]) <= line_no <= int(span["end_line"]):
            return span
    return None


def counter_key_for_block(
    block: dict[str, object],
    plan_rows: list[dict[str, str]],
    chapter_prefix: str,
    numbering_scope: str,
) -> str:
    if numbering_scope == "section":
        return top_section_number_for_line(plan_rows, int(block["start_line"])) or chapter_prefix
    return chapter_prefix


def next_inferred_identifier(
    block: dict[str, object],
    plan_rows: list[dict[str, str]],
    counters: dict[str, int],
    chapter_prefix: str,
    numbering_scope: str,
) -> str:
    key = counter_key_for_block(block, plan_rows, chapter_prefix, numbering_scope)
    counters[key] = counters.get(key, 0) + 1
    return f"{key}.{counters[key]}" if key else str(counters[key])


def sync_counter_from_identifier(counters: dict[str, int], identifier: str) -> None:
    prefix = identifier_prefix(identifier)
    value = identifier_counter(identifier)
    if prefix and value is not None:
        counters[prefix] = max(counters.get(prefix, 0), value)


def main() -> int:
    parser = argparse.ArgumentParser(description="Extract TTS fidelity checkpoints from a LaTeX source.")
    parser.add_argument("--tex", required=True, help="Canonical source .tex file.")
    parser.add_argument("--plan", required=True, help="Section plan CSV.")
    parser.add_argument("--out", required=True, help="Output checkpoint CSV.")
    args = parser.parse_args()

    tex_path = Path(args.tex)
    plan_path = Path(args.plan)
    out_path = Path(args.out)
    lines = tex_path.read_text(encoding="utf-8").splitlines()
    plan_rows = load_plan(plan_path)
    chapter_prefix = infer_chapter_prefix(plan_rows)
    numbering_scope = infer_numbering_scope(lines, plan_rows, chapter_prefix)
    equation_blocks = extract_equation_blocks(lines)
    continuation_skip_starts = untagged_blocks_followed_by_tagged_continuation(equation_blocks)
    subequation_spans = extract_subequation_spans(lines)
    counters: dict[str, int] = {}
    subequation_bases: dict[int, str] = {}
    subequation_counts: dict[int, int] = {}

    checkpoints: list[dict[str, str]] = []
    for row in plan_rows:
        spoken_hint = normalize_whitespace(" ".join(part for part in [row["spoken_label"], row["title"]] if part))
        checkpoints.append(
            {
                "section_seq": row["seq"],
                "kind": "heading",
                "identifier": row.get("number", "") or row["title"],
                "spoken_hint": spoken_hint,
                "source_line": row["start_line"],
                "notes": row["title"],
            }
        )
        if section_is_problem_heavy(row, lines):
            item_lines = collect_item_lines(lines, int(row["start_line"]), int(row["end_line"]), top_level_only=True)
            for item_idx, source_line in enumerate(item_lines, start=1):
                checkpoints.append(
                    {
                        "section_seq": row["seq"],
                        "kind": "exercise",
                        "identifier": str(item_idx),
                        "spoken_hint": f"Problem {item_idx}",
                        "source_line": source_line,
                        "notes": "problem-item",
                    }
                )
                item_start, item_end = item_span(lines, source_line, int(row["end_line"]))
                item_text = clean_item_text_for_anchor("\n".join(lines[item_start - 1 : item_end]))
                start_anchor = extract_prose_anchor(item_text)
                if start_anchor:
                    checkpoints.append(
                        {
                            "section_seq": row["seq"],
                            "kind": "prose-anchor",
                            "identifier": f"{row['seq']}-problem-{item_idx}-start",
                            "spoken_hint": start_anchor,
                            "source_line": source_line,
                            "notes": start_anchor,
                        }
                    )
                tail_anchor = extract_prose_anchor(item_text, tail=True)
                if tail_anchor and tail_anchor != start_anchor:
                    checkpoints.append(
                        {
                            "section_seq": row["seq"],
                            "kind": "prose-anchor",
                            "identifier": f"{row['seq']}-problem-{item_idx}-tail",
                            "spoken_hint": tail_anchor,
                            "source_line": item_end,
                            "notes": tail_anchor,
                        }
                    )
        else:
            for anchor_idx, (source_line, anchor) in enumerate(
                collect_prose_anchors(lines, int(row["start_line"]), int(row["end_line"])),
                start=1,
            ):
                checkpoints.append(
                    {
                        "section_seq": row["seq"],
                        "kind": "prose-anchor",
                        "identifier": f"{row['seq']}-p{anchor_idx}",
                        "spoken_hint": anchor,
                        "source_line": source_line,
                        "notes": anchor,
                    }
                )

    for block in equation_blocks:
        if int(block["start_line"]) in continuation_skip_starts:
            continue
        if block["star"] and not block["tags"] and not block["labels"]:
            continue
        row = section_for_line(plan_rows, int(block["start_line"]))
        if row is None:
            continue

        entries: list[tuple[int, str, str]]
        tags = list(block["tags"])
        labels = list(block["labels"])
        if tags:
            entries = [(line_no, tag, f"tag:{tag}") for line_no, tag in tags]
        elif labels:
            entries = [(line_no, label, f"label:{label}") for line_no, label in labels]
        else:
            entries = [(int(block["start_line"]), "", "")]

        for source_line, raw_identifier, note in entries:
            checkpoint_kind = "equation"
            if note.startswith("tag:") and raw_identifier:
                identifier = raw_identifier
                sync_counter_from_identifier(counters, identifier)
            elif note.startswith("label:") and raw_identifier:
                label_identifier = identifier_from_label(raw_identifier)
                if label_identifier:
                    identifier = label_identifier
                    sync_counter_from_identifier(counters, identifier)
                else:
                    span = subequation_span_for_line(subequation_spans, int(block["start_line"]))
                    if span is not None:
                        span_id = int(span["start_line"])
                        if span_id not in subequation_bases:
                            subequation_bases[span_id] = next_inferred_identifier(
                                block,
                                plan_rows,
                                counters,
                                chapter_prefix,
                                numbering_scope,
                            )
                            subequation_counts[span_id] = 0
                        subequation_counts[span_id] += 1
                        identifier = f"{subequation_bases[span_id]}{chr(ord('a') + subequation_counts[span_id] - 1)}"
                    else:
                        identifier = next_inferred_identifier(
                            block,
                            plan_rows,
                            counters,
                            chapter_prefix,
                            numbering_scope,
                        )
            elif subequation_span_for_line(subequation_spans, int(block["start_line"])) is not None:
                span = subequation_span_for_line(subequation_spans, int(block["start_line"]))
                assert span is not None
                span_id = int(span["start_line"])
                if span_id not in subequation_bases:
                    subequation_bases[span_id] = next_inferred_identifier(
                        block,
                        plan_rows,
                        counters,
                        chapter_prefix,
                        numbering_scope,
                    )
                    subequation_counts[span_id] = 0
                subequation_counts[span_id] += 1
                identifier = f"{subequation_bases[span_id]}{chr(ord('a') + subequation_counts[span_id] - 1)}"
            else:
                checkpoint_kind = "display-equation"
                inferred_display_identifier = next_inferred_identifier(
                    block,
                    plan_rows,
                    counters,
                    chapter_prefix,
                    numbering_scope,
                )
                identifier = f"display-{inferred_display_identifier}"
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": checkpoint_kind,
                    "identifier": identifier,
                    "spoken_hint": f"Equation {identifier}" if checkpoint_kind == "equation" else "Displayed equation",
                    "source_line": source_line,
                    "notes": note or raw_identifier or strip_latex(str(block.get("body", "")))[:240],
                }
            )

    figure_index = 0
    table_index = 0
    float_stack: list[str] = []
    for idx, line in enumerate(lines, start=1):
        row = section_for_line(plan_rows, idx)
        begin_float = re.search(r"\\begin\{(figure|table)\*?\}", line)
        if begin_float:
            float_stack.append(begin_float.group(1))
        if row is None:
            end_float = re.search(r"\\end\{(figure|table)\*?\}", line)
            if end_float and float_stack:
                float_stack.pop()
            continue
        alg_match = ALGORITHM_PATTERN.search(line)
        if alg_match:
            identifier = alg_match.group(1).strip()
            title = strip_latex(alg_match.group(2).strip())
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": "algorithm",
                    "identifier": identifier,
                    "spoken_hint": f"Algorithm {identifier}",
                    "source_line": idx,
                    "notes": title,
                }
            )
        ex_match = EXERCISE_PATTERN.search(line)
        if ex_match:
            identifier = ex_match.group("id").strip()
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": "exercise",
                    "identifier": identifier,
                    "spoken_hint": f"Exercise {identifier}",
                    "source_line": idx,
                    "notes": "",
                }
            )
        answer_match = ANSWER_ENTRY_PATTERN.match(line)
        if answer_match:
            identifier = answer_match.group("id").strip()
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": "exercise",
                    "identifier": identifier,
                    "spoken_hint": f"Answer {identifier}",
                    "source_line": idx,
                    "notes": "answer-entry",
                }
            )
        if CAPTION_START in line:
            start_col = line.index(CAPTION_START) + len(CAPTION_START)
            caption = strip_latex(extract_braced_content(lines, idx - 1, start_col))
            current_float = float_stack[-1] if float_stack else "figure"
            if current_float == "table":
                table_index += 1
                kind = "table-caption"
                identifier = f"{chapter_prefix}.{table_index}" if chapter_prefix else f"table-{table_index}"
                spoken_hint = f"Table {identifier}"
            else:
                figure_index += 1
                kind = "figure-caption"
                identifier = f"{chapter_prefix}.{figure_index}" if chapter_prefix else f"figure-{figure_index}"
                spoken_hint = f"Figure {identifier}"
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": kind,
                    "identifier": identifier,
                    "spoken_hint": spoken_hint,
                    "source_line": idx,
                    "notes": caption,
                }
            )
        end_float = re.search(r"\\end\{(figure|table)\*?\}", line)
        if end_float and float_stack:
            float_stack.pop()

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["section_seq", "kind", "identifier", "spoken_hint", "source_line", "notes"],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(checkpoints)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
