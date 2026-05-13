#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
from pathlib import Path


TAG_PATTERN = re.compile(r"\\tag\{([A-Za-z0-9.\-–—]+)\}")
LABEL_PATTERN = re.compile(r"\\label\{([^}]+)\}")
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
BRACKET_DISPLAY_BEGIN_PATTERN = re.compile(r"\\\[")
BRACKET_DISPLAY_END_PATTERN = re.compile(r"\\\]")
HEADING_PATTERN = re.compile(r"^\s*\\(chapter|section|subsection|subsubsection)\*?\{")
DECORATION_PATTERN = re.compile(r"\\(?:mathbf|boldsymbol|bm|mathcal|mathfrak|mathrm|vec|bar|overline|hat|widehat|tilde|widetilde|underline)\b")
INDEX_TOKEN_PATTERN = re.compile(r"[A-Za-z]+|\d+|[+\-]|'")
GREEK_INDEX_NAMES = {
    "alpha",
    "beta",
    "gamma",
    "delta",
    "epsilon",
    "varepsilon",
    "zeta",
    "eta",
    "theta",
    "vartheta",
    "iota",
    "kappa",
    "lambda",
    "mu",
    "nu",
    "xi",
    "pi",
    "rho",
    "sigma",
    "tau",
    "upsilon",
    "phi",
    "varphi",
    "chi",
    "psi",
    "omega",
}
ALLOWED_SIGNATURE_BASES = [
    "varepsilon",
    "vartheta",
    "varphi",
    "alpha",
    "beta",
    "gamma",
    "delta",
    "epsilon",
    "sigma",
    "lambda",
    "theta",
    "phi",
    "tau",
    "eta",
    "nu",
    "xi",
    "rho",
    "psi",
    "omega",
    "kappa",
    "zeta",
    "mu",
    "pi",
    "x",
    "y",
    "z",
    "w",
    "u",
    "v",
    "r",
    "q",
    "m",
    "n",
    "k",
    "h",
    "g",
    "f",
    "e",
    "d",
    "c",
    "b",
    "a",
    "s",
    "t",
    "A",
    "B",
    "C",
    "E",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "Z",
]
INDEXED_SYMBOL_PATTERN = re.compile(
    r"(?<![A-Za-z])("
    + "|".join(sorted(ALLOWED_SIGNATURE_BASES, key=len, reverse=True))
    + r")\s*_(?:\{([^{}]+)\}|(\\[A-Za-z]+|[A-Za-z]|\d+)'?)"
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


def simplify_math_text(text: str) -> str:
    value = text
    replacements = {
        r"\mathbf": "",
        r"\boldsymbol": "",
        r"\bm": "",
        r"\mathcal": "",
        r"\mathfrak": "",
        r"\mathrm": "",
        r"\vec": "",
        r"\bar": "",
        r"\overline": "",
        r"\hat": "",
        r"\widehat": "",
        r"\tilde": "",
        r"\widetilde": "",
        r"\underline": "",
        r"\left": "",
        r"\right": "",
        r"\lVert": "",
        r"\rVert": "",
        r"\mid": "|",
        r"\langle": " ",
        r"\rangle": " ",
        r"\lvert": " ",
        r"\rvert": " ",
        r"\qquad": " ",
        r"\quad": " ",
    }
    for src, dst in replacements.items():
        value = value.replace(src, dst)
    value = value.replace("{", "{").replace("}", "}")
    value = re.sub(r"\\([A-Za-z]+)", r" \1 ", value)
    return value


def contains_digits(text: str) -> bool:
    return any(char.isdigit() for char in text)


def spoken_index(index: str) -> str:
    cleaned = DECORATION_PATTERN.sub(" ", index)
    cleaned = re.sub(r"\\([A-Za-z]+)", r" \1 ", cleaned)
    cleaned = cleaned.replace("{", " ").replace("}", " ")
    cleaned = cleaned.replace(",", " ")
    cleaned = cleaned.replace("_", " ")
    tokens = INDEX_TOKEN_PATTERN.findall(cleaned)
    if not tokens:
        return ""

    spoken_parts: list[str] = []
    word_map = {
        "0": "zero",
        "1": "one",
        "2": "two",
        "3": "three",
        "4": "four",
        "5": "five",
        "6": "six",
        "7": "seven",
        "8": "eight",
        "9": "nine",
        "10": "ten",
    }

    for token in tokens:
        if token == "'":
            if spoken_parts:
                spoken_parts.append("prime")
            continue
        if token in {"+", "-"}:
            spoken_parts.append("plus" if token == "+" else "minus")
            continue
        if token.isdigit():
            spoken_parts.append(word_map.get(token, token))
            continue
        lowered = token.lower()
        if lowered == "pm":
            spoken_parts.extend(["plus", "minus"])
        elif lowered == "mp":
            spoken_parts.extend(["minus", "plus"])
        elif lowered in GREEK_INDEX_NAMES:
            spoken_parts.append(lowered)
        elif lowered.isalpha() and len(lowered) <= 3:
            spoken_parts.extend(list(lowered))
        else:
            spoken_parts.append(lowered)
    return " ".join(spoken_parts)


def equation_signature_hints(body: str) -> str:
    simplified = simplify_math_text(body)
    phrases: list[str] = []
    for symbol, braced_index, simple_index in INDEXED_SYMBOL_PATTERN.findall(simplified):
        index = braced_index or simple_index
        spoken = spoken_index(index)
        if not spoken:
            continue
        phrase = f"{symbol.lower()} sub {spoken}"
        if phrase not in phrases:
            phrases.append(phrase)
    return "|".join(phrases[:6])


def equation_render_hints(body: str) -> str:
    hints: list[str] = []
    has_cases = r"\begin{cases}" in body
    if r"\frac" in body:
        hints.append("fraction")
    if r"\lVert" in body or r"\rVert" in body:
        hints.append("norm")
    if r"\sum" in body:
        hints.append("sum")
    if r"\prod" in body:
        hints.append("product")
    if r"\int" in body:
        hints.append("integral")
    if r"\mid" in body:
        hints.append("conditional")
    if "\\\\" in body and not has_cases:
        hints.append("multiline")
    if has_cases:
        hints.append("cases")
    if r"\operatorname{Tr}" in body or r"\Tr" in body:
        hints.append("trace")
    if r"\lim" in body:
        hints.append("limit")
    if r"\delta" in body:
        hints.append("delta")
    if re.search(r"(?:\([^)]{2,}\)|\\left[^\\]*(?:\\right)?|\{[^{}]{2,}\})\s*\^", body):
        hints.append("grouped-power")
    if re.search(r"(?:\([^)]*[+\-][^)]*\)|\{[^{}]*[+\-][^{}]*\})\s*!", body):
        hints.append("factorial-scope")
    if re.search(r"\\frac\{[^{}]+\}\{[^{}]*(?:[+\-]|\^|!|\\times|\\cdot|\\pi|\\epsilon|\\lvert|\\rvert|\\vec)[^{}]*\}", body):
        hints.append("compound-denominator")
    if re.search(r"[A-Za-z\\]+(?:_\{?[^}]+\}?)?\([^)]*=[^)]*\)", body):
        hints.append("evaluation-assignment")
    return "|".join(hints)


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


def collect_prose_anchors(lines: list[str], start_line: int, end_line: int) -> list[tuple[int, str]]:
    anchors: list[tuple[int, str]] = []
    paragraph_lines: list[str] = []
    paragraph_start = start_line

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
        if not line.strip():
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


def local_equation_body(block: dict[str, object], source_line: int) -> str:
    body_lines = str(block["body"]).splitlines()
    start_line = int(block["start_line"])
    rel_line = max(0, min(source_line - start_line, len(body_lines) - 1))

    segment_start = 0
    for idx in range(rel_line - 1, -1, -1):
        if (LABEL_PATTERN.search(body_lines[idx]) or TAG_PATTERN.search(body_lines[idx])) and r"\\" in body_lines[idx]:
            segment_start = idx + 1
            break

    segment_end = rel_line

    if segment_end < segment_start:
        return str(block["body"])
    return "\n".join(body_lines[segment_start : segment_end + 1])


def section_for_line(plan_rows: list[dict[str, str]], line_no: int) -> dict[str, str] | None:
    for row in plan_rows:
        if int(row["start_line"]) <= line_no <= int(row["end_line"]):
            return row
    return None


def infer_chapter_prefix(plan_rows: list[dict[str, str]]) -> str:
    for row in plan_rows:
        number = row.get("number", "")
        if "." in number:
            return number.split(".", 1)[0]
    if plan_rows:
        return plan_rows[0].get("number", "")
    return ""


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
    equation_blocks = extract_equation_blocks(lines)

    checkpoints: list[dict[str, str]] = []
    for row in plan_rows:
        spoken_hint = normalize_whitespace(" ".join(part for part in [row["spoken_label"], row["title"]] if part))
        checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": "heading",
                    "identifier": row.get("number", "") or row["title"],
                    "spoken_hint": spoken_hint,
                    "signature_hints": "",
                    "render_hints": "",
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
                        "signature_hints": "",
                        "render_hints": "",
                        "source_line": source_line,
                        "notes": "problem-item",
                    }
                )
                item_start, item_end = item_span(lines, source_line, int(row["end_line"]))
                item_text = "\n".join(lines[item_start - 1 : item_end])
                start_anchor = extract_prose_anchor(item_text)
                if start_anchor:
                    checkpoints.append(
                        {
                            "section_seq": row["seq"],
                            "kind": "prose-anchor",
                            "identifier": f"{row['seq']}-problem-{item_idx}-start",
                            "spoken_hint": start_anchor,
                            "signature_hints": "",
                            "render_hints": "",
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
                            "signature_hints": "",
                            "render_hints": "",
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
                        "signature_hints": "",
                        "render_hints": "",
                        "source_line": source_line,
                        "notes": anchor,
                    }
                )

    equation_index = 0
    for block in equation_blocks:
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
            local_body = local_equation_body(block, source_line) if len(entries) > 1 else str(block["body"])
            equation_index += 1
            if note.startswith("tag:") and raw_identifier:
                identifier = raw_identifier
            elif chapter_prefix:
                identifier = f"{chapter_prefix}.{equation_index}"
            else:
                identifier = str(equation_index)
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": "equation",
                    "identifier": identifier,
                    "spoken_hint": f"Equation {identifier}",
                    "signature_hints": equation_signature_hints(local_body),
                    "render_hints": equation_render_hints(local_body),
                    "source_line": source_line,
                    "notes": note or raw_identifier,
                }
            )

    figure_index = 0
    for idx, line in enumerate(lines, start=1):
        row = section_for_line(plan_rows, idx)
        if row is None:
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
                    "signature_hints": "",
                    "render_hints": "",
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
                    "signature_hints": "",
                    "render_hints": "",
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
                    "signature_hints": "",
                    "render_hints": "",
                    "source_line": idx,
                    "notes": "answer-entry",
                }
            )
        if CAPTION_START in line:
            figure_index += 1
            start_col = line.index(CAPTION_START) + len(CAPTION_START)
            caption = strip_latex(extract_braced_content(lines, idx - 1, start_col))
            identifier = f"{chapter_prefix}.{figure_index}" if chapter_prefix else f"figure-{figure_index}"
            checkpoints.append(
                {
                    "section_seq": row["seq"],
                    "kind": "figure-caption",
                    "identifier": identifier,
                    "spoken_hint": f"Figure {identifier}",
                    "signature_hints": "",
                    "render_hints": "",
                    "source_line": idx,
                    "notes": caption,
                }
            )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["section_seq", "kind", "identifier", "spoken_hint", "signature_hints", "render_hints", "source_line", "notes"],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(checkpoints)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
