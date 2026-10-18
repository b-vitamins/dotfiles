#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import re
from collections import defaultdict
from pathlib import Path


RAW_LATEX_PATTERN = re.compile(r"\\[A-Za-z]+")
DIGIT_PATTERN = re.compile(r"\d")
UNDERSCORE_PATTERN = re.compile(r"_")
STRIPPED_STRUCTURAL_PATTERN = re.compile(
    r"\b(?:"
    r"begin(?:document|equation|align|gather|multline|flalign|alignat|eqnarray|figure|table|itemize|enumerate)"
    r"|end(?:document|equation|align|gather|multline|flalign|alignat|eqnarray|figure|table|itemize|enumerate)"
    r"|aligned|nonumber|qedhere"
    r")\b"
)
MECHANICAL_RESIDUE_PATTERN = re.compile(
    r"\b(?:operatorname|mathcal|mathbf|mathrm|longrightarrow|xrightarrow|rightarrow|leftarrow|imath|jmath|cdot|[a-z]cdot)\b"
)
MECHANICAL_MEASURE_PATTERN = re.compile(
    r"\bd to the (?:d|[a-z]+)(?: minus one)? (?:vector )?[a-z]\b"
)
METADATA_RESIDUE_PATTERN = re.compile(
    r"\b(?:source label|source tag|chapter source label|for bookkeeping the source label|compact notation|compact checkpoint notation|compact source signature|checkpoint notation|source signature|displayed group contains)\b"
)
VALIDATION_APPEASEMENT_PATTERN = re.compile(
    r"\b(?:"
    r"with denominator (?:the )?(?:quantity |factor |term )?"
    r"|over the denominator (?:the )?(?:quantity |factor |term )?"
    r"|the denominator (?:in|terms are|term is)"
    r"|with denominator [a-z ]+ outside the cases"
    r"|the numerator and denominator are grouped"
    r")"
)
PARENTHESIZED_SUBSCRIPT_PATTERN = re.compile(r"\b(?:sub|super) parenthesized\b|\b(?:sub|super) open parenthesis\b")
ROW_COMPRESSION_PATTERN = re.compile(
    r"\b(?:paired with (?:the )?(?:preceding|previous|next) line|companion relation|from the preceding line|together with (?:the )?(?:preceding|previous|next) line)\b"
)
INITIALISM_APOSTROPHE_PATTERN = re.compile(r"\b(?:[A-Z]\s+){1,}[A-Z]'s\b|\b[A-Z]{2,}'s\b")
DUPLICATED_WORD_PATTERN = re.compile(
    r"\b(?P<word>equation|if|vector|matrix|figure|problem)\s+(?P=word)\b",
    re.IGNORECASE,
)
NUMBER_WORD_PATTERN = (
    r"(?:zero|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|"
    r"sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|"
    r"hundred|thousand|point|through|capital|[a-z])"
)
STANDALONE_NAV_CALLOUT_PATTERN = re.compile(
    rf"^(?:see )?(?:chapter|section|exercise|problem) {NUMBER_WORD_PATTERN}(?: {NUMBER_WORD_PATTERN})*$"
)
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
STOPWORDS = {
    "the",
    "and",
    "for",
    "with",
    "from",
    "that",
    "this",
    "into",
    "then",
    "than",
    "reads",
    "read",
    "showing",
    "shows",
    "using",
    "used",
    "given",
    "which",
    "where",
    "when",
    "have",
    "has",
    "point",
    "figure",
    "caption",
    "equation",
    "algorithm",
    "exercise",
}


def normalize_text(text: str) -> str:
    text = text.lower()
    text = text.replace("–", " ")
    text = text.replace("—", " ")
    text = re.sub(r"[^a-z0-9]+", " ", text)
    return " ".join(text.split())


def int_to_words(value: int) -> str:
    ones = [
        "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen",
    ]
    tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
    if value < 20:
        return ones[value]
    if value < 100:
        ten, rem = divmod(value, 10)
        return tens[ten] if rem == 0 else f"{tens[ten]} {ones[rem]}"
    if value < 1000:
        hundred, rem = divmod(value, 100)
        return f"{ones[hundred]} hundred" if rem == 0 else f"{ones[hundred]} hundred {int_to_words(rem)}"
    thousand, rem = divmod(value, 1000)
    return f"{int_to_words(thousand)} thousand" if rem == 0 else f"{int_to_words(thousand)} thousand {int_to_words(rem)}"


def identifier_to_spoken(identifier: str) -> str:
    normalized = identifier.replace("—", "--").replace("–", "--")
    if "--" in normalized:
        return " through ".join(identifier_to_spoken(part.strip()) for part in normalized.split("--") if part.strip())
    parts = identifier.split(".")
    spoken_parts = []
    for part in parts:
        number_letter_match = re.fullmatch(r"([0-9]+)([A-Za-z]+)", part)
        if part.isdigit():
            spoken_parts.append(int_to_words(int(part)))
        elif number_letter_match:
            number, letters = number_letter_match.groups()
            spoken_parts.append(f"{int_to_words(int(number))} {' '.join(letters.lower())}")
        else:
            spoken_parts.append(part.lower())
    return " point ".join(spoken_parts)


def strip_latex_fragments(text: str) -> str:
    value = re.sub(r"\\textsuperscript\{[^{}]*\}", " ", text)
    prev = None
    while prev != value:
        prev = value
        value = re.sub(r"\\[A-Za-z]+\*?(?:\[[^\]]*\])?\{([^{}]*)\}", r"\1", value)
    value = re.sub(r"\\[A-Za-z]+\*?", " ", value)
    value = value.replace("{", " ").replace("}", " ")
    value = value.replace("$", " ").replace("_", " ").replace("^", " ")
    return value


def load_csv(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def infer_workspace_root(plan_path: Path) -> Path:
    resolved_parent = plan_path.resolve().parent
    for candidate in (resolved_parent, *resolved_parent.parents):
        if (candidate / "plans").is_dir() and (candidate / "scripts").is_dir():
            return candidate
    if resolved_parent.name == "plans":
        return resolved_parent.parent
    if resolved_parent.parent.name == "plans":
        return resolved_parent.parent.parent
    return resolved_parent


def resolve_target_path(raw_path: str, workspace_root: Path) -> Path:
    path = Path(raw_path)
    return path if path.is_absolute() else workspace_root / path


def contains_identifier(script_norm: str, identifier: str) -> bool:
    numeric_norm = normalize_text(identifier)
    spoken_norm = normalize_text(identifier_to_spoken(identifier))
    return (numeric_norm and numeric_norm in script_norm) or (spoken_norm and spoken_norm in script_norm)


def contains_numbered_artifact(script_norm: str, kind: str, identifier: str) -> bool:
    numeric_norm = normalize_text(f"{kind} {identifier}")
    spoken_norm = normalize_text(f"{kind} {identifier_to_spoken(identifier)}")
    return (numeric_norm and numeric_norm in script_norm) or (spoken_norm and spoken_norm in script_norm)


def contains_numbered_artifact_any(script_norm: str, kinds: list[str], identifier: str) -> bool:
    return any(contains_numbered_artifact(script_norm, kind, identifier) for kind in kinds)


def extract_keywords(text: str, limit: int = 6) -> list[str]:
    tokens = [tok for tok in normalize_text(text).split() if len(tok) > 3 and tok not in STOPWORDS]
    seen: list[str] = []
    for token in tokens:
        if token not in seen:
            seen.append(token)
        if len(seen) >= limit:
            break
    return seen


def prose_anchor_present(script_norm: str, anchor_text: str) -> bool:
    anchor_norm = normalize_text(anchor_text)
    if anchor_norm and anchor_norm in script_norm:
        return True
    keywords = extract_keywords(anchor_text)
    if not keywords:
        return True
    overlap = sum(1 for token in keywords if token in script_norm)
    return overlap >= min(4, len(keywords))


def expected_heading_norms(row: dict[str, str]) -> set[str]:
    number = row.get("number", "")
    if not number:
        return set()
    if row.get("kind") == "intro":
        return {normalize_text(f"Chapter {identifier_to_spoken(number)}")}
    if row.get("kind") in {"section", "subsection", "subsubsection"}:
        return {normalize_text(f"Section {identifier_to_spoken(number)}")}
    return set()


def heading_title_tokens(text: str) -> list[str]:
    structural_tokens = {"ddag", "dag", "textsuperscript"}
    return [
        tok
        for tok in normalize_text(strip_latex_fragments(text)).split()
        if len(tok) > 2 and tok not in structural_tokens
    ]


def heading_token_present(token: str, script_norm: str) -> bool:
    if token in script_norm:
        return True
    if 2 <= len(token) <= 5 and " ".join(token) in script_norm:
        return True
    return False


def standalone_navigation_callouts(script_text: str, row: dict[str, str]) -> list[str]:
    allowed = expected_heading_norms(row)
    hits: list[str] = []
    for paragraph in re.split(r"\n\s*\n", script_text):
        paragraph_norm = normalize_text(paragraph)
        if not paragraph_norm or paragraph_norm in allowed:
            continue
        if STANDALONE_NAV_CALLOUT_PATTERN.fullmatch(paragraph_norm):
            hits.append(paragraph_norm)
    return hits


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate coarse readiness of spoken TTS scripts.")
    parser.add_argument("--plan", required=True, help="Section plan CSV.")
    parser.add_argument("--checkpoints", required=True, help="Checkpoint CSV.")
    parser.add_argument("--source-brief", help="Optional source brief that must exist and be non-empty for readiness validation.")
    parser.add_argument("--out", required=True, help="Validation report CSV.")
    parser.add_argument("--allow-digits", action="store_true", help="Allow Arabic numerals in final scripts.")
    parser.add_argument(
        "--allow-out-of-scope",
        action="store_true",
        help="Allow default-out-of-scope rows such as Problems, Exercises, References, or Index.",
    )
    args = parser.parse_args()

    plan_path = Path(args.plan)
    plan_rows = load_csv(plan_path)
    checkpoint_rows = load_csv(Path(args.checkpoints))
    workspace_root = infer_workspace_root(plan_path)
    source_brief_issue = ""
    if args.source_brief:
        source_brief_path = resolve_target_path(args.source_brief, workspace_root)
        if not source_brief_path.exists():
            source_brief_issue = "missing-source-brief"
        elif not source_brief_path.read_text(encoding="utf-8").strip():
            source_brief_issue = "empty-source-brief"

    grouped_checkpoints: dict[str, list[dict[str, str]]] = defaultdict(list)
    equation_identifier_locations: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in checkpoint_rows:
        grouped_checkpoints[row["section_seq"]].append(row)
        if row.get("kind") == "equation" and row.get("identifier"):
            equation_identifier_locations[row["identifier"]].append(row)

    duplicate_equation_identifiers = {
        identifier: rows
        for identifier, rows in equation_identifier_locations.items()
        if len(rows) > 1
    }

    script_cache: dict[str, tuple[bool, str, str]] = {}
    for row in plan_rows:
        target_path = resolve_target_path(row["target_txt"], workspace_root)
        if target_path.exists():
            text = target_path.read_text(encoding="utf-8").strip()
            script_cache[row["seq"]] = (True, text, normalize_text(text))
        else:
            script_cache[row["seq"]] = (False, "", "")
    all_script_norm = normalize_text("\n\n".join(text for _, text, _ in script_cache.values()))

    out_rows: list[dict[str, str]] = []
    fail_count = 0
    warn_count = 0

    for row in plan_rows:
        target_path = resolve_target_path(row["target_txt"], workspace_root)
        failures: list[str] = []
        warnings: list[str] = []

        target_exists, script_text, script_norm = script_cache.get(row["seq"], (False, "", ""))
        if not target_exists:
            failures.append("missing-output-file")
        else:
            if not script_text:
                failures.append("empty-output-file")

        if script_text and not args.allow_digits and DIGIT_PATTERN.search(script_text):
            failures.append("arabic-digits-present")
        if source_brief_issue:
            failures.append(source_brief_issue)
        if (
            not args.allow_out_of_scope
            and OUT_OF_SCOPE_TRAILING_TITLE_PATTERN.match(row.get("title", "").strip())
        ):
            failures.append(f"default-out-of-scope-section:{row.get('title', '')}")

        section_duplicate_ids = sorted(
            identifier
            for identifier, duplicate_rows in duplicate_equation_identifiers.items()
            if any(duplicate_row.get("section_seq") == row["seq"] for duplicate_row in duplicate_rows)
        )
        for identifier in section_duplicate_ids:
            locations = "|".join(
                f"{duplicate_row.get('section_seq', '?')}@{duplicate_row.get('source_line', '?')}"
                for duplicate_row in duplicate_equation_identifiers[identifier]
            )
            failures.append(f"duplicate-equation-identifier:{identifier}:{locations}")

        for checkpoint in grouped_checkpoints.get(row["seq"], []):
            kind = checkpoint["kind"]
            identifier = checkpoint["identifier"]
            if kind == "heading":
                title_tokens = heading_title_tokens(checkpoint["notes"])
                if title_tokens and not all(
                    heading_token_present(token, script_norm) for token in title_tokens[: min(4, len(title_tokens))]
                ):
                    failures.append(f"heading-title:{checkpoint['notes']}")
                if identifier and row.get("spoken_label") and not contains_identifier(script_norm, identifier):
                    failures.append(f"heading-number:{identifier}")
            elif kind == "prose-anchor":
                if script_text and not prose_anchor_present(script_norm, checkpoint["notes"]):
                    warnings.append(f"prose-anchor:{identifier}")
            elif kind in {"equation", "algorithm", "exercise"}:
                expected_words = [kind]
                if kind == "exercise":
                    expected_words.extend(["problem", "answer"])
                if not contains_numbered_artifact_any(script_norm, expected_words, identifier):
                    failures.append(f"{kind}:{identifier}")
            elif kind == "figure-caption":
                keywords = extract_keywords(checkpoint["notes"])
                overlap = sum(1 for token in keywords if token in script_norm)
                if not contains_numbered_artifact(script_norm, "figure", identifier):
                    if contains_numbered_artifact(all_script_norm, "figure", identifier):
                        warnings.append(f"figure-caption-moved:{identifier}")
                    else:
                        failures.append(f"figure-caption:{identifier}")
                elif overlap < min(2, len(keywords)):
                    warnings.append(f"figure-caption-keywords:{identifier}")
            elif kind == "table-caption":
                keywords = extract_keywords(checkpoint["notes"])
                overlap = sum(1 for token in keywords if token in script_norm)
                if not contains_numbered_artifact(script_norm, "table", identifier):
                    if contains_numbered_artifact(all_script_norm, "table", identifier):
                        warnings.append(f"table-caption-moved:{identifier}")
                    else:
                        failures.append(f"table-caption:{identifier}")
                elif overlap < min(2, len(keywords)):
                    warnings.append(f"table-caption-keywords:{identifier}")

        raw_hits = sorted(set(RAW_LATEX_PATTERN.findall(script_text))) if script_text else []
        if raw_hits:
            failures.append("raw-latex:" + "|".join(raw_hits[:8]))

        stripped_hits = sorted(set(STRIPPED_STRUCTURAL_PATTERN.findall(script_norm)))
        if stripped_hits:
            failures.append("structural-residue:" + "|".join(stripped_hits[:8]))
        metadata_hits = sorted(set(METADATA_RESIDUE_PATTERN.findall(script_norm)))
        if metadata_hits:
            failures.append("metadata-residue:" + "|".join(metadata_hits[:8]))
        appeasement_hits = sorted(set(VALIDATION_APPEASEMENT_PATTERN.findall(script_norm)))
        if appeasement_hits:
            failures.append("validation-appeasement:" + "|".join(appeasement_hits[:8]))
        nav_callout_hits = standalone_navigation_callouts(script_text, row)
        if nav_callout_hits:
            failures.append("standalone-nav-callout:" + "|".join(nav_callout_hits[:8]))
        if script_text and UNDERSCORE_PATTERN.search(script_text):
            failures.append("literal-underscore-present")

        mechanical_hits = sorted(set(MECHANICAL_RESIDUE_PATTERN.findall(script_norm)))
        if mechanical_hits:
            warnings.append("mechanical-residue:" + "|".join(mechanical_hits[:8]))
        mechanical_measure_hits = sorted(set(MECHANICAL_MEASURE_PATTERN.findall(script_norm)))
        if mechanical_measure_hits:
            warnings.append("mechanical-measure:" + "|".join(mechanical_measure_hits[:8]))
        duplicate_hits = sorted(set(match.group("word").lower() for match in DUPLICATED_WORD_PATTERN.finditer(script_text)))
        if duplicate_hits:
            warnings.append("duplicated-word:" + "|".join(duplicate_hits[:8]))
        if PARENTHESIZED_SUBSCRIPT_PATTERN.search(script_norm):
            warnings.append("parenthesized-subscript")
        row_compression_hits = sorted(set(ROW_COMPRESSION_PATTERN.findall(script_norm)))
        if row_compression_hits:
            warnings.append("row-compression:" + "|".join(row_compression_hits[:8]))
        initialism_apostrophe_hits = sorted(set(INITIALISM_APOSTROPHE_PATTERN.findall(script_text))) if script_text else []
        if initialism_apostrophe_hits:
            warnings.append("initialism-apostrophe-plural:" + "|".join(initialism_apostrophe_hits[:8]))

        status = "ok"
        if failures:
            status = "fail"
            fail_count += 1
        elif warnings:
            status = "warn"
            warn_count += 1

        out_rows.append(
            {
                "seq": row["seq"],
                "title": row["title"],
                "target_txt": row["target_txt"],
                "status": status,
                "failure_count": str(len(failures)),
                "failure_details": "; ".join(failures),
                "warning_count": str(len(warnings)),
                "warning_details": "; ".join(warnings),
            }
        )

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with out_path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "seq",
                "title",
                "target_txt",
                "status",
                "failure_count",
                "failure_details",
                "warning_count",
                "warning_details",
            ],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(out_rows)

    total = len(out_rows)
    print(f"validated {total} section files: {total - fail_count - warn_count} ok, {warn_count} warn, {fail_count} fail")  # noqa: print
    return 1 if fail_count else 0


if __name__ == "__main__":
    raise SystemExit(main())
