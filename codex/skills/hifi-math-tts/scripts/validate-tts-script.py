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
UNMATCHED_CLOSER_PATTERN = re.compile(r"\bclose (?:parenthesis|bracket|brace) sub\b")
ROW_COMPRESSION_PATTERN = re.compile(
    r"\b(?:paired with (?:the )?(?:preceding|previous|next) line|companion relation|from the preceding line|together with (?:the )?(?:preceding|previous|next) line)\b"
)
COMPACT_SUBSCRIPT_PATTERN = re.compile(
    r"\b(?:sub|super) "
    r"(?!(?:max|min|inf|sup|lim|sin|cos|tan|log|exp|det|dim|ker|hom|out|in|on|off|ex|mu|nu|xi|pi)\b)"
    r"[abcdeijklmnrstxyz]{2}\b"
)
DUPLICATED_WORD_PATTERN = re.compile(r"\b(?P<word>equation|if|vector|matrix|figure|problem)\s+(?P=word)\b")
METADATA_RESIDUE_PATTERN = re.compile(
    r"\b(?:source label|source tag|chapter source label|for bookkeeping the source label|compact notation|compact checkpoint notation|compact source signature|checkpoint notation|source signature|render hint|displayed group contains|these lines form|relevant factors)\b"
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
    "into",
    "point",
    "figure",
    "caption",
    "equation",
    "algorithm",
    "exercise",
}

RENDER_HINT_PATTERNS = {
    "fraction": (" over ", " all over ", " numerator ", " denominator ", " divided by "),
    "norm": (" norm ", " squared norm ", " inside the norm "),
    "sum": (" sum ", " summation "),
    "product": (" product ",),
    "integral": (" integral ", " with respect to "),
    "conditional": (" given ", " conditioned on "),
    "multiline": (" equivalently ", " first line ", " second line ", " third line ", " final line "),
    "cases": (" case ", " cases ", " if ", " otherwise "),
    "trace": (" trace ",),
    "limit": (" limit ", " tends to ", " goes to "),
    "delta": (" delta ",),
    "grouped-power": (" quantity ", " squared ", " cubed ", " raised to "),
    "factorial-scope": (" factorial of the quantity ",),
    "compound-denominator": (" denominator ", " all over the product ", " all over the quantity "),
    "evaluation-assignment": (" evaluated at ",),
}


def normalize_text(text: str) -> str:
    text = text.lower()
    text = text.replace("–", " ")
    text = text.replace("—", " ")
    text = re.sub(r"[^a-z0-9]+", " ", text)
    return " ".join(text.split())


def contains_token_sequence(haystack_norm: str, needle_norm: str) -> bool:
    if not haystack_norm or not needle_norm:
        return False
    haystack_tokens = haystack_norm.split()
    needle_tokens = needle_norm.split()
    width = len(needle_tokens)
    if width == 0 or width > len(haystack_tokens):
        return False
    return any(haystack_tokens[idx : idx + width] == needle_tokens for idx in range(len(haystack_tokens) - width + 1))


def digit_token_variants(token: str) -> list[str]:
    if not token.isdigit():
        return [token]
    variants = [token]
    if len(token) == 1:
        variants.append(int_to_words(int(token)))
        return variants
    variants.append(int_to_words(int(token)))
    variants.append(" ".join(int_to_words(int(ch)) for ch in token))
    seen: list[str] = []
    for variant in variants:
        if variant not in seen:
            seen.append(variant)
    return seen


def signature_phrase_variants(phrase: str) -> list[str]:
    tokens = phrase.split()
    variants = [""]
    for token in tokens:
        expanded = digit_token_variants(token)
        next_variants: list[str] = []
        for prefix in variants:
            for choice in expanded:
                candidate = f"{prefix} {choice}".strip()
                if candidate not in next_variants:
                    next_variants.append(candidate)
        variants = next_variants
    return [normalize_text(variant) for variant in variants if normalize_text(variant)]


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
        if part.isdigit():
            spoken_parts.append(int_to_words(int(part)))
        else:
            spoken_parts.append(part.lower())
    return " point ".join(spoken_parts)


def extract_caption_keywords(caption: str, limit: int = 5) -> list[str]:
    tokens = [tok for tok in normalize_text(caption).split() if len(tok) > 3 and tok not in STOPWORDS]
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
    keywords = extract_caption_keywords(anchor_text, limit=6)
    if not keywords:
        return True
    overlap = sum(1 for token in keywords if token in script_norm)
    return overlap >= min(4, len(keywords))


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


def local_window_after_identifier(script_norm: str, identifier: str, kind: str | None = None, max_words: int = 220) -> str:
    numeric_norm = normalize_text(f"{kind} {identifier}") if kind else normalize_text(identifier)
    spoken_norm = normalize_text(f"{kind} {identifier_to_spoken(identifier)}") if kind else normalize_text(identifier_to_spoken(identifier))
    positions = []
    if numeric_norm:
        pos = script_norm.find(numeric_norm)
        if pos >= 0:
            positions.append((pos, len(numeric_norm)))
    if spoken_norm:
        pos = script_norm.find(spoken_norm)
        if pos >= 0:
            positions.append((pos, len(spoken_norm)))
    if not positions:
        return script_norm
    start, length = min(positions, key=lambda item: item[0])
    tail = script_norm[start + length :].strip()
    words = tail.split()
    return " ".join(words[:max_words])


def has_render_hint(local_norm: str, hint: str) -> bool:
    if hint == "grouped-power":
        return " quantity " in f" {local_norm} " and any(term in local_norm for term in (" squared", " cubed", " raised to"))
    if hint == "compound-denominator":
        return any(term in local_norm for term in (" denominator ", " all over the product ", " all over the quantity "))
    if hint == "factorial-scope":
        return "factorial of the quantity" in local_norm
    if hint == "evaluation-assignment":
        return "evaluated at" in local_norm
    patterns = RENDER_HINT_PATTERNS.get(hint, ())
    return any(pattern.strip() in local_norm for pattern in patterns)


def contains_numbered_artifact_any(script_norm: str, kinds: list[str], identifier: str) -> bool:
    return any(contains_numbered_artifact(script_norm, kind, identifier) for kind in kinds)


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate spoken TTS scripts against source checkpoints.")
    parser.add_argument("--plan", required=True, help="Section plan CSV.")
    parser.add_argument("--checkpoints", required=True, help="Checkpoint CSV.")
    parser.add_argument("--lexicon", help="Optional notation lexicon CSV.")
    parser.add_argument("--out", required=True, help="Validation report CSV.")
    parser.add_argument("--allow-digits", action="store_true", help="Allow Arabic numerals in final scripts.")
    args = parser.parse_args()

    plan_path = Path(args.plan)
    plan_rows = load_csv(plan_path)
    checkpoint_rows = load_csv(Path(args.checkpoints))
    lexicon_rows = load_csv(Path(args.lexicon)) if args.lexicon else []
    workspace_root = infer_workspace_root(plan_path)
    grouped_checkpoints: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in checkpoint_rows:
        grouped_checkpoints[row["section_seq"]].append(row)

    out_rows: list[dict[str, str]] = []
    fail_count = 0
    warn_count = 0

    for row in plan_rows:
        target_path = resolve_target_path(row["target_txt"], workspace_root)
        missing: list[str] = []
        warnings: list[str] = []
        raw_hits: list[str] = []

        if not target_path.exists():
            missing.append("missing-output-file")
            script_text = ""
        else:
            script_text = target_path.read_text(encoding="utf-8").strip()
            if not script_text:
                missing.append("empty-output-file")
        script_norm = normalize_text(script_text)

        if script_text:
            raw_hits = sorted(set(RAW_LATEX_PATTERN.findall(script_text)))

        if script_text and not args.allow_digits and DIGIT_PATTERN.search(script_text):
            missing.append("arabic-digits-present")

        for checkpoint in grouped_checkpoints.get(row["seq"], []):
            kind = checkpoint["kind"]
            identifier = checkpoint["identifier"]
            if kind == "heading":
                title_tokens = [tok for tok in normalize_text(checkpoint["notes"]).split() if len(tok) > 2]
                if title_tokens and not all(token in script_norm for token in title_tokens[: min(4, len(title_tokens))]):
                    missing.append(f"heading-title:{checkpoint['notes']}")
                if identifier and row.get("spoken_label") and not contains_identifier(script_norm, identifier):
                    missing.append(f"heading-number:{identifier}")
            elif kind == "prose-anchor":
                if not prose_anchor_present(script_norm, checkpoint["notes"]):
                    missing.append(f"prose-anchor:{identifier}")
            elif kind in {"equation", "algorithm", "exercise"}:
                expected_word = kind.split("-")[0]
                expected_words = [expected_word]
                if kind == "exercise":
                    expected_words.append("problem")
                    expected_words.append("answer")
                if not contains_numbered_artifact_any(script_norm, expected_words, identifier):
                    missing.append(f"{kind}:{identifier}")
                if kind == "equation" and checkpoint.get("signature_hints"):
                    local_norm = local_window_after_identifier(script_norm, identifier, kind="equation")
                    for phrase in checkpoint["signature_hints"].split("|"):
                        phrase_variants = signature_phrase_variants(phrase)
                        if phrase_variants and not any(variant in local_norm for variant in phrase_variants):
                            missing.append(f"equation-signature:{identifier}:{phrase}")
                    for hint in checkpoint.get("render_hints", "").split("|"):
                        if hint and not has_render_hint(local_norm, hint):
                            missing.append(f"equation-render:{identifier}:{hint}")
            elif kind == "figure-caption":
                keywords = extract_caption_keywords(checkpoint["notes"])
                overlap = sum(1 for token in keywords if token in script_norm)
                if not contains_numbered_artifact(script_norm, "figure", identifier) or overlap < min(2, len(keywords)):
                    missing.append(f"figure-caption:{identifier}")

        for lexicon_row in lexicon_rows:
            for phrase in lexicon_row.get("forbidden_forms", "").split("|"):
                phrase_norm = normalize_text(phrase)
                if contains_token_sequence(script_norm, phrase_norm):
                    missing.append(f"forbidden-alias:{lexicon_row['canonical_spoken']}:{phrase}")

        if raw_hits:
            missing.append("raw-latex:" + "|".join(raw_hits[:8]))

        stripped_hits = sorted(set(STRIPPED_STRUCTURAL_PATTERN.findall(script_norm)))
        if stripped_hits:
            missing.append("structural-residue:" + "|".join(stripped_hits[:8]))
        mechanical_hits = sorted(set(MECHANICAL_RESIDUE_PATTERN.findall(script_norm)))
        if mechanical_hits:
            missing.append("mechanical-residue:" + "|".join(mechanical_hits[:8]))
        unmatched_closer_hits = sorted(set(UNMATCHED_CLOSER_PATTERN.findall(script_norm)))
        if unmatched_closer_hits:
            missing.append("unmatched-closer:" + "|".join(unmatched_closer_hits[:8]))
        duplicate_hits = sorted(set(match.group("word") for match in DUPLICATED_WORD_PATTERN.finditer(script_norm)))
        if duplicate_hits:
            missing.append("duplicated-word:" + "|".join(duplicate_hits[:8]))
        metadata_hits = sorted(set(METADATA_RESIDUE_PATTERN.findall(script_norm)))
        if metadata_hits:
            missing.append("metadata-residue:" + "|".join(metadata_hits[:8]))
        row_compression_hits = sorted(set(ROW_COMPRESSION_PATTERN.findall(script_norm)))
        if row_compression_hits:
            missing.append("row-compression:" + "|".join(row_compression_hits[:8]))
        compact_subscript_hits = sorted(set(COMPACT_SUBSCRIPT_PATTERN.findall(script_norm)))
        if compact_subscript_hits:
            missing.append("compact-subscript:" + "|".join(compact_subscript_hits[:8]))
        if script_text and UNDERSCORE_PATTERN.search(script_text):
            missing.append("literal-underscore-present")

        status = "ok"
        if missing:
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
                "missing_count": str(len(missing)),
                "missing_details": "; ".join(missing),
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
                "missing_count",
                "missing_details",
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
