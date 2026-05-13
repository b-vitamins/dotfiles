#!/usr/bin/env python3
from __future__ import annotations

import re
from dataclasses import dataclass


BLACKBOARD_SPOKEN = {
    "n": "natural numbers",
    "r": "real numbers",
    "z": "integers",
    "q": "rational numbers",
    "c": "complex numbers",
    "p": "probability",
    "e": "expectation",
    "i": "indicator",
}

OPERATOR_SPOKEN = {
    "tr": "trace",
    "Tr": "trace",
    "argmax": "arg max",
    "argmin": "arg min",
    "diag": "diagonal",
    "rank": "rank",
    "supp": "support",
    "Span": "span",
    "span": "span",
    "sgn": "sign",
    "Var": "variance",
    "Cov": "covariance",
    "Re": "real part",
    "Im": "imaginary part",
    "det": "determinant",
}

NON_SYMBOL_BASES = {"cdots", "ldots", "dots", "vdots", "ddots"}

DECORATED_PATTERN = re.compile(
    r"(?P<lemma>"
    r"\\(?P<cmd>mathbf|boldsymbol|bm|mathcal|mathfrak|mathbb|vec|bar|overline|hat|widehat|tilde|widetilde|underline)"
    r"(?:\{(?P<braced>\\?[A-Za-z]+)\}|(?:\s+)?(?P<bare>\\?[A-Za-z]+|[A-Za-z]))"
    r")"
    r"(?P<suffix>(?:\s*(?:_\{[^}]+\}|_[A-Za-z0-9]+|\^\{[^}]+\}|\^[A-Za-z0-9*+\-]+))*)"
)
OPERATOR_PATTERN = re.compile(r"(?P<lemma>\\operatorname\{(?P<name>[A-Za-z]+)\})")
BRACKET_DOT_PATTERN = re.compile(
    r"(?P<lemma>\[(?:\\cdot|\\dots|\\ldots)\]\s*_\s*(?:\{(?P<sub>(?:[^{}]|\{[^{}]*\})+)\}|(?P<subbare>[A-Za-z0-9]+)))"
)


@dataclass(frozen=True)
class NotationCandidate:
    candidate_type: str
    source_form: str
    canonical_spoken: str
    forbidden_forms: str
    notes: str


def normalize_source_form(text: str) -> str:
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


def strip_wrappers(text: str) -> str:
    value = text.strip()
    prev = None
    while prev != value:
        prev = value
        value = re.sub(r"\\[A-Za-z]+\*?\{([^{}]*)\}", r"\1", value)
    return value.strip()


def base_name(token: str) -> str:
    token = token.strip()
    if token.startswith("\\"):
        command = token[1:].lower()
        if command == "imath":
            return "i"
        if command == "jmath":
            return "j"
        return command
    return token.lower()


def is_meaningful_base(token: str) -> bool:
    return base_name(token) not in NON_SYMBOL_BASES


def speak_annotation_token(token: str) -> list[str]:
    lower = token.lower()
    if token.isdigit():
        return [int_to_words(int(token))]
    if token == "+":
        return ["plus"]
    if token == "-":
        return ["minus"]
    if token == "*":
        return ["star"]
    if token.startswith("\\"):
        return [token[1:].lower()]
    if lower.isalpha() and len(lower) <= 3:
        return list(lower)
    return [lower]


def speak_annotation(text: str) -> str:
    value = strip_wrappers(text)
    value = value.replace(r"\cdot", " dot ")
    value = value.replace(r"\dots", " dots ")
    value = value.replace(r"\ldots", " dots ")
    value = value.replace(r"\pm", " plus minus ")
    tokens = re.findall(r"\\[A-Za-z]+|[A-Za-z]+|\d+|[+\-*]", value)
    spoken: list[str] = []
    for token in tokens:
        spoken.extend(speak_annotation_token(token))
    return " ".join(spoken)


def candidate_from_decorated_match(match: re.Match[str]) -> NotationCandidate | None:
    cmd = match.group("cmd")
    base_token = match.group("braced") or match.group("bare") or ""
    if not is_meaningful_base(base_token):
        return None

    source_form = normalize_source_form(match.group("lemma"))
    raw_base = base_token[1:] if base_token.startswith("\\") else base_token
    base = base_name(base_token)

    if cmd in {"mathbf", "boldsymbol", "bm"}:
        if base_token.startswith("\\"):
            return NotationCandidate(
                candidate_type="bold-greek",
                source_form=source_form,
                canonical_spoken=f"vector {base}",
                forbidden_forms=f"bold {base}|{base} in bold",
                notes="bold Greek symbol; prefer vector naming",
            )
        if len(raw_base) == 1 and raw_base.isupper():
            letter = raw_base.lower()
            return NotationCandidate(
                candidate_type="bold-upper",
                source_form=source_form,
                canonical_spoken=f"matrix {letter}",
                forbidden_forms=f"bold {letter}|{letter} in bold",
                notes="uppercase bold symbol; prefer matrix naming",
            )
        return NotationCandidate(
            candidate_type="bold-lower",
            source_form=source_form,
            canonical_spoken=f"vector {base}",
            forbidden_forms=f"bold {base}|{base} in bold",
            notes="lowercase bold symbol; prefer vector naming",
        )

    if cmd == "mathcal":
        return NotationCandidate(
            candidate_type="script",
            source_form=source_form,
            canonical_spoken=f"script {base}",
            forbidden_forms=f"calligraphic {base}",
            notes="calligraphic symbol; prefer script naming",
        )

    if cmd == "mathfrak":
        return NotationCandidate(
            candidate_type="fraktur",
            source_form=source_form,
            canonical_spoken=f"fraktur {base}",
            forbidden_forms=f"gothic {base}",
            notes="fraktur symbol; keep a stable spoken name",
        )

    if cmd == "mathbb":
        return NotationCandidate(
            candidate_type="blackboard",
            source_form=source_form,
            canonical_spoken=BLACKBOARD_SPOKEN.get(base, f"blackboard {base}"),
            forbidden_forms="",
            notes="blackboard-bold symbol; use standard mathematical name when available",
        )

    if cmd == "vec":
        return NotationCandidate(
            candidate_type="vec",
            source_form=source_form,
            canonical_spoken=f"vector {base}",
            forbidden_forms=f"bold {base}|{base} vector",
            notes="vector arrow notation; prefer vector naming",
        )

    if cmd in {"bar", "overline"}:
        return NotationCandidate(
            candidate_type="bar",
            source_form=source_form,
            canonical_spoken=f"{base} bar",
            forbidden_forms=f"bar {base}",
            notes="bar decoration; keep one stable spoken order",
        )

    if cmd in {"hat", "widehat"}:
        return NotationCandidate(
            candidate_type="hat",
            source_form=source_form,
            canonical_spoken=f"{base} hat",
            forbidden_forms=f"hat {base}",
            notes="decorated symbol with hat; keep one stable spoken order",
        )

    if cmd in {"tilde", "widetilde"}:
        return NotationCandidate(
            candidate_type="tilde",
            source_form=source_form,
            canonical_spoken=f"{base} tilde",
            forbidden_forms=f"tilde {base}",
            notes="tilde decoration; keep one stable spoken order",
        )

    if cmd == "underline":
        return NotationCandidate(
            candidate_type="underline",
            source_form=source_form,
            canonical_spoken=f"underline {base}",
            forbidden_forms="",
            notes="underlined symbol; preserve decoration in speech",
        )

    return None


def candidate_from_operator_match(match: re.Match[str]) -> NotationCandidate:
    operator = match.group("name")
    spoken = OPERATOR_SPOKEN.get(operator, operator.replace("_", " ").lower())
    return NotationCandidate(
        candidate_type="operator",
        source_form=match.group("lemma"),
        canonical_spoken=spoken,
        forbidden_forms=f"operatorname {operator.lower()}",
        notes="operator name should be spoken naturally",
    )


def candidate_from_bracket_dot_match(match: re.Match[str]) -> NotationCandidate:
    subscript = match.group("sub") or match.group("subbare") or ""
    spoken_sub = speak_annotation(subscript)
    return NotationCandidate(
        candidate_type="bracket-dot",
        source_form=normalize_source_form(match.group("lemma")),
        canonical_spoken=f"bracket dot sub {spoken_sub}".strip(),
        forbidden_forms="",
        notes="bracketed placeholder notation; preserve the subscript in speech",
    )


def extract_notation_candidates(text: str) -> list[NotationCandidate]:
    candidates: list[NotationCandidate] = []

    for match in DECORATED_PATTERN.finditer(text):
        candidate = candidate_from_decorated_match(match)
        if candidate is not None:
            candidates.append(candidate)

    for match in OPERATOR_PATTERN.finditer(text):
        candidates.append(candidate_from_operator_match(match))

    for match in BRACKET_DOT_PATTERN.finditer(text):
        candidates.append(candidate_from_bracket_dot_match(match))

    return candidates
