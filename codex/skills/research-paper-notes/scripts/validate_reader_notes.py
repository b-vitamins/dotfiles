#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

THIS_DIR = Path(__file__).resolve().parent
SCHEMA_PATH = THIS_DIR.parent / "assets" / "reader-notes-schema.json"

REQUIRED_HEADINGS = [
    "## Source coverage",
    "## 0) Header",
    "## 1) One-sentence claim (verifiable)",
    "## 2) Problem & setup (canonicalized)",
    "## 3) Notation & constants table",
    "## 4) Method/Model (algorithmic core)",
    "## 5) Theory/Derivation summary",
    "## 6) Assumptions & conditions ledger",
    "## 7) Experiments: reproduction checklist",
    "## 8) Results with matched deltas",
    "## 9) Figures -> findings map",
    "## 10) Comparison to prior work",
    "## 11) External validity & limits",
    "## 12) Threats to validity",
    "## 13) Vital verbatim sentences",
    "## 14) Reproduction/verification plan",
    "## 15) Artifacts",
    "## 16) Red flags & green flags",
    "## 17) Who should care & why it matters",
    "## 18) Open questions",
    "## 19) Machine-readable block (JSON)",
]
AUDITED_SECTIONS = {
    "## 1) One-sentence claim (verifiable)",
    "## 5) Theory/Derivation summary",
    "## 8) Results with matched deltas",
    "## 10) Comparison to prior work",
    "## 11) External validity & limits",
    "## 12) Threats to validity",
    "## 16) Red flags & green flags",
    "## 17) Who should care & why it matters",
    "## 18) Open questions",
}
REQUIRED_SOURCE_COVERAGE_FIELDS = [
    "TeX source gate:",
    "Source version used:",
    "Source coverage:",
    "Source provenance:",
    "External IDs:",
    "Input quality:",
    "Figure availability:",
    "Readthrough coverage:",
    "Confidence:",
]
STATEMENT_PREFIX_RE = re.compile(r"^(?:[-*]\s+|\d+\.\s+)?((?:\[(?:P|D|I)\])+)\s+(Anchor|Basis):\s+\S")
CONTROLLED_VOCAB_RE = re.compile(r"^[-*]\s+`[^`]+`\s*$")
PLACEHOLDER_RE = re.compile(r"<[A-Za-z][^>\n]*>")


def split_sections(text: str) -> dict[str, str]:
    matches = list(re.finditer(r"^## .*$", text, flags=re.MULTILINE))
    sections: dict[str, str] = {}
    for index, match in enumerate(matches):
        start = match.start()
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        sections[match.group(0).strip()] = text[start:end]
    return sections


def section_body(section_text: str) -> str:
    parts = section_text.splitlines()
    return "\n".join(parts[1:]).strip()


def extract_last_json_block(text: str) -> str | None:
    blocks = re.findall(r"```json\s*(\{.*?\})\s*```", text, flags=re.DOTALL)
    return blocks[-1] if blocks else None


def iter_statement_blocks(body: str) -> list[list[str]]:
    blocks: list[list[str]] = []
    current: list[str] = []
    in_code_block = False
    for raw_line in body.splitlines():
        stripped = raw_line.strip()
        if stripped.startswith("```"):
            in_code_block = not in_code_block
            continue
        if in_code_block:
            continue
        if not stripped:
            if current:
                blocks.append(current)
                current = []
            continue
        current.append(raw_line)
    if current:
        blocks.append(current)
    return blocks


def block_requires_evidence(block: list[str]) -> bool:
    stripped_lines = [line.strip() for line in block if line.strip()]
    if not stripped_lines:
        return False
    if all(line.startswith("|") for line in stripped_lines):
        return False
    if all(CONTROLLED_VOCAB_RE.match(line) for line in stripped_lines):
        return False
    if len(stripped_lines) == 1 and stripped_lines[0].endswith(":"):
        return False
    joined = " ".join(stripped_lines)
    return bool(re.search(r"[A-Za-z]", joined))


def validate_audited_section(heading: str, body: str) -> list[str]:
    errors: list[str] = []
    substantive_blocks = 0
    for block_index, block in enumerate(iter_statement_blocks(body), start=1):
        if not block_requires_evidence(block):
            continue
        substantive_blocks += 1
        first_line = next(line.strip() for line in block if line.strip())
        match = STATEMENT_PREFIX_RE.match(first_line)
        if match is None:
            errors.append(f"{heading}: block {block_index} missing statement prefix like '[P] Anchor:' / '[D] Basis:' / '[P][D] Anchor:'")
            continue
        raw_tags, label = match.groups()
        tags = set(re.findall(r"\[([PDI])\]", raw_tags))
        if "P" in tags and label not in {"Anchor", "Basis"}:
            errors.append(f"{heading}: block {block_index} paper-stated content must use an anchored prefix")
        if tags <= {"D", "I"} and label not in {"Anchor", "Basis"}:
            errors.append(f"{heading}: block {block_index} derived or inferred content must use '[D]/[I] Anchor:' or '[D]/[I] Basis:'")
    if substantive_blocks == 0:
        errors.append(f"{heading}: no substantive tagged statements found")
    return errors


def schema_type_matches(value: object, schema_type: str) -> bool:
    if schema_type == "string":
        return isinstance(value, str)
    if schema_type == "integer":
        return isinstance(value, int) and not isinstance(value, bool)
    if schema_type == "boolean":
        return isinstance(value, bool)
    if schema_type == "object":
        return isinstance(value, dict)
    if schema_type == "array":
        return isinstance(value, list)
    if schema_type == "null":
        return value is None
    return False


def validate_against_schema(value: object, schema: dict[str, object], path: str = "$") -> list[str]:
    errors: list[str] = []
    schema_type = schema.get("type")
    if schema_type is not None:
        allowed_types = schema_type if isinstance(schema_type, list) else [schema_type]
        if not any(schema_type_matches(value, str(item)) for item in allowed_types):
            expected = " | ".join(str(item) for item in allowed_types)
            return [f"{path}: expected {expected}, got {type(value).__name__}"]

    if "enum" in schema and value not in schema["enum"]:
        errors.append(f"{path}: value {value!r} not in enum {schema['enum']}")

    if isinstance(value, dict):
        required = schema.get("required", [])
        for key in required:
            if key not in value:
                errors.append(f"{path}: missing required key '{key}'")
        properties = schema.get("properties", {})
        for key, subschema in properties.items():
            if key in value and isinstance(subschema, dict):
                errors.extend(validate_against_schema(value[key], subschema, f"{path}.{key}"))

    if isinstance(value, list) and isinstance(schema.get("items"), dict):
        for index, item in enumerate(value):
            errors.extend(validate_against_schema(item, schema["items"], f"{path}[{index}]"))

    if isinstance(value, (int, float)) and "minimum" in schema and value < schema["minimum"]:
        errors.append(f"{path}: value {value} is below minimum {schema['minimum']}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate strict reader notes output.")
    parser.add_argument("--notes", required=True)
    parser.add_argument("--workspace", required=False)
    parser.add_argument("--readthrough-log", required=False)
    parser.add_argument("--report-out", required=False)
    parser.add_argument("--export-json", required=False)
    args = parser.parse_args()

    notes_path = Path(args.notes)
    text = notes_path.read_text(encoding="utf-8")
    sections = split_sections(text)

    report: dict[str, object] = {"passed": True, "errors": [], "warnings": []}

    for heading in REQUIRED_HEADINGS:
        if heading not in sections:
            report["errors"].append(f"missing heading: {heading}")

    if "N/R" in text:
        report["errors"].append("forbidden token 'N/R' found")

    if PLACEHOLDER_RE.search(text):
        report["warnings"].append("angle-bracket placeholder text remains in notes")

    source_coverage = sections.get("## Source coverage", "")
    for field in REQUIRED_SOURCE_COVERAGE_FIELDS:
        if field not in source_coverage:
            report["errors"].append(f"source coverage block missing field: {field}")
    if "TeX source gate: PASS" not in source_coverage:
        report["errors"].append("source coverage block does not state 'TeX source gate: PASS'")

    workspace = None
    if args.workspace:
        workspace = json.loads(Path(args.workspace).read_text(encoding="utf-8"))
        if not workspace.get("strict_gate_passed"):
            report["errors"].append("workspace strict_gate_passed is false")

    readthrough_log = None
    if args.readthrough_log:
        readthrough_log = json.loads(Path(args.readthrough_log).read_text(encoding="utf-8"))
        if not readthrough_log.get("completed"):
            report["errors"].append("readthrough log is incomplete")

    for heading in AUDITED_SECTIONS:
        if heading in sections:
            body = section_body(sections[heading])
            report["errors"].extend(validate_audited_section(heading, body))

    json_block = extract_last_json_block(text)
    parsed_json = None
    if json_block is None:
        report["errors"].append("missing embedded JSON block")
    else:
        try:
            parsed_json = json.loads(json_block)
        except Exception as exc:
            report["errors"].append(f"embedded JSON parse error: {exc}")

    if isinstance(parsed_json, dict):
        if not SCHEMA_PATH.exists():
            report["warnings"].append(f"schema file missing: {SCHEMA_PATH}")
        else:
            schema = json.loads(SCHEMA_PATH.read_text(encoding="utf-8"))
            report["errors"].extend(validate_against_schema(parsed_json, schema))
        if parsed_json.get("tex_source_gate") != "PASS":
            report["errors"].append("embedded JSON tex_source_gate is not PASS")
        if parsed_json.get("readthrough_complete") is not True:
            report["errors"].append("embedded JSON readthrough_complete is not true")
        if readthrough_log is not None:
            readthrough_chunks = parsed_json.get("readthrough_chunks", {})
            if readthrough_chunks.get("completed") != readthrough_log.get("chunks_completed"):
                report["errors"].append("embedded JSON readthrough_chunks.completed does not match readthrough log")
            if readthrough_chunks.get("total") != readthrough_log.get("chunks_total"):
                report["errors"].append("embedded JSON readthrough_chunks.total does not match readthrough log")
        if workspace is not None:
            provenance = parsed_json.get("source_provenance", {})
            input_label = provenance.get("input_label")
            if input_label and input_label != workspace.get("input"):
                report["errors"].append("embedded JSON source_provenance.input_label does not match workspace input")

    report["passed"] = not report["errors"]
    serialized = json.dumps(report, indent=2)
    sys.stdout.write(serialized + "\n")
    if args.report_out:
        Path(args.report_out).write_text(serialized, encoding="utf-8")
    if args.export_json and isinstance(parsed_json, dict):
        Path(args.export_json).write_text(json.dumps(parsed_json, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return 0 if report["passed"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
