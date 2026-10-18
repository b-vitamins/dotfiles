#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

from paper_utils import ARXIV_FETCH_MANIFEST_FILENAME, COMBINED_SOURCE_FILENAME, FACT_LEDGER_FILENAME, FIGURE_CATALOG_FILENAME, READTHROUGH_LOG_FILENAME, READTHROUGH_MANIFEST_FILENAME, READER_NOTES_FILENAME, SECTIONS_FILENAME, SOURCE_GATE_REPORT_FILENAME, TEX_OUTLINE_FILENAME, WORKSPACE_FILENAME, copy_input_to_dir, ensure_dir, extract_source_bundle, normalize_arxiv_id, slugify, write_json

LOCAL_BUNDLE_SUFFIXES = {
    ".tar",
    ".tgz",
    ".gz",
    ".tar.gz",
    ".tar.bz2",
    ".tbz",
    ".tar.xz",
    ".txz",
}


def run_python(script: Path, args: list[str]) -> tuple[int, str]:
    command = [sys.executable, str(script)] + args
    result = subprocess.run(command, capture_output=True, text=True)
    output = result.stdout.strip()
    if result.stderr.strip():
        output = f"{output}\n{result.stderr.strip()}".strip()
    return result.returncode, output


def init_readthrough_log(manifest_path: Path, output_path: Path) -> None:
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    chunks = []
    for chunk in manifest.get("chunks", []):
        chunks.append(
            {
                **chunk,
                "read": False,
                "summary": "",
                "timestamp_utc": None,
            }
        )
    write_json(
        output_path,
        {
            "completed": False,
            "chunks_completed": 0,
            "chunks_total": len(chunks),
            "chunks": chunks,
        },
    )


def sync_readthrough_log(manifest_path: Path, output_path: Path) -> dict[str, object]:
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    desired_chunks = [
        {
            **chunk,
            "read": False,
            "summary": "",
            "timestamp_utc": None,
        }
        for chunk in manifest.get("chunks", [])
    ]

    if not output_path.exists():
        init_readthrough_log(manifest_path, output_path)
        return {"status": "initialized", "preserved_progress": False}

    existing = json.loads(output_path.read_text(encoding="utf-8"))
    existing_chunks = existing.get("chunks", [])
    same_layout = len(existing_chunks) == len(desired_chunks) and all(
        existing_chunk.get("id") == desired_chunk.get("id")
        and existing_chunk.get("start_line") == desired_chunk.get("start_line")
        and existing_chunk.get("end_line") == desired_chunk.get("end_line")
        for existing_chunk, desired_chunk in zip(existing_chunks, desired_chunks)
    )

    if not same_layout:
        init_readthrough_log(manifest_path, output_path)
        return {"status": "reinitialized", "preserved_progress": False}

    merged_chunks = []
    for existing_chunk, desired_chunk in zip(existing_chunks, desired_chunks):
        merged_chunks.append(
            {
                **desired_chunk,
                "read": bool(existing_chunk.get("read")),
                "summary": str(existing_chunk.get("summary", "")),
                "timestamp_utc": existing_chunk.get("timestamp_utc"),
            }
        )
    write_json(
        output_path,
        {
            "completed": False,
            "chunks_completed": 0,
            "chunks_total": len(merged_chunks),
            "chunks": merged_chunks,
        },
    )
    return {"status": "preserved", "preserved_progress": True}


def extract_last_json_block(text: str) -> str | None:
    blocks = re.findall(r"```json\s*(\{.*?\})\s*```", text, flags=re.DOTALL)
    return blocks[-1] if blocks else None


def replace_last_json_block(text: str, replacement: str) -> str:
    matches = list(re.finditer(r"```json\s*\{.*?\}\s*```", text, flags=re.DOTALL))
    if not matches:
        return text
    start, end = matches[-1].span()
    return f"{text[:start]}```json\n{replacement}\n```{text[end:]}"


def format_external_ids(external_ids: dict[str, object]) -> str:
    parts = []
    for key, value in external_ids.items():
        if value:
            label = "OpenReview" if key == "openreview_forum" else "Semantic Scholar" if key == "semantic_scholar" else key
            parts.append(f"{label}: {value}")
    return "; ".join(parts) if parts else "Not reported"


def build_reader_notes_payload(
    workspace_manifest: dict[str, object],
    gate_report: dict[str, object],
    readthrough_log_path: Path,
    fetch_manifest: dict[str, object] | None,
    companion_pdf: Path | None,
) -> dict[str, object]:
    metadata = fetch_manifest.get("metadata", {}) if fetch_manifest else {}
    arxiv_external_id = metadata.get("arxiv_id")
    if arxiv_external_id is None and workspace_manifest.get("is_arxiv"):
        arxiv_external_id = normalize_arxiv_id(str(workspace_manifest.get("input", "")))
    external_ids = {
        "arxiv": arxiv_external_id,
        "doi": metadata.get("doi"),
        "openreview_forum": None,
        "openalex": None,
        "semantic_scholar": None,
    }
    readthrough_log = json.loads(readthrough_log_path.read_text(encoding="utf-8"))
    chunk_total = int(readthrough_log.get("chunks_total", 0))
    missing_graphics = gate_report.get("missing_graphics", [])
    figure_availability = "source assets"
    if missing_graphics and companion_pdf is not None and companion_pdf.exists():
        figure_availability = "captions/source text with companion PDF cross-check"
    elif missing_graphics:
        figure_availability = "captions/source text only"

    source_version = external_ids["arxiv"] or str(workspace_manifest.get("input"))
    source_coverage = f"combined TeX source ({gate_report.get('combined_line_count', 0)} lines, {gate_report.get('chunk_count', chunk_total)} chunks)"
    if companion_pdf is not None and companion_pdf.exists():
        source_coverage += "; companion PDF staged"

    return {
        "title": metadata.get("title"),
        "doi": metadata.get("doi"),
        "venue": None,
        "year": None,
        "paper_type": None,
        "external_ids": external_ids,
        "source_version": source_version,
        "source_coverage": source_coverage,
        "source_provenance": {
            "input_type": "arxiv_source_bundle" if workspace_manifest.get("is_arxiv") else "local_source_tree",
            "input_label": workspace_manifest.get("input"),
            "tex_source_authority": "TeX",
            "companion_pdf_used": False,
        },
        "input_quality": "native text",
        "tex_source_gate": "PASS",
        "readthrough_complete": False,
        "readthrough_chunks": {
            "completed": int(readthrough_log.get("chunks_completed", 0)),
            "total": chunk_total,
        },
        "coordinates": {
            "purpose": [],
            "problem_form": [],
            "learning_signal": [],
            "mechanism": [],
            "evidence_regime": [],
        },
        "claim_surfaces": {
            "problem": None,
            "data": None,
            "objective": None,
            "mechanism": None,
            "regime": None,
            "evaluation": None,
            "deployment": None,
        },
        "problem": None,
        "main_claim": None,
        "notation": [],
        "assumptions": [],
        "results": [],
        "algorithms": [],
        "datasets": [],
        "metrics": [],
        "hardware": {},
        "effects": [],
        "artifacts": {},
        "relation_hints": [],
        "repro_thresholds": [],
        "limitations": [],
        "red_flags": [],
        "_scaffold": {
            "figure_availability": figure_availability,
            "source_coverage_line": source_coverage,
        },
    }


def render_reader_notes_scaffold(template_path: Path, payload: dict[str, object]) -> str:
    text = template_path.read_text(encoding="utf-8")
    external_ids = payload["external_ids"]
    replacements = {
        "- Source version used: <exact version or arXiv id>": f"- Source version used: {payload['source_version'] or 'Not reported'}",
        "- Source coverage: <main paper / appendix / source files / companion PDF>": f"- Source coverage: {payload['_scaffold']['source_coverage_line']}",
        "- Source provenance: <local source tree / arXiv source bundle / other strict source>": f"- Source provenance: {payload['source_provenance']['input_type']}",
        "- External IDs: <arXiv / DOI / OpenReview / OpenAlex / Semantic Scholar when known>": f"- External IDs: {format_external_ids(external_ids)}",
        "- Input quality: <native text / partial / unclear>": f"- Input quality: {payload['input_quality']}",
        "- Figure availability: <source assets / PDF pages / captions only>": f"- Figure availability: {payload['_scaffold']['figure_availability']}",
        "- Readthrough coverage: <completed>/<total> chunks": f"- Readthrough coverage: {payload['readthrough_chunks']['completed']}/{payload['readthrough_chunks']['total']} chunks",
        "- Citation:": f"- Citation: {payload['title'] or 'Not yet populated'}",
    }
    for source, replacement in replacements.items():
        text = text.replace(source, replacement)
    text = text.replace("\n- External IDs:\n", f"\n- External IDs: {format_external_ids(external_ids)}\n", 1)

    json_payload = dict(payload)
    json_payload.pop("_scaffold", None)
    json_block = extract_last_json_block(text)
    if json_block is not None:
        text = replace_last_json_block(text, json.dumps(json_payload, indent=2, ensure_ascii=False))
    return text


def initialize_note_templates(
    notes_dir: Path,
    assets_dir: Path,
    workspace_manifest: dict[str, object],
    gate_report: dict[str, object],
    readthrough_log_path: Path,
    fetch_manifest: dict[str, object] | None,
    companion_pdf: Path | None,
) -> list[str]:
    created: list[str] = []
    fact_ledger = notes_dir / FACT_LEDGER_FILENAME
    if not fact_ledger.exists():
        fact_ledger.write_text((assets_dir / "fact-ledger-template.md").read_text(encoding="utf-8"), encoding="utf-8")
        created.append(str(fact_ledger))

    reader_notes = notes_dir / READER_NOTES_FILENAME
    if not reader_notes.exists():
        payload = build_reader_notes_payload(workspace_manifest, gate_report, readthrough_log_path, fetch_manifest, companion_pdf)
        reader_notes.write_text(render_reader_notes_scaffold(assets_dir / "reader-notes-template.md", payload), encoding="utf-8")
        created.append(str(reader_notes))
    return created


def main() -> int:
    parser = argparse.ArgumentParser(description="Prepare a strict TeX-first paper workspace.")
    parser.add_argument("--input", required=True, help="arXiv URL/id or local source path")
    parser.add_argument("--output-dir", required=True, help="Root directory for paper workspaces")
    parser.add_argument("--chunk-size", type=int, default=220)
    args = parser.parse_args()

    raw_input = args.input
    local_path = Path(raw_input).expanduser()
    arxiv_id = normalize_arxiv_id(raw_input)
    is_local = local_path.exists()
    is_arxiv = (not is_local) and (arxiv_id is not None)

    slug_seed = arxiv_id if is_arxiv else local_path.stem if is_local else raw_input
    slug = slugify(slug_seed)
    workspace = ensure_dir(Path(args.output_dir) / slug)
    artifacts = ensure_dir(workspace / "artifacts")
    extracted = ensure_dir(workspace / "extracted")
    notes = ensure_dir(workspace / "notes")
    manifests = ensure_dir(workspace / "manifests")

    workspace_manifest: dict[str, object] = {
        "slug": slug,
        "input": raw_input,
        "is_arxiv": is_arxiv,
        "is_local": is_local,
        "strict_gate_passed": False,
        "paths": {
            "workspace": str(workspace),
            "artifacts": str(artifacts),
            "extracted": str(extracted),
            "notes": str(notes),
            "manifests": str(manifests),
        },
        "recommended_files": [],
        "errors": [],
    }

    assets_dir = THIS_DIR.parent / "assets"
    source_root: Path | None = None
    companion_pdf: Path | None = None
    fetch_manifest: dict[str, object] | None = None
    gate_report: dict[str, object] = {"strict_gate_passed": False, "fail_reasons": []}

    if is_arxiv:
        fetch_root = artifacts / "arxiv"
        rc, output = run_python(THIS_DIR / "fetch_arxiv_bundle.py", ["--input", raw_input, "--output-dir", str(fetch_root)])
        workspace_manifest["fetch_output"] = output
        fetch_manifest_path = fetch_root / ARXIV_FETCH_MANIFEST_FILENAME
        if fetch_manifest_path.exists():
            fetch_manifest = json.loads(fetch_manifest_path.read_text(encoding="utf-8"))
            workspace_manifest["fetch_manifest"] = str(fetch_manifest_path)
            if fetch_manifest.get("metadata"):
                workspace_manifest["paper_metadata"] = fetch_manifest["metadata"]
            if fetch_manifest.get("success"):
                source_root = Path(fetch_manifest["paths"]["source_extract_dir"])
                companion_pdf = Path(fetch_manifest["paths"]["pdf"])
            else:
                workspace_manifest["errors"].append("arxiv_source_fetch_failed")
        else:
            workspace_manifest["errors"].append("arxiv_fetch_manifest_missing")
        if rc != 0:
            workspace_manifest["errors"].append("arxiv_fetch_command_failed")
    elif is_local:
        lowered_name = local_path.name.lower()
        suffix_combo = "".join(local_path.suffixes[-2:]) if len(local_path.suffixes) >= 2 else local_path.suffix
        if local_path.is_dir() or local_path.suffix.lower() in {".tex", ".ltx", ".latex"}:
            staged = copy_input_to_dir(local_path, artifacts / "local")
            source_root = staged if staged.is_dir() else staged.parent
        elif suffix_combo in LOCAL_BUNDLE_SUFFIXES or local_path.suffix.lower() in LOCAL_BUNDLE_SUFFIXES:
            staged_bundle = copy_input_to_dir(local_path, artifacts / "bundle")
            bundle_extract_dir = artifacts / "bundle" / "extracted"
            try:
                extraction = extract_source_bundle(staged_bundle, bundle_extract_dir)
                workspace_manifest["local_bundle_extraction"] = extraction
                source_root = bundle_extract_dir
            except Exception as exc:
                workspace_manifest["errors"].append(f"local_source_bundle_extract_failed: {exc}")
        elif lowered_name.endswith(".pdf"):
            workspace_manifest["errors"].append("pdf_only_input_not_allowed_in_strict_mode")
        else:
            workspace_manifest["errors"].append("local_input_not_recognized_as_tex_source")
    else:
        workspace_manifest["errors"].append("input_not_found_or_not_recognized")

    if source_root is not None and source_root.exists():
        rc, output = run_python(
            THIS_DIR / "index_tex_project.py",
            ["--source-root", str(source_root), "--output-dir", str(extracted), "--chunk-size", str(args.chunk_size)],
        )
        workspace_manifest["index_output"] = output
        gate_report_path = extracted / SOURCE_GATE_REPORT_FILENAME
        if gate_report_path.exists():
            gate_report = json.loads(gate_report_path.read_text(encoding="utf-8"))
            workspace_manifest["source_gate_report"] = str(manifests / SOURCE_GATE_REPORT_FILENAME)
            workspace_manifest["strict_gate_passed"] = bool(gate_report.get("strict_gate_passed")) and not workspace_manifest["errors"]
            if not gate_report.get("strict_gate_passed"):
                for reason in gate_report.get("fail_reasons", []):
                    if reason not in workspace_manifest["errors"]:
                        workspace_manifest["errors"].append(reason)
        else:
            workspace_manifest["errors"].append("source_gate_report_missing")
        if rc != 0 and "unresolved_text_includes" not in workspace_manifest["errors"]:
            workspace_manifest["errors"].append("index_tex_project_failed")

        readthrough_manifest_path = extracted / READTHROUGH_MANIFEST_FILENAME
        if readthrough_manifest_path.exists():
            readthrough_log_path = manifests / READTHROUGH_LOG_FILENAME
            workspace_manifest["readthrough_log_sync"] = sync_readthrough_log(readthrough_manifest_path, readthrough_log_path)
            workspace_manifest["recommended_files"] = [
                str(extracted / COMBINED_SOURCE_FILENAME),
                str(extracted / READTHROUGH_MANIFEST_FILENAME),
                str(readthrough_log_path),
                str(extracted / SECTIONS_FILENAME),
                str(extracted / TEX_OUTLINE_FILENAME),
                str(extracted / FIGURE_CATALOG_FILENAME),
            ]
            if bool(gate_report.get("strict_gate_passed")) and not workspace_manifest["errors"]:
                created_notes = initialize_note_templates(
                    notes,
                    assets_dir,
                    workspace_manifest,
                    gate_report,
                    readthrough_log_path,
                    fetch_manifest,
                    companion_pdf,
                )
                if created_notes:
                    workspace_manifest["initialized_note_files"] = created_notes
                workspace_manifest["recommended_files"].extend(
                    [
                        str(notes / FACT_LEDGER_FILENAME),
                        str(notes / READER_NOTES_FILENAME),
                    ]
                )
        else:
            workspace_manifest["errors"].append("readthrough_manifest_missing")
    else:
        workspace_manifest["errors"].append("source_root_unavailable")

    if companion_pdf is not None and companion_pdf.exists():
        workspace_manifest["recommended_files"].append(str(companion_pdf))

    workspace_manifest["strict_gate_passed"] = bool(workspace_manifest.get("strict_gate_passed")) and not workspace_manifest["errors"]
    merged_gate_report = dict(gate_report)
    merged_gate_report["strict_gate_passed"] = workspace_manifest["strict_gate_passed"]
    merged_gate_report["fail_reasons"] = sorted(set(list(gate_report.get("fail_reasons", [])) + list(workspace_manifest["errors"])))
    merged_gate_report["workspace_errors"] = sorted(set(workspace_manifest["errors"]))
    merged_gate_report["workspace_input"] = raw_input
    merged_gate_report["source_root"] = str(source_root) if source_root is not None else None
    if fetch_manifest is not None:
        merged_gate_report["fetch_manifest"] = workspace_manifest.get("fetch_manifest")
        if fetch_manifest.get("metadata"):
            merged_gate_report["paper_metadata"] = fetch_manifest["metadata"]
    if workspace_manifest.get("readthrough_log_sync"):
        merged_gate_report["readthrough_log_sync"] = workspace_manifest["readthrough_log_sync"]
    if workspace_manifest.get("initialized_note_files"):
        merged_gate_report["initialized_note_files"] = workspace_manifest["initialized_note_files"]
    workspace_manifest["recommended_files"].extend(
        [
            str(manifests / SOURCE_GATE_REPORT_FILENAME),
            str(manifests / WORKSPACE_FILENAME),
        ]
    )
    workspace_manifest["recommended_files"] = list(dict.fromkeys(workspace_manifest["recommended_files"]))
    write_json(
        manifests / SOURCE_GATE_REPORT_FILENAME,
        merged_gate_report,
    )
    write_json(manifests / WORKSPACE_FILENAME, workspace_manifest)
    sys.stdout.write(json.dumps(workspace_manifest, indent=2) + "\n")
    return 0 if workspace_manifest["strict_gate_passed"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
