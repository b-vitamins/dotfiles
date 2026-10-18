#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Optional

THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

from paper_utils import COMBINED_LINE_MAP_FILENAME, COMBINED_SOURCE_FILENAME, FIGURE_CATALOG_FILENAME, INCLUDE_REPORT_FILENAME, READTHROUGH_MANIFEST_FILENAME, SECTIONS_FILENAME, SOURCE_GATE_REPORT_FILENAME, TEX_OUTLINE_FILENAME, choose_main_tex, ensure_dir, find_tex_files, read_text, resolve_tex_path, uncommented_tex, write_json, write_text

INCLUDE_RE = re.compile(r"\\(input|include|subfile)\s*\{([^}]+)\}")
IMPORT_RE = re.compile(r"\\(import|subimport)\s*\{([^}]+)\}\s*\{([^}]+)\}")
SECTION_RE = re.compile(r"\\(part|chapter|section|subsection|subsubsection)\*?\{([^}]*)\}")
GRAPHICS_RE = re.compile(r"\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}")
CAPTION_RE = re.compile(r"\\caption\*?\{([^}]*)\}")
LABEL_RE = re.compile(r"\\label\{([^}]*)\}")
FIGURE_BEGIN_RE = re.compile(r"\\begin\{figure\*?\}")
TABLE_BEGIN_RE = re.compile(r"\\begin\{table\*?\}")


def build_combined_source(main_tex: Path, source_root: Path) -> tuple[list[str], list[dict[str, object]], list[dict[str, object]], list[dict[str, object]]]:
    combined_lines: list[str] = []
    line_map: list[dict[str, object]] = []
    include_edges: list[dict[str, object]] = []
    unresolved_includes: list[dict[str, object]] = []
    stack: list[Path] = []
    project_roots = [main_tex.parent, source_root]

    def emit(text: str, file_path: Path, source_line: int) -> None:
        combined_lines.append(text.rstrip("\n"))
        line_map.append(
            {
                "combined_line": len(combined_lines),
                "source_file": str(file_path),
                "source_line": source_line,
            }
        )

    def walk(tex_file: Path) -> None:
        resolved_file = tex_file.resolve()
        if resolved_file in stack:
            unresolved_includes.append(
                {
                    "kind": "cycle",
                    "file": str(resolved_file),
                    "stack": [str(item) for item in stack],
                }
            )
            return
        stack.append(resolved_file)
        emit(f"% >>> BEGIN FILE {resolved_file}", resolved_file, 0)
        for line_no, raw_line in enumerate(read_text(resolved_file).splitlines(), start=1):
            body = uncommented_tex(raw_line)
            emit(raw_line, resolved_file, line_no)

            for match in IMPORT_RE.finditer(body):
                command = match.group(1)
                target_dir = (resolved_file.parent / match.group(2).strip()).resolve()
                target_file = match.group(3).strip()
                resolved_target, resolution_mode = resolve_tex_path(
                    target_dir,
                    target_file,
                    source_root=source_root,
                    extra_roots=project_roots,
                    return_details=True,
                )
                if resolved_target is None:
                    unresolved_includes.append(
                        {
                            "kind": "text-include",
                            "command": command,
                            "file": str(resolved_file),
                            "line": line_no,
                            "target": str(target_dir / target_file),
                            "searched_roots": [str(target_dir), *(str(root) for root in project_roots if root != target_dir)],
                        }
                    )
                    continue
                include_edges.append(
                    {
                        "from": str(resolved_file),
                        "line": line_no,
                        "to": str(resolved_target),
                        "command": command,
                        "resolution_mode": resolution_mode,
                    }
                )
                walk(resolved_target)

            for match in INCLUDE_RE.finditer(body):
                command = match.group(1)
                target_file = match.group(2).strip()
                resolved_target, resolution_mode = resolve_tex_path(
                    resolved_file.parent,
                    target_file,
                    source_root=source_root,
                    extra_roots=project_roots,
                    return_details=True,
                )
                if resolved_target is None:
                    unresolved_includes.append(
                        {
                            "kind": "text-include",
                            "command": command,
                            "file": str(resolved_file),
                            "line": line_no,
                            "target": target_file,
                            "searched_roots": [str(resolved_file.parent), *(str(root) for root in project_roots if root != resolved_file.parent)],
                        }
                    )
                    continue
                include_edges.append(
                    {
                        "from": str(resolved_file),
                        "line": line_no,
                        "to": str(resolved_target),
                        "command": command,
                        "resolution_mode": resolution_mode,
                    }
                )
                walk(resolved_target)

        emit(f"% <<< END FILE {resolved_file}", resolved_file, 0)
        stack.pop()

    walk(main_tex)
    return combined_lines, line_map, include_edges, unresolved_includes


def collect_sections(combined_lines: list[str]) -> list[dict[str, object]]:
    sections: list[dict[str, object]] = []
    for line_no, line in enumerate(combined_lines, start=1):
        match = SECTION_RE.search(uncommented_tex(line))
        if match:
            sections.append(
                {
                    "line": line_no,
                    "level": match.group(1),
                    "title": match.group(2),
                }
            )
    return sections


def nearest_section_title(sections: list[dict[str, object]], line_no: int) -> str:
    current = "<preamble>"
    for section in sections:
        if int(section["line"]) <= line_no:
            current = f"{section['level']}: {section['title']}"
        else:
            break
    return current


def chunk_manifest(combined_lines: list[str], sections: list[dict[str, object]], chunk_size: int) -> list[dict[str, object]]:
    chunks: list[dict[str, object]] = []
    start_line = 1
    chunk_index = 1
    total_lines = len(combined_lines)
    while start_line <= total_lines:
        end_line = min(total_lines, start_line + chunk_size - 1)
        chunks.append(
            {
                "id": f"C{chunk_index:03d}",
                "start_line": start_line,
                "end_line": end_line,
                "section": nearest_section_title(sections, start_line),
                "must_read": True,
            }
        )
        chunk_index += 1
        start_line = end_line + 1
    return chunks


def collect_figures_and_tables(tex_files: list[Path]) -> tuple[list[dict[str, object]], list[dict[str, object]], list[dict[str, object]]]:
    figures: list[dict[str, object]] = []
    tables: list[dict[str, object]] = []
    missing_graphics: list[dict[str, object]] = []

    for tex_file in tex_files:
        lines = read_text(tex_file).splitlines()
        figure_block: Optional[dict[str, object]] = None
        table_block: Optional[dict[str, object]] = None
        for line_no, raw_line in enumerate(lines, start=1):
            line = uncommented_tex(raw_line)
            if FIGURE_BEGIN_RE.search(line):
                figure_block = {
                    "file": str(tex_file),
                    "start_line": line_no,
                    "captions": [],
                    "labels": [],
                    "graphics": [],
                }
            if TABLE_BEGIN_RE.search(line):
                table_block = {
                    "file": str(tex_file),
                    "start_line": line_no,
                    "captions": [],
                    "labels": [],
                }

            for match in GRAPHICS_RE.finditer(line):
                target = match.group(1).strip()
                candidates = [
                    tex_file.parent / target,
                    tex_file.parent / f"{target}.pdf",
                    tex_file.parent / f"{target}.png",
                    tex_file.parent / f"{target}.jpg",
                    tex_file.parent / f"{target}.jpeg",
                    tex_file.parent / f"{target}.eps",
                ]
                exists = any(candidate.exists() for candidate in candidates)
                record = {
                    "file": str(tex_file),
                    "line": line_no,
                    "target": target,
                    "exists": exists,
                }
                if figure_block is not None:
                    figure_block["graphics"].append(record)
                if not exists:
                    missing_graphics.append(record)

            for match in CAPTION_RE.finditer(line):
                if figure_block is not None:
                    figure_block["captions"].append(match.group(1))
                if table_block is not None:
                    table_block["captions"].append(match.group(1))

            for match in LABEL_RE.finditer(line):
                if figure_block is not None:
                    figure_block["labels"].append(match.group(1))
                if table_block is not None:
                    table_block["labels"].append(match.group(1))

            if figure_block is not None and "\\end{figure" in line:
                figure_block["end_line"] = line_no
                figures.append(figure_block)
                figure_block = None
            if table_block is not None and "\\end{table" in line:
                table_block["end_line"] = line_no
                tables.append(table_block)
                table_block = None

    return figures, tables, missing_graphics


def render_outline(main_tex: Path, candidates: list[dict[str, object]], sections: list[dict[str, object]], tex_files: list[Path], unresolved_includes: list[dict[str, object]], missing_graphics: list[dict[str, object]]) -> str:
    lines = [f"# TeX outline for {main_tex.name}", ""]
    lines.append("## Main TeX candidates")
    for candidate in candidates[:10]:
        lines.append(f"- {candidate['score']:>4}  {candidate['path']}")
    lines.append("")
    lines.append(f"## Source files ({len(tex_files)})")
    for tex_file in tex_files:
        lines.append(f"- {tex_file}")
    lines.append("")
    lines.append(f"## Sections ({len(sections)})")
    for section in sections:
        lines.append(f"- line {section['line']}: {section['level']} {section['title']}")
    lines.append("")
    lines.append(f"## Unresolved textual includes ({len(unresolved_includes)})")
    for item in unresolved_includes:
        lines.append(f"- {item}")
    lines.append("")
    lines.append(f"## Missing graphics references ({len(missing_graphics)})")
    for item in missing_graphics[:100]:
        lines.append(f"- {item}")
    lines.append("")
    return "\n".join(lines)


def render_figure_catalog(figures: list[dict[str, object]], tables: list[dict[str, object]]) -> str:
    lines = ["# Figure and table catalog", ""]
    lines.append(f"## Figures ({len(figures)})")
    for index, figure in enumerate(figures, start=1):
        lines.append(f"### Figure block {index}")
        lines.append(f"- file: {figure.get('file')}")
        lines.append(f"- lines: {figure.get('start_line')} - {figure.get('end_line')}")
        if figure.get("labels"):
            lines.append(f"- labels: {', '.join(figure['labels'])}")
        if figure.get("captions"):
            lines.append(f"- captions: {' | '.join(figure['captions'])}")
        if figure.get("graphics"):
            lines.append(f"- graphics: {json.dumps(figure['graphics'], ensure_ascii=False)}")
        lines.append("")
    lines.append(f"## Tables ({len(tables)})")
    for index, table in enumerate(tables, start=1):
        lines.append(f"### Table block {index}")
        lines.append(f"- file: {table.get('file')}")
        lines.append(f"- lines: {table.get('start_line')} - {table.get('end_line')}")
        if table.get("labels"):
            lines.append(f"- labels: {', '.join(table['labels'])}")
        if table.get("captions"):
            lines.append(f"- captions: {' | '.join(table['captions'])}")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(description="Index a TeX source tree and build readthrough artifacts.")
    parser.add_argument("--source-root", required=True)
    parser.add_argument("--output-dir", required=True)
    parser.add_argument("--chunk-size", type=int, default=220)
    args = parser.parse_args()

    source_root = Path(args.source_root).resolve()
    output_dir = ensure_dir(args.output_dir)
    tex_files = find_tex_files(source_root)
    main_tex, candidates = choose_main_tex(source_root)

    report: dict[str, object] = {
        "source_root": str(source_root),
        "tex_file_count": len(tex_files),
        "candidate_main_tex": candidates,
        "main_tex": str(main_tex) if main_tex else None,
        "strict_gate_passed": False,
        "fail_reasons": [],
    }

    if not tex_files:
        report["fail_reasons"].append("no_tex_files_found")
        write_json(output_dir / SOURCE_GATE_REPORT_FILENAME, report)
        sys.stdout.write(json.dumps(report, indent=2) + "\n")
        return 2
    if main_tex is None:
        report["fail_reasons"].append("main_tex_not_found")
        write_json(output_dir / SOURCE_GATE_REPORT_FILENAME, report)
        sys.stdout.write(json.dumps(report, indent=2) + "\n")
        return 2

    combined_lines, line_map, include_edges, unresolved_includes = build_combined_source(main_tex, source_root)
    sections = collect_sections(combined_lines)
    chunks = chunk_manifest(combined_lines, sections, args.chunk_size)
    figures, tables, missing_graphics = collect_figures_and_tables(tex_files)

    write_text(output_dir / COMBINED_SOURCE_FILENAME, "\n".join(combined_lines) + "\n")
    with (output_dir / COMBINED_LINE_MAP_FILENAME).open("w", encoding="utf-8") as handle:
        for row in line_map:
            handle.write(json.dumps(row, ensure_ascii=False) + "\n")
    write_json(
        output_dir / READTHROUGH_MANIFEST_FILENAME,
        {
            "total_chunks": len(chunks),
            "total_lines": len(combined_lines),
            "chunks": chunks,
        },
    )
    write_json(
        output_dir / INCLUDE_REPORT_FILENAME,
        {
            "main_tex": str(main_tex),
            "include_edges": include_edges,
            "unresolved_includes": unresolved_includes,
            "missing_graphics": missing_graphics,
        },
    )
    write_json(output_dir / SECTIONS_FILENAME, sections)
    write_text(output_dir / TEX_OUTLINE_FILENAME, render_outline(main_tex, candidates, sections, tex_files, unresolved_includes, missing_graphics))
    write_text(output_dir / FIGURE_CATALOG_FILENAME, render_figure_catalog(figures, tables))

    report["combined_source"] = str(output_dir / COMBINED_SOURCE_FILENAME)
    report["combined_line_count"] = len(combined_lines)
    report["chunk_count"] = len(chunks)
    report["section_count"] = len(sections)
    report["unresolved_includes"] = unresolved_includes
    report["missing_graphics"] = missing_graphics
    report["strict_gate_passed"] = len(unresolved_includes) == 0 and len(combined_lines) > 0
    if unresolved_includes:
        report["fail_reasons"].append("unresolved_text_includes")

    write_json(output_dir / SOURCE_GATE_REPORT_FILENAME, report)
    sys.stdout.write(json.dumps(report, indent=2) + "\n")
    return 0 if report["strict_gate_passed"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
