#!/usr/bin/env python3
from __future__ import annotations

import gzip
import io
import json
import re
import shutil
import tarfile
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Optional

USER_AGENT = "research-paper-notes-skill/0.1"
ARXIV_ABS = "https://arxiv.org/abs/{id}"
ARXIV_PDF = "https://arxiv.org/pdf/{id}.pdf"
ARXIV_SRC = "https://arxiv.org/src/{id}"
ARXIV_FETCH_MANIFEST_FILENAME = "arxiv-fetch-manifest.json"
COMBINED_SOURCE_FILENAME = "combined-source.tex"
COMBINED_LINE_MAP_FILENAME = "combined-line-map.jsonl"
READTHROUGH_MANIFEST_FILENAME = "readthrough-manifest.json"
READTHROUGH_LOG_FILENAME = "readthrough-log.json"
INCLUDE_REPORT_FILENAME = "include-report.json"
SECTIONS_FILENAME = "sections.json"
TEX_OUTLINE_FILENAME = "tex-outline.md"
FIGURE_CATALOG_FILENAME = "figure-catalog.md"
SOURCE_GATE_REPORT_FILENAME = "source-gate-report.json"
WORKSPACE_FILENAME = "workspace.json"
FACT_LEDGER_FILENAME = "fact-ledger.md"
READER_NOTES_FILENAME = "reader-notes.md"
READER_NOTES_JSON_FILENAME = "reader-notes.json"
VALIDATION_REPORT_FILENAME = "validation-report.json"
INCLUDE_RE = re.compile(r"\\(input|include|subfile)\s*\{([^}]+)\}")
IMPORT_RE = re.compile(r"\\(import|subimport)\s*\{([^}]+)\}\s*\{([^}]+)\}")
META_TAG_RE = re.compile(r'<meta\s+(?:name|property)="([^"]+)"\s+content="([^"]*)"[^>]*>', flags=re.IGNORECASE)


def ensure_dir(path: str | Path) -> Path:
    directory = Path(path)
    directory.mkdir(parents=True, exist_ok=True)
    return directory


def read_text(path: str | Path) -> str:
    candidate = Path(path)
    for encoding in ("utf-8", "latin-1"):
        try:
            return candidate.read_text(encoding=encoding)
        except UnicodeDecodeError:
            continue
    return candidate.read_text(encoding="utf-8", errors="replace")


def write_text(path: str | Path, content: str) -> None:
    Path(path).write_text(content, encoding="utf-8")


def write_json(path: str | Path, obj: object) -> None:
    Path(path).write_text(json.dumps(obj, indent=2, ensure_ascii=False), encoding="utf-8")


def slugify(value: str) -> str:
    text = value.strip().lower()
    text = re.sub(r"https?://", "", text)
    text = text.replace("_", "-")
    text = re.sub(r"[^a-z0-9.-]+", "-", text)
    text = re.sub(r"-{2,}", "-", text)
    return text.strip("-.")[:120] or "paper"


def artifact_slug(value: str) -> str:
    text = value.strip().replace("_", "-").replace("/", "-")
    text = re.sub(r"[^A-Za-z0-9.-]+", "-", text)
    text = re.sub(r"-{2,}", "-", text)
    return text.strip("-.") or "artifact"


def normalize_arxiv_id(value: str) -> Optional[str]:
    text = value.strip()
    if not text:
        return None
    text = text.replace("arXiv:", "").replace("arxiv:", "")
    if "arxiv.org" in text:
        parsed = urllib.parse.urlparse(text)
        path = parsed.path.strip("/")
        parts = path.split("/")
        if not parts:
            return None
        if parts[0] in {"abs", "pdf", "src", "html"} and len(parts) >= 2:
            ident = "/".join(parts[1:])
        else:
            ident = path
    else:
        ident = text
    ident = ident.removesuffix(".pdf").strip("/")
    return ident or None


def extract_html_meta_tags(html_text: str) -> dict[str, str]:
    tags: dict[str, str] = {}
    for name, content in META_TAG_RE.findall(html_text):
        key = name.strip().lower()
        if key and key not in tags:
            tags[key] = content.strip()
    return tags


def fetch_url(url: str, dest: str | Path, timeout: int = 60) -> dict:
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    target = Path(dest)
    with urllib.request.urlopen(request, timeout=timeout) as response:
        data = response.read()
        headers = dict(response.headers.items())
        status = getattr(response, "status", None)
        final_url = response.geturl()
    target.write_bytes(data)
    return {
        "url": url,
        "final_url": final_url,
        "dest": str(target),
        "status": status,
        "bytes": len(data),
        "headers": headers,
    }


def _safe_extract_tar_fileobj(fileobj: io.BytesIO, extract_dir: Path) -> list[str]:
    members: list[str] = []
    with tarfile.open(fileobj=fileobj, mode="r:*") as tar:
        for member in tar.getmembers():
            resolved = (extract_dir / member.name).resolve()
            if not str(resolved).startswith(str(extract_dir.resolve())):
                raise RuntimeError(f"unsafe tar path: {member.name}")
        tar.extractall(extract_dir)
        members = [member.name for member in tar.getmembers()]
    return members


def _write_single_source_file(data: bytes, bundle_path: Path, extract_dir: Path) -> dict:
    decoded = data.decode("utf-8", errors="replace")
    output = extract_dir / f"{bundle_path.stem or 'source'}.tex"
    output.write_text(decoded, encoding="utf-8")
    return {
        "mode": "single_tex",
        "extract_dir": str(extract_dir),
        "members": [output.name],
    }


def extract_source_bundle(bundle_path: str | Path, extract_dir: str | Path) -> dict:
    archive = Path(bundle_path)
    output_dir = ensure_dir(extract_dir)
    raw = archive.read_bytes()
    errors: list[str] = []

    try:
        members = _safe_extract_tar_fileobj(io.BytesIO(raw), output_dir)
        return {
            "mode": "tar",
            "extract_dir": str(output_dir),
            "members": members,
        }
    except Exception as exc:
        errors.append(f"tar extraction failed: {exc}")

    try:
        decompressed = gzip.decompress(raw)
    except Exception as exc:
        decompressed = None
        errors.append(f"gzip extraction failed: {exc}")

    if decompressed is not None:
        try:
            members = _safe_extract_tar_fileobj(io.BytesIO(decompressed), output_dir)
            return {
                "mode": "gzip+tar",
                "extract_dir": str(output_dir),
                "members": members,
            }
        except Exception as exc:
            errors.append(f"gzip+tar extraction failed: {exc}")
        if any(token in decompressed.decode("utf-8", errors="ignore") for token in ("\\documentclass", "\\begin{document}", "\\section", "\\title")):
            result = _write_single_source_file(decompressed, archive, output_dir)
            result["errors"] = errors
            return result

    if any(token in raw.decode("utf-8", errors="ignore") for token in ("\\documentclass", "\\begin{document}", "\\section", "\\title")):
        result = _write_single_source_file(raw, archive, output_dir)
        result["errors"] = errors
        return result

    raise RuntimeError("; ".join(errors) if errors else "unknown source bundle format")


def copy_input_to_dir(src: str | Path, dst_dir: str | Path) -> Path:
    source = Path(src)
    destination_root = ensure_dir(dst_dir)
    if source.is_dir():
        destination_name = artifact_slug(source.name)
    else:
        suffixes = "".join(source.suffixes)
        stem = source.name[: -len(suffixes)] if suffixes else source.name
        destination_name = f"{artifact_slug(stem)}{suffixes}"
    destination = destination_root / destination_name
    if source.is_dir():
        if destination.exists():
            shutil.rmtree(destination)
        shutil.copytree(source, destination)
    else:
        shutil.copy2(source, destination)
    return destination


def find_tex_files(root: str | Path) -> list[Path]:
    source_root = Path(root)
    suffixes = {".tex", ".ltx", ".latex"}
    tex_files = []
    for candidate in source_root.rglob("*"):
        if candidate.is_file() and candidate.suffix.lower() in suffixes and ".git" not in candidate.parts:
            tex_files.append(candidate)
    return sorted(tex_files)


def uncommented_tex(line: str) -> str:
    output: list[str] = []
    escaped = False
    for character in line:
        if character == "\\":
            escaped = not escaped
            output.append(character)
            continue
        if character == "%" and not escaped:
            break
        escaped = False
        output.append(character)
    return "".join(output)


def normalize_include_target(target: str) -> str:
    cleaned = target.strip().replace("\\", "/")
    cleaned = re.sub(r"/{2,}", "/", cleaned)
    cleaned = re.sub(r"^\./", "", cleaned)
    cleaned = cleaned.strip("/")
    if cleaned.endswith(".tex"):
        cleaned = cleaned[:-4]
    return cleaned


def extract_include_references(text: str) -> set[str]:
    references: set[str] = set()
    for raw_line in text.splitlines():
        body = uncommented_tex(raw_line)
        for match in IMPORT_RE.finditer(body):
            base_dir = normalize_include_target(match.group(2))
            target = normalize_include_target(match.group(3))
            joined = "/".join(part for part in (base_dir, target) if part)
            if joined:
                references.add(joined)
            if target:
                references.add(target)
        for match in INCLUDE_RE.finditer(body):
            target = normalize_include_target(match.group(2))
            if target:
                references.add(target)
    return references


def choose_main_tex(root: str | Path) -> tuple[Optional[Path], list[dict[str, object]]]:
    root_path = Path(root)
    tex_files = find_tex_files(root_path)
    referenced_targets: set[str] = set()
    cached_text: dict[Path, str] = {}
    for tex_file in tex_files:
        text = read_text(tex_file)
        cached_text[tex_file] = text
        referenced_targets.update(extract_include_references(text))

    candidates: list[dict[str, object]] = []
    for tex_file in tex_files:
        text = cached_text[tex_file]
        score = 0
        if "\\documentclass" in text:
            score += 1000
        if "\\begin{document}" in text:
            score += 500
        if re.search(r"\\title\s*\{", text):
            score += 30
        if "\\maketitle" in text:
            score += 25
        if tex_file.parent == root_path:
            score += 40
        score += min(len(text), 300000) // 500
        lowered = tex_file.name.lower()
        stem = tex_file.stem.lower()
        rel_without_suffix = normalize_include_target(str(tex_file.relative_to(root_path)))
        if any(token == stem for token in ("main", "paper", "manuscript", "article")):
            score += 80
        if any(token in stem for token in ("response", "rebuttal", "camera-ready")):
            score -= 140
        if any(token in lowered for token in ("supp", "appendix", "supplement", "si")):
            score -= 120
        candidate_keys = {
            normalize_include_target(tex_file.name),
            normalize_include_target(tex_file.stem),
            rel_without_suffix,
        }
        if any(key and key in referenced_targets for key in candidate_keys):
            score -= 180
        candidates.append({"path": str(tex_file), "score": score})
    candidates.sort(key=lambda item: int(item["score"]), reverse=True)
    main_tex = Path(candidates[0]["path"]) if candidates else None
    return main_tex, candidates


def resolve_tex_path(
    base_dir: str | Path,
    target: str,
    *,
    source_root: str | Path | None = None,
    extra_roots: list[str | Path] | None = None,
    return_details: bool = False,
) -> Optional[Path] | tuple[Optional[Path], Optional[str]]:
    root = Path(base_dir)
    cleaned = target.strip()
    if not cleaned:
        return (None, None) if return_details else None

    search_roots: list[tuple[str, Path]] = [("relative", root)]
    if source_root is not None:
        source_path = Path(source_root)
        if source_path not in {item[1] for item in search_roots}:
            search_roots.append(("source_root", source_path))
    for extra_root in extra_roots or []:
        extra_path = Path(extra_root)
        if extra_path not in {item[1] for item in search_roots}:
            search_roots.append(("extra_root", extra_path))

    for label, search_root in search_roots:
        candidates = [search_root / cleaned]
        if not Path(cleaned).suffix:
            candidates.append((search_root / cleaned).with_suffix(".tex"))
        for candidate in candidates:
            if candidate.exists():
                resolved = candidate.resolve()
                return (resolved, label) if return_details else resolved
    return (None, None) if return_details else None
