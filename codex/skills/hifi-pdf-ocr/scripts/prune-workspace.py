#!/usr/bin/env python3
"""Prune a finished OCR workspace to minimal handoff artifacts."""

from __future__ import annotations

import argparse
import re
import shutil
from pathlib import Path


INPUT_RE = re.compile(r"\\(?:input|include)\{([^}]+)\}")
GRAPHICS_RE = re.compile(r"\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}")
IMAGE_EXTS = (".pdf", ".png", ".jpg", ".jpeg", ".eps")


def resolve_local_path(base_dir: Path, workspace: Path, raw: str, default_suffix: str | None) -> Path | None:
    raw_path = Path(raw)
    candidates: list[Path] = []

    if raw_path.suffix:
        candidates.extend([base_dir / raw_path, workspace / raw_path])
    else:
        if default_suffix is not None:
            raw_path = raw_path.with_suffix(default_suffix)
        candidates.extend([base_dir / raw_path, workspace / raw_path])

    for candidate in candidates:
        if not candidate.exists():
            continue
        resolved = candidate.resolve(strict=False)
        if resolved == workspace or workspace in resolved.parents:
            return resolved
    return None


def discover_references(tex_path: Path, workspace: Path) -> tuple[set[Path], set[Path]]:
    text = tex_path.read_text(encoding="utf-8", errors="ignore")
    tex_refs: set[Path] = set()
    asset_refs: set[Path] = set()

    for raw in INPUT_RE.findall(text):
        ref = resolve_local_path(tex_path.parent, workspace, raw, ".tex")
        if ref is not None:
            tex_refs.add(ref)

    for raw in GRAPHICS_RE.findall(text):
        raw_path = Path(raw)
        candidates = [raw_path] if raw_path.suffix else [raw_path.with_suffix(ext) for ext in IMAGE_EXTS]
        for candidate in candidates:
            ref = resolve_local_path(tex_path.parent, workspace, str(candidate), None)
            if ref is not None:
                asset_refs.add(ref)
                break

    return tex_refs, asset_refs


def collect_keep_set(workspace: Path, source_rel: str, transcriptions_dir: str) -> set[Path]:
    keep: set[Path] = set()

    source_pdf = workspace / source_rel
    if not source_pdf.exists():
        raise FileNotFoundError(f"missing source PDF: {source_pdf}")
    keep.add(source_pdf)

    tx_dir = workspace / transcriptions_dir
    if not tx_dir.is_dir():
        raise FileNotFoundError(f"missing transcriptions directory: {tx_dir}")

    queue = sorted(path for path in tx_dir.iterdir() if path.is_file() and path.suffix == ".tex")
    if not queue:
        raise FileNotFoundError(f"no direct child .tex files found in {tx_dir}")

    seen_tex: set[Path] = set()
    while queue:
        tex_path = queue.pop(0)
        if tex_path in seen_tex:
            continue
        seen_tex.add(tex_path)
        keep.add(tex_path)

        tex_refs, asset_refs = discover_references(tex_path, workspace)
        keep.update(asset_refs)
        for ref in sorted(tex_refs):
            if ref not in seen_tex:
                queue.append(ref)

    return keep


def remove_path(path: Path, dry_run: bool) -> None:
    label = "REMOVE_DIR" if path.is_dir() and not path.is_symlink() else "REMOVE_FILE"
    print(f"{label} {path}")  # noqa: print
    if dry_run:
        return
    if path.is_dir() and not path.is_symlink():
        shutil.rmtree(path)
    else:
        path.unlink()


def prune_workspace(workspace: Path, keep: set[Path], dry_run: bool) -> tuple[int, int]:
    removed_files = 0
    removed_dirs = 0

    all_paths = sorted(workspace.rglob("*"), key=lambda p: (len(p.parts), str(p)), reverse=True)
    for path in all_paths:
        if path == workspace:
            continue
        if path.is_dir() and not path.is_symlink():
            if any(k == path or path in k.parents for k in keep):
                continue
            if any(path.iterdir()):
                continue
            remove_path(path, dry_run)
            removed_dirs += 1
            continue

        if path in keep:
            continue
        remove_path(path, dry_run)
        removed_files += 1

    return removed_files, removed_dirs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workspace", required=True, help="Workspace to prune.")
    parser.add_argument("--source-rel", default="source/book.pdf", help="Relative path to the canonical source PDF.")
    parser.add_argument(
        "--transcriptions-dir",
        default="transcriptions",
        help="Directory whose direct child .tex files define the kept deliverables.",
    )
    parser.add_argument("--dry-run", action="store_true", help="Show what would be removed without deleting anything.")
    args = parser.parse_args()

    workspace = Path(args.workspace).resolve()
    if not workspace.is_dir():
        raise SystemExit(f"workspace does not exist: {workspace}")

    keep = collect_keep_set(workspace, args.source_rel, args.transcriptions_dir)
    print("KEEP_SET")  # noqa: print
    for path in sorted(keep):
        print(path)  # noqa: print

    removed_files, removed_dirs = prune_workspace(workspace, keep, args.dry_run)
    print(  # noqa: print
        f"SUMMARY removed_files={removed_files} removed_dirs={removed_dirs} dry_run={'yes' if args.dry_run else 'no'}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
