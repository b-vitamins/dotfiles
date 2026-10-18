#!/usr/bin/env python3
"""Sync local CuTe DSL docs/examples into the CuTe skill suite.

This script keeps the skills self-contained and version-controlled while a
cute-docs checkout stays the canonical source of truth. By default that is
`$CUTE_DOCS_ROOT`, or a `cute-docs` checkout next to this repository.

Skills that run kernels also receive the shared environment probe and setup
reference from this directory; they assume no particular package manager.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
from pathlib import Path

SUPPORT_DIR = Path(__file__).resolve().parent
DEFAULT_CUTE_DOCS_ROOT = Path(
    os.environ.get("CUTE_DOCS_ROOT", SUPPORT_DIR.parents[2] / "cute-docs")
)

# Skills that only read the corpus and need no GPU environment.
DOCS_ONLY = {"cute-dsl-doc-navigator"}

# Shared files copied into every skill that builds or runs kernels.
ENVIRONMENT_FILES = {
    "scripts/check_env.py": SUPPORT_DIR / "check_env.py",
    "references/environment.md": SUPPORT_DIR / "environment.md",
    "assets/guix-manifest.scm": SUPPORT_DIR / "guix-manifest.scm",
}

# Files from the earlier Guix-only layout, removed on every sync.
STALE_FILES = ("manifest.scm", "scripts/guix-run")

COVERAGE_REPORT = SUPPORT_DIR / "suite-coverage.json"

ALL = "__all__"

SKILL_COVERAGE = {
    "cute-dsl-doc-navigator": {
        "docs": ALL,
        "examples": ALL,
    },
    "cute-dsl-layout-engineer": {
        "docs": [
            "overview.md",
            "quick_start.md",
            "guide/dsl_introduction.md",
            "guide/dsl_control_flow.md",
            "guide/dsl_dynamic_layout.md",
            "guide/dsl_code_generation.md",
            "guide/notebooks.md",
            "api/cute.md",
            "api/cute_arch.md",
            "api/cute_runtime.md",
            "api/pipeline.md",
            "api/cute_nvgpu_cpasync.md",
            "api/cute_nvgpu_warp.md",
            "api/utils.md",
        ],
        "examples": [
            "print.ipynb",
            "data_types.ipynb",
            "tensor.ipynb",
            "tensorssa.ipynb",
            "cute_layout_algebra.ipynb",
            "elementwise_add.ipynb",
            "elementwise_add.py",
            "elementwise_apply.py",
            "flash_attention_v2.py",
        ],
    },
    "cute-dsl-kernel-author": {
        "docs": [
            "overview.md",
            "quick_start.md",
            "functionality.md",
            "guide/dsl_introduction.md",
            "guide/dsl_control_flow.md",
            "guide/dsl_code_generation.md",
            "guide/dsl_jit_arg_generation.md",
            "guide/dsl_jit_compilation_options.md",
            "guide/framework_integration.md",
            "guide/dsl_ahead_of_time_compilation.md",
            "guide/compile_with_tvm_ffi.md",
            "api/cute.md",
            "api/cute_runtime.md",
            "api/pipeline.md",
            "api/cute_nvgpu_cpasync.md",
            "api/utils.md",
        ],
        "examples": [
            "hello_world.ipynb",
            "print.ipynb",
            "data_types.ipynb",
            "tensor.ipynb",
            "elementwise_add.ipynb",
            "elementwise_add.py",
            "elementwise_apply.py",
            "call_from_jit.py",
            "dynamic_smem_size.py",
            "smem_allocator.py",
            "sgemm.py",
            "tensorop_gemm.py",
            "flash_attention_v2.py",
        ],
    },
    "cute-dsl-refactor-review": {
        "docs": [
            "deprecation.md",
            "faqs.md",
            "functionality.md",
            "limitations.md",
            "guide/dsl_dynamic_layout.md",
            "guide/dsl_jit_caching.md",
            "guide/framework_integration.md",
            "api/changelog.md",
            "api/cute.md",
            "api/cute_runtime.md",
            "api/utils.md",
        ],
        "examples": [
            "call_from_jit.py",
            "call_bypass_dlpack.py",
            "elementwise_add.py",
            "elementwise_apply.py",
            "sgemm.py",
            "tensorop_gemm.py",
        ],
    },
    "cute-dsl-debugger": {
        "docs": [
            "faqs.md",
            "limitations.md",
            "guide/debugging.md",
            "guide/dsl_dynamic_layout.md",
            "guide/dsl_jit_arg_generation.md",
            "guide/dsl_jit_caching.md",
            "guide/dsl_jit_compilation_options.md",
            "api/cute_runtime.md",
            "api/utils.md",
        ],
        "examples": [
            "call_bypass_dlpack.py",
            "call_from_jit.py",
            "dynamic_smem_size.py",
            "smem_allocator.py",
            "elementwise_apply.py",
            "cuda_graphs.ipynb",
        ],
    },
    "cute-dsl-benchmarker": {
        "docs": [
            "guide/autotuning_gemm.md",
            "guide/dsl_jit_caching.md",
            "guide/framework_integration.md",
            "api/cute_runtime.md",
            "api/utils.md",
        ],
        "examples": [
            "elementwise_add.py",
            "elementwise_apply.py",
            "sgemm.py",
            "tensorop_gemm.py",
            "flash_attention_v2.py",
            "all_reduce.py",
            "distributed_vector_add.py",
            "cuda_graphs.ipynb",
        ],
    },
    "cute-dsl-profiler": {
        "docs": [
            "guide/debugging.md",
            "guide/autotuning_gemm.md",
            "guide/dsl_jit_caching.md",
            "api/pipeline.md",
            "api/cute_nvgpu_cpasync.md",
            "api/cute_nvgpu_warp.md",
            "api/cute_nvgpu_warpgroup.md",
        ],
        "examples": [
            "sgemm.py",
            "tensorop_gemm.py",
            "flash_attention_v2.py",
            "all_reduce.py",
            "distributed_vector_add.py",
            "dynamic_smem_size.py",
        ],
    },
    "cute-dsl-optimizer": {
        "docs": [
            "guide/autotuning_gemm.md",
            "guide/dsl_code_generation.md",
            "guide/dsl_jit_caching.md",
            "api/pipeline.md",
            "api/cute_nvgpu.md",
            "api/cute_nvgpu_common.md",
            "api/cute_nvgpu_cpasync.md",
            "api/cute_nvgpu_tcgen05.md",
            "api/cute_nvgpu_warp.md",
            "api/cute_nvgpu_warpgroup.md",
            "api/utils_sm90.md",
            "api/utils_sm100.md",
        ],
        "examples": [
            "sgemm.py",
            "tensorop_gemm.py",
            "flash_attention_v2.py",
            "all_reduce.py",
            "distributed_vector_add.py",
            "dynamic_smem_size.py",
            "smem_allocator.py",
        ],
    },
}

IMAGE_LINK_RE = re.compile(r"!\[[^\]]*\]\(([^)]+)\)")


def list_files(root: Path) -> list[str]:
    return sorted(
        str(path.relative_to(root))
        for path in root.rglob("*")
        if path.is_file()
    )


def copy_file(
    src_root: Path,
    dst_root: Path,
    relpath: str,
    *,
    normalize_markdown: bool = True,
) -> None:
    src = src_root / relpath
    dst = dst_root / relpath
    if not src.exists():
        raise FileNotFoundError(f"Missing source file: {src}")
    dst.parent.mkdir(parents=True, exist_ok=True)
    if normalize_markdown and src.suffix.lower() == ".md":
        text = src.read_text(encoding="utf-8")
        normalized = "\n".join(line.rstrip() for line in text.splitlines())
        if text.endswith("\n"):
            normalized += "\n"
        dst.write_text(normalized, encoding="utf-8")
        shutil.copystat(src, dst)
        return
    shutil.copy2(src, dst)


def referenced_assets(src_root: Path, relpath: str) -> list[str]:
    src = src_root / relpath
    if src.suffix.lower() != ".md":
        return []
    text = src.read_text(encoding="utf-8")
    assets: list[str] = []
    for match in IMAGE_LINK_RE.finditer(text):
        target = match.group(1)
        if "://" in target or target.startswith("#"):
            continue
        asset = (src.parent / target).resolve().relative_to(src_root.resolve())
        assets.append(str(asset))
    return sorted(set(assets))


def clean_generated_tree(skill_dir: Path) -> None:
    for rel in ("references/docs", "references/examples"):
        path = skill_dir / rel
        if path.exists():
            shutil.rmtree(path)
        path.mkdir(parents=True, exist_ok=True)


def resolve_selection(root: Path, selection: str | list[str]) -> list[str]:
    if selection == ALL:
        return list_files(root)
    return sorted(selection)


def display_path(path: Path, docs_root: Path, examples_root: Path) -> str:
    """Name a corpus path relative to the cute-docs checkout, not this machine."""
    common = Path(os.path.commonpath([docs_root, examples_root]))
    if common == Path(common.anchor):
        return path.name
    return str(Path(common.name) / path.relative_to(common))


def build_catalog(
    skill_name: str,
    docs: list[str],
    examples: list[str],
    docs_root: Path,
    examples_root: Path,
) -> str:
    lines = [
        f"# {skill_name} coverage",
        "",
        f"- Generated from `{display_path(docs_root, docs_root, examples_root)}` and "
        f"`{display_path(examples_root, docs_root, examples_root)}`.",
        ("- Documentation only; no GPU environment is needed."
         if skill_name in DOCS_ONLY
         else "- Environment requirements and setup: `references/environment.md`; "
              "verify with `scripts/check_env.py`."),
        "",
        "## Docs",
        "",
    ]
    lines.extend(f"- `{rel}`" for rel in docs)
    lines.extend(["", "## Examples", ""])
    lines.extend(f"- `{rel}`" for rel in examples)
    lines.append("")
    return "\n".join(lines)


def sync_skill(skill_dir: Path, docs_root: Path, examples_root: Path) -> dict[str, object]:
    skill_name = skill_dir.name
    selection = SKILL_COVERAGE[skill_name]
    docs = resolve_selection(docs_root, selection["docs"])
    examples = resolve_selection(examples_root, selection["examples"])

    clean_generated_tree(skill_dir)

    for relpath in STALE_FILES:
        (skill_dir / relpath).unlink(missing_ok=True)
    if skill_name not in DOCS_ONLY:
        for relpath, source in ENVIRONMENT_FILES.items():
            target = skill_dir / relpath
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)

    docs_dst = skill_dir / "references" / "docs"
    for relpath in docs:
        copy_file(docs_root, docs_dst, relpath)
        for asset in referenced_assets(docs_root, relpath):
            copy_file(docs_root, docs_dst, asset)

    examples_dst = skill_dir / "references" / "examples"
    for relpath in examples:
        copy_file(examples_root, examples_dst, relpath, normalize_markdown=False)

    catalog = build_catalog(skill_name, docs, examples, docs_root, examples_root)
    (skill_dir / "references" / "catalog.md").write_text(catalog, encoding="utf-8")

    return {
        "skill": skill_name,
        "docs_root": display_path(docs_root, docs_root, examples_root),
        "examples_root": display_path(examples_root, docs_root, examples_root),
        "docs": docs,
        "examples": examples,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--skills-root",
        type=Path,
        default=Path(__file__).resolve().parent.parent,
        help="Directory containing the CuTe skill folders.",
    )
    parser.add_argument(
        "--cute-docs-root",
        type=Path,
        default=DEFAULT_CUTE_DOCS_ROOT,
        help="cute-docs checkout (default: $CUTE_DOCS_ROOT or ../cute-docs next to this repository).",
    )
    parser.add_argument(
        "--docs-root",
        type=Path,
        help="Local CuTe docs snapshot root (default: <cute-docs>/docs/cutlass/cute-dsl/latest).",
    )
    parser.add_argument(
        "--examples-root",
        type=Path,
        help="Local CuTe examples root (default: <cute-docs>/examples).",
    )
    parser.add_argument(
        "--skill",
        action="append",
        choices=sorted(SKILL_COVERAGE),
        help="Sync only this skill; repeat to select several. Defaults to the full suite.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    cute_docs_root = args.cute_docs_root
    docs_root = (args.docs_root or cute_docs_root / "docs/cutlass/cute-dsl/latest").resolve()
    examples_root = (args.examples_root or cute_docs_root / "examples").resolve()
    skills_root = args.skills_root.resolve()

    # Skills can be synced from different snapshots, so the report keeps one
    # entry per skill and replaces only the entries synced in this run.
    coverage: dict[str, dict[str, object]] = {}
    if COVERAGE_REPORT.exists():
        previous = json.loads(COVERAGE_REPORT.read_text(encoding="utf-8"))
        coverage = {entry["skill"]: entry for entry in previous.get("skills", [])
                    if "docs_root" in entry}
    for skill_name in sorted(set(args.skill or SKILL_COVERAGE)):
        skill_dir = skills_root / skill_name
        if not skill_dir.exists():
            raise FileNotFoundError(f"Skill directory not found: {skill_dir}")
        coverage[skill_name] = sync_skill(skill_dir, docs_root, examples_root)

    COVERAGE_REPORT.write_text(
        json.dumps({"skills": [coverage[name] for name in sorted(coverage)]},
                   indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    print(f"Wrote {COVERAGE_REPORT}")  # noqa: print

if __name__ == "__main__":
    main()
