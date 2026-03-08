#!/usr/bin/env python3
"""Sync local CuTe DSL docs/examples into the CuTe skill suite.

This script keeps the skills self-contained and version-controlled while still
using `/home/b/projects/cute-docs` as the canonical source of truth.
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
from pathlib import Path

MANIFEST_TEXT = """(specifications->manifest '("python" "python-pytorch-cuda"
                            "python-einops"
                            "python-numpy@1"
                            "python-pytest"
                            "python-nvidia-cutlass-dsl"
                            "python-cuda-python"
                            "python-nvtx"
                            "python-cupti-python"
                            "python-ncu-report"
                            "python-nsight-python"
                            "cuda-toolkit"
                            "cutlass-headers"
                            "cutlass-tools"
                            "cpp-httplib"
                            "gcc-toolchain@14"
                            "ninja"
                            "cmake"
                            "pybind11"
                            "onednn"
                            "cudnn"
                            "unzip"
                            "python-ruff"
                            "node-pyright"))
"""

GUIX_RUN_TEXT = """#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exec guix shell -m "$ROOT/manifest.scm" -- "$@"
"""

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


def copy_file(src_root: Path, dst_root: Path, relpath: str) -> None:
    src = src_root / relpath
    dst = dst_root / relpath
    if not src.exists():
        raise FileNotFoundError(f"Missing source file: {src}")
    dst.parent.mkdir(parents=True, exist_ok=True)
    if src.suffix.lower() == ".md":
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
        f"- Generated from `{docs_root}` and `{examples_root}`.",
        "- All commands in this skill must run through `scripts/guix-run`.",
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

    (skill_dir / "manifest.scm").write_text(MANIFEST_TEXT, encoding="utf-8")
    guix_run = skill_dir / "scripts" / "guix-run"
    guix_run.parent.mkdir(parents=True, exist_ok=True)
    guix_run.write_text(GUIX_RUN_TEXT, encoding="utf-8")
    guix_run.chmod(0o755)

    docs_dst = skill_dir / "references" / "docs"
    for relpath in docs:
        copy_file(docs_root, docs_dst, relpath)
        for asset in referenced_assets(docs_root, relpath):
            copy_file(docs_root, docs_dst, asset)

    examples_dst = skill_dir / "references" / "examples"
    for relpath in examples:
        copy_file(examples_root, examples_dst, relpath)

    catalog = build_catalog(skill_name, docs, examples, docs_root, examples_root)
    (skill_dir / "references" / "catalog.md").write_text(catalog, encoding="utf-8")

    return {
        "skill": skill_name,
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
        "--docs-root",
        type=Path,
        default=Path("/home/b/projects/cute-docs/docs/cutlass/cute-dsl/latest"),
        help="Local CuTe docs snapshot root.",
    )
    parser.add_argument(
        "--examples-root",
        type=Path,
        default=Path("/home/b/projects/cute-docs/examples"),
        help="Local CuTe examples root.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    docs_root = args.docs_root.resolve()
    examples_root = args.examples_root.resolve()
    skills_root = args.skills_root.resolve()

    coverage_report: list[dict[str, object]] = []
    for skill_name in sorted(SKILL_COVERAGE):
        skill_dir = skills_root / skill_name
        if not skill_dir.exists():
            raise FileNotFoundError(f"Skill directory not found: {skill_dir}")
        coverage_report.append(sync_skill(skill_dir, docs_root, examples_root))

    report_path = Path(__file__).resolve().parent / "suite-coverage.json"
    report_path.write_text(
        json.dumps(
            {
                "docs_root": str(docs_root),
                "examples_root": str(examples_root),
                "skills": coverage_report,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    print(f"Wrote {report_path}")  # noqa: print

if __name__ == "__main__":
    main()
