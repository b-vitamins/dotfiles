#!/usr/bin/env python3
"""Run a matrix of benchmark variants via benchmark_python_callable.py."""

from __future__ import annotations

import argparse
import json
import re
import shlex
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("module_file", type=Path, help="Driver module passed to benchmark_python_callable.py.")
    parser.add_argument(
        "--variant",
        action="append",
        default=[],
        help="Variant arguments as a single shell-style string. Repeatable.",
    )
    parser.add_argument(
        "--variants-file",
        type=Path,
        default=None,
        help="JSON file describing variants. Accepts {'variants': [...]} or a raw list.",
    )
    parser.add_argument(
        "--benchmark-script",
        type=Path,
        default=Path(__file__).resolve().with_name("benchmark_python_callable.py"),
        help="Path to benchmark_python_callable.py.",
    )
    parser.add_argument("--python", default=sys.executable, help="Python executable for the subprocess.")
    parser.add_argument("--factory", default="build_benchmark")
    parser.add_argument("--label-prefix", default="matrix", help="Prefix for output directories.")
    parser.add_argument("--warmup", type=int, default=None)
    parser.add_argument("--iterations", type=int, default=None)
    parser.add_argument("--repeat", type=int, default=7)
    parser.add_argument("--cooldown-ms", type=float, default=0.0)
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Output root. Defaults to /tmp/cute-dsl-bench-matrix-<timestamp>.",
    )
    parser.add_argument(
        "--stop-on-failure",
        action="store_true",
        help="Abort the matrix after the first failing variant.",
    )
    return parser.parse_args()


def slugify(text: str) -> str:
    slug = re.sub(r"[^a-zA-Z0-9]+", "-", text).strip("-").lower()
    return slug or "variant"


def summarize_failure(stderr: str, stdout: str, returncode: int) -> str:
    text = (stderr or stdout).strip()
    if not text:
        return f"subprocess failed with return code {returncode}"
    lines = [line for line in text.splitlines() if line.strip()]
    if not lines:
        return f"subprocess failed with return code {returncode}"
    return lines[-1]


def load_variants(args: argparse.Namespace) -> list[dict[str, Any]]:
    variants: list[dict[str, Any]] = []
    for idx, raw in enumerate(args.variant, start=1):
        variants.append(
            {
                "name": f"variant-{idx}",
                "args": shlex.split(raw),
            }
        )
    if args.variants_file is not None:
        data = json.loads(args.variants_file.read_text(encoding="utf-8"))
        items = data.get("variants", data) if isinstance(data, dict) else data
        if not isinstance(items, list):
            raise TypeError("variants file must contain a list or an object with a 'variants' list")
        for idx, item in enumerate(items, start=1):
            if isinstance(item, str):
                variants.append({"name": f"file-variant-{idx}", "args": shlex.split(item)})
                continue
            if not isinstance(item, dict):
                raise TypeError("each variant entry must be a string or object")
            raw_args = item.get("args")
            if raw_args is None:
                raise KeyError("variant objects must contain 'args'")
            if isinstance(raw_args, str):
                parsed_args = shlex.split(raw_args)
            else:
                parsed_args = [str(arg) for arg in raw_args]
            variants.append(
                {
                    "name": item.get("name", f"file-variant-{idx}"),
                    "args": parsed_args,
                    "metadata": item.get("metadata", {}),
                }
            )
    if not variants:
        raise SystemExit("Provide at least one --variant or --variants-file.")
    return variants


def build_command(
    args: argparse.Namespace,
    variant: dict[str, Any],
    variant_out_dir: Path,
) -> list[str]:
    cmd = [
        args.python,
        str(args.benchmark_script),
        "--repeat",
        str(args.repeat),
        "--cooldown-ms",
        str(args.cooldown_ms),
        "--factory",
        args.factory,
        "--out-dir",
        str(variant_out_dir),
    ]
    if args.warmup is not None:
        cmd.extend(["--warmup", str(args.warmup)])
    if args.iterations is not None:
        cmd.extend(["--iterations", str(args.iterations)])
    cmd.extend([str(args.module_file.resolve()), "--", *variant["args"]])
    return cmd


def main() -> None:
    args = parse_args()
    variants = load_variants(args)
    out_dir = args.out_dir
    if out_dir is None:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        out_dir = Path("/tmp") / f"cute-dsl-bench-matrix-{args.label_prefix}-{stamp}"
    out_dir.mkdir(parents=True, exist_ok=True)

    results: list[dict[str, Any]] = []
    for index, variant in enumerate(variants, start=1):
        variant_name = variant.get("name", f"variant-{index}")
        variant_out_dir = out_dir / f"{index:02d}-{slugify(variant_name)}"
        command = build_command(args, variant, variant_out_dir)
        proc = subprocess.run(command, capture_output=True, text=True)
        record: dict[str, Any] = {
            "index": index,
            "name": variant_name,
            "args": variant["args"],
            "metadata": variant.get("metadata", {}),
            "command": command,
            "returncode": proc.returncode,
            "stdout": proc.stdout,
            "stderr": proc.stderr,
            "variant_out_dir": str(variant_out_dir),
        }
        report_path = variant_out_dir / "benchmark.json"
        if proc.returncode == 0 and report_path.exists():
            record["report"] = json.loads(report_path.read_text(encoding="utf-8"))
        results.append(record)
        if proc.returncode != 0 and args.stop_on_failure:
            break

    matrix_path = out_dir / "matrix.json"
    matrix_path.write_text(
        json.dumps(
            {
                "generated_at": datetime.now(timezone.utc).isoformat(),
                "module_file": str(args.module_file.resolve()),
                "variants_file": str(args.variants_file.resolve()) if args.variants_file else None,
                "results": results,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )

    print("| variant | status | mean_ms | note | out_dir |")  # noqa: print
    print("| --- | --- | ---: | --- | --- |")  # noqa: print
    for record in results:
        if "report" in record:
            mean_ms = record["report"]["summary"]["mean_ms"]
            note = record["report"]["label"]
            status = "ok"
            mean_text = f"{mean_ms:.6f}"
        else:
            status = f"fail({record['returncode']})"
            note = summarize_failure(
                record.get("stderr", ""),
                record.get("stdout", ""),
                record["returncode"],
            )
            mean_text = ""
        print(  # noqa: print
            f"| {record['name']} | {status} | {mean_text} | {note} | {record['variant_out_dir']} |"
        )
    print(f"matrix: {matrix_path}")  # noqa: print

if __name__ == "__main__":
    sys.exit(main())
