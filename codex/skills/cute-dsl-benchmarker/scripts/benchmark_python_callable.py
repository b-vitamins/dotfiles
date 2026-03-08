#!/usr/bin/env python3
"""Benchmark a Python callable returned by a small driver module.

The driver module must export a factory function, by default `build_benchmark`,
with the signature:

    def build_benchmark(argv: list[str]) -> dict:
        ...
        return {
            "callable": fn,            # required, zero-arg callable
            "label": "optional-name",
            "warmup": 10,
            "iterations": 100,
            "metadata": {...},
            "cuda_required": True,
        }
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import statistics
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from types import ModuleType
from typing import Any

import torch


def load_module(module_file: Path) -> ModuleType:
    spec = importlib.util.spec_from_file_location(module_file.stem, module_file)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Failed to load module spec from {module_file}")
    module = importlib.util.module_from_spec(spec)
    sys.path.insert(0, str(module_file.parent))
    try:
        spec.loader.exec_module(module)
    finally:
        if sys.path and sys.path[0] == str(module_file.parent):
            sys.path.pop(0)
    return module


def time_once(fn, iterations: int, cuda_required: bool) -> float:
    if cuda_required and not torch.cuda.is_available():
        raise RuntimeError("CUDA is required but torch.cuda.is_available() is false")
    if torch.cuda.is_available():
        torch.cuda.synchronize()
        start = torch.cuda.Event(enable_timing=True)
        end = torch.cuda.Event(enable_timing=True)
        start.record()
        for _ in range(iterations):
            fn()
        end.record()
        torch.cuda.synchronize()
        return float(start.elapsed_time(end) / max(1, iterations))

    started = time.perf_counter()
    for _ in range(iterations):
        fn()
    ended = time.perf_counter()
    return (ended - started) * 1000.0 / max(1, iterations)


def build_report(
    label: str,
    module_file: Path,
    factory: str,
    iterations: int,
    warmup: int,
    repeat: int,
    metadata: dict[str, Any],
    samples_ms: list[float],
    driver_argv: list[str],
) -> dict[str, Any]:
    mean_ms = statistics.fmean(samples_ms)
    median_ms = statistics.median(samples_ms)
    stdev_ms = statistics.stdev(samples_ms) if len(samples_ms) > 1 else 0.0
    return {
        "label": label,
        "module_file": str(module_file),
        "factory": factory,
        "driver_argv": driver_argv,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "iterations": iterations,
        "warmup": warmup,
        "repeat": repeat,
        "samples_ms": samples_ms,
        "summary": {
            "min_ms": min(samples_ms),
            "max_ms": max(samples_ms),
            "mean_ms": mean_ms,
            "median_ms": median_ms,
            "stdev_ms": stdev_ms,
        },
        "metadata": metadata,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("module_file", type=Path, help="Path to the benchmark driver module.")
    parser.add_argument(
        "--factory",
        default="build_benchmark",
        help="Factory function name inside the driver module.",
    )
    parser.add_argument("--label", default=None, help="Override report label.")
    parser.add_argument("--warmup", type=int, default=None, help="Override warmup count.")
    parser.add_argument(
        "--iterations",
        type=int,
        default=None,
        help="Override iterations per repeat.",
    )
    parser.add_argument("--repeat", type=int, default=7, help="Number of timed repeats.")
    parser.add_argument(
        "--cooldown-ms",
        type=float,
        default=0.0,
        help="Sleep after each timed repeat.",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Write reports here. Defaults to /tmp/cute-dsl-bench-<timestamp>.",
    )
    parser.add_argument(
        "driver_argv",
        nargs=argparse.REMAINDER,
        help="Arguments passed through to the driver factory after `--`.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    driver_argv = [arg for arg in args.driver_argv if arg != "--"]
    module = load_module(args.module_file.resolve())
    if not hasattr(module, args.factory):
        raise RuntimeError(f"{args.module_file} does not export {args.factory}")

    config = getattr(module, args.factory)(driver_argv)
    if not isinstance(config, dict):
        raise TypeError(f"{args.factory} must return a dict")
    if "callable" not in config:
        raise KeyError(f"{args.factory} must return a dict containing 'callable'")

    fn = config["callable"]
    label = args.label or config.get("label") or args.module_file.stem
    warmup = int(args.warmup if args.warmup is not None else config.get("warmup", 10))
    iterations = int(
        args.iterations if args.iterations is not None else config.get("iterations", 100)
    )
    repeat = int(args.repeat)
    cuda_required = bool(config.get("cuda_required", True))
    metadata = dict(config.get("metadata", {}))
    metadata.setdefault("cuda_available", torch.cuda.is_available())

    if repeat <= 0:
        raise ValueError("--repeat must be positive")
    if warmup < 0 or iterations <= 0:
        raise ValueError("warmup must be >= 0 and iterations must be > 0")

    for _ in range(warmup):
        fn()
    if torch.cuda.is_available():
        torch.cuda.synchronize()

    samples_ms: list[float] = []
    for _ in range(repeat):
        samples_ms.append(time_once(fn, iterations, cuda_required))
        if args.cooldown_ms > 0:
            time.sleep(args.cooldown_ms / 1000.0)

    out_dir = args.out_dir
    if out_dir is None:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        out_dir = Path("/tmp") / f"cute-dsl-bench-{label}-{stamp}"
    out_dir.mkdir(parents=True, exist_ok=True)

    report = build_report(
        label=label,
        module_file=args.module_file.resolve(),
        factory=args.factory,
        iterations=iterations,
        warmup=warmup,
        repeat=repeat,
        metadata=metadata,
        samples_ms=samples_ms,
        driver_argv=driver_argv,
    )
    report_path = out_dir / "benchmark.json"
    report_path.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")

    summary = report["summary"]
    print(f"label: {label}")  # noqa: print
    print(f"mean_ms: {summary['mean_ms']:.6f}")  # noqa: print
    print(f"median_ms: {summary['median_ms']:.6f}")  # noqa: print
    print(f"min_ms: {summary['min_ms']:.6f}")  # noqa: print
    print(f"max_ms: {summary['max_ms']:.6f}")  # noqa: print
    print(f"stdev_ms: {summary['stdev_ms']:.6f}")  # noqa: print
    print(f"report: {report_path}")  # noqa: print

if __name__ == "__main__":
    sys.exit(main())
