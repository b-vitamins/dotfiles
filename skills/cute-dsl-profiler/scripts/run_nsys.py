#!/usr/bin/env python3
"""Run Nsight Systems against an arbitrary command and capture stats under /tmp."""

from __future__ import annotations

import argparse
import subprocess
from datetime import datetime, timezone
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--nsys", default="nsys", help="Nsight Systems executable.")
    parser.add_argument("--label", default="profile", help="Output prefix label.")
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Output directory. Defaults to /tmp/cute-dsl-nsys-<timestamp>.",
    )
    parser.add_argument(
        "--trace",
        default="cuda,nvtx,osrt",
        help="Comma-separated trace domains for nsys profile.",
    )
    parser.add_argument(
        "--sample",
        default="none",
        help="Sampling mode for nsys profile.",
    )
    parser.add_argument(
        "--capture-range",
        default="cudaProfilerApi",
        help="Capture range mode. Defaults to cudaProfilerApi for focused driver profiling.",
    )
    parser.add_argument(
        "--capture-range-end",
        default="stop",
        help="Capture-range end behavior when capture-range is enabled.",
    )
    parser.add_argument(
        "command",
        nargs=argparse.REMAINDER,
        help="Command to profile after `--`.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    command = [arg for arg in args.command if arg != "--"]
    if not command:
        raise SystemExit("A command is required after `--`.")

    out_dir = args.out_dir
    if out_dir is None:
        stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        out_dir = Path("/tmp") / f"cute-dsl-nsys-{args.label}-{stamp}"
    out_dir.mkdir(parents=True, exist_ok=True)
    prefix = out_dir / args.label

    profile_cmd = [
        args.nsys,
        "profile",
        "--force-overwrite=true",
        f"--trace={args.trace}",
        f"--sample={args.sample}",
        "--stats=true",
        "-o",
        str(prefix),
    ]
    if args.capture_range != "none":
        profile_cmd.extend(
            [
                f"--capture-range={args.capture_range}",
                f"--capture-range-end={args.capture_range_end}",
            ]
        )
    profile_cmd.extend(command)
    prof = subprocess.run(profile_cmd, capture_output=True, text=True)
    profile_log = (prof.stdout or "") + (prof.stderr or "")
    (out_dir / "profile.log").write_text(profile_log, encoding="utf-8")
    if prof.returncode != 0:
        raise RuntimeError(f"nsys profile failed:\n{profile_log}")

    report_path = prefix.with_suffix(".nsys-rep")
    stats_cmd = [
        args.nsys,
        "stats",
        "--force-export=true",
        "--report",
        "cuda_gpu_kern_sum,nvtx_sum,osrt_sum",
        str(report_path),
    ]
    stats = subprocess.run(stats_cmd, capture_output=True, text=True)
    stats_log = (stats.stdout or "") + (stats.stderr or "")
    (out_dir / "stats.txt").write_text(stats_log, encoding="utf-8")
    if stats.returncode != 0:
        raise RuntimeError(f"nsys stats failed:\n{stats_log}")

    print(f"out_dir: {out_dir}")  # noqa: print
    print(f"report: {report_path}")  # noqa: print
    print(f"profile_log: {out_dir / 'profile.log'}")  # noqa: print
    print(f"stats_txt: {out_dir / 'stats.txt'}")  # noqa: print

if __name__ == "__main__":
    main()
