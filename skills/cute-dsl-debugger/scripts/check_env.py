#!/usr/bin/env python3
"""Report whether the current Python environment can build and run CuTe DSL kernels.

Run it with the interpreter you intend to use for the work (a virtualenv,
conda env, container, `guix shell`, or a remote GPU host). It only inspects
the environment; it never installs anything. Exit status is 0 when every
required check passes and 1 otherwise.

Requirements follow NVIDIA's CuTe DSL quick start: Linux on x86_64 or
aarch64, Python 3.10-3.14, the `nvidia-cutlass-dsl` wheel, and an NVIDIA
driver new enough for CUDA 12.9 (or 13.x with the `cu13` extra).
"""

from __future__ import annotations

import argparse
import importlib
import importlib.metadata
import json
import platform
import shutil
import subprocess
import sys

PYTHON_MIN = (3, 10)
PYTHON_MAX = (3, 14)
SUPPORTED_MACHINES = {"x86_64", "amd64", "aarch64", "arm64"}


def dist_version(name: str) -> str | None:
    try:
        return importlib.metadata.version(name)
    except importlib.metadata.PackageNotFoundError:
        return None


def first_line(command: list[str]) -> str | None:
    try:
        result = subprocess.run(command, capture_output=True, text=True, timeout=30, check=False)
    except (OSError, subprocess.TimeoutExpired):
        return None
    lines = [line.strip() for line in (result.stdout + result.stderr).splitlines() if line.strip()]
    return lines[-1] if lines and result.returncode == 0 else None


def try_import(module: str) -> str | None:
    """Return None if `module` imports, else a one-line error description."""
    try:
        importlib.import_module(module)
    except Exception as exc:  # noqa: BLE001 - any import failure is a finding
        message = str(exc).splitlines()[0] if str(exc) else ""
        return f"{type(exc).__name__}: {message}".rstrip(": ")
    return None


def collect(profiling: bool) -> list[dict[str, object]]:
    checks: list[dict[str, object]] = []

    def add(name: str, ok: bool, detail: str, required: bool = True) -> None:
        checks.append({"check": name, "ok": ok, "required": required, "detail": detail})

    system, machine = platform.system(), platform.machine().lower()
    add("host", system == "Linux" and machine in SUPPORTED_MACHINES,
        f"{system} {machine} (CuTe DSL supports Linux x86_64/aarch64)")

    version = sys.version_info[:2]
    add("python", PYTHON_MIN <= version <= PYTHON_MAX,
        f"{platform.python_version()} at {sys.executable} "
        f"(supported {PYTHON_MIN[0]}.{PYTHON_MIN[1]}-{PYTHON_MAX[0]}.{PYTHON_MAX[1]})")

    cutlass_version = dist_version("nvidia-cutlass-dsl")
    import_error = try_import("cutlass.cute") if cutlass_version else "not installed"
    add("nvidia-cutlass-dsl", cutlass_version is not None and import_error is None,
        f"{cutlass_version or 'missing'}" + (f"; import cutlass.cute failed: {import_error}"
                                             if cutlass_version and import_error else ""))

    gpus = first_line(["nvidia-smi", "--query-gpu=name,compute_cap,driver_version",
                       "--format=csv,noheader"]) if shutil.which("nvidia-smi") else None
    torch_detail, torch_gpu = "missing", False
    if dist_version("torch"):
        try:
            import torch  # noqa: PLC0415 - optional dependency

            torch_gpu = bool(torch.cuda.is_available())
            torch_detail = f"{torch.__version__} (CUDA {torch.version.cuda})"
            if torch_gpu:
                major, minor = torch.cuda.get_device_capability(0)
                torch_detail += f"; device 0: {torch.cuda.get_device_name(0)} sm_{major}{minor}"
            else:
                torch_detail += "; no CUDA device visible"
        except Exception as exc:  # noqa: BLE001
            torch_detail = f"import failed: {type(exc).__name__}: {exc}"
    add("gpu", bool(gpus) or torch_gpu,
        gpus or ("visible through torch" if torch_gpu else "no NVIDIA GPU visible"))
    add("torch", torch_gpu, torch_detail, required=False)
    add("cuda-python", dist_version("cuda-python") is not None,
        dist_version("cuda-python") or "missing", required=False)

    for tool in ("ncu", "nsys"):
        path = shutil.which(tool)
        detail = f"{first_line([tool, '--version']) or 'version unknown'} at {path}" if path else "not on PATH"
        add(tool, path is not None, detail, required=profiling)
    return checks


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--profiling", action="store_true",
                        help="Also require Nsight Compute (ncu) and Nsight Systems (nsys).")
    parser.add_argument("--json", action="store_true", help="Print machine-readable results.")
    args = parser.parse_args()

    checks = collect(args.profiling)
    ready = all(check["ok"] for check in checks if check["required"])
    if args.json:
        print(json.dumps({"ready": ready, "checks": checks}, indent=2))  # noqa: print
    else:
        for check in checks:
            status = "ok" if check["ok"] else ("MISSING" if check["required"] else "absent")
            print(f"{status:8} {check['check']:20} {check['detail']}")  # noqa: print
        print("ready" if ready else "not ready: fix the MISSING items above")  # noqa: print
    return 0 if ready else 1


if __name__ == "__main__":
    sys.exit(main())
