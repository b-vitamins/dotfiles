# CuTe DSL environment

The CuTe skills work inside whatever Python environment the project already
uses. They never create, select, or modify one on their own.

## Requirements

From NVIDIA's quick start (see `docs/quick_start.md` in the doc navigator):

- Linux on x86_64 or aarch64 with an NVIDIA GPU.
- An NVIDIA driver that supports CUDA 12.9, or CUDA 13.x for the `cu13` wheels.
- Python 3.10-3.14.
- The `nvidia-cutlass-dsl` wheel. It includes everything needed to generate
  kernels; a separate CUDA toolkit is not required for that.
- PyTorch with CUDA for most examples and for the benchmark harness; `cuda-python`
  where an example imports it.
- Profiling only: Nsight Compute (`ncu`) and Nsight Systems (`nsys`) on `PATH`,
  from a CUDA toolkit or NVIDIA's standalone installers.

## Check before working

Run the bundled probe with the interpreter that will run the code:

```bash
python3 <skill-dir>/scripts/check_env.py              # add --profiling to require ncu/nsys
```

It reports the host, Python, CuTe DSL version, visible GPUs with their compute
capability, PyTorch, and the profilers, and exits non-zero when a required piece
is missing. If it fails, report what is missing and ask before installing
packages or changing the environment.

If this machine has no suitable GPU, run the work on one that does, such as a
remote GPU host, instead of reasoning about results that were never measured.

## Running commands

Run every command, including the bundled scripts, with the same environment as
the code under test: activate it first, or prefix commands with its runner
(`uv run`, `conda run -n <env>`, `guix shell -m <manifest> --`, `docker exec
<container>`, and so on). Do not put an extra `bash -lc` layer between such a
runner and the real command; it obscures the argv and the exit status.

## Setup recipes

Pick the one that matches how the project already manages dependencies.

Virtual environment (pip or uv):

```bash
python3 -m venv .venv && . .venv/bin/activate
pip install nvidia-cutlass-dsl torch            # "nvidia-cutlass-dsl[cu13]" for CUDA 13
# or: uv venv && uv pip install nvidia-cutlass-dsl torch
```

Examples from NVIDIA's `main` branch can need a newer build than the last
stable wheel; NVIDIA's installer covers that case:

```bash
git clone https://github.com/NVIDIA/cutlass.git
./cutlass/python/CuTeDSL/setup.sh --cu12        # or --cu13
```

Conda: create an environment with a supported Python, then install the same
packages with `pip` inside it.

Containers: start from an image with CUDA-enabled PyTorch, such as NVIDIA's
PyTorch container, and `pip install nvidia-cutlass-dsl` inside it.

Guix: `<skill-dir>/assets/guix-manifest.scm` lists equivalent packages. Upstream
Guix does not package the NVIDIA stack, so it needs a channel that provides
these packages:

```bash
guix shell -m <skill-dir>/assets/guix-manifest.scm -- python3 my_driver.py
```
